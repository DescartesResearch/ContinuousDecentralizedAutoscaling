from app import db, kubernetes_api_ref, kubernetes_namespace
from app.models import ScalingRecord, ExecutorState
from flask import Blueprint, request

import datetime
import logging

api = Blueprint("api", __name__)

MIN_INSTANCES = 1
MAX_INSTANCES = 30
UPSCALE_COUNTER = 0
UPSCALE_LIMITED_COUNTER = 0
DOWNSCALE_COUNTER = 0
DOWNSCALE_LIMITED_COUNTER = 0
HOLD_COUNTER = 0
logger = logging.getLogger(__name__)


@api.route('/')
def index():
    return "Hello World!"


@api.route('/getStats')
def get_stats():
    return {"minimum_replicas_allowed": MIN_INSTANCES,
            "maximum_replicas_allowed": MAX_INSTANCES,
            "total_upscales": UPSCALE_COUNTER,
            "total_downscales": DOWNSCALE_COUNTER,
            "total_upscales_blocked": UPSCALE_LIMITED_COUNTER,
            "total_downscales_blocked": DOWNSCALE_LIMITED_COUNTER,
            "total_holds": HOLD_COUNTER}


@api.route('/setMin')
def set_min():
    global MIN_INSTANCES
    newMin = int(request.args.get('min'))
    if newMin >= 0:
        MIN_INSTANCES = newMin
        logger.info("New minimum set")
        return "OK", 200
    else:
        logger.info("Minimum smaller zero")
        return "ILLEGAL VALUE", 400


@api.route('/setMax')
def set_max():
    global MIN_INSTANCES, MAX_INSTANCES
    newMax = int(request.args.get('max'))
    if newMax >= 0 and newMax >= MIN_INSTANCES:
        MAX_INSTANCES = newMax
        logger.info("New maximum set")
        return "OK", 200
    else:
        logger.info("Maximum smaller zero or smaller than minimum")
        return "ILLEGAL VALUE", 400


@api.route('/<deployment>/up', methods=['POST'])
def scale_up(deployment):
    global MAX_INSTANCES, UPSCALE_COUNTER, UPSCALE_LIMITED_COUNTER
    probabilities = request.json
    current_replicas = kubernetes_api_ref.read_namespaced_deployment_scale(deployment, kubernetes_namespace).spec.replicas
    desired_replicas = current_replicas + 1
    record = ScalingRecord()
    record.upscale_probability = probabilities['up']
    record.downscale_probability = probabilities['down']
    record.receive_time = datetime.datetime.now()
    if is_executor_running():
        if desired_replicas <= MAX_INSTANCES:
            execute_scaling(deployment, desired_replicas)
            UPSCALE_COUNTER = UPSCALE_COUNTER + 1
            logger.info(f"Deployment {deployment} scaled up, upscale probability was: {str(probabilities['up'])}")
            record.kind = 1
        else:
            UPSCALE_LIMITED_COUNTER = UPSCALE_LIMITED_COUNTER + 1
            logger.info(f"Maximum replicas for {deployment} hit, upscale probability was: {str(probabilities['up'])}")
            record.kind = 2
    else:
        record.kind = 3
    db.session.add(record)
    db.session.commit()
    return "OK", 200


@api.route('/<deployment>/down', methods=['POST'])
def scale_down(deployment):
    global MIN_INSTANCES, DOWNSCALE_COUNTER, DOWNSCALE_LIMITED_COUNTER
    probabilities = request.json
    current_replicas = kubernetes_api_ref.read_namespaced_deployment_scale(deployment, kubernetes_namespace).spec.replicas
    desired_replicas = current_replicas - 1
    record = ScalingRecord()
    record.upscale_probability = probabilities['up']
    record.downscale_probability = probabilities['down']
    record.receive_time = datetime.datetime.now()
    if is_executor_running():
        if desired_replicas >= MIN_INSTANCES:
            execute_scaling(deployment, desired_replicas)
            DOWNSCALE_COUNTER = DOWNSCALE_COUNTER + 1
            logger.info(f"Deployment {deployment} scaled down, downscale probability was: {str(probabilities['down'])}")
            record.kind = -1
        else:
            DOWNSCALE_LIMITED_COUNTER = DOWNSCALE_LIMITED_COUNTER + 1
            logger.info(f"Minimum replicas for {deployment} hit, downscale probability was: {str(probabilities['down'])}")
            record.kind = -2
    else:
        record.kind = -4
    db.session.add(record)
    db.session.commit()
    return "OK", 200


@api.route('/<deployment>/hold', methods=['POST'])
def report_hold(deployment):
    global HOLD_COUNTER
    probabilities = request.json
    HOLD_COUNTER = HOLD_COUNTER + 1
    logger.info(f"Recorded hold for deployment {deployment}, upscale probability was: {str(probabilities['up'])}, downscale probability was: {str(probabilities['down'])}")
    record = ScalingRecord()
    record.upscale_probability = probabilities['up']
    record.downscale_probability = probabilities['down']
    record.receive_time = datetime.datetime.now()
    if is_executor_running():
        record.kind = 0
    else:
        record.kind = -3
    db.session.add(record)
    db.session.commit()
    return "OK", 200


@api.route('/block', methods=['GET'])
def block():
    set_executor_running(False)
    return "OK", 200


@api.route('/unblock', methods=['GET'])
def unblock():
    set_executor_running(True)
    return "OK", 200


# HELPER FUNCTIONS
def execute_scaling(deployment, desired_replicas):
    body = {
        "spec": {
            "replicas": desired_replicas
        }
    }
    kubernetes_api_ref.patch_namespaced_deployment_scale(deployment, kubernetes_namespace, body=body)


def is_executor_running() -> bool:
    running = ExecutorState.query.filter_by(propertyname="running").first()
    if running is None:
        logger.warning("Running property of executor could not be found")
        return False
    if running.propertyvalue == 'True':
        return True
    else:
        return False


def set_executor_running(running: bool) -> None:
    dbrunning = ExecutorState.query.filter_by(propertyname="running").first()
    if dbrunning is None:
        dbrunning = ExecutorState()
        dbrunning.propertyname = "running"
        dbrunning.propertyvalue = str(running)
        db.session.add(dbrunning)
    else:
        dbrunning.propertyvalue = str(running)
    db.session.commit()
