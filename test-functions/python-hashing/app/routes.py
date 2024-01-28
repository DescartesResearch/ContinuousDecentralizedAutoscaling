from flask import Blueprint, current_app

import datetime
import hashlib
import logging

api = Blueprint("api", __name__)

logging.basicConfig(level=logging.DEBUG, format='[%(asctime)s]: %(levelname)s %(message)s', datefmt='%Y-%m-%d %H:%M:%S',
                    handlers=[logging.StreamHandler()])

logger = logging.getLogger()


@api.route('/test', methods=['GET'])
def test_method():
    logger.debug("Request received")
    start_time = datetime.datetime.now()
    while True:
        current_time = datetime.datetime.now()
        if current_time - start_time > datetime.timedelta(milliseconds=int(current_app.config['TARGET_SERVICE_TIME'])):
            break
        current_time_string = current_time.strftime("%m/%d/%Y, %H:%M:%S")
        hashlib.sha256(current_time_string.encode('ascii')).hexdigest()
    return 'OK'
