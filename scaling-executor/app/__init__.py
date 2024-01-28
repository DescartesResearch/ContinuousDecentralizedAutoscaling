from flask import Flask
from flask_sqlalchemy import SQLAlchemy
from kubernetes import client, config
import os
from sqlathanor import FlaskBaseModel, initialize_flask_sqlathanor

db = initialize_flask_sqlathanor(SQLAlchemy(model_class=FlaskBaseModel))
config.load_incluster_config()
kubernetes_api_ref = client.AppsV1Api()
kubernetes_namespace = os.environ.get("NAMESPACE", "default")


class Config(object):
    SQLALCHEMY_DATABASE_URI = f"mariadb+mariadbconnector://{os.environ.get('MARIADB_USER')}:{os.environ.get('MARIADB_PASSWORD')}@{os.environ.get('MARIADB_URL')}/{os.environ.get('MARIADB_DATABASE')}"


def create_app():
    app = Flask(__name__)
    app.config.from_object(Config)
    db.init_app(app)

    with app.app_context():
        from app.scaling_executor import api

        app.register_blueprint(api)

        # Database initialization
        from app import scaling_executor
        from app import models

    return app
