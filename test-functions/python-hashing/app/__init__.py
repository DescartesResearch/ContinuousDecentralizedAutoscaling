from config import BaseConfig
from flask import Flask


def create_app():
    app = Flask(__name__)
    app.config.from_object(BaseConfig)

    with app.app_context():
        from app.routes import api

        app.register_blueprint(api)

        # Database initialization
        from app import routes

    return app
