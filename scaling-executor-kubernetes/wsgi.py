from app import create_app, db
from app.models import ExecutorState

from logging.config import dictConfig

dictConfig({
    'version': 1,
    'formatters': {'default': {
        'format': '[%(asctime)s] [%(process)s] %(levelname)-6s in %(module)s: %(message)s',
    }},
    'handlers': {'wsgi': {
        'class': 'logging.StreamHandler',
        'formatter': 'default'
    }},
    'root': {
        'level': 'DEBUG',
        'handlers': ['wsgi']
    }
})

app = create_app()
with app.app_context():
    db.create_all()
    dbrunning = ExecutorState()
    dbrunning.propertyname = "running"
    dbrunning.propertyvalue = str(False)
    db.session.add(dbrunning)
    db.session.commit()
