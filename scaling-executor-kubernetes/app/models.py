from app import db


class ScalingRecord(db.Model):
    id = db.Column(db.Integer, primary_key=True)
    # 0 - HOLD
    # 1 - UP
    # -1 - DOWN
    # 2 - UP_DECLINED
    # -2 - DOWN_DECLINED
    # -3 - HOLD_DURING_WARMUP
    # -4 - DOWN_DURING_WARMUP
    # 3 - UP_DURING_WARMUP
    kind = db.Column(db.Integer)
    upscale_probability = db.Column(db.Float)
    downscale_probability = db.Column(db.Float)
    receive_time = db.Column(db.DateTime)


class ExecutorState(db.Model):
    id = db.Column(db.Integer, primary_key=True)
    propertyname = db.Column(db.Text, unique=True)
    propertyvalue = db.Column(db.Text)
