import os


class BaseConfig(object):
    TARGET_SERVICE_TIME = os.environ.get('TARGET_SERVICE_TIME', '100')
