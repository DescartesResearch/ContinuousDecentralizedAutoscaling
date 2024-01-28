from flask import Blueprint

import numpy as np
import logging

api = Blueprint("api", __name__)

logging.basicConfig(level=logging.DEBUG, format='[%(asctime)s]: %(levelname)s %(message)s', datefmt='%Y-%m-%d %H:%M:%S',
                    handlers=[logging.StreamHandler()])

logger = logging.getLogger()


@api.route('/test', methods=['GET'])
def test_method():
    matrix = np.random.rand(10, 10)
    inverted_matrix = np.linalg.inv(matrix)
    return 'OK'
