#!/usr/bin/env bash

source /srv/flask_app/venv/bin/activate

echo "Start nginx"
service nginx start
uwsgi --ini uwsgi.ini