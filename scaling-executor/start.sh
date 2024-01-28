#!/usr/bin/env bash

source /srv/flask_app/venv/bin/activate

echo "Wait for MariaDB to be up and running"
./wait-for-it.sh -t 30 $MARIADB_URL
echo "MariaDB is now up and running"

echo "Start nginx"
service nginx start
uwsgi --ini uwsgi.ini