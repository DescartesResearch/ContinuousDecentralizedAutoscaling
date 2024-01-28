FROM nginx:latest

SHELL ["/bin/bash", "-c"]
WORKDIR /srv/flask_app

COPY requirements.txt /srv/flask_app/

RUN apt-get -y update  \
    && apt-get -y install python3-pip python3-dev python3-venv build-essential libmariadb3 libmariadb-dev  \
    && python3 -m venv /srv/flask_app/venv \
    && source /srv/flask_app/venv/bin/activate \
    && python3 -m pip install --no-cache-dir -r /srv/flask_app/requirements.txt \
    && python3 -m pip install --no-cache-dir uwsgi \
    && apt-get -y purge build-essential python3-pip \
    && apt-get -y autoremove \
    && apt-get -y clean

COPY wait-for-it.sh start.sh uwsgi.ini wsgi.py /srv/flask_app/
RUN chmod +x /srv/flask_app/start.sh /srv/flask_app/wait-for-it.sh

COPY nginx.conf     /etc/nginx
COPY app/           /srv/flask_app/app/

USER nginx

CMD ["./start.sh"]
