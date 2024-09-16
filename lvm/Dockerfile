FROM faust.cs.fau.de:5000/lvm_deps

COPY frontend/app.py /app.py
COPY frontend/static/ /static/
COPY frontend/launch.py /launch.py

RUN mkdir -p /opt/lvm/
COPY lisp /opt/lvm/lisp

CMD ["/bin/sh", "-c", "socat -6 tcp-listen:1337,fork,reuseaddr EXEC:'python3 /launch.py',stderr & exec gunicorn --bind [::]:8000 app:app"]
