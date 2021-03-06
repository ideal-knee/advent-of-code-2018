FROM ubuntu:xenial

RUN apt-get update && apt-get install -y git python-pip time

RUN pip install --upgrade pip
RUN pip install --upgrade git+https://github.com/hylang/hy.git

ENV PYTHONPATH /src

ENTRYPOINT time -p hy $MAIN $ARG_1 $ARG_2
