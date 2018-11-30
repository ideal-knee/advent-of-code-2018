FROM ubuntu:xenial

RUN apt-get update && apt-get install -y git python-pip

RUN pip install --upgrade pip
RUN pip install --upgrade git+https://github.com/hylang/hy.git

ENTRYPOINT hy $MAIN
