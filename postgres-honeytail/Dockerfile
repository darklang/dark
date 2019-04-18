FROM python:3.7.3-stretch

# See run.sh for docs

# Required for google-cloud-pubsub
RUN apt update && apt install -y python-dev build-essential
RUN pip install google-cloud-pubsub==0.40.0

RUN wget -q -O honeytail https://honeycomb.io/download/honeytail/linux/1.756 \
      && echo '5666f4a2664d87db4a436ee689b13a8a8c0137535f6498c66b5df1c8b54047d2 honeytail' \
        | sha256sum -c \
      && chmod 755 ./honeytail

ADD logs.py .
ADD test_logs.py .
ADD run.sh .
RUN chmod +x run.sh

CMD ./run.sh
