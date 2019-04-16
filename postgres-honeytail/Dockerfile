FROM python:3.7.3-stretch

# See run.sh for docs

# Required for google-cloud-pubsub
RUN apt update && apt install -y python-dev build-essential
RUN pip install google-cloud-pubsub==0.40.0

RUN wget -q -O honeytail https://honeycomb.io/download/honeytail/linux/1.733 \
      && echo '8ee4089f16e41c53de3ea7ed7cd151d958ace6edcdc08ec43bf5be2630d1b61d honeytail' \
        | sha256sum -c \
      && chmod 755 ./honeytail

ADD logs.py .
ADD test_logs.py .
ADD run.sh .
RUN chmod +x run.sh

CMD ./run.sh
