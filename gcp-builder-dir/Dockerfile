FROM ubuntu:17.10

RUN apt-get update && \
    apt-get install \
      -y \
      --no-install-recommends \
      curl \
      apt-transport-https \
      ca-certificates

RUN curl -sSL https://www.postgresql.org/media/keys/ACCC4CF8.asc | apt-key add -
RUN echo "deb http://apt.postgresql.org/pub/repos/apt/ zesty-pgdg main" > /etc/apt/sources.list.d/pgdg.list

RUN apt-get update \
    && apt-get install \
      --no-install-recommends \
      -y \
      libcurl4-gnutls-dev=7.55.1-1ubuntu2.3 \
      locales=2.26-0ubuntu2.1 \
      libpq-dev=10.1-1.pgdg17.04+1 \
      cron \
      libev-dev=1:4.22-1 \
      postgresql-10=10.1-1.pgdg17.04+1 \
      sudo \
    && rm -rf /var/lib/apt/lists/*

# Locales
RUN locale-gen en_US.UTF-8
ENV LANG en_US.UTF-8
ENV LANGUAGE en_US:en

# Dark user
RUN adduser --disabled-password --gecos '' dark
RUN echo "dark:dark" | chpasswd && adduser dark sudo
RUN chown -R dark:dark /home/dark
RUN echo '%sudo ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers

# cron for queue workers
RUN (crontab -l 2>/dev/null; echo "*/1 * * * * /home/dark/trigger_queue_workers >> /home/dark/cron.log 2>&1") | crontab -

# Postgres
USER postgres
RUN /etc/init.d/postgresql start && \
    psql --command "CREATE USER dark WITH SUPERUSER PASSWORD 'eapnsdc';" && \
    createdb -O dark proddb

# Adjust PostgreSQL configuration so that remote connections to the
# database are possible.
#RUN echo "host all  all    0.0.0.0/0  md5" >> /etc/postgresql/9.3/main/pg_hba.conf

RUN echo "listen_addresses='*'" >> /etc/postgresql/10/main/postgresql.conf



# Set up app
USER dark
WORKDIR /home/dark

COPY . /home/dark

CMD "./run-gcp-server"
