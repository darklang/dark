FROM golang:1.13-alpine3.10 AS build

RUN mkdir /src
WORKDIR /src

# Do go mod stuff first to cache if it hasn't changed
COPY go.mod .
COPY go.sum .
RUN go mod download

COPY cmd/ ./cmd
RUN CGO_ENABLED=0 GOOS=linux GOARCH=amd64 go build -o /go/bin/cloudsqltail ./cmd/cloudsqltail

# runtime container
FROM alpine:3.10

RUN apk add --update ca-certificates
# honeytail was compiled against libc, not musl, but they're compatible.
RUN mkdir /lib64 && ln -s /lib/libc.musl-x86_64.so.1 /lib64/ld-linux-x86-64.so.2

RUN mkdir /app
WORKDIR /app
COPY --from=build /go/bin/cloudsqltail ./cloudsqltail
COPY run.sh .
RUN wget -q -O honeytail https://honeycomb.io/download/honeytail/v1.1.4/honeytail-linux-amd64 && \
      echo 'd23e6f1808bdf5f99a2fd4183d143ffaf81f5b41af26c5e0e11a2d45ed04f277  honeytail' | sha256sum -c && \
      chmod 755 ./honeytail
CMD ["ash", "/app/run.sh"]
