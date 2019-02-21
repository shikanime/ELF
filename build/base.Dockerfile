FROM alpine:3.8

WORKDIR /opt/app

RUN apk update && \
    apk upgrade --no-cache

RUN apk add --no-cache \
      bash \
      openssl
