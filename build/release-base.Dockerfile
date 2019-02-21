FROM alpine:3.8

WORKDIR /opt/app

RUN apk update && \
    apk add --no-cache \
        --repository http://dl-cdn.alpinelinux.org/alpine/edge/main openssl && \
    apk add --no-cache \
        bash

ENV REPLACE_OS_VARS=true

EXPOSE 45892

EXPOSE 4369
EXPOSE 49200

EXPOSE 4213
EXPOSE 4214
