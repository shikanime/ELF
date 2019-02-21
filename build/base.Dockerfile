FROM alpine:3.8

WORKDIR /opt/app

RUN apk update && \
    apk add --no-cache \
        --repository http://dl-cdn.alpinelinux.org/alpine/edge/main openssl && \
    apk add --no-cache \
        bash
