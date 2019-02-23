FROM alpine:3.9

RUN apk update && \
    apk upgrade --no-cache

RUN apk add --no-cache \
      openssl \
      bash
