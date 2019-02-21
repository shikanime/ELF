FROM elixir:1.8-alpine

RUN apk update && \
    apk upgrade --no-cache && \
    apk add --no-cache \
        git \
        build-base

RUN mix local.rebar --force && \
    mix local.hex --force
