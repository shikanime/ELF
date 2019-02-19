FROM elixir:1.8-alpine AS builder

WORKDIR /opt/app/src

RUN apk update && \
    apk upgrade --no-cache && \
    apk add --no-cache \
        git \
        build-base

RUN mix local.rebar --force && \
    mix local.hex --force

COPY mix.* ./
COPY config ./config
COPY apps/elven_gard_gate/mix.exs ./apps/elven_gard_gate/
COPY apps/elven_gard_tower/mix.exs ./apps/elven_gard_tower/
COPY apps/elven_gard_gate/config/ ./apps/elven_gard_gate/config/
COPY apps/elven_gard_tower/config/  ./apps/elven_gard_tower/config/

ARG MIX_ENV=prod
ENV MIX_ENV=${MIX_ENV}

RUN mix do deps.get, deps.compile, compile

COPY apps/elven_gard_gate/lib apps/elven_gard_gate/lib
COPY apps/elven_gard_tower/lib apps/elven_gard_tower/lib

ARG APP_NAME
ARG APP_VSN
ENV APP_NAME=${APP_NAME} \
    APP_VSN=${APP_VSN}

COPY rel rel

RUN mix release --name ${APP_NAME} --verbose && \
    mkdir /opt/app/build && \
    tar -xf _build/${MIX_ENV}/rel/${APP_NAME}/releases/${APP_VSN}/${APP_NAME}.tar.gz \
        --directory /opt/app/build

FROM erlang:21-alpine

WORKDIR /opt/app/src

RUN apk update && \
    apk add --no-cache bash

COPY --from=builder /opt/app/build .

ARG APP_NAME

ENV REPLACE_OS_VARS=true \
    APP_NAME=${APP_NAME}

EXPOSE 4213
EXPOSE 4214

CMD trap 'exit' INT; /opt/app/src/bin/${APP_NAME} foreground
