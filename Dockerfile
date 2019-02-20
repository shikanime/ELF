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

RUN mix do deps.get, deps.compile

COPY apps/elven_gard_gate/lib apps/elven_gard_gate/lib
COPY apps/elven_gard_tower/lib apps/elven_gard_tower/lib

RUN mix compile

COPY rel rel

ARG REL_ENV=prod
ARG APP_NAME
ARG APP_VSN

RUN mix release --name ${APP_NAME} --env ${REL_ENV} --verbose && \
    mkdir /opt/app/build && \
    tar -xf _build/${MIX_ENV}/rel/${APP_NAME}/releases/${APP_VSN}/${APP_NAME}.tar.gz \
        --directory /opt/app/build

FROM alpine:3.8

WORKDIR /opt/app

RUN apk update && \
    apk add --no-cache \
        --repository http://dl-cdn.alpinelinux.org/alpine/edge/main openssl && \
    apk add --no-cache \
        bash

ENV REPLACE_OS_VARS=true

COPY --from=builder /opt/app/build .

EXPOSE 45892

EXPOSE 4369
EXPOSE 49200

EXPOSE 4213
EXPOSE 4214

ARG APP_NAME
ENV APP_NAME=${APP_NAME}

CMD /opt/app/bin/${APP_NAME} foreground
