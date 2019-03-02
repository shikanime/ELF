ARG DEPS_IMAGE
ARG BASE_IMAGE

FROM ${DEPS_IMAGE} AS builder

WORKDIR /opt/app/src

ARG APP_NAME
COPY apps apps

RUN mix compile

COPY rel rel

ARG REL_ENV=prod
ARG APP_VSN

RUN mix release --name ${APP_NAME} --env ${REL_ENV} --verbose && \
    mkdir /opt/app/build && \
    tar -xf _build/${MIX_ENV}/rel/${APP_NAME}/releases/${APP_VSN}/${APP_NAME}.tar.gz \
        --directory /opt/app/build

FROM ${BASE_IMAGE}

WORKDIR /opt/app

ENV REPLACE_OS_VARS=true
EXPOSE 45892
EXPOSE 4369
EXPOSE 49200

COPY --from=builder /opt/app/build .

ARG APP_NAME
ENV APP_NAME=${APP_NAME}

EXPOSE 4213
EXPOSE 4214

CMD trap 'exit' INT; /opt/app/bin/${APP_NAME} foreground
