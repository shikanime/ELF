ARG BUILD_IMAGE
FROM BUILD_IMAGE AS builder

WORKDIR /opt/app/src

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

ARG RELEASE_IMAGE
FROM RELEASE_IMAGE

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

CMD /opt/app/bin/${APP_NAME} foreground
