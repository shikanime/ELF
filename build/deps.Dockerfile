ARG BUILD_IMAGE

FROM ${BUILD_IMAGE}

WORKDIR /opt/app/src

COPY mix.* ./
COPY config ./config
COPY apps/elven_gard/mix.exs ./apps/elven_gard/
COPY apps/elven_gard_tower/mix.exs ./apps/elven_gard_tower/
COPY apps/elven_gard/config/ ./apps/elven_gard/config/
COPY apps/elven_gard_tower/config/  ./apps/elven_gard_tower/config/

ARG MIX_ENV=prod
ENV MIX_ENV=${MIX_ENV}

RUN mix do deps.get, deps.compile
