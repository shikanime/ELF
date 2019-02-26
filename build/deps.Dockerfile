ARG BUILD_IMAGE

FROM ${BUILD_IMAGE}

WORKDIR /opt/app/src

COPY mix.* ./
COPY config ./config

COPY apps/elven_gard_bastion/mix.exs ./apps/elven_gard_bastion/
COPY apps/elven_gard_citadel/mix.exs ./apps/elven_gard_citadel/
COPY apps/elven_gard_stdlib/mix.exs ./apps/elven_gard_stdlib/

COPY apps/elven_gard_bastion/config/ ./apps/elven_gard_bastion/config/
COPY apps/elven_gard_citadel/config/  ./apps/elven_gard_citadel/config/
COPY apps/elven_gard_stdlib/config/  ./apps/elven_gard_stdlib/config/

ARG MIX_ENV=prod
ENV MIX_ENV=${MIX_ENV}

RUN mix do deps.get, deps.compile
