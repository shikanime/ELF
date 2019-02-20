all: image

.PHONY: compile
compile:
	mix compile

.PHONY: image
image:
	docker build \
		--build-arg APP_NAME=gate \
		--build-arg APP_VSN=0.1.0 \
		--build-arg REL_ENV=prod \
		-t elven_gard/gate:0.1.0 \
		-t elven_gard/gate:latest \
		.

.PHONY: deploy
deploy:
	helm install \
		--name elven \
		./deploy

.PHONY: release
release:
	MIX_ENV=prod mix do deps.get, compile
	MIX_ENV=prod mix release --verbose

.PHONY: doc
doc:
	ex_doc "ElvenGardGate" "0.1.0" _build/dev/lib/elven_gard_gate --output doc/elven_gard_gate
	ex_doc "ElvenGardTower" "0.1.0" _build/dev/lib/elven_gard_tower --output doc/elven_gard_tower
