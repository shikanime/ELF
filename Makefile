all: kbuild krun

.PHONY: kbuild
kbuild:
	docker build \
		--build-arg APP_NAME=gate \
		--build-arg APP_VSN=0.1.0 \
		--build-arg REL_ENV=prod \
		-t elven_gard/gate:0.1.0 \
		-t elven_gard/gate:latest \
		.

.PHONY: krun
krun:
	docker run \
		--expose 4123 -p 4123:4123 \
		--expose 4124 -p 4124:4124 \
		--rm -it elven_gard/gate:latest

.PHONY: kdeploy
kdeploy:
	helm install --name elven ./deploy

.PHONY: doc-gate
doc-gate:
	ex_doc "ElvenGardGate" "0.1.0" _build/dev/lib/elven_gard_gate --output doc/elven_gard_gate

.PHONY: doc-tower
doc-tower:
	ex_doc "ElvenGardTower" "0.1.0" _build/dev/lib/elven_gard_tower --output doc/elven_gard_tower
