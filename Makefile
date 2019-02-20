all: kbuild krun

.PHONY: kbuild
kbuild:
	docker build \
		--build-arg APP_NAME=nostale \
		--build-arg APP_VSN=0.1.0 \
		--build-arg REL_ENV=prod \
		-t elven_gard/nostale:0.1.0 \
		-t elven_gard/nostale:latest \
		.

.PHONY: krun
krun:
	docker run \
		--expose 4123 -p 4123:4123 \
		--expose 4124 -p 4124:4124 \
		--rm -it elven_gard/nostale:latest

.PHONY: kdeploy
kdeploy:
	helm install --name elven ./deploy
