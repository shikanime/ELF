all: dbuild drun

.PHONY: dbuild
dbuild:
	docker build \
		--build-arg APP_NAME=nosale \
		--build-arg APP_VSN=0.1.0 \
		-t elven_gard/nostale:0.1.0 \
		-t elven_gard/nostale:latest \
		.

.PHONY: drun
drun:
	docker run \
		--expose 4123 -p 4123:4123 \
		--expose 4124 -p 4124:4124 \
		--rm -it elven_gard/nostale:latest
