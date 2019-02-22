CI_REGISTRY_IMAGE=registry.gitlab.com/deva-hub/elf
CI_COMMIT_REF_SLUG=master

all: image

.PHONY: compile
compile:
	mix compile

.PHONY: image
image:
	docker build \
			--pull \
			--file build/build.Dockerfile \
			--tag ${CI_REGISTRY_IMAGE}/ci:${CI_COMMIT_REF_SLUG}-build \
			.
	docker build \
		--pull \
		--file build/base.Dockerfile \
		--tag ${CI_REGISTRY_IMAGE}/ci:${CI_COMMIT_REF_SLUG}-base \
		.
	docker build \
		--pull \
		--build-arg BUILD_IMAGE=${CI_REGISTRY_IMAGE}/ci:${CI_COMMIT_REF_SLUG}-build \
		--file build/deps.Dockerfile \
		--tag ${CI_REGISTRY_IMAGE}/ci:${CI_COMMIT_REF_SLUG}-deps \
		.
	docker build \
		--build-arg DEPS_IMAGE=${CI_REGISTRY_IMAGE}/ci:${CI_COMMIT_REF_SLUG}-deps \
		--build-arg BASE_IMAGE=${CI_REGISTRY_IMAGE}/ci:${CI_COMMIT_REF_SLUG}-base \
		--build-arg APP_NAME=gate \
		--build-arg APP_VSN=0.1.0 \
		-t ${CI_REGISTRY_IMAGE}:latest \
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
	ex_doc "ElvenGard" "0.1.0" _build/dev/lib/elven_gard --output doc/elven_gard
	ex_doc "ElvenGardTower" "0.1.0" _build/dev/lib/elven_gard_tower --output doc/elven_gard_tower

.PHONY: changelog
changelog:
	yarn changelog \
		--infile CHANGELOG.md \
		--release-count 0 \
		--same-file
