CI_REGISTRY_IMAGE=registry.gitlab.com/deva-hub/elf
CI_COMMIT_REF_SLUG=master

all: image publish

.PHONY: run
run:
	docker-compose up \
		--build \
		--detach \
		datastore
	docker-compose build app
	docker-compose run \
		--rm \
		--name elf_interactive \
		--publish 4123:4123 \
		--publish 4124:4124 \
		app sh

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
		--build-arg APP_NAME=elven_gard_bastion \
		--build-arg APP_VSN=2.0.0-beta.1 \
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
	ex_doc "ElvenGardBastion" "2.0.0-beta.1" _build/dev/lib/elven_gard_bastion --output doc/elven_gard_bastion

.PHONY: changelog
changelog:
	yarn changelog \
		--infile CHANGELOG.md \
		--release-count 0 \
		--same-file

.PHONY: publish
publish:
	docker push ${CI_REGISTRY_IMAGE}:latest
