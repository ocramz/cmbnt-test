# tag of the docker development image
IMAGE_DEV=ocramz/cmbnt-test-dev
# local directory for project binaries
ARTIFACTS_DIR=${PWD}/docker/deploy

IMAGE=pred-serv:1.0

# development Docker image 
docker-dev-build:
	docker build --no-cache=true -t ${IMAGE_DEV} docker/dev

docker-dev-login:
	docker run -v ${ARTIFACTS_DIR}:/dist -it ${IMAGE_DEV} /bin/bash

all:
	make docker-dev-build
	make compile-static
	make docker-deploy-build

# 1) compile project as a statically-linked binary (~ 15 MB in size)
compile-static:
	stack install --test --local-bin-path ${ARTIFACTS_DIR} --ghc-options '-optl-static -fPIC -optc-Os'

# 2) build and tag a docker image with our
docker-deploy-build:
	docker build -t ${IMAGE} docker/deploy

docker-deploy-run:
	docker run -p 3000:3000 -it ${IMAGE}

### UPX segfaults with current compilation setup (?), so we skip this step for now
# pack-binary:
# 	docker run -v ${ARTIFACTS_DIR}:/dist -it ${IMAGE_DEV} upx --best --ultra-brute dist/pred-serv


