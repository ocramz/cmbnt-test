# tag of the docker development image
IMAGE_DEV=ocramz/cmbnt-test-dev
# local directory for project binaries
ARTIFACTS_DIR=${PWD}/dist

# development Docker image 
docker-dev:
	docker build --no-cache=true -t ${IMAGE_DEV} docker/dev

docker-dev-login:
	docker run -v ${ARTIFACTS_DIR}:/dist -it ${IMAGE_DEV} /bin/bash


all:
	make compile-static
	# make pack-binary
	make docker-deploy

# 1) compile project as a statically-linked binary (~ 15 MB in size)
compile-static:
	stack install --test --local-bin-path ${ARTIFACTS_DIR} --ghc-options '-optl-static -fPIC -optc-Os'

docker-deploy:
	docker build --tag pred-serv:1.0 docker/deploy

### UPX segfaults with current compilation setup, so not compressing artifacts for now
# pack-binary:
# 	docker run -v ${ARTIFACTS_DIR}:/dist -it ${IMAGE_DEV} upx --best --ultra-brute dist/pred-serv








####

# docker-build:
# 	docker run --rm  -v $(pwd):/usr/src/build -v ${HOME}/.stack:/root/.stack -w /usr/src/build  -it q4uw/haskell_build_env:0.0.1



# upx-pack:
# 	upx --best --ultra-brute dist/pred-serv

# docker-build-app:
# 	docker build --tag pred-serv:1.0 .
