LOCAL_BIN=/usr/src/build/dist

# development Docker image 
docker-dev:
	docker build -t ocramz/cmbnt-test-dev docker/dev




####



# docker-build:
# 	docker run --rm  -v $(pwd):/usr/src/build -v ${HOME}/.stack:/root/.stack -w /usr/src/build  -it q4uw/haskell_build_env:0.0.1

# compile-static:
# 	stack install --local-bin-path ${LOCAL_BIN} --ghc-options '-optl-static -fPIC -optc-Os'

# upx-pack:
# 	upx --best --ultra-brute dist/pred-serv

# docker-build-app:
# 	docker build --tag pred-serv:1.0 .
