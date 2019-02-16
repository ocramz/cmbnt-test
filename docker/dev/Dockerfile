FROM alpine

RUN apk update
RUN apk add bash alpine-sdk git ca-certificates gmp-dev zlib-dev ghc

# RECENT STACK
RUN curl -sSL https://get.haskellstack.org/ | sh
RUN chmod 755 /usr/local/bin/stack

# UPX BINARY COMPRESSION APP
ADD https://github.com/lalyos/docker-upx/releases/download/v3.91/upx /usr/local/bin/upx
RUN chmod 755 /usr/local/bin/upx

CMD ["bash"]