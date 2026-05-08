FROM alpine:edge

RUN apk add --no-cache \
        cmake \
        ninja \
        g++ \
        gdb \
        python3 \
        git \
        make

WORKDIR /src
