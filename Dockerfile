FROM sbtscala/scala-sbt:eclipse-temurin-jammy-21.0.2_13_1.9.9_3.4.0 as build
RUN apt update && apt install -y clang mold libstdc++-12-dev libgc-dev libutf8proc-dev libssl-dev libsodium-dev git cmake
RUN git clone -b v1.4.14 https://github.com/aws/s2n-tls /tmp/s2n-tls
WORKDIR /tmp/s2n-tls
RUN prefix=/usr make install

WORKDIR /src
COPY . /src
ENV DOCKER_BUILD=1
RUN sbt "nativeLink;stageBinary"

#FROM scratch
RUN cp /src/router /router
ENV S2N_DONT_MLOCK=1
ENV LOGLEVEL=Info
ENV LD_LIBRARY_PATH=/usr/lib64
ENTRYPOINT ["/router", "config.yml"]
