FROM sbtscala/scala-sbt:eclipse-temurin-jammy-21.0.2_13_1.9.9_3.4.0 as build
RUN apt update && apt install -y clang mold libstdc++-12-dev libgc-dev libutf8proc-dev libssl-dev libsodium-dev git cmake
RUN git clone -b v1.4.14 https://github.com/aws/s2n-tls /tmp/s2n-tls
WORKDIR /tmp/s2n-tls
RUN prefix=/usr make install

WORKDIR /src
COPY . /src
ENV DOCKER_BUILD=1
RUN sbt "nativeLink;stageBinary"

FROM sbtscala/scala-sbt:eclipse-temurin-jammy-21.0.2_13_1.9.9_3.4.0 as runner
RUN apt update && apt install -y libstdc++-12-dev libgc-dev libutf8proc-dev libssl-dev
COPY --from=build /src/router /router
COPY --from=build /usr/lib64/libs2n.so /usr/lib/libs2n.so
COPY --from=build /usr/lib64/libs2n.a /usr/lib/libs2n.a
COPY systems.json systems.json
WORKDIR /
ENV S2N_DONT_MLOCK=1
ENV LOGLEVEL=Info
ENTRYPOINT ["/router", "/config.yml"]
