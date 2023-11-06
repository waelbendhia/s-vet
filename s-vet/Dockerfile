FROM haskell:9.2.8 as build
RUN mkdir /opt/build
WORKDIR /opt/build

RUN apt-get update &&\
    apt-get download libgmp10 postgresql
RUN mv libgmp*.deb libgmp.deb

RUN cabal update

COPY cabal.project s-vet.cabal ./
RUN cabal v2-build -v1 --only-dependencies all
COPY . .
RUN cabal build s-vet-server
RUN cp $(cabal list-bin s-vet-server) .

FROM debian:buster as app

LABEL org.opencontainers.image.source=https://github.com/waelbendhia/s-vet

RUN mkdir -p /opt/app
WORKDIR /opt/app

RUN apt-get update &&\
    apt-get install -y libpq-dev

COPY --from=build /opt/build/libgmp.deb /tmp
RUN dpkg -i /tmp/libgmp.deb && rm /tmp/libgmp.deb

COPY --from=build /opt/build/s-vet-server ./s-vet-server

RUN ls /opt/app
RUN chmod +x /opt/app/s-vet-server

ENV PORT=8080
ENV DB_URL=""
ENV DB_POOL=64

EXPOSE $PORT
CMD /opt/app/s-vet-server --port $PORT --dbUrl $DB_URL --dbPool $DB_POOL
