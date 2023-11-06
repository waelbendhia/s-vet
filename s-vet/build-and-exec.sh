#!/usr/bin/env bash

cabal build
mv $(cabal list-bin s-vet-server) ./
echo "sql://${DB_USER}:${DB_PASSWORD}@localhost:5432/svet"
./s-vet-server --port 1234 --dbUrl "sql://${DB_USER}:${DB_PASSWORD}@localhost:5432/svet" --dbPool 16
