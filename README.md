# S-Vet

The `s-vet` repository contains code for an application of the management of a Vet clinic.

This serves as kind of a monorepo where the code for the Haskell backend lives in `s-vet` and the code for the React frontend lives in `s-vet-web`.

The application as is serves a single tenant because that's what I needed, I might change it to support multi tenancy eventually.

# Running

The easiest way to run the full application is using the included docker compose, running `docker compose up` will be Docker containers for both server and web app (using Nginx) as well as a Postgres container.

# Development

You can use the included script `dev.sh` to run the React application in dev mode and run a process that uses `watchexec` to monitor the `s-vet` directory for code changes and rebuild and rerun the server binary.
