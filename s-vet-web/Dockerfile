FROM node:14-alpine AS build
WORKDIR /app

COPY package.json yarn.lock ./
RUN yarn --frozen-lockfile

COPY . .

RUN yarn build

FROM nginx:stable
RUN rm /etc/nginx/conf.d/default.conf
COPY --from=build /app/build /opt/s-vet
COPY docker.conf.d /etc/nginx/conf.d/s-vet.conf
