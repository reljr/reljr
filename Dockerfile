FROM clojure:temurin-17-alpine AS build

WORKDIR /app
COPY . /app

RUN apk update && \
    apk add --update nodejs npm && \
    npm install && \
    npx shadow-cljs release reljr

FROM node:18-alpine AS main

COPY --from=build /app/public /

ENV PORT=3000

EXPOSE $PORT

ENTRYPOINT exec npx http-server -p $PORT
