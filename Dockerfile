# syntax=docker.io/docker/dockerfile:1.10.0-labs
# 👆enable copy --exclude

### FRONTEND ###

# use the official Bun image https://hub.docker.com/r/oven/bun/tags
FROM oven/bun:1 AS base-fe
WORKDIR /usr/src/app

# install dependencies into temp directory
# this will cache them and speed up future builds
FROM base-fe AS install-fe
RUN mkdir -p /temp/dev
COPY frontend/package.json frontend/bun.lockb /temp/dev/
RUN cd /temp/dev && bun install --frozen-lockfile

# install with --production (exclude devDependencies)
RUN mkdir -p /temp/prod
COPY frontend/package.json frontend/bun.lockb /temp/prod/
RUN cd /temp/prod && bun install --frozen-lockfile --production

# copy node_modules from temp directory
# then copy all (non-ignored) project files into the image
FROM base-fe AS prerelease-fe
COPY --from=install-fe /temp/dev/node_modules node_modules
COPY frontend/ .

# tests & build
ENV NODE_ENV=production
RUN bun test
RUN bun run build

### BACKEND ###
FROM ghcr.io/gleam-lang/gleam:v1.4.1-erlang AS install-be
WORKDIR /usr/src/app

COPY --exclude=frontend/ . .
RUN gleam export erlang-shipment

# final image
FROM erlang:27.0-slim AS release
WORKDIR /usr/src/app
COPY --from=prerelease-fe /usr/src/app/dist frontend/dist
COPY --from=install-be /usr/src/app/build/erlang-shipment .
COPY --from=install-be /usr/src/app/files files/.
COPY entrypoint.sh .

EXPOSE 3000/tcp
# run the app
ENTRYPOINT ["/usr/src/app/entrypoint.sh"]
CMD ["run"]