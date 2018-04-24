FROM erlang:19.3.6.3 as builder

WORKDIR /src
COPY . /src
COPY /creds/catbot/prod.config /src/files/
RUN rebar3 as prod tar

RUN mkdir -p /release
RUN tar -zxvf /src/_build/prod/rel/*/*.tar.gz -C /release

FROM debian:jessie-slim

RUN apt-get update && apt-get install -y openssl

WORKDIR /deploy

COPY --from=builder /release /deploy

EXPOSE 80

CMD /deploy/bin/catbot foreground
