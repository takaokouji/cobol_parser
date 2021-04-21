ARG RUBY_VERSION=3.0.1

FROM ruby:$RUBY_VERSION-slim-buster

ENV LANG C.UTF-8
ENV DEBIAN_FRONTEND noninteractive

WORKDIR /app

COPY cobol_parser.gemspec Gemfile Gemfile.lock /app/
COPY lib/cobol_parser/version.rb /app/lib/cobol_parser/
COPY .git /app/.git

RUN set -eux && \
  apt-get update && \
  apt-get install -y --no-install-recommends \
    build-essential \
    git \
  && \
  gem install bundler && \
  bundle install && \
  bundle clean --force && \
  apt-get purge -y --autoremove \
    build-essential \
  && \
  rm -rf /var/lib/apt/lists/*

COPY . /app/
