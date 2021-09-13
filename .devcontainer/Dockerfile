# See here for image contents: https://github.com/microsoft/vscode-dev-containers/tree/v0.194.0/containers/cpp/.devcontainer/base.Dockerfile

# [Choice] Debian / Ubuntu version: debian-11, debian-10, debian-9, ubuntu-20.04, ubuntu-18.04
ARG VARIANT="ubuntu-20.04"
FROM mcr.microsoft.com/vscode/devcontainers/cpp:0-${VARIANT}

RUN echo '\ndeb https://packages.erlang-solutions.com/ubuntu trusty contrib\n\
deb https://packages.erlang-solutions.com/ubuntu saucy contrib\n\
deb https://packages.erlang-solutions.com/ubuntu precise contrib\n' >> /etc/apt/sources.list && \
    wget https://packages.erlang-solutions.com/ubuntu/erlang_solutions.asc && apt-key add erlang_solutions.asc

# [Optional] Uncomment this section to install additional packages.
RUN apt-get update && export DEBIAN_FRONTEND=noninteractive \
    && apt-get -y install --no-install-recommends esl-erlang