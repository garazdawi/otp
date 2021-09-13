# See here for image contents: https://github.com/microsoft/vscode-dev-containers/tree/v0.194.0/containers/cpp/.devcontainer/base.Dockerfile

# [Choice] Debian / Ubuntu version: debian-11, debian-10, debian-9, ubuntu-20.04, ubuntu-18.04
ARG VARIANT="buster"
FROM mcr.microsoft.com/vscode/devcontainers/cpp:0-${VARIANT}

# [Optional] Uncomment this section to install additional packages.
RUN apt-get update && export DEBIAN_FRONTEND=noninteractive \
    && apt-get -y install --no-install-recommends erlang

RUN echo 'otp_path: "~/erlang/23.2" \
include_dirs:\
  - "lib/*/src"\
  - "lib/*/include"\
diagnostics:\
  enabled:\
    - bound_var_in_pattern' > /home/vscode/erlang_ls.config