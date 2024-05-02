FROM gitpod/workspace-python:2024-05-02-10-29-11

ENV SYSTEM_SETUP_SCRIPTS="./tmp_system_setup_scripts"

COPY ./system_setup_scripts $SYSTEM_SETUP_SCRIPTS

RUN "$SYSTEM_SETUP_SCRIPTS"/install_all_no_sudo.sh

USER root
RUN apt-get update \
  && "$SYSTEM_SETUP_SCRIPTS"/install_all.sh \
  && rm -rf "$SYSTEM_SETUP_SCRIPTS" \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/*
USER gitpod

RUN echo "unset JAVA_TOOL_OPTIONS" >> ~/.bashrc.d/string_to_code_java \
  && echo "export PATH=${HOME}/.nimble/bin:\$PATH" >> ~/.bashrc >> ~/.bashrc.d/string_to_code_nim

