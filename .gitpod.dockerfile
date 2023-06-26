FROM gitpod/workspace-python:2023-01-02-17-16-30

ENV SYSTEM_SETUP_SCRIPTS="./tmp_system_setup_scripts"

COPY ./system_setup_scripts $SYSTEM_SETUP_SCRIPTS

USER root
RUN apt-get update \
  && $SYSTEM_SETUP_SCRIPTS/install_all.sh \
  && rm -rf $SYSTEM_SETUP_SCRIPTS \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/*
USER gitpod

RUN echo "unset JAVA_TOOL_OPTIONS" >> ~/.bashrc
