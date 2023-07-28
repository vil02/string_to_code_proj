FROM gitpod/workspace-python:2023-07-17-21-17-42

ENV SYSTEM_SETUP_SCRIPTS="./tmp_system_setup_scripts"

COPY ./system_setup_scripts $SYSTEM_SETUP_SCRIPTS

USER root
RUN apt-get update \
  && $SYSTEM_SETUP_SCRIPTS/install_all.sh \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/*
USER gitpod

RUN $SYSTEM_SETUP_SCRIPTS/install_all_no_sudo.sh

USER root
RUN rm -rf $SYSTEM_SETUP_SCRIPTS
USER gitpod

RUN echo "unset JAVA_TOOL_OPTIONS" >> ~/.bashrc
