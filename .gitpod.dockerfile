FROM gitpod/workspace-python:2023-01-02-17-16-30


RUN sudo apt-get update
COPY ./system_setup_scripts ./tmp_system_setup_scripts

USER root
RUN ./tmp_system_setup_scripts/install_all.sh
RUN apt-get clean \
  && rm -rf /var/lib/apt/lists/*
USER gitpod

RUN echo "unset JAVA_TOOL_OPTIONS" >> ~/.bashrc
