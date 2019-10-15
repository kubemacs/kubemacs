# "iiorgmacs" Add "Spacemacs" layer, supporting files and "ii" user
# Version 0.1 Sept 2019

FROM heyste/iiorgmacs:0.2

ENV HOME=
USER root

# install some useful packages
RUN apt update && \
	apt install -y sudo wget acl docker docker-compose golang apt-transport-https build-essential zsh sqlite3 vim nano apt-utils

# install Kubernetes client and Google Cloud SDK
RUN echo "deb [signed-by=/usr/share/keyrings/cloud.google.gpg] https://packages.cloud.google.com/apt cloud-sdk main" | sudo tee -a /etc/apt/sources.list.d/google-cloud-sdk.list && \
	curl https://packages.cloud.google.com/apt/doc/apt-key.gpg | sudo apt-key --keyring /usr/share/keyrings/cloud.google.gpg add - && \
	apt-get update && \
	apt-get install -y kubectl google-cloud-sdk

# get ssh-find-agent
RUN curl https://gitlab.ii.coop/ii/tooling/ssh-find-agent/raw/master/ssh-find-agent.sh > /usr/local/bin/ssh-find-agent.sh && \
	chmod +x /usr/local/bin/ssh-find-agent.sh

CMD ["/bin/bash"]
