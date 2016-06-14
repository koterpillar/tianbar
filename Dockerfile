FROM ubuntu

# http://docs.haskellstack.org/en/stable/install_and_upgrade/#ubuntu
RUN \
    apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 575159689BEFB442 && \
    echo 'deb http://download.fpcomplete.com/ubuntu xenial main' > /etc/apt/sources.list.d/fpco.list && \
    apt-get update && \
    apt-get -y install stack && \
    true

RUN stack setup

RUN \
    apt-get -y install \
        libcairo2-dev \
        libgirepository1.0-dev \
        libx11-dev \
        libxft-dev \
        libxml2-dev \
        libxrandr-dev \
        pkg-config \
    && \
    true

ADD . /tianbar
WORKDIR /tianbar

RUN stack build
