FROM ubuntu

RUN locale-gen en_US.UTF-8
ENV LANG en_US.UTF-8
ENV LANGUAGE en_US:en
ENV LC_ALL en_US.UTF-8

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
        libgtk-3-dev \
        libjavascriptcoregtk-4.0-dev \
        libx11-dev \
        libxft-dev \
        libxml2-dev \
        libxrandr-dev \
        libwebkit2gtk-4.0-dev \
        pkg-config \
    && \
    true

ADD . /tianbar
WORKDIR /tianbar

RUN stack build
