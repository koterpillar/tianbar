FROM fedora:24

ENV LANG C.UTF-8
ENV LANGUAGE C:en
ENV LC_ALL C.UTF-8

# http://docs.haskellstack.org/en/stable/install_and_upgrade/#fedora
RUN \
    curl -sSL https://s3.amazonaws.com/download.fpcomplete.com/fedora/22/fpco.repo > /etc/yum.repos.d/fpco.repo && \
    dnf -y install ncurses-compat-libs stack && \
    true

RUN stack setup

RUN \
    dnf -y install \
        cairo-devel \
        gobject-introspection-devel \
        gtk3-devel \
        libX11-devel \
        libXft-devel \
        libxml2-devel \
        libXrandr-devel \
        webkitgtk4-jsc-devel \
        webkitgtk4-devel \
    && \
    true

RUN stack build haskell-gi
RUN stack build xmonad
RUN stack build lens

ADD . /tianbar
WORKDIR /tianbar

RUN stack build --only-dependencies

RUN stack build
