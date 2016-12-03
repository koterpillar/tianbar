# Base image for testing Tianbar

FROM fedora:25

ENV LANG C.UTF-8
ENV LANGUAGE C:en
ENV LC_ALL C.UTF-8

# http://docs.haskellstack.org/en/stable/install_and_upgrade/#fedora
RUN \
    curl -sSL https://s3.amazonaws.com/download.fpcomplete.com/fedora/24/fpco.repo > /etc/yum.repos.d/fpco.repo && \
    dnf -y install ncurses-compat-libs stack && \
    true

RUN stack setup

RUN dnf -y install \
        cairo-devel \
        gobject-introspection-devel \
        gtk3-devel \
        libX11-devel \
        libXft-devel \
        libxml2-devel \
        libXrandr-devel \
        webkitgtk4-devel \
        webkitgtk4-jsc-devel \
    && true

ADD stack.yaml /root/.stack/global-project/stack.yaml
RUN sed -i "s/packages:/packages: []/;/- '\.'/d" /root/.stack/global-project/stack.yaml

# gi-* packages depend on haskell-gi but somehow don't declare it
RUN stack build haskell-gi
RUN stack build xmonad
RUN stack build lens
RUN stack build gi-webkit2
RUN stack build dbus

ADD . /tianbar
WORKDIR /tianbar

RUN stack build --only-dependencies
