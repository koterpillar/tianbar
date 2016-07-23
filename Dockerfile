# Image for testing Tianbar

FROM koterpillar/tianbar_base

ADD . /tianbar
WORKDIR /tianbar

RUN stack build
