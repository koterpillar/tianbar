# Image for testing Tianbar

FROM koterpillar/tianbar

ADD . /tianbar
WORKDIR /tianbar

RUN stack build
