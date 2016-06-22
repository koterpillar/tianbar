FROM koterpillar/tianbar_base

ADD . /tianbar
WORKDIR /tianbar

RUN stack build --only-dependencies

RUN stack build
