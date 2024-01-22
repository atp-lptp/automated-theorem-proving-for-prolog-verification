#!/bin/bash

function build() {
  tar cvzf \
    '/usr/local/atp-prolog-verification/'$RELEASE_NAME'.tar.gz' \
    './out/'*.out \
    './out/'results.* \
    './src/'*/*.fof \
    && \
    echo '=> Built ./'$RELEASE_NAME
}
build