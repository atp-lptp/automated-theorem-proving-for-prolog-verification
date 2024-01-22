FROM        ghcr.io/atp-lptp/automated-theorem-proving-for-prolog-verification:base

LABEL       maintainer="Thierry Marianne <thierry.marianne@univ-reunion.fr>"

LABEL       org.opencontainers.image.source="https://github.com/atp-lptp/automated-theorem-proving-for-prolog-verification"

COPY        --chown=1001:1001 \
            ./ \
            /usr/local/atp-prolog-verification

USER        root

RUN         apt update \
            --assume-yes && \
            apt-get install \
            build-essential \
            curl \
            jq \
            gcc \
            git \
            make \
            --assume-yes \
            --no-install-recommends && \
            /bin/bash -c 'cd /usr/local/atp-prolog-verification && make help'

USER        1001:1001

WORKDIR     /usr/local/atp-prolog-verification
