FROM        mcr.microsoft.com/vscode/devcontainers/base:bullseye

LABEL       maintainer="Thierry Marianne <thierry.marianne@univ-reunion.fr>"

ARG         OWNER_UID
ARG         OWNER_GID

ENV         PROGRAM_OWNER_UID=${OWNER_UID}
ENV         PROGRAM_OWNER_GID=${OWNER_GID}

COPY        --chown=${PROGRAM_OWNER_UID}:${PROGRAM_OWNER_GID} \
            ./bin/docker.sh \
            ./bin/entrypoint.sh \
            ./etc/.bc \
            /usr/local/bin/

RUN         bash -c '. /usr/local/bin/docker.sh && install_requirements'

USER        ${PROGRAM_OWNER_UID}:${PROGRAM_OWNER_GID}

ENTRYPOINT  ["/usr/local/bin/tini", "--"]

CMD         ["/usr/local/bin/entrypoint.sh"]