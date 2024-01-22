#!/usr/bin/env bash
set -Eeu

function add_system_user_group {
  if [ $(cat /etc/group | grep "${PROGRAM_OWNER_GID}" -c) -eq 0 ]; then
    groupadd \
      --gid "${PROGRAM_OWNER_GID}" \
      service
  fi

  if [ $(cat /etc/passwd | grep "${PROGRAM_OWNER_UID}" -c) -eq 0 ]; then
    useradd \
      --shell /usr/sbin/nologin \
      --uid ${PROGRAM_OWNER_UID} \
      --gid ${PROGRAM_OWNER_GID} \
      --no-user-group \
      --no-create-home \
      service
  fi
}

function clear_package_management_system_cache {
  # Remove packages installed with apt except for tini
  apt-get remove --assume-yes build-essential gcc build-essential
  apt-get autoremove --assume-yes &&
  apt-get purge --assume-yes
  apt-get clean &&
  rm -rf /var/lib/apt/lists/*
}

# Install Vampire system and z3
function install_vampire {
  (
    git clone https://github.com/vprover/vampire.git
    cd vampire
    git fetch --all
    git checkout v4.8casc2023
    git submodule init
    git submodule update
    mkdir -p ./z3/build
    cd ./z3/build
    cmake .. -DZ3_SINGLE_THREADED=1 -DCMAKE_BUILD_TYPE=Release
    make

    mkdir /vampire/build
    cd /vampire/build
    cmake /vampire
    make

    mv '/vampire/build/bin/vampire_z3_rel_HEAD_6749' /usr/local/bin/vampire
    ln -s /usr/local/bin/vampire /usr/bin/vampire
    chmod +x /usr/local/bin/vampire
  ) >> /var/log/install_vampire.log 2>&1 && \
  printf '%s%s' 'Installed Vampire successfully.' $'\n' 1>&2 || \
  cat /var/log/install_vampire.log
  rm /var/log/install_vampire.log
  /usr/local/bin/vampire --version
}

# Install E Theorem Prover
function install_e_prover {
  (
    git clone https://github.com/eprover/eprover.git
    cd eprover
    git fetch --all
    git checkout E-3.0.03
    ./configure --bindir=/usr/local/bin
    make
    make install
  ) >> /var/log/install_e_prover.log 2>&1 && \
  printf '%s%s' 'Installed E Theorem Prover successfully.' $'\n' 1>&2 || \
  cat /var/log/install_e_prover.log
  rm /var/log/install_e_prover.log
  /usr/local/bin/eprover --version
}

# Install package manager updates (aptitude source repositories)
function update_package_manager {
  apt-get update --assume-yes --quiet >> /dev/null 2>&1 && \
  printf '%s%s' \
    'Updated package manager repositories successfully.' \
    $'\n' \
    1>&2
}

function install_tini() {
  export TINI_VERSION=v0.19.0 && \
  wget --no-check-certificate --no-cookies --quiet https://github.com/krallin/tini/releases/download/${TINI_VERSION}/tini-amd64 && \
  wget --no-check-certificate --no-cookies --quiet https://github.com/krallin/tini/releases/download/${TINI_VERSION}/tini-amd64.sha256sum && \
  echo "$(cat tini-amd64.sha256sum)" | sha256sum -c
  mv tini-amd64 /usr/local/bin/tini
  chmod +x /usr/local/bin/tini
}

# Install packages
function install_required_packages {
  apt-get install --assume-yes --no-install-recommends --quiet \
    bc \
    build-essential \
    cmake \
    curl \
    emacs \
    jq \
    git \
    gcc \
    make \
    nano \
    software-properties-common \
    vim \
    wget \
   >> /dev/null 2>&1 && \
  printf '%s.%s' 'Installed required packages successfully' $'\n'  1>&2 ||
  printf '%s.%s' 'Could not install required packages.' $'\n'  1>&2
}

# Configure [bc](https://www.gnu.org/software/bc/)
function configure_bc {
  mv /usr/local/bin/.bc /root/.bc && \
  echo 'export BC_ENV_ARGS='"${HOME}/.bc" > ~/.bashrc && \
  printf '%s.%s' 'Configured bc successfully' $'\n'  1>&2 ||
  printf '%s.%s' 'Could not configure bc.' $'\n'  1>&2
}

# [Installing from PPA](https://www.swi-prolog.org/build/PPA.html)
function install_swi_prolog {
  apt-add-repository ppa:swi-prolog/stable --assume-yes >> /dev/null 2>&1 && \
  printf '%s%s' 'Added swi-prolog repository' $'\n' 1>&2
  (
    apt-get update --assume-yes --quiet && \
    printf '%s.%s' 'Updated aptitude package repository successfully' $'\n' 1>&2 || \
    printf '%s.%s' 'Could not update aptitude package repository' $'\n' 1>&2

    apt-get install swi-prolog --assume-yes --quiet && \
    printf '%s.%s' 'Installed SWI-Prolog successfully' $'\n' 1>&2 || \
    printf '%s.%s' 'Could not install SWI-Prolog' $'\n' 1>&2
  ) >> /dev/null 2>&1 && \
  printf '%s.%s' 'Installed swi-prolog successfully' $'\n' 1>&2
}

function install_entrypoint() {
  chmod +x /usr/local/bin/entrypoint.sh
}

# Install requirements
# - add system user and group
# - update package manager
# - install packages
#   - bc,
#   - build-essential,
#   - cmake,
#   - emacs,
#   - git,
#   - nano,
#   - software-properties-common,
#   - tini
#   - vim,
#   - wget
# - install E Theorem Prover
# - install Vampire system
# - install SWI-Prolog
# - configure bc
# - clean up
function install_requirements {
  add_system_user_group || printf '%s.%s' 'Could not add system user group' $'\n' 1>&2
  update_package_manager || printf '%s.%s' 'Could not update package manager' $'\n' 1>&2
  configure_bc || printf '%s.%s' 'Could not configure bc' $'\n' 1>&2
  install_required_packages || printf '%s.%s' 'Could not install required packages' $'\n' 1>&2
  install_tini || printf '%s.%s' 'Could not install tini' $'\n' 1>&2
  install_swi_prolog || printf '%s.%s' 'Could not install SWI-Prolog' $'\n' 1>&2
  install_vampire || printf '%s.%s' 'Could not install vampire system' $'\n' 1>&2
  install_e_prover || printf '%s.%s' 'Could not install E Theorem Prover' $'\n' 1>&2
  install_entrypoint || printf '%s.%s' 'Could not install container entrypoint' $'\n' 1>&2
  clear_package_management_system_cache || printf '%s.%s' 'Could not clean up' $'\n' 1>&2
}

# Build prover image by running [docker compose](https://docs.docker.com/get-docker/) command
function build_prover_image {
    if [ ! -e ./.env.local ];
    then
      cp ./.env.local{.dist,}
    fi

    . ./.env.local

    docker compose \
    -f ./etc/docker-compose.yaml \
    build \
    --build-arg OWNER_UID="${PROGRAM_OWNER_UID}" \
    --build-arg OWNER_GID="${PROGRAM_OWNER_UID}" \
    prolog-verification
}

function run_prover_container {
    . ./.env.local

    docker compose \
    -f ./etc/docker-compose.yaml \
    run \
    --rm \
    --user root \
    prolog-verification \
    bash
}

function run_main_command() {
    make test
    find ./src/*/*pl \( -not -path '*-all.pl' \) \
        -exec bash -c 'echo Building requirements for "${1}"; export NO_INTERACTION=1 VERBOSE= LOGIC_PROGRAM="${1}"; make build-fof >> /dev/null 2>&1 || exit 255' \
        shell {} \;
    find ./src/*/*pl \( -not -path '*-all.pl' \) \
        -exec bash -c 'echo Building requirements for "${1}"; export NO_INTERACTION=1 VERBOSE= LOGIC_PROGRAM="${1}"; make start >> /dev/null 2>&1 || exit 255' \
        shell {} \;
}

set +Eeu