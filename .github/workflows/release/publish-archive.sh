#!/bin/bash

function publish() {
  local results
  results="${1}"

  echo "=> About to publish ""${results}"

  if [ ! -e "${results}" ];
  then
      echo 'Invalid results ('"${results}"')'
      return 1
  fi

  local checksum
  checksum="$(sha256sum "${results}" | cut -d ' ' -f 1)"

  local base_url
  base_url='https://api.github.com/repos/'"${GITHUB_REPOSITORY}"

  local upload_url
  upload_url="$(curl \
    -H 'Content-Type: application/octet-stream' \
    -H "Authorization: Bearer ${GITHUB_TOKEN}" \
    "${base_url}"/releases 2>> /dev/null | \
    jq -r '.[] | .upload_url' | \
    head -n1)"

  upload_url=${upload_url/\{?name,label\}/}

  local archive_name
  archive_name="$(curl \
    -H 'Content-Type: application/octet-stream' \
    -H "Authorization: Bearer ${GITHUB_TOKEN}" \
    "${base_url}"/releases 2>> /dev/null | \
    jq -r '.[] | .tag_name' | \
    head -n1).tar.gz"

  echo '=> Archive name is '"${archive_name}"

  curl \
    -X POST \
    --data-binary @"${results}" \
    -H 'Content-Type: application/octet-stream' \
    -H "Authorization: Bearer ${GITHUB_TOKEN}" \
    "${upload_url}?name=${archive_name}"

  curl \
    -X POST \
    --data "$checksum" \
    -H 'Content-Type: text/plain' \
    -H "Authorization: Bearer ${GITHUB_TOKEN}" \
    "${upload_url}?name=${archive_name}.sha256sum"
}

publish '/usr/local/atp-prolog-verification/'"${RELEASE_NAME}"
