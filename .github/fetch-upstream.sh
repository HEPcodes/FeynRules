#!/bin/bash
set -eu
set -o pipefail

# Create a temporary directory.
# NOTE: We use mktemp, which must be available on Linux.
temp_dir=$(mktemp -d)
trap 'rm -rf -- "$temp_dir"' EXIT

# Download the distribution from the upstream and expand it.
# NOTE: We assume curl and GNU tar are available.
(
    cd "$temp_dir"
    curl -O https://feynrules.irmp.ucl.ac.be/downloads/feynrules-current.tar.gz
    tar xfz feynrules-current.tar.gz
)

# Remove all files/directories from the upstream.
for f in $(ls); do
  # NOTE: files/directories starting with "." are excluded.
  [ "$f" == "readme.md" ] && continue
  rm -fr "$f"
done

# Copy all files/directories from the distribution.
for f in $(cd "$temp_dir/feynrules-current" && ls); do
  cp -fpr "$temp_dir/feynrules-current/$f" .
done
