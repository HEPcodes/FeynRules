#!/bin/bash
set -eu
set -o pipefail

# extract_value FILE KEY prints VALUE associated with KEY in FILE.
extract_value() {
  local file=$1
  local key=$2
  # Extract the first "KEY = VALUE" pair and perform regexp match.
  [[ $(grep "$key" "$file" | grep '=' | head -1) =~ ^[^=]*=(.*)$ ]]
  # Remove a trailing semicolon and double quotation marks.
  echo ${BASH_REMATCH[1]} | sed 's/;$//' | sed 's/^"//' | sed 's/"$//'
}

# Get FR$Version and FR$VersionDate from FeynRulesPackage.m.
version=$(extract_value FeynRulesPackage.m 'FR$Version')
version_date=$(extract_value FeynRulesPackage.m 'FR$VersionDate')

# Construct a commit message.
echo "v$version ($version_date)"
