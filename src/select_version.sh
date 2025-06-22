#!/bin/sh

CUTOFF_VERSION=$1
PRE_VERSION_FILE=$2
POST_VERSION_FILE=$3
## replace '~' by space
ACTUAL_VERSION=$(echo $4 | sed 's/~/ /g')

if [ -z "$ACTUAL_VERSION" ]; then
  echo "(* Selected $POST_VERSION_FILE because version is not set *)"
  cat $POST_VERSION_FILE
elif [ "$(printf '%s\n' "$ACTUAL_VERSION" "$CUTOFF_VERSION" | sort -V | head -n1)" = "$CUTOFF_VERSION" ]; then
  echo "(* Selected $POST_VERSION_FILE because $ACTUAL_VERSION >= $CUTOFF_VERSION *)"
  cat $POST_VERSION_FILE
else
  echo "(* Selected $PRE_VERSION_FILE because $ACTUAL_VERSION < $CUTOFF_VERSION *)"
  cat $PRE_VERSION_FILE
fi

