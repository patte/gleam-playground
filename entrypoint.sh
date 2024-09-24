#!/bin/sh
set -eu

# copied from build/erlang-shipment/entrypoint.sh

PACKAGE=hello_world
BASE=$(dirname "$0")
COMMAND="${1-default}"

run() {
  ERL_ARGS=""

  # expand ERL_XARGS environment variable eval to support env vars like $HOST
  if [ -n "${ERL_XARGS-}" ]; then
    ERL_ARGS=$(eval echo "$ERL_XARGS")
  fi

  # add -setcookie if COOKIE is set
  if [ -n "${COOKIE-}" ]; then
    ERL_ARGS="$ERL_ARGS -setcookie $COOKIE"
  fi

  echo "ERL_ARGS: $ERL_ARGS"

  erl \
    -pa "$BASE"/*/ebin \
    -eval "$PACKAGE@@main:run($PACKAGE)" \
    -noshell \
    $ERL_ARGS \
    -extra "$@"
}

shell() {
  erl -pa "$BASE"/*/ebin
}

case "$COMMAND" in
  run)
    shift
    run "$@"
  ;;

  shell)
    shell
  ;;

  *)
    echo "usage:" >&2
    echo "  entrypoint.sh \$COMMAND" >&2
    echo "" >&2
    echo "commands:" >&2
    echo "  run    Run the project main function" >&2
    echo "  shell  Run an Erlang shell" >&2
    exit 1
esac
