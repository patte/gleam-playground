#!/bin/sh
set -eu

# copied from build/erlang-shipment/entrypoint.sh

PACKAGE=hello_world
BASE=$(dirname "$0")
COMMAND="${1-default}"

run() {
  ERL_ARGS=""

  # Check if the environment variable SNAME is set and add it to ERL_ARGS
  if [ -n "${SNAME-}" ]; then
    ERL_ARGS="$ERL_ARGS -sname $SNAME"
  fi

  # Check if the environment variable COOKIE is set and add it to ERL_ARGS
  if [ -n "${COOKIE-}" ]; then
    ERL_ARGS="$ERL_ARGS -setcookie $COOKIE"
  fi

  erl \
    -pa "$BASE"/*/ebin \
    -eval "$PACKAGE@@main:run($PACKAGE)" \
    -noshell \
    $ERL_ARGS \
    -proto_dist inet6_tcp \
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
