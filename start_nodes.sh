#!/bin/bash

COOKIE="secret"

echo "--- Launching all 5 nodes in separate terminals ---"

gnome-terminal -- bash -c "echo 'Starting waiters_node...'; cd $(pwd); rebar3 shell --name waiters_node@127.0.0.1 --setcookie $COOKIE; exec bash"

sleep 1

gnome-terminal -- bash -c "echo 'Starting machines_node...'; cd $(pwd); rebar3 shell --name machines_node@127.0.0.1 --setcookie $COOKIE; exec bash"

sleep 1

gnome-terminal -- bash -c "echo 'Starting safe_node...'; cd $(pwd); rebar3 shell --name safe_node@127.0.0.1 --setcookie $COOKIE; exec bash"

sleep 1

gnome-terminal -- bash -c "echo 'Starting tables_node...'; cd $(pwd); rebar3 shell --name tables_node@127.0.0.1 --setcookie $COOKIE; exec bash"

sleep 1

gnome-terminal -- bash -c "echo 'Starting customers_node...'; cd $(pwd); rebar3 shell --name customers_node@127.0.0.1 --setcookie $COOKIE; exec bash"
