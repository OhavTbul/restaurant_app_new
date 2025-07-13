#!/bin/bash

# Define the cookie for the nodes
COOKIE="secret"

echo "--- Launching Erlang nodes ---"

# Launch the central 'safe_node' first, as other nodes connect to it
gnome-terminal -- bash -c "echo 'Starting safe_node...'; cd $(pwd); rebar3 shell --name safe_node@127.0.0.1 --setcookie $COOKIE --eval 'start_safe:start().'; exec bash" &

sleep 2

# Launch the other nodes and automatically start their applications and connect
gnome-terminal -- bash -c "echo 'Starting waiters_node...'; cd $(pwd); rebar3 shell --name waiters_node@127.0.0.1 --setcookie $COOKIE --eval 'start_waiters:start().'; exec bash" &

sleep 1

gnome-terminal -- bash -c "echo 'Starting machines_node...'; cd $(pwd); rebar3 shell --name machines_node@127.0.0.1 --setcookie $COOKIE --eval 'start_machines:start().'; exec bash" &

sleep 1

gnome-terminal -- bash -c "echo 'Starting tables_node...'; cd $(pwd); rebar3 shell --name tables_node@127.0.0.1 --setcookie $COOKIE --eval 'start_tables:start().'; exec bash" &

sleep 1

gnome-terminal -- bash -c "echo 'Starting customers_node...'; cd $(pwd); rebar3 shell --name customers_node@127.0.0.1 --setcookie $COOKIE --eval 'start_customers:start().'; exec bash" &

echo "--- All Erlang nodes launched. Waiting for network connections... ---"

sleep 5

echo "--- Launching the GUI ---"

# Launch the Python GUI
python3 gui/game_gui.py

echo "--- All done. You can close this terminal. ---"