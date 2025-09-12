#!/bin/bash


NODE_TYPE=$1
HOST="127.0.0.1"
COOKIE="secret" 
SNAME=""


 (--name)
if [ "$NODE_TYPE" == "safe" ]; then
  SNAME="safe_node@${HOST}"
else
  SNAME="${NODE_TYPE}_node@${HOST}"
fi


echo "--- Launching node ${SNAME} in a new terminal... ---"
gnome-terminal -- bash -c "echo 'Starting ${SNAME}...'; \
  rebar3 shell --name '${SNAME}' --setcookie '${COOKIE}' --eval 'general_start:start(${NODE_TYPE}).'; \
  exec bash" &
