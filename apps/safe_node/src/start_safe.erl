-module(start_safe).
-export([start/0]).

start() ->
    application:start(safe_node),
    net_kernel:connect_node('tables_node@127.0.0.1'),
    net_kernel:connect_node('customers_node@127.0.0.1'),
    net_kernel:connect_node('machines_node@127.0.0.1'),
    net_kernel:connect_node('waiters_node@127.0.0.1').



