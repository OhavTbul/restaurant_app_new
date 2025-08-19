# Restaurant Game - How to Run

This document provides detailed instructions on how to set up and run the restaurant game distributed across multiple Erlang nodes.

## Prerequisites

Before running the game, ensure you have the following installed on your system:

### Required Software
- **Erlang/OTP** (version 20 or higher)
- **rebar3** - Erlang build tool
- **Python 3** (version 3.6 or higher)
- **pygame** Python package

### Installation Commands

#### Install rebar3
```bash
# On macOS (using Homebrew)
brew install rebar3

# On Ubuntu/Debian
sudo apt-get install rebar3

# Or download from GitHub
curl -O https://s3.amazonaws.com/rebar3/rebar3
chmod +x rebar3
sudo mv rebar3 /usr/local/bin/
```

#### Install pygame
```bash
pip3 install pygame
```

## Project Structure

The game consists of 5 Erlang nodes, each with a specific role:

- **safe_node** - Central coordination node
- **waiters_node** - Manages waiter processes
- **machines_node** - Manages cooking machine processes
- **tables_node** - Manages table processes
- **customers_node** - Manages customer processes

## Step-by-Step Setup

### 1. Clone and Navigate to Repository
```bash
cd /path/to/restaurant_app_new
```

### 2. Compile the Project
From the project root directory, compile all applications:
```bash
rebar3 compile
```

### 3. Start Each Node

**Important**: Start nodes in the following order to ensure proper network connections.

#### Step 3.1: Start the Safe Node (Central Coordinator)
```bash
rebar3 shell --name safe_node@<127.0.0.1> --setcookie 'secret' --eval "start_safe:start()."
```

#### Step 3.2: Start the Waiters Node
```bash
rebar3 shell --name waiters_node@127.0.0.1 --setcookie 'secret' --eval "start_waiters:start()."
```

#### Step 3.3: Start the Machines Node
```bash
rebar3 shell --name machines_node@127.0.0.1 --setcookie 'secret' --eval "start_machines:start()."
```

#### Step 3.4: Start the Tables Node
```bash
rebar3 shell --name tables_node@127.0.0.1 --setcookie 'secret' --eval "start_tables:start()."
```

#### Step 3.5: Start the Customers Node
```bash
rebar3 shell --name customers_node@127.0.0.1 --setcookie 'secret' --eval "start_customers:start()."
```

### 4. Launch the Game GUI

After all nodes are running and connected, launch the game interface from the safe node terminal:

```bash
python3 gui/game_gui.py
```

## Alternative: Automated Startup

If you prefer to start all nodes automatically, you can use the provided script:

```bash
chmod +x run.sh
./run.sh
```

This script will:
1. Start all Erlang nodes in separate terminal windows
2. Wait for network connections to establish
3. Launch the Python GUI automatically

## Troubleshooting

### Common Issues

#### Node Connection Problems
- Ensure all nodes use the same cookie (`secret`)
- Check that IP addresses are correct (default: `127.0.0.1`)
- Verify Erlang is properly installed and accessible

#### Compilation Errors
- Run `rebar3 clean` followed by `rebar3 compile`
- Check that all dependencies are available
- Ensure you're in the project root directory

#### GUI Launch Issues
- Verify pygame is installed: `pip3 list | grep pygame`
- Check that all Erlang nodes are running and connected
- Ensure you're running the GUI command from the safe node

### Verification Commands

Check if nodes are connected:
```erlang
% In any Erlang shell
nodes().
```

Check application status:
```erlang
% In any Erlang shell
application:which_applications().
```

## Network Configuration

The game is configured to run on localhost (`127.0.0.1`) by default. To run on different machines:

1. Update the IP addresses in the start commands
2. Ensure firewall rules allow Erlang communication
3. Use the same cookie across all nodes

**NOTE**: If you're working on physically different nodes (separate machines), you'll need to change all instances of `127.0.0.1` to the actual IP addresses of your machines. Using the "Replace All" function in your text editor will be very helpful since `127.0.0.1` appears in multiple files:

- All the start commands in this README
- The `run.sh` script
- Various Erlang source files (start modules)
- Configuration files

**Example**: If your safe node is on machine `192.168.1.100`, you would replace all instances of `127.0.0.1` with `192.168.1.100` in the relevant files.

## Stopping the Game

To stop the game:
1. Close the Python GUI window
2. In each Erlang shell, press `Ctrl+C` twice
3. Or use `q().` in each Erlang shell

## Development Notes

- The game uses distributed Erlang for inter-node communication
- Each node runs as a separate Erlang application
- The safe_node acts as the central coordinator
- The Python GUI connects to the Erlang backend via network communication

## Support

If you encounter issues:
1. Check the Erlang shell output for error messages
2. Verify all prerequisites are installed
3. Ensure proper network connectivity between nodes
4. Check that all applications compiled successfully 