#!/usr/bin/python3

################################################################################

import argparse
import errno
import re
import sys

#===============================================================================

sys.path.append("./python")
from  utils import IPv4Addr
import utils

################################################################################

DEFAULT_NETWORK = "192.168.10.0"

################################################################################

#===============================================================================
def setup(args):
    """Run a series of commands to set up network interfaces."""
    def command_and_status(command, fill=70, print_success=False):
        returncode = utils.complete_command(command, echo=False)
        if returncode != 0 or print_success:
            command_str = f"`{command}`"
            status_str  =  "SUCCESS" if returncode == 0 else "FAILURE"
            print(f"{command_str:.<70}{status_str}")
        return returncode == 0
    bridge_if = f"{args.if_prefix}br1"
    host_ip = IPv4Addr(args.network)
    total_interfaces = 1 + 1 + args.max_clients
    # Set up bridge interface
    host_ip[-1] = 1
    print(f"\nSetting up {bridge_if} at {host_ip}")
    bridge_commands = (
        f"ip link add name {bridge_if} type bridge",
        f"ip link set {bridge_if} up",
        f"ip addr add {host_ip} brd + dev {bridge_if}" )
    br_statuses = [ command_and_status(command, fill=70, print_success=False)
                    for command in bridge_commands ]
    ns_statuses = list()
    # Set up virtual host interface
    for i_namespace in range(2, total_interfaces+1):
        host_ip[-1] += 1
        namespace = f"{args.if_prefix}vhost{i_namespace}"
        veth      = f"{args.if_prefix}veth{i_namespace}"
        veth_peer = f"{args.if_prefix}br-veth{i_namespace}"
        print(f"Setting up {namespace} at {host_ip}")
        namespace_commands = (
            f"ip netns add {namespace}",
            f"ip link add {veth} type veth peer name {veth_peer}",
            f"ip link set {veth} netns {namespace}",
            f"ip netns exec {namespace} ip addr add {host_ip}/24 dev {veth}",
            f"ip link set {veth_peer} up",
            f"ip netns exec {namespace} ip link set {veth} up",
            f"ip link set {veth_peer} master {bridge_if}")
        cmd_statuses = [ command_and_status(command, fill=70, print_success=False)
                        for command in namespace_commands ]
        ns_statuses.append(all(cmd_statuses))
    if all(br_statuses) and all(ns_statuses):
        print(f"Example set up for {total_interfaces-2} max clients.")


################################################################################

#===============================================================================
def teardown(args):
    def teardown_bridge(if_prefix):
        # check and get bridge name
        bridge_if = f"{if_prefix}br1"
        stdout = utils.output_command(f"ip link show", echo=False)[0]
        match = re.search(f"{bridge_if}", stdout)
        if match:
            bridge_if = match.group(0)
            commands = (
                f"ip link set {bridge_if} down",
                f"ip link del {bridge_if}" )
            for command in commands:
                command_str = f"`{command}`"
                print(f"{command_str:.<30}", end='')
                returncode = utils.complete_command(command, echo=False)
                status = "SUCCESS" if returncode == 0 else "FAILURE"
                print(status)
        else:
            print(f"Did not find '{bridge_if}'.")
    def teardown_namespace(if_prefix):
        stdout = utils.output_command(f"ip netns")[0]
        matches = re.findall(f"{if_prefix}vhost[0-9]+", stdout)
        if matches:
            for namespace in matches:
                command = f"ip netns del {namespace}"
                command_str = f"`{command}`"
                print(f"{command_str:.<30}", end='')
                returncode = utils.complete_command(command, echo=False)
                status = "SUCCESS" if returncode == 0 else "FAILURE"
                print(status)
        else:
            print(f"Did not find namespaces matching '{if_prefix}vhost*'.")
    if_prefix = args.if_prefix
    teardown_bridge(if_prefix)
    print()
    teardown_namespace(if_prefix)

################################################################################

#===============================================================================
def run(args):
    def ip_netns_exec(namespace, command, **kwargs):
        ip_netns_cmd = f"ip netns exec {namespace} {command}"
        return utils.start_process(ip_netns_cmd, echo=True, **kwargs)
    network_ip = IPv4Addr(args.network)
    bc_ip     = IPv4Addr(args.network)
    bc_ip[-1] = 255
    bc_addr   = f"{bc_ip}:{args.broadcast_port}"
    sink_ip     = IPv4Addr(args.network)
    sink_ip[-1] = 2
    sink_addr   = f"{sink_ip}:{args.sink_port}"
    if_prefix = args.if_prefix
    sink    =   ip_netns_exec(f"{if_prefix}vhost2",
                    f"./sink.py {args.sink_port}", stdout=None, stderr=None)
    clients = [ ip_netns_exec(f"{if_prefix}vhost{3+i_}",
                    f"./client.py {args.broadcast_port} {sink_addr} {i_}",
                    logfilename=f"client.{i_}.out" if args.logging else None)
                for i_ in range(args.num_clients) ]
    servers = [ ip_netns_exec(f"{if_prefix}vhost{3+i_}",
                    f"./server.py {bc_addr} {i_} -p {args.period}",
                    logfilename=f"server.{i_}.out" if args.logging else None)
                for i_ in range(args.num_servers) ]
    try:
        while all([utils.check_process(server) for server in servers]):
            if not all([utils.check_process(client) for client in clients]):
                break
    except KeyboardInterrupt as ki:
        print(f"\nKeyboardInterrupt Received")
    except Exception as e:
        print(f"! Error: Caught unexpected exception {e}")
    finally:
        print("Terminating subprocesses")
        server_exit_codes = [ utils.terminate_process(server) for server in servers ]
        client_exit_codes = [ utils.terminate_process(client) for client in clients ]
        sink_exit_code    = utils.terminate_process(sink)

################################################################################

#===============================================================================
def main(args):
    if not utils.has_root_privileges():
        print("Root privileges required")
        sys.exit(errno.EPERM)
    function = args.func
    function(args)

#===============================================================================
DEFAULT_PREFIX = "py_"

def parse_args():
    parser = argparse.ArgumentParser()
    parser.add_argument("-l", "--logging", action='store_true', dest="logging")
    parser.add_argument("-p", "--if-prefix", type=str,
                        dest="if_prefix", default=DEFAULT_PREFIX)
    subparsers = parser.add_subparsers()
    # Parser for `run` command
    r = subparsers.add_parser("run", help="run the example")
    r.add_argument("broadcast_port", type=int,
                   help="Port messages are broadcast to")
    r.add_argument("sink_port", type=int,
                   help="Port that clients send messages to")
    r.add_argument("-n", "--network", type=str,
                   dest="network", default=DEFAULT_NETWORK,
                   help="IP addr in the form X.Y.Z.0" )
    r.add_argument("-s", "--num-servers", type=int,
                   dest="num_servers", default=1)
    r.add_argument("-c", "--num-clients", type=int,
                   dest="num_clients", default=1)
    r.add_argument("-p", "--period", type=float, default=5)
    r.set_defaults(func=run)
    # Parser for `setup` command
    s = subparsers.add_parser("setup", help="set up virtual network")
    s.add_argument("-c", "--max-clients", type=int,
                   dest="max_clients", default=12)
    s.add_argument("-n", "--network", type=str,
                   dest="network", default=DEFAULT_NETWORK)
    s.set_defaults(func=setup)
    # Parser for `teardown` command
    t = subparsers.add_parser("teardown", help="tear down virtual network")
    t.set_defaults(func=teardown)
    # Parse and return args
    return parser.parse_args()

################################################################################

if "__main__" == __name__:
    args = parse_args()
    main(args)
