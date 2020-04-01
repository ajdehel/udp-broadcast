#!/usr/bin/python3

################################################################################

import argparse
import pathlib
import signal
import subprocess

################################################################################

#===============================================================================
def start_process(command, logfilename=None, logging=False, **kwargs):
    command_list = command.split() if isinstance(command, str) else command
    logging = logfilename is not None and logging
    logfile = open(logfilename, 'w') if logging else subprocess.DEVNULL
    default_kwargs = dict(stdout=logfile,
                          stderr=logfile,
                          universal_newlines=True)
    default_kwargs.update(kwargs)
    try:
        process = subprocess.Popen(command_list, **default_kwargs)
        print(f"Started `{command}`")
    except OSError as e:
        print(f"! Error {e}")
    return process

#===============================================================================
def check_process(process):
    return process.poll() is None

#===============================================================================
def terminate_process(process):
    status = process.poll()
    if status is not None:
        return status
    else:
        process.send_signal(signal.SIGINT)
        return process.poll()

#===============================================================================
def main(args):
    sink    =   start_process(f"./sink.py {args.sink_port}",
                              stdout=None, stderr=None)
    servers = [ start_process(f"./server.py {args.data_port} {args.sink_port} {i_} -p {args.period}",
                              logfilename=f"server.{i_}.out",
                              logging=args.logging)
                for i_ in range(args.num_servers) ]
    clients = [ start_process(f"./client.py {args.data_port} {args.sink_port} {i_}",
                              logfilename=f"client.{i_}.out",
                              logging=args.logging)
                for i_ in range(args.num_clients) ]
    try:
        while all([check_process(server) for server in servers]):
            if not all([check_process(client) for client in clients]):
                break
    except KeyboardInterrupt as ki:
        print(f"\nKeyboardInterrupt Received")
    except Exception as e:
        print(f"! Error: Caught unexpected exception {e}")
    finally:
        print("Terminating subprocesses")
        server_exit_codes = [ terminate_process(server) for server in servers ]
        client_exit_codes = [ terminate_process(client) for client in clients ]
        sink_exit_code    = terminate_process(sink)

#===============================================================================
def parse_args():
    parser = argparse.ArgumentParser()
    parser.add_argument("data_port", type=int)
    parser.add_argument("sink_port", type=int)
    parser.add_argument("-s", "--num-servers", type=int,
                        dest="num_servers", default=1)
    parser.add_argument("-c", "--num-clients", type=int,
                        dest="num_clients", default=1)
    parser.add_argument("-p", "--period", type=float, default=5)
    parser.add_argument("-l", "--logging", action='store_true', dest="logging")
    return parser.parse_args()

################################################################################

if "__main__" == __name__:
    args = parse_args()
    main(args)
