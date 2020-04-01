#!/usr/bin/python3

################################################################################

import argparse
import random
import socket
import time

#===============================================================================

import udp

################################################################################

#===============================================================================
def main(args):
    random.seed(args.id)
    broadcast_addr = ("127.255.255.255", args.data_port)
    sockopts = dict()
    sockopts[socket.SOL_SOCKET] = dict()
    sockopts[socket.SOL_SOCKET][socket.SO_BROADCAST] = 1
    server, server_addr = udp.get_server(sockopts=sockopts)
    if not server:
        sys.exit(1)
    try:
        i_msg= 0
        while True:
            i_msg += 1
            sendtime = args.period * random.random()
            waittime = args.period - sendtime
            time.sleep(sendtime)
            msg    = f"Server[{args.id}] msg #{i_msg};;;;"
            tx_msg = f"Server[{args.id}] 0.0.0.0:0" +\
                     f" => {broadcast_addr[0]}:{broadcast_addr[1]};;;;"
            if not udp.send_msg(server, broadcast_addr, msg, tx_msg):
                break
            time.sleep(waittime)
    except KeyboardInterrupt:
        print("Terminate Signal Captured")
    finally:
        udp.close_socket(server)
        print("Successfully closed server socket.")

#===============================================================================
def parse_args():
    parser = argparse.ArgumentParser()
    parser.add_argument("data_port", type=int)
    parser.add_argument("logger_port", type=int)
    parser.add_argument("id", type=int)
    parser.add_argument("-p", "--period", type=float, default=5)
    return parser.parse_args()

################################################################################

if "__main__" == __name__:
    args = parse_args()
    main(args)