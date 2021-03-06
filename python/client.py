#!/usr/bin/python3

################################################################################

import argparse
import socket
import struct
import time

#===============================================================================

import udp

################################################################################

#===============================================================================
def main(args):
    host = ""
    sockopts = dict()
    sockopts[socket.SOL_SOCKET] = dict()
    sockopts[socket.SOL_SOCKET][socket.SO_BROADCAST] = 1
    client, client_addr = udp.get_client(addr=(host, args.data_port),
                                         sockopts=sockopts)
    sink_addr = args.sink_addr.split(":")
    sink_addr = sink_addr[0], int(sink_addr[1])
    if not client:
        sys.exit(1)
    try:
        i_msg = 0
        while True:
            i_msg += 1
            rx_data, from_addr = udp.recv_msg(client)
            print(f"Msg {i_msg:3} received")
            msg    = f"py3 Client[{args.id}] msg #{i_msg};;;;"
            rx_msg = f"py3 Client[{args.id}] {client_addr[0]}:{client_addr[1]}" +\
                     f" <= {from_addr[0]}:{from_addr[1]};;;;"
            tx_msg = f"py3 Client[{args.id}] {client_addr[0]}:{client_addr[1]}" +\
                     f" => {sink_addr[0]}:{sink_addr[1]};;;;"
            udp.send_msg(client, sink_addr, rx_data, msg, rx_msg, tx_msg)
    except KeyboardInterrupt:
        print("Terminate Signal Captured.")
    except Exception as e:
        print(f"! Error: Caught unexpected exception {e}")
    finally:
        client.close()
        print("Successfully closed client socket.")

#===============================================================================
def parse_args():
    parser = argparse.ArgumentParser()
    parser.add_argument("data_port", type=int)
    parser.add_argument("sink_addr", type=str)
    parser.add_argument("id", type=int)
    return parser.parse_args()

################################################################################

if "__main__" == __name__:
    args = parse_args()
    main(args)
