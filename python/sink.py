#!/usr/bin/python3

################################################################################

import argparse
import socket
import struct
import time

#===============================================================================

import udp

################################################################################

LINE = "".join([ "_" for i_ in range(50) ])

#===============================================================================
def main(args):
    host = ""
    port = args.port
    sink, sink_addr = udp.get_client(addr=(host, port))
    try:
        i_msg = 0
        while True:
            i_msg += 1
            msg, from_addr = udp.recv_msg(sink, encoding="UTF-8")
            msgs = msg.strip(";;;;").split(";;;;")
            print(LINE)
            print("\n".join(msgs))
            print(f"py3 Sink msg #{i_msg}")
            print(f"py3 Sink 0:{sink_addr[1]}" +\
                  f" <= {from_addr[0]}:{from_addr[1]}")
        print(LINE)
    except KeyboardInterrupt:
        print("Terminate Signal Captured")
    except Exception as e:
        print(f"! Error: Caught unexpected exception {e}")
    finally:
        sink.close()
        print("Successfully closed socket.")

#===============================================================================
def parse_args():
    parser = argparse.ArgumentParser()
    parser.add_argument("port", type=int)
    return parser.parse_args()

################################################################################

if "__main__" == __name__:
    args = parse_args()
    main(args)
