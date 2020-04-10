
import argparse
import socket
import struct
import time

################################################################################

#===============================================================================
def get_socket(addr=None, sockopts=dict()):
    try:
        sock = socket.socket(family=socket.AF_INET,
                             type=socket.SOCK_DGRAM,
                             proto=socket.IPPROTO_UDP)
        for protocol in sockopts:
            for sockopt, value in sockopts[protocol].items():
                sock.setsockopt(protocol, sockopt, value)
        if addr:
            sock.bind(addr)
    except OSError as e:
        print(f"! Error: Could not create socket; {e}")
        sock = sock
    finally:
        return sock, addr

#===============================================================================
def get_server(addr=None, sockopts=dict()):
    server, server_addr = get_socket(addr=addr, sockopts=sockopts)
    return server, server_addr

#===============================================================================
def get_client(addr=None, sockopts=dict()):
    client, client_addr = get_socket(addr=addr, sockopts=sockopts)
    return client, client_addr

#===============================================================================
def send_msg(sock, addr, *msgs):
    buf = bytearray()
    for msg in msgs:
        msg = msg if isinstance(msg, bytes) else bytes(msg, encoding="UTF-8")
        buf.extend(msg)
    try:
        sent_bytes = sock.sendto(buf, addr)
        if not sent_bytes is len(buf):
            print(f"! Warning: {len(data)} != {sent_bytes} bytes transmitted")
        msg_sent = True
    except OSError as e:
        print(f"! Error: Could not send message; {e}")
        msg_sent = False
    finally:
        return msg_sent

#===============================================================================
def recv_msg(sock, encoding=None):
    try:
        rx_data, from_addr = sock.recvfrom(4096)
        msg = str(rx_data, encoding=encoding) if encoding else rx_data
    except OSError as e:
        print(f"! Error: Could not receive data; {e}")
        msg       = None
        from_addr = None
    return msg, from_addr

#===============================================================================
def close_socket(sock):
    try:
        sock.close()
        is_closed = True
    except KeyboardInterrupt:
        is_closed = close_socket(sock)
    except OSError as e:
        print("! Error: Could not close socket; {e}")
        is_closed = False
    finally:
        return is_closed

