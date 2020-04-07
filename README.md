# udp-broadcast

## The Example

An example can be set up and run from one machine, simulating a simple network. Network namespaces with names of the form py_vhost\* are created and can interface with each other through a bridge interface named py_br1. These interfaces are set up and torn down with example.py.

### Setup first

$ sudo ./example.py setup [-c MAX_CLIENTS] [-n NETWORK]
NETWORK is an IPv4 address with 0 as the last octet. It defaults to 192.168.10.0

### To Run:

$ sudo ./example.py setup CLIENT_PORT SINK_PORT [-s NUM_SERVERS] [-c NUM_CLIENTS] [-n NETWORK] [-p PERIOD]
NETWORK is an IPv4 address with 0 as the last octet. It defaults to "192.168.10.0".
PERIOD is a length of time during which a message will be sent. It defaults to 5.

### Teardown

$ sudo ./example.py teardown


