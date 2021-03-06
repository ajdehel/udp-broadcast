# udp-broadcast

## The Example

An example can be set up and run from one machine, simulating a simple network. Network namespaces with names of the form py_vhost\* are created and can interface with each other through a bridge interface named py_br1. These interfaces are set up and torn down with example.py.

### Setup first

```
$ sudo ./example.py setup [-c MAX_CLIENTS] [-n NETWORK]
```

`NETWORK` is an IPv4 address with 0 as the last octet. It defaults to `192.168.10.0`

`MAX_CLIENTS` informs how many interfaces will be created beyond the bridge interface and host for sink.py.

You can see what this command has created with:
```
$ ip addr show
$ ip netns
$ sudo ip netns exec <HOST> ip addr show
```

`HOST` in the above command would be any of the outputs from `ip netns`. For example

```
$ ip netns
...
py_vhost5 (id: 3)
...

$ sudo ip netns exec py_vhost5 ip addr show
...
```

### To Run:

```
$ sudo ./example.py run CLIENT_PORT SINK_PORT [-i IMPLEMENTATION] [-s NUM_SERVERS] [-c NUM_CLIENTS] [-n NETWORK] [-p PERIOD]
```

`IMPLEMENTATION` is one of \<c++|python\>. It allows you to choose the implementation of the server, client and sink programs.

`NETWORK` is an IPv4 address with 0 as the last octet. It defaults to `192.168.10.0`.

`PERIOD` is a length of time during which a message will be sent. It defaults to 5.

`NUM_CLIENTS` and `NUM_SERVERS` inform how many instances of server.py and client.py should be run. The number for either should not be greater than the number supplied to `MAX_CLIENTS` in setup.

`BROADCAST_PORT` is what clients will listen to and servers will send to.

`SINK_PORT` is what the sink will listen to and clients will send to.

### Teardown

```
$ sudo ./example.py teardown
```

You can verify that everything has born torn down with:
```
$ ip addr show
$ ip netns
$ sudo ip netns exec <HOST> ip addr show
```

## References

The commands to create the virtual network came from Ciro S. Costa via https://ops.tips/blog/using-network-namespaces-and-bridge-to-isolate-servers/
