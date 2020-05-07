# Requires

GNAT PRO

# To Build

```
$ gprbuild -p
```

The `-p` ensures directories that are needed, are built.

On a successful build, all executables will be in the bin/ directory

# To Run

```
$ server_ada <BROADCAST_IP>:<PORT> <INSTANCE_NUM> [-p <PERIOD>]
```

```
$ client_ada <LISTENER_PORT> <SINK_IP>:<PORT> <INSTANCE_NUM>
```

```
$ sink_ada <SINK_PORT>
```


