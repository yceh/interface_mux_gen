SystemVerilog requires index to an array of interface to be a constant known at compile time, because each interface in the "array" can be parametrized differently.
However, this is not very convenient when I actually want to multiplex an array of homogenous interfaces, so I built this work-in-progress interface multiplexer generator. Current output still need some manual editing.

It takes a single interface declaration as input, expecting a modport named "master" and another modport named "slave" and generate a module that multiplexes multiple "slave" interfaces to a single "master" interface.

Only packed types are supported currently. The behavior of output ports on "slave" modport is decided by the name of the port. If the port name contains "valid" or "ready", it is high only if that "slave" interface is selected, otherwise, the value from the "master" interface is broadcasted.  Once I got time, I will support unpacked type properly, and allow behavior of output ports in the multiplexed interface to be specified from the comments.

