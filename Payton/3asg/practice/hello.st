execname := 'hello.st'.

nl := Character nl.

stdout << 'Hello, World!' << nl.

status := 127.
stderr << execname << ': ' << 'Quitting with status ' << status << nl.
ObjectMemory quit: status.

"Test: hell.st"

