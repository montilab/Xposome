# uuidtools
Tools for working with Universally Unique Identifiers (UUIDs)

This is a substantial extension of the [uuid](https://github.com/s-u/uuid) R package written and maintained by Simon Urbanek.  It will hopefully be integrated into that package in the near future.

It provides the following added functionality:
* R wrapper interfaces to all C functions in [libuuid](https://github.com/tytso/e2fsprogs/tree/master/lib/uuid) from Theodore Ts'o's e2fsprogs suite
* S4 objects for compactly representing UUIDs and lists of UUIDs
* S4 methods for performing routine operations (comparison, matching, sorting, coercion) with these S4 objects
