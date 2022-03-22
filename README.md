# websockets

A fast, lightweight websockets server implementation. Supports version 13 of the core websocket protocol.

Fork of the [websockets](https://wiki.call-cc.org/eggref/4/websockets) egg to CHICKEN 5 scheme. Original module authored by [Thomas Hintz](http://thintz.com/) with contributions from Seth Alves.

## Installation

`chicken-install` in the repository root

## Usage

See original [websockets](https://wiki.call-cc.org/eggref/4/websockets) documentation for more information.

### Quick start example
Launch the sample implementation from the test directory with
```
csi -s example.scm
```
Navigate to [http://localhost:8080](http://localhost:8080) to send and receive messages

## Test
Passes sections 1-10 of the [Autobahn](https://crossbar.io/autobahn/) websocket compliance test suite.

Build and start the echo server:
```
csc -O3 echo-server.scm;./echo-server
```

Run the test suite against the listening server from another terminal:
```
docker run -it --rm \
    -v "${PWD}/test:/config" \
    -v "${PWD}/reports:/reports" \
    --net="host" \
    crossbario/autobahn-testsuite \
    wstest --mode fuzzingclient --spec /config/ws-test.spec
```

## Changelog
* Replaced various imported modules to CHICKEN 5 compatible modules
* Repackaged setup to declarative egg syntax
* Fix out-of-range exception when constructing u8vector from 2 byte close code
* Fix error for non-toplevel definitions outside of a body context (shift)
* Validated against autobahn test suite
* Formatting, indentation

## License
[BSD-3-Clause](https://opensource.org/licenses/BSD-3-Clause), see [LICENSE](/LICENSE)