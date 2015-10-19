See https://github.com/michaxm/hdfs-thrift-bindings - this contains a Haskell client for that API.

setup of the hs-thrift bindings:
At the moment, the thrift packages in hackage seems to be broken (at least for me), I solved this by
downloading thrift, descending into lib/hs directory (let stack work out the dependencies), fixed the build and linked
that directory ( to "thrift", see stack.yaml).

The sources here are generated with version 0.9.3.
