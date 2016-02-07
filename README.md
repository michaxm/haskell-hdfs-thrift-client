Warning: abandoned, obsolete. Although this approach works, it is slow and tedious compared to for example hadoop-rpc, which uses the Hadoop RPC API (protocol buffers) instead of Thrift. I switched to this completely.

See https://github.com/michaxm/hdfs-thrift-bindings - this contains a Haskell client for that API.

setup of the hs-thrift bindings:
At the moment, the thrift packages in hackage seems to be broken (at least for me), I solved this by
downloading thrift, descending into lib/hs directory (let stack work out the dependencies), fixed the build and linked
that directory ( to "thrift", see stack.yaml).

ln -s <path-to-thrift>/thrift-0.9.3/lib/hs thrift

The sources here are generated with version 0.9.3.

<path-to-thrift-compiler> -gen hs -out src-gen-thrift -v -r <path-to-hdfs-thrift-bindings>/src/main/resources/hadoopfs.thrift
