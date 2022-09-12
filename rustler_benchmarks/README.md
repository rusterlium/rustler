# Rustler Benchmarks

A collection of benchmark to test the performance of certain rustler features.

## Running a benchmark

Each module can be run as a benchmark on the command line. Note that the `benchmark` crate is compiled with mode `release`!

```
$ mix run -e Benchmark.NifStruct.run
Name                        ips        average  deviation         median         99th %
decode                 404.50 K        2.47 μs   ±651.07%        1.63 μs        7.23 μs
decode and encode      117.27 K        8.53 μs   ±306.83%        6.34 μs       33.76 μs

Comparison:
decode                 404.50 K
decode and encode      117.27 K - 3.45x slower +6.06 μs

$ mix run -e Benchmark.NifRecord.run
Name                        ips        average  deviation         median         99th %
decode                   1.20 M        0.83 μs   ±689.21%        0.49 μs        2.22 μs
decode and encode        0.80 M        1.25 μs  ±1472.17%        0.65 μs        3.51 μs

Comparison:
decode                   1.20 M
decode and encode        0.80 M - 1.50x slower +0.42 μs
```
