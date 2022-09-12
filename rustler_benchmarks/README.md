# Rustler Benchmarks

A collection of benchmark to test the performance of certain rustler features.

## Running a benchmark

Each module can be run as a benchmark on the command line:

```
$ mix run -e Benchmark.NifStruct.run
Name                        ips        average  deviation         median         99th %
decode                 160.48 K        6.23 μs   ±294.52%        3.89 μs       19.07 μs
decode and encode       66.93 K       14.94 μs   ±167.67%        9.92 μs       56.48 μs

Comparison:
decode                 160.48 K
decode and encode       66.93 K - 2.40x slower +8.71 μs

$ mix run -e Benchmark.NifRecord.run
...
Name                        ips        average  deviation         median         99th %
decode                 128.04 K        7.81 μs   ±263.53%        4.90 μs       27.33 μs
decode and encode       90.70 K       11.03 μs   ±191.96%        6.66 μs       39.91 μs

Comparison:
decode                 128.04 K
decode and encode       90.70 K - 1.41x slower +3.22 μs
```
