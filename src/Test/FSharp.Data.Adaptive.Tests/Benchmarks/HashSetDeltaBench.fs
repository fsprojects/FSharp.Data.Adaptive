namespace Benchmarks

open System
open FSharp.Data.Adaptive
open FSharp.Data.Traceable
open BenchmarkDotNet.Attributes
open System.Runtime.CompilerServices
open BenchmarkDotNet.Configs


//BenchmarkDotNet=v0.12.1, OS=Windows 10.0.19042
//Intel Core i7-8700K CPU 3.70GHz (Coffee Lake), 1 CPU, 12 logical and 6 physical cores
//.NET Core SDK=5.0.103
//  [Host]     : .NET Core 3.1.12 (CoreCLR 4.700.21.6504, CoreFX 4.700.21.6905), X64 RyuJIT DEBUG
//  DefaultJob : .NET Core 3.1.12 (CoreCLR 4.700.21.6504, CoreFX 4.700.21.6905), X64 RyuJIT

//|               Method | Count |            Mean |          Error |         StdDev |    Gen 0 |    Gen 1 |    Gen 2 | Allocated |
//|--------------------- |------ |----------------:|---------------:|---------------:|---------:|---------:|---------:|----------:|
//| HashSetDelta_toArray |     1 |        33.65 ns |       0.717 ns |       0.670 ns |   0.0140 |        - |        - |      88 B |
//|  HashSetDelta_toList |     1 |        29.30 ns |       0.298 ns |       0.264 ns |   0.0178 |        - |        - |     112 B |
//| HashSetDelta_collect |     1 |        60.83 ns |       0.297 ns |       0.263 ns |   0.0114 |        - |        - |      72 B |
//|  HashSetDelta_choose |     1 |        80.61 ns |       1.622 ns |       1.802 ns |   0.0293 |        - |        - |     184 B |
//|     HashSetDelta_map |     1 |        80.10 ns |       0.692 ns |       0.614 ns |   0.0254 |        - |        - |     160 B |
//| HashSetDelta_toArray |    10 |       206.89 ns |       3.477 ns |       4.004 ns |   0.0713 |        - |        - |     448 B |
//|  HashSetDelta_toList |    10 |       258.62 ns |       4.873 ns |       4.558 ns |   0.1440 |        - |        - |     904 B |
//| HashSetDelta_collect |    10 |       513.56 ns |       3.690 ns |       3.451 ns |   0.1259 |        - |        - |     792 B |
//|  HashSetDelta_choose |    10 |     1,221.75 ns |      23.793 ns |      23.368 ns |   0.4501 |   0.0019 |        - |    2824 B |
//|     HashSetDelta_map |    10 |     1,188.03 ns |      14.238 ns |      13.318 ns |   0.4101 |   0.0019 |        - |    2584 B |
//| HashSetDelta_toArray |    50 |     1,050.46 ns |      13.305 ns |      11.794 ns |   0.3262 |   0.0019 |        - |    2048 B |
//|  HashSetDelta_toList |    50 |     1,343.84 ns |      25.207 ns |      30.007 ns |   0.7038 |   0.0076 |        - |    4424 B |
//| HashSetDelta_collect |    50 |     2,584.56 ns |      40.086 ns |      35.536 ns |   0.6332 |        - |        - |    3992 B |
//|  HashSetDelta_choose |    50 |     7,806.11 ns |      90.428 ns |      80.162 ns |   2.6245 |   0.0610 |        - |   16504 B |
//|     HashSetDelta_map |    50 |     7,596.07 ns |     105.889 ns |      99.049 ns |   2.4338 |   0.0534 |        - |   15304 B |
//| HashSetDelta_toArray |   100 |     2,225.24 ns |      40.207 ns |      37.610 ns |   0.6447 |   0.0038 |        - |    4048 B |
//|  HashSetDelta_toList |   100 |     2,757.42 ns |      18.465 ns |      14.416 ns |   1.4038 |   0.0343 |        - |    8824 B |
//| HashSetDelta_collect |   100 |     5,181.38 ns |      56.571 ns |      50.148 ns |   1.2665 |        - |        - |    7992 B |
//|  HashSetDelta_choose |   100 |    15,877.98 ns |      63.709 ns |      59.593 ns |   5.5542 |   0.2441 |        - |   34984 B |
//|     HashSetDelta_map |   100 |    15,893.42 ns |     206.609 ns |     193.262 ns |   5.1880 |   0.2441 |        - |   32584 B |
//| HashSetDelta_toArray |  1000 |    25,014.55 ns |     343.265 ns |     321.091 ns |   6.3782 |   0.6104 |        - |   40048 B |
//|  HashSetDelta_toList |  1000 |    30,442.11 ns |     130.942 ns |     109.343 ns |  13.9771 |   2.7466 |        - |   88025 B |
//| HashSetDelta_collect |  1000 |    52,601.02 ns |     319.173 ns |     282.939 ns |  12.6953 |        - |        - |   79992 B |
//|  HashSetDelta_choose |  1000 |   225,669.60 ns |   3,316.778 ns |   2,769.660 ns |  69.0918 |   0.4883 |        - |  433912 B |
//|     HashSetDelta_map |  1000 |   215,547.13 ns |   1,167.642 ns |   1,035.084 ns |  65.1855 |   0.2441 |        - |  409913 B |
//| HashSetDelta_toArray | 10000 |   279,727.79 ns |   2,915.267 ns |   2,434.380 ns |  63.4766 |  23.9258 |        - |  400049 B |
//|  HashSetDelta_toList | 10000 |   404,182.83 ns |   1,829.871 ns |   1,622.133 ns | 140.1367 |  69.8242 |        - |  880027 B |
//| HashSetDelta_collect | 10000 |   550,144.62 ns |  10,146.302 ns |   9,490.858 ns | 126.9531 |        - |        - |  799992 B |
//|  HashSetDelta_choose | 10000 | 5,112,571.70 ns | 101,969.925 ns | 191,524.235 ns | 820.3125 | 296.8750 | 101.5625 | 5180008 B |
//|     HashSetDelta_map | 10000 | 4,649,713.97 ns |  92,470.972 ns | 208,722.393 ns | 781.2500 | 359.3750 |  70.3125 | 4940008 B |

// NEW
//|               Method | Count |            Mean |         Error |         StdDev |    Gen 0 |    Gen 1 |   Gen 2 | Allocated |
//|--------------------- |------ |----------------:|--------------:|---------------:|---------:|---------:|--------:|----------:|
//| HashSetDelta_toArray |     1 |        32.43 ns |      0.683 ns |       0.863 ns |   0.0140 |        - |       - |      88 B |
//|  HashSetDelta_toList |     1 |        23.43 ns |      0.138 ns |       0.129 ns |   0.0127 |        - |       - |      80 B |
//| HashSetDelta_collect |     1 |        62.79 ns |      0.283 ns |       0.265 ns |   0.0178 |        - |       - |     112 B |
//|  HashSetDelta_choose |     1 |        94.19 ns |      0.999 ns |       0.934 ns |   0.0318 |        - |       - |     200 B |
//|     HashSetDelta_map |     1 |        88.00 ns |      1.481 ns |       1.386 ns |   0.0280 |        - |       - |     176 B |
//|  HashSetDelta_filter |     1 |        23.15 ns |      0.418 ns |       0.370 ns |   0.0076 |        - |       - |      48 B |
//| HashSetDelta_toArray |    10 |       115.60 ns |      2.291 ns |       2.638 ns |   0.0254 |        - |       - |     160 B |
//|  HashSetDelta_toList |    10 |       162.58 ns |      3.071 ns |       3.016 ns |   0.0587 |        - |       - |     368 B |
//| HashSetDelta_collect |    10 |       443.31 ns |      1.260 ns |       1.052 ns |   0.0520 |        - |       - |     328 B |
//|  HashSetDelta_choose |    10 |     1,205.91 ns |     10.425 ns |       9.751 ns |   0.3376 |   0.0019 |       - |    2120 B |
//|     HashSetDelta_map |    10 |     1,140.73 ns |      2.412 ns |       1.883 ns |   0.2995 |   0.0019 |       - |    1880 B |
//|  HashSetDelta_filter |    10 |       171.83 ns |      0.354 ns |       0.314 ns |   0.0076 |        - |       - |      48 B |
//| HashSetDelta_toArray |    50 |       490.97 ns |      5.726 ns |       5.356 ns |   0.0763 |        - |       - |     480 B |
//|  HashSetDelta_toList |    50 |       812.97 ns |      5.764 ns |       5.110 ns |   0.2623 |   0.0029 |       - |    1648 B |
//| HashSetDelta_collect |    50 |     2,173.74 ns |     15.226 ns |      13.497 ns |   0.2022 |        - |       - |    1288 B |
//|  HashSetDelta_choose |    50 |     7,465.20 ns |    131.634 ns |     161.659 ns |   2.0065 |   0.0610 |       - |   12600 B |
//|     HashSetDelta_map |    50 |     6,901.29 ns |     48.256 ns |      42.778 ns |   1.8158 |   0.0534 |       - |   11400 B |
//|  HashSetDelta_filter |    50 |       833.69 ns |      4.272 ns |       3.996 ns |   0.0076 |        - |       - |      48 B |
//| HashSetDelta_toArray |   100 |     1,097.53 ns |      9.699 ns |       9.072 ns |   0.1392 |        - |       - |     880 B |
//|  HashSetDelta_toList |   100 |     1,704.91 ns |      5.229 ns |       4.891 ns |   0.5169 |   0.0114 |       - |    3248 B |
//| HashSetDelta_collect |   100 |     4,523.45 ns |     78.694 ns |      69.760 ns |   0.3891 |        - |       - |    2488 B |
//|  HashSetDelta_choose |   100 |    15,857.98 ns |    295.777 ns |     316.478 ns |   4.3030 |   0.2594 |       - |   27080 B |
//|     HashSetDelta_map |   100 |    15,271.78 ns |    254.432 ns |     237.996 ns |   3.9063 |   0.2441 |       - |   24680 B |
//|  HashSetDelta_filter |   100 |     1,687.08 ns |     16.197 ns |      15.151 ns |   0.0076 |        - |       - |      48 B |
//| HashSetDelta_toArray |  1000 |    14,428.72 ns |    127.252 ns |     119.032 ns |   1.2817 |   0.0305 |       - |    8080 B |
//|  HashSetDelta_toList |  1000 |    20,322.05 ns |    371.328 ns |     347.340 ns |   5.0964 |   0.8545 |       - |   32048 B |
//| HashSetDelta_collect |  1000 |    46,833.54 ns |    518.605 ns |     485.104 ns |   3.7842 |        - |       - |   24088 B |
//|  HashSetDelta_choose |  1000 |   217,708.50 ns |    896.312 ns |     748.461 ns |  56.3965 |  18.7988 |       - |  354009 B |
//|     HashSetDelta_map |  1000 |   212,728.68 ns |  2,600.566 ns |   2,432.571 ns |  52.4902 |  17.3340 |       - |  330008 B |
//|  HashSetDelta_filter |  1000 |    17,730.73 ns |     33.381 ns |      27.874 ns |        - |        - |       - |      48 B |
//| HashSetDelta_toArray | 10000 |   163,281.76 ns |  1,934.069 ns |   1,809.129 ns |  12.4512 |   2.4414 |       - |   80082 B |
//|  HashSetDelta_toList | 10000 |   244,113.85 ns |  3,929.273 ns |   3,675.444 ns |  50.7813 |  25.3906 |       - |  320048 B |
//| HashSetDelta_collect | 10000 |   478,125.82 ns |  9,153.677 ns |  11,241.538 ns |  38.0859 |        - |       - |  240088 B |
//|  HashSetDelta_choose | 10000 | 4,896,571.35 ns | 97,033.235 ns | 262,334.994 ns | 695.3125 | 328.1250 | 46.8750 | 4380104 B |
//|     HashSetDelta_map | 10000 | 4,523,904.30 ns | 90,366.983 ns | 244,312.394 ns | 656.2500 | 328.1250 | 23.4375 | 4140104 B |
//|  HashSetDelta_filter | 10000 |   193,923.41 ns |  1,333.503 ns |   1,113.535 ns |        - |        - |       - |      48 B |

[<PlainExporter; MemoryDiagnoser>]
type HashSetDeltaBench() =

    let mutable delta = HashSetDelta.empty
   
    [<Params(1, 10, 50, 100, 1000, 10000); DefaultValue>]
    val mutable public Count : int

    [<GlobalSetup>]
    member x.Setup() =
        let rand = Random(123)
        delta <- HashSet.computeDelta HashSet.empty (HashSet.ofArray (Array.init x.Count (fun i -> rand.Next() )))
        
    [<Benchmark>]
    member x.HashSetDelta_toArray() =
        delta |> HashSetDelta.toArray
        
    [<Benchmark>]
    member x.HashSetDelta_toList() =
        delta |> HashSetDelta.toList

    [<Benchmark>]
    member x.HashSetDelta_collect() =
        delta |> HashSetDelta.collect (fun f -> if f.Value > 1000 then HashSetDelta.empty else HashSetDelta.single f)
        
    [<Benchmark>]
    member x.HashSetDelta_choose() =
        delta |> HashSetDelta.choose (fun f -> if f.Value > 1000 then Some f else None)
                
    [<Benchmark>]
    member x.HashSetDelta_map() =
        delta |> HashSetDelta.map (fun f -> if f.Count > 0 then SetOperation.Rem(f.Value) else SetOperation.Add(f.Value))

    [<Benchmark>]
    member x.HashSetDelta_filter() =
        delta |> HashSetDelta.filter (fun f -> f.Count > Int32.MaxValue / 8)