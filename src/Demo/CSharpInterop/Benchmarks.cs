using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using BenchmarkDotNet.Attributes;
using CSharp.Data.Adaptive;
using FSharp.Data.Adaptive;

namespace CSharpInterop
{
    //BenchmarkDotNet=v0.12.1, OS=Windows 10.0.19042
    //Intel Core i7-8700K CPU 3.70GHz(Coffee Lake), 1 CPU, 12 logical and 6 physical cores
    //.NET Core SDK = 5.0.103

    // [Host]     : .NET Core 3.1.12 (CoreCLR 4.700.21.6504, CoreFX 4.700.21.6905), X64 RyuJIT
    //  DefaultJob : .NET Core 3.1.12 (CoreCLR 4.700.21.6504, CoreFX 4.700.21.6905), X64 RyuJIT

    //|                Method |      Mean |     Error |    StdDev |  Gen 0 | Gen 1 | Gen 2 | Allocated |
    //|---------------------- |----------:|----------:|----------:|-------:|------:|------:|----------:|
    //|       HashSet_Foreach | 23.771 us | 0.1822 us | 0.1704 us | 5.0659 |     - |     - |   31968 B |
    //|   HashSet_ForEachIter | 10.961 us | 0.0822 us | 0.0729 us | 0.0153 |     - |     - |     112 B |
    //|          HashSet_Fold | 10.112 us | 0.0241 us | 0.0225 us |      - |     - |     - |      24 B |
    //| HashSet_LinqAggregate | 34.105 us | 0.1605 us | 0.1501 us | 5.0659 |     - |     - |   32056 B |
    //|     IndexList_Foreach |  6.330 us | 0.0200 us | 0.0167 us | 0.9613 |     - |     - |    6040 B |
    //| IndexList_ForEachIter |  4.964 us | 0.0347 us | 0.0290 us | 0.0153 |     - |     - |     112 B |
    //|        IndexList_Fold |  4.702 us | 0.0886 us | 0.0786 us | 0.0076 |     - |     - |      48 B |
    //|       HashMap_Foreach | 32.054 us | 0.2642 us | 0.2206 us | 8.9111 |     - |     - |   55968 B |
    //|   HashMap_ForEachIter | 11.683 us | 0.1114 us | 0.1042 us | 0.0153 |     - |     - |     112 B |

    [MemoryDiagnoser, PlainExporter]
    public class ForEachBenchmarks
    {
        FSharpHashSet<int> hs = FSharpHashSet.OfSeq(Enumerable.Range(0, 1000));
        IndexList<int> il = IndexList.OfSeq(Enumerable.Range(0, 1000));
        FSharpHashMap<int, int> hm = Enumerable.Range(0, 1000).Select(x => (x, x)).ToFSharpHashMap();

        [Benchmark]
        public int HashSet_Foreach()
        {
            int sum = 0;
            foreach (var v in hs)
                sum += v;
            return sum;
        }

        [Benchmark]
        public int HashSet_ForEachIter()
        {
            int sum = 0;
            hs.ForEach(v => sum += v);
            return sum;
        }

        [Benchmark]
        public int HashSet_Fold()
        {
            return hs.Fold(0, (a, b) => a + b);
        }

        [Benchmark]
        public int HashSet_LinqAggregate()
        {
            return hs.Aggregate(0, (a, b) => a + b);
        }

        [Benchmark]
        public int IndexList_Foreach()
        {
            int sum = 0;
            foreach (var v in il)
                sum += v;
            return sum;
        }

        [Benchmark]
        public int IndexList_ForEachIter()
        {
            int sum = 0;
            il.ForEach(v => sum += v);
            return sum;
        }

        [Benchmark]
        public int IndexList_Fold()
        {
            return il.Fold(0, (a, b) => a + b);
        }

        [Benchmark]
        public int HashMap_Foreach()
        {
            int sum = 0;
            foreach (var kv in hm)
                sum += kv.Item1 + kv.Item2;
            return sum;
        }

        [Benchmark]
        public int HashMap_ForEachIter()
        {
            int sum = 0;
            hm.ForEach((k, v) => sum += k + v);
            return sum;
        }
    }
}
