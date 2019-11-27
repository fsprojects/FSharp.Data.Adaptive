using System;
using FSharp.Data.Adaptive;

namespace CSharpInterop
{
    class Program
    {
        static void Main(string[] args)
        {
            var changeableSet = new ChangeableHashSet<int>(new[] { 1, 2, 3, 4 });

            var dependent =
                changeableSet
                .Filter(a => a < 10)
                .MapAdaptive(a => AdaptiveValue.Constant(a))
                .MapNullable(a => a < 3 ? (int?)a : null)
                .Collect(a => new[] { a % 2 }.ToAdaptiveHashSet())
                .FilterAdaptive(a => AdaptiveValue.Constant(true))
                .UnionWith(new[] { 4, 5, 6 }.ToAdaptiveHashSet())
                .SortBy(a => -a);

            dependent.AddCallback((s, d) => { Console.WriteLine("{0}, {1}", s, d); });

            Console.WriteLine("{0}", dependent.GetValue());
            using (Adaptive.Transact)
            {
                changeableSet.Value = new HashSetBuilder<int>() { 5, 6, 7, 8, 9, 0 }.ToHashSet();
            }
            Console.WriteLine("{0}", dependent.GetValue());


        }
    }
}
