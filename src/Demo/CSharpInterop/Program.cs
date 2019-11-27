using System;
using FSharp.Data.Adaptive;

namespace CSharpInterop
{
    class Program
    {
        static void Main(string[] args)
        {
            var dict = new ChangeableHashMap<int, int> { { 1, 2 } };
            dict[10] = 100;

            var constant = new ConstantValue<int>(10);

            var c = new ChangeableValue<int>(10);
            var dependent = c.Select(v => v * 2);

            var d = dependent.AddCallback(v => Console.WriteLine("{0}", v));

            dependent.AddMarkingCallback(() => Console.WriteLine("marked"));


            using(Adaptive.Transact)
            {
                c.Value = 100;
            }


            using (Adaptive.Transact)
            {
                c.Value = 321;
            }

            d.Dispose();
        }
    }
}
