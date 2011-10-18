using System;
using System.Linq;
using Microsoft.FSharp.Collections;
using Microsoft.FSharp.Core;
using NUnit.Framework;

namespace FSharpx.CSharpTests {
    [TestFixture]
    public class MapTests {
        [Test]
        public void New() {
            FSharpMap<int, string> m1 = FSharpMap.Create(Tuple.Create(1, "one"), Tuple.Create(2, "two"));
            FSharpMap<int, string> m2 = new FSharpMap<int, string>(new[] {
                Tuple.Create(1, "one"), 
                Tuple.Create(2, "two"),
            });
            FSharpMap<int, string> m3 = new FSharpMap<int, string>(Enumerable.Empty<Tuple<int, string>>()) {
                { 1, "one" },
                { 2, "two" },
            };            
            Assert.AreEqual(m1, m2);
        }

        [Test]
        public void TryFind() {
            var map = FSharpMap.Create(Tuple.Create(1, "one"), Tuple.Create(2, "two"));
            var r = map.TryFind(2).Match(x => x, () => "not found");
            Assert.AreEqual("two", r);
        }
    }
}