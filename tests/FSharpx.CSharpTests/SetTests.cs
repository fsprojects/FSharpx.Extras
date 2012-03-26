using System;
using Microsoft.FSharp.Collections;
using NUnit.Framework;

namespace FSharpx.CSharpTests {
    [TestFixture]
    public class SetTests {
        [Test]
        public void Create() {
            FSharpSet<int> set1 = FSharpSet.Create(1, 2, 3);
            FSharpSet<int> set2 = new FSharpSet<int>(new[] { 1, 2, 3 });
            Assert.AreEqual(set1, set2);
        }

        [Test]
        public void ToFSharpSet() {
            FSharpSet<int> set = new[] { 1, 2, 3 }.ToFSharpSet();
            Assert.AreEqual(3, set.Count);
        }
    }
}
