using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Microsoft.FSharp.Collections;
using Microsoft.FSharp.Core;
using NUnit.Framework;

namespace FSharp.Core.CS.Tests {
    [TestFixture]
    public class SetTests {
        [Test]
        public void New() {
            FSharpSet<int> set = FSharpSet.New(1, 2, 3);
            FSharpSet<int> set2 = new FSharpSet<int>(new[] { 1, 2, 3 });
            Assert.AreEqual(set, set2);
        }

        [Test]
        public void ToSet() {
            FSharpSet<int> set = new[] { 1, 2, 3 }.ToFSharpSet();

        }
    }
}
