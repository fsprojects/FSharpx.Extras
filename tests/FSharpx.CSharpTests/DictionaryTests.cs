using System;
using System.Collections.Generic;
using NUnit.Framework;
using Microsoft.FSharp.Core;

namespace FSharpx.CSharpTests {
    [TestFixture]
    public class DictionaryTests {
        [Test]
        public void TryFind() {
            var d = new Dictionary<int, string> {
                {1, "one"},
                {2, "two"},
            };
            var r = d.TryFind(1).Match(x => x, () => "not found");
            Assert.AreEqual("one", r);
        }
    }
}
