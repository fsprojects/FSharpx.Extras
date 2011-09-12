using System;
using System.Collections.Specialized;
using NUnit.Framework;

namespace FSharpx.CSharpTests {
    [TestFixture]
    public class NameValueCollectionTests {
        [Test]
        public void Concat() {
            var a = new NameValueCollection { { "1", "one" } };
            var b = new NameValueCollection { { "2", "two" } };
            var c = a.Concat(b);
            b.Add("2", "dos");
            Assert.AreEqual("one", c["1"]);
            Assert.AreEqual("two", c["2"]);
            Assert.AreEqual("two,dos", b["2"]);
        }
    }
}
