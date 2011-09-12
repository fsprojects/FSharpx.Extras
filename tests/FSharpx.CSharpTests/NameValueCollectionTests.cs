using System;
using System.Collections.Specialized;
using System.Linq;
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

        [Test]
        public void ToEnumerable() {
            var a = new NameValueCollection { { "1", "one" } };
            var e = a.ToEnumerable().ToList();
            Assert.AreEqual(1, e.Count);
            Assert.AreEqual(Tuple.Create("1", "one"), e[0]);
        }
    }
}
