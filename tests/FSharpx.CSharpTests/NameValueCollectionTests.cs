using System;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.Linq;
using NUnit.Framework;
using FSharpx.Collections;

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

        [Test]
        public void ToLookup() {
            var a = new NameValueCollection {
                { "1", "one" },
                { "1", "uno" },
            };
            var l = a.ToLookup();
            Assert.AreEqual(1, l.Count);
            Assert.AreEqual(2, l["1"].Count());
            Assert.AreEqual(0, l["2"].Count());
        }

        [Test]
        public void AsDictionary() {
            var a = new NameValueCollection {
                { "1", "one" },
                { "1", "uno" },
            };
            var d = a.AsDictionary();
            Assert.AreEqual(new[] { "one", "uno" }, d["1"]);
            Assert.Throws<KeyNotFoundException>(() => { var x = d["2"]; });
        }

        [Test]
        public void AsLookup() {
            var a = new NameValueCollection {
                { "1", "one" },
                { "1", "uno" },
            };
            var l = a.AsLookup();
            Assert.AreEqual(1, l.Count);
            Assert.AreEqual(2, l["1"].Count());
            Assert.AreEqual(0, l["2"].Count());
            a.Add("2", "dos");
            Assert.AreEqual(2, l.Count);
            Assert.AreEqual(1, l["2"].Count());
        }
    }
}
