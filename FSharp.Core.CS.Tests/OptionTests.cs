using Microsoft.FSharp.Core;
using NUnit.Framework;

namespace FSharp.Core.CS.Tests {
    [TestFixture]
    public class OptionTests {
        [Test]
        public void HasValue() {
            var a = FSharpOption<int>.Some(5);
            Assert.IsTrue(a.HasValue());
        }

        [Test]
        public void NotHasValue() {
            var a = FSharpOption<int>.None;
            Assert.IsFalse(a.HasValue());
        }

        [Test]
        public void FromNullable_WithValue() {
            int? a = 99;
            var o = a.ToOption();
            Assert.AreEqual(99, o.Value);
        }

        [Test]
        public void FromNullable_WithoutValue() {
            int? a = null;
            var o = a.ToOption();
            Assert.False(o.HasValue());
        }

        [Test]
        public void Some() {
            var a = FSharpOption.Some(5);
            Assert.AreEqual(5, a.Value);
        }

        [Test]
        public void SomeExtensionMethod() {
            var a = 5.Some();
            Assert.AreEqual(5, a.Value);
        }

        [Test]
        public void LINQ_Select() {
            var a = from i in 5.Some() select i;
            Assert.AreEqual(5, a.Value);
        }

        [Test]
        public void LINQ_Where() {
            var a = from i in 5.Some() where i > 2 select i;
            Assert.AreEqual(5, a.Value);
        }

        [Test]
        public void LINQ_SelectMany() {
            var a = 5.Some();
            var b = 6.Some();
            var c = a.SelectMany(x => b.SelectMany(y => (x+y).Some()));
            Assert.AreEqual(11, c.Value);
        }
    }
}