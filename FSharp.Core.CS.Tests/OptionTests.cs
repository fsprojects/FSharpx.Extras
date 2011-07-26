using System;
using System.Globalization;
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
            var a = Option.Some(5);
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
        public void LINQ_SelectMany() {
            var a = 5.Some();
            var b = 6.Some();
            var c = a.SelectMany(x => b.SelectMany(y => (x+y).Some()));
            Assert.AreEqual(11, c.Value);
        }

        [Test]
        public void LINQ_SelectMany2() {
            var a = 5.Some();
            var b = 6.Some();
            var c = from x in a
                    from y in b
                    select x + y;
            Assert.AreEqual(11, c.Value);
        }

        [Test]
        public void LINQ_SelectMany2_with_none() {
            var a = 5.Some();
            var c = from x in a
                    from y in FSharpOption<int>.None
                    select x + y;
            Assert.IsFalse(c.HasValue());
        }

        [Test]
        public void LINQ_Aggregate() {
            var a = 5.Some();
            var c = a.Aggregate(10, (s, x) => s + x);
            Assert.AreEqual(15, c);
        }

        [Test]
        public void LINQ_Aggregate_None() {
            var a = FSharpOption<int>.None;
            var c = a.Aggregate(10, (s, x) => s + x);
            Assert.AreEqual(10, c);
        }

        [Test]
        public void Match_Some() {
            var a = 5.Some();
            var b = a.Match(x => x + 2, () => 99);
            Assert.AreEqual(7, b);
        }

        [Test]
        public void Match_None() {
            var a = FSharpOption<int>.None;
            var b = a.Match(x => x + 2, () => 99);
            Assert.AreEqual(99, b);
        }

        [Test]
        public void Match_Action() {
            var a = 5.Some();
            a.Match(Console.WriteLine, Assert.Fail);
        }

        [Test]
        public void TryParseInt() {
            var a = Option.TryParseInt("123");
            Assert.AreEqual(123, a.Value);
        }

        [Test]
        public void TryParseDec() {
            var a = Option.TryParseDec("123.44", NumberStyles.Any, CultureInfo.InvariantCulture);
            Assert.AreEqual(123.44m, a.Value);
        }

        [Test]
        public void TryParseDouble() {
            FSharpOption<double> a = Option.TryParseDouble("123E12");
            Assert.AreEqual(123E12, a.Value);
        }

        [Test]
        public void TryParseFloat() {
            FSharpOption<float> a = Option.TryParseFloat("12");
            Assert.AreEqual(12, a.Value);
        }

    }
}