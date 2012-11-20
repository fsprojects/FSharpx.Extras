using System;
using System.Globalization;
using Microsoft.FSharp.Core;
using NUnit.Framework;
using Microsoft.FSharp.Collections;

namespace FSharpx.CSharpTests {
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
            var o = a.ToFSharpOption();
            Assert.AreEqual(99, o.Value);
        }

        [Test]
        public void FromNullable_WithoutValue() {
            int? a = null;
            var o = a.ToFSharpOption();
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
        public void LINQ_Where() {
            var a = 5.Some();
            var c = from i in a where i > 7 select i;
            Assert.IsFalse(c.HasValue());
        }

        [Test]
        public void LINQ_Where2() {
            var a = 5.Some();
            var c = from i in a where i > 3 select i;
            Assert.AreEqual(5, c.Value);
        }

        [Test]
        public void LINQ_Where3() {
            var a = FSharpOption<int>.None;
            var c = from i in a where i > 3 select i;
            Assert.IsFalse(c.HasValue());
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
        public void Match_Value() {
            var a = FSharpOption<int>.None;
            var b = a.Match(x => x + 2, 99);
            Assert.AreEqual(99, b);
        }

        [Test]
        public void Match_Action() {
            var a = 5.Some();
            a.Match(Console.WriteLine, Assert.Fail);
        }

        [Test]
        public void Do() {
            var a = 5.Some();
            a.Do(v => Assert.AreEqual(5, v));
        }

        [Test]
        public void Dont() {
            var a = FSharpOption<int>.None;
            a.Do(_ => Assert.Fail());
        }

        [Test]
        public void TryParseInt() {
            var a = FSharpOption.ParseInt("123");
            Assert.AreEqual(123, a.Value);
        }

        [Test]
        public void TryParseDec() {
            var a = FSharpOption.ParseDecimal("123.44", NumberStyles.Any, CultureInfo.InvariantCulture);
            Assert.AreEqual(123.44m, a.Value);
        }

        [Test]
        public void TryParseDouble() {
            FSharpOption<double> a = FSharpOption.ParseDouble("123E12");
            Assert.AreEqual(123E12, a.Value);
        }

        [Test]
        public void TryParseFloat() {
            FSharpOption<float> a = FSharpOption.ParseFloat("12");
            Assert.AreEqual(12, a.Value);
        }

        [Test]
        public void CastInt() {
            object o = 22;
            FSharpOption<int> a = Option.Cast<int>(o);
            Assert.AreEqual(22, a.Value);
        }

        [Test]
        public void CastInt_null() {
            object o = null;
            FSharpOption<int> a = Option.Cast<int>(o);
            Assert.IsFalse(a.HasValue());
        }

        [Test]
        public void DBNull_is_None() {
            var n = DBNull.Value;
            Assert.IsFalse(n.ToFSharpOption().HasValue());
        }

        [Test]
        public void OrElse() {
            var a = 5.Some();
            var b = a.OrElse(9.Some());
            Assert.AreEqual(5, b.Value);
        }

        [Test]
        public void OrElse_None() {
            var a = FSharpOption<int>.None;
            var b = a.OrElse(9.Some());
            Assert.AreEqual(9, b.Value);
        }

        [Test]
        public void GetOrElse() {
            var a = FSharpOption<int>.None;
            var b = a.GetOrElse(9);
            Assert.AreEqual(9, b);
        }

        [Test]
        public void GetOrElse_function() {
            var a = FSharpOption<int>.None;
            var b = a.GetOrElse(() => 9);
            Assert.AreEqual(9, b);
        }

        [Test]
        public void GetOrDefault_ReferenceType()
        {
            var a = FSharpOption<string>.None;
            var b = a.GetOrDefault();
            Assert.AreEqual(null, b);
        }

        [Test]
        public void GetOrDefault_ValueType()
        {
            var a = FSharpOption<int>.None;
            var b = a.GetOrDefault();
            Assert.AreEqual(0, b);
        }

        [Test]
        public void Sequence_Some() {
            var r = FSharpList.Create(1.Some(), 2.Some(), 3.Some()).Sequence();
            Assert.AreEqual(FSharpList.Create(1,2,3).Some(), r);
        }

        [Test]
        public void Sequence_None() {
            var noInt = FSharpOption<int>.None;
            var r = FSharpList.Create(1.Some(), 2.Some(), noInt).Sequence();
            var noList = FSharpOption<FSharpList<int>>.None;
            Assert.AreEqual(noList, r);
        }

        [Test]
        public void ToFSharpList_None() {
            FSharpOption<int> a = null;
            FSharpList<int> b = a.ToFSharpList();
            Assert.AreEqual(0, b.Length);
        }

        [Test]
        public void ToFSharpList_Some() {
            var a = 5.Some().ToFSharpList();
            Assert.AreEqual(1, a.Length);
            Assert.AreEqual(5, a[0]);
        }

        [Test]
        public void ToArray_None() {
            FSharpOption<int> a = null;
            int[] b = a.ToArray();
            Assert.AreEqual(0, b.Length);
        }

        [Test]
        public void ToArray_Some() {
            var a = 5.Some().ToArray();
            Assert.AreEqual(1, a.Length);
            Assert.AreEqual(5, a[0]);
        }
    }
}