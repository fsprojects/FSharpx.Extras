using Microsoft.FSharp.Collections;
using Microsoft.FSharp.Core;
using NUnit.Framework;

namespace FSharpx.CSharpTests {
    [TestFixture]
    public class ListTests {
        [Test]
        public void Match_empty() {
            var a = FSharpList<int>.Empty;
            var s = a.Match(() => "empty", (x, xs) => "non empty");
            Assert.AreEqual("empty", s);
        }

        [Test]
        public void Match_non_empty() {
            var a = FSharpList<int>.Cons(5, FSharpList<int>.Empty);
            var s = a.Match(() => "empty", 
                (x, xs) => "head is " + x + ", tail is " + (xs.IsEmpty ? "empty" : "non-empty"));
            Assert.AreEqual("head is 5, tail is empty", s);
        }

        [Test]
        public void NewList() {
            var a = FSharpList.New(1, 2, 3);
            Assert.AreEqual(3, a.Length);
        }

        [Test]
        public void Choose() {
            var a = FSharpList.New(1.Some(), FSharpOption<int>.None, 3.Some());
            var b = a.Choose(x => x);
            Assert.AreEqual(2, b.Length);
        }

        [Test]
        public void Cons() {
            var a = FSharpList.New(1, 2, 3);
            var b = a.Cons(0);
            var c = FSharpList.Cons(a, 0);
            Assert.AreEqual(FSharpList.New(0, 1, 2, 3), b);
            Assert.AreEqual(FSharpList.New(0, 1, 2, 3), c);
        }

        [Test]
        public void ToFSharpList() {
            var a = new[] { 1, 2, 3 };
            var b = a.ToFSharpList();
            Assert.AreEqual(FSharpList.New(1, 2, 3), b);
        }

        [Test]
        public void TryFind_predicate() {
            var a = FSharpList.New(1, 2, 3);
            a.TryFind(x => x > 4)
                .Match(v => Assert.Fail("shouldn't have found value {0}", v),
                       () => { });
        }

        [Test]
        public void TryFind_value() {
            var a = FSharpList.New(1, 2, 3);
            a.TryFind(2).Match(v => { }, 
                               () => Assert.Fail("Should have found value"));
        }
    }
}