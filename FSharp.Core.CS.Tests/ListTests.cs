using Microsoft.FSharp.Collections;
using Microsoft.FSharp.Core;
using NUnit.Framework;

namespace FSharp.Core.CS.Tests {
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
    }
}