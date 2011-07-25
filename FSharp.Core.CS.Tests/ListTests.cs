using Microsoft.FSharp.Collections;
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
    }
}