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
    }
}