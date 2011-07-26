using Microsoft.FSharp.Core;
using NUnit.Framework;

namespace FSharp.Core.CS.Tests {
    [TestFixture]
    public class TaggedUnionTests {
        [Test]
        public void Match() {
            var a = FSharpChoice<int, string>.NewChoice1Of2(1);
            var c = a.Match(i => 0, _ => 1);
            Assert.AreEqual(0, c);
        }
    }
}