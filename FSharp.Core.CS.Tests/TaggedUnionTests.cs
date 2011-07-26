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

        [Test]
        public void New() {
            var a = Choice.New1Of2<int, string>(1);
            var b = FSharpChoice<int, string>.NewChoice1Of2(1);
            Assert.AreEqual(a, b);

            var c = Choice.New2Of2<int, string>("a");
            var d = FSharpChoice<int, string>.NewChoice2Of2("a");
            Assert.AreEqual(c, d);
        }
    }
}