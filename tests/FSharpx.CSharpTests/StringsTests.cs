using NUnit.Framework;

namespace FSharpx.CSharpTests
{
    [TestFixture]
    public class StringsTests
    {
        [Test]
        public void WhenDetectingIfStringStartsWithHelloAndItDoes()
        {
            Assert.IsTrue(Strings.startsWith("Hello", "Hello World"));
        }

        [Test]
        public void WhenDetectingIfStringStartsWithHelloAndItDoesnt()
        {
            Assert.IsFalse(Strings.startsWith("Hello", "World"));
        }

        [Test]
        public void WhenReplacingSimplePatternInStringWhichContainsThePattern()
        {
            Assert.AreEqual(Strings.replace("Hello", "World", "Hello World!"), "World World!");
        }

        [Test]
        public void WhenReplacingSimplePatternInStringWhichDoesntContainThePattern()
        {
            Assert.AreEqual(Strings.replace("Hello?", "World", "Hello World!"), "Hello World!");
        }
    }
}