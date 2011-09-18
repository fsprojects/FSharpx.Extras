using NUnit.Framework;

namespace FSharpx.CSharpTests
{
    [TestFixture]
    public class StringTests
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
    }
}