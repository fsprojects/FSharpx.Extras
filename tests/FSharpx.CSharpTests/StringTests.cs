using NUnit.Framework;

namespace FSharpx.CSharpTests
{
    [TestFixture]
    public class StringTests
    {
        [Test]
        public void WhenDetectingIfStringStartsWithHelloAndItDoes()
        {
            Assert.IsTrue(String.startsWith("Hello", "Hello World"));
        }

        [Test]
        public void WhenDetectingIfStringStartsWithHelloAndItDoesnt()
        {
            Assert.IsFalse(String.startsWith("Hello", "World"));
        }
    }
}