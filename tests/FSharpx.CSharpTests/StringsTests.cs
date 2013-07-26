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
            (Strings.replace("Hello", "World", "Hello World!")).ShouldEqual("World World!");
        }

        [Test]
        public void WhenReplacingSimplePatternInStringWhichDoesntContainThePattern()
        {
            (Strings.replace("Hello?", "World", "Hello World!")).ShouldEqual("Hello World!");
        }

        [Test]
        public void WhenSplittingAStringWithEmptyGroup()
        {
            string[] strings = Strings.split(',', "Hello,,fsharpx");
            strings.Length.ShouldEqual(3);
            strings[0].ShouldEqual("Hello");
            strings[2].ShouldEqual("fsharpx");
        }

        [Test]
        public void WhenSplittingAStringWithQuestionMark()
        {
            string[] strings = Strings.split('?', "Hello?World?fsharpx");
            strings[0].ShouldEqual("Hello");
            strings[1].ShouldEqual("World");
            strings[2].ShouldEqual("fsharpx");
        }
    }
}