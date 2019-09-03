using NUnit.Framework;
using FSharpx.Text;
using System;

namespace FSharpx.CSharpTests
{
    [TestFixture]
    public class StringsTests
    {
        [Test]
        public void WhenDetectingIfStringStartsWithHelloAndItDoes()
        {
            Assert.IsTrue(Strings.StartsWith("Hello", "Hello World"));
        }

        [Test]
        public void WhenDetectingIfStringStartsWithHelloAndItDoesnt()
        {
            Assert.IsFalse(Strings.StartsWith("Hello", "World"));
        }

        [Test]
        public void WhenReplacingSimplePatternInStringWhichContainsThePattern()
        {
            (Strings.Replace("Hello", "World", "Hello World!")).ShouldEqual("World World!");
        }

        [Test]
        public void WhenReplacingSimplePatternInStringWhichDoesntContainThePattern()
        {
            (Strings.Replace("Hello?", "World", "Hello World!")).ShouldEqual("Hello World!");
        }

        [Test]
        public void WhenSplittingAStringWithEmptyGroup()
        {
            string[] strings = Strings.Split(',', "Hello,,fsharpx");
            strings.Length.ShouldEqual(3);
            strings[0].ShouldEqual("Hello");
            strings[2].ShouldEqual("fsharpx");
        }

        [Test]
        public void WhenSplittingAStringWithQuestionMark()
        {
            string[] strings = Strings.Split('?', "Hello?World?fsharpx");
            strings[0].ShouldEqual("Hello");
            strings[1].ShouldEqual("World");
            strings[2].ShouldEqual("fsharpx");
        }


        [Test]
        public void WhenAnExistingStringFunctionIsUsed()
        {
            string strings = String.Concat(Strings.Split('?', "Hello?World?fsharpx"));
        }
    }
}