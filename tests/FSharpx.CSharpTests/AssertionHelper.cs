using NUnit.Framework;

namespace FSharpx.CSharpTests
{
    public static class AssertionHelper
    {
        public static void ShouldEqual<T>(this T actual, T expected)
        {
            Assert.AreEqual(expected, actual);
        }

        public static void ShouldBeTrue(this bool actual)
        {
            Assert.IsTrue(actual);
        }

        public static void ShouldBeFalse(this bool actual)
        {
            Assert.IsFalse(actual);
        }
    }
}