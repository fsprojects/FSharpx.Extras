using NUnit.Framework;
using FSharpx.CSharpTests;

namespace FSharpx.IO.CSharpTests
{
    [TestFixture]
    public class IOTests
    {
        [Test]
        public void WhenConvertingTextToWindowsLineBreaks()
        {
            IOPrimitives.convertTextToWindowsLineBreaks("\rMy text\n\ris from different os versions\r\nbut thats ok\treally")
                .ShouldEqual("\r\nMy text\r\n\r\nis from different os versions\r\nbut thats ok\treally");
        }

    }
}