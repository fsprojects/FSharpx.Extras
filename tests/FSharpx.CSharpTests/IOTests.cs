using NUnit.Framework;

namespace FSharpx.CSharpTests
{
    [TestFixture]
    public class IOTests
    {
        [Test]
        public void WhenConvertingTextToWindowsLineBreaks()
        {
            IO.convertTextToWindowsLineBreaks("\rMy text\n\ris from different os versions\r\nbut thats ok\treally")
                .ShouldEqual("\r\nMy text\r\n\r\nis from different os versions\r\nbut thats ok\treally");
        }

    }
}