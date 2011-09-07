using System;
using System.Net;
using System.Text.RegularExpressions;
using System.Threading;
using Microsoft.FSharp.Control;
using Microsoft.FSharp.Core;
using NUnit.Framework;

namespace FSharpx.CSharpTests {
  [TestFixture]
  public class AsyncTests {
    private readonly WebClient web = new WebClient();
    private FSharpAsync<string> Get(string u) {
      return WebExtensions.AsyncDownloadString(web, new Uri(u));
    }

    [Test]
    public void LINQ() {
      var qq =
        from google in Get("http://www.google.com")
        from bing in Get("http://www.bing.com")
        select google + bing;
      var result = FSharpAsync.RunSynchronously(qq, FSharpOption<int>.None, FSharpOption<CancellationToken>.None);
      var rx = new Regex(@"<html");
      Assert.AreEqual(2, rx.Matches(result).Count);
    }
  }
}