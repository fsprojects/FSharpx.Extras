using System;
using System.Linq;
using System.Net;
using System.Text.RegularExpressions;
using System.Threading;
using Microsoft.FSharp.Control;
using Microsoft.FSharp.Core;
using NUnit.Framework;

namespace FSharpx.CSharpTests {
    [TestFixture]
    public class AsyncTests {
        static FSharpAsync<string> Get(string u) {
            var web = new WebClient();
            return web.AsyncDownloadString(new Uri(u));
        }

        [Test]
        public void LINQ() {
            FSharpAsync<string> qq =
                from google in Get("http://www.google.com")
                from bing in Get("http://www.bing.com")
                select google + bing;
            string result = FSharpAsync.RunSynchronously(qq, FSharpOption<int>.None,
                FSharpOption<CancellationToken>.None);
            var rx = new Regex(@"<html");
            Assert.AreEqual(2, rx.Matches(result).Count);
        }

        [Test]
        public void Parallel() {
            var urls = FSharpList.Create(
                  "http://www.google.com"
                , "http://www.bing.com"
                , "http://www.yahoo.com"
                , "http://www.microsoft.com"
                );
            var result = FSharpAsync.Parallel(urls.Select(Get)).Select(s => string.Join("", s)).Run();
            var rx = new Regex(@"<html");
            Assert.AreEqual(4, rx.Matches(result).Count);
        }

        [Test]
        public void FuncToAsync() {
            FSharpAsync<int> a = L.F(() => 1).ToAsync();
            FSharpAsync<Unit> b = a.Select(_ => (Unit) null);
            //b.Start();
        }
    }
}