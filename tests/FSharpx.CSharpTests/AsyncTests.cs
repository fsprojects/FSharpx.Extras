using System;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Linq;
using System.Net;
using System.Text.RegularExpressions;
using System.Threading;
using Microsoft.FSharp.Control;
using FSharpx;
using Microsoft.FSharp.Core;
using NUnit.Framework;

#pragma warning disable 219

namespace FSharpx.CSharpTests {
    [TestFixture]
    public class AsyncTests {
        private readonly Random random = new Random();

        private FSharpAsync<string> Get(string u) {
            var delay = FSharpAsync.Sleep(random.Next(1000, 2000));
            return delay.Select(x => u);
        }

        [Test]
        public void LINQ() {
            FSharpAsync<string> asyncGet =
                from google in Get("http://www.google.com")
                from bing in Get("http://www.bing.com")
                select google + bing;
            string result = asyncGet.Run();
            var rx = new Regex(@"http");
            Assert.AreEqual(2, rx.Matches(result).Count);
        }

        [Test]
        public void Parallel() {
            var urls = FSharpList.Create(
                  "http://www.google.com"
                , "http://www.bing.com"
                , "http://www.microsoft.com"
                );
            var result = urls.Select(Get).Parallel()
                .Select(s => string.Join("", s))
                .Run();
            var rx = new Regex(@"http");
            Assert.AreEqual(3, rx.Matches(result).Count);
        }

        [Test]
        public void ToAsync() {
            FSharpAsync<int> a = L.F(() => 1).ToFSharpAsync();
            FSharpAsync<Unit> b = a.IgnoreResult();
            
            // super verbose return
            FSharpAsync<int> i = ExtraTopLevelOperators.DefaultAsyncBuilder.Return(1);

            FSharpAsync<int> x = FSharpAsyncEx.Return(1);
            FSharpAsync<Func<int>> fa = FSharpAsyncEx.Return(L.F(() => 1));
        }

        [Test]
        public void Example() {
            var urls = FSharpList.Create(
                  "http://www.google.com"
                , "http://www.bing.com"
                , "http://www.yahoo.com"
                , "http://www.microsoft.com"
                );
            var realJoinWebPages = JoinWebPages(url => Get(url).Protect());
            var testJoinWebPages = JoinWebPages(_ => FSharpAsyncEx.Return(FSharpChoice.New1Of2<string, Exception>("hello")));
            var result = testJoinWebPages(urls);
            Assert.AreEqual("hellohellohellohello", result);
        }

        Func<IEnumerable<string>, string> JoinWebPages(Func<string, FSharpAsync<FSharpChoice<string, Exception>>> fetch) {
            return urls =>
                urls.Select(fetch).Parallel()
                    .Select(c => string.Join("", c.Select(s => s.Match(x => x, _ => "")))) // swallow exceptions!
                    .Run();
        }
    }
}