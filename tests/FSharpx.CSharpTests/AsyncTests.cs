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
using FSharpx.Control;
using Microsoft.FSharp.Core;
using NUnit.Framework;

namespace FSharpx.CSharpTests {
    [TestFixture]
    public class AsyncTests {
        static FSharpAsync<string> Get(string u) {
            var web = new WebClient();
            return web.FSharpAsyncDownloadString(new Uri(u));
        }

        [Test]
        public void LINQ() {
            FSharpAsync<string> asyncGet =
                from google in Get("http://www.google.com")
                from bing in Get("http://www.bing.com")
                select google + bing;
            string result = asyncGet.Run();
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
            var result = urls.Select(Get).Parallel()
                .Select(s => string.Join("", s))
                .Run();
            var rx = new Regex(@"<html");
            Assert.AreEqual(4, rx.Matches(result).Count);
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
        public void Protect() {
            Get("http://www.google.comco").Protect().Run()
                .Match(result => Assert.Fail("request should have failed"),
                       e => { });
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

        // http://stackoverflow.com/questions/6893998/c-how-to-implement-as-async-and-in-f

        [Test]
        [Ignore("just an example")]
        public void Example2() {
            var price = LoadPrices("MSFT").Run().First();
            Assert.AreEqual(new DateTime(2008,10,30), price.Item1);
            Assert.AreEqual(20.82m, price.Item2);
        }

        FSharpAsync<IEnumerable<Tuple<DateTime, decimal>>> LoadPrices(string ticker) {
            var url = "http://ichart.finance.yahoo.com/table.csv?s=" + ticker + "&d=9&e=30&f=2008&g=d&a=2&b=13&c=1986&ignore=.csv";
            var req = WebRequest.Create(url);
            return
                from resp in req.FSharpAsyncGetResponse()
                let stream = resp.GetResponseStream()
                let reader = new StreamReader(stream)
                from csv in reader.FSharpAsyncReadToEnd()
                select csv.Split('\n')
                    .Skip(1)
                    .Select(line => line.Split(','))
                    .Where(values => values.Length == 7)
                    .Select(values => Tuple.Create(DateTime.Parse(values[0]), decimal.Parse(values[6], CultureInfo.InvariantCulture)));
        }
    }
}