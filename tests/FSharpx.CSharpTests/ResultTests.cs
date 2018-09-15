using System;
using Microsoft.FSharp.Core;
using NUnit.Framework;

namespace FSharpx.CSharpTests {
    [TestFixture]
    public class ResultTests {
        [Test]
        public void Match() {
            var a = FSharpResult<int, string>.NewOk(1);
            var c = a.Match(i => 0, _ => 1);
            Assert.AreEqual(0, c);
        }

        [Test]
        public void MatchAction() {
            var a = FSharpResult<int, string>.NewOk(1);
            a.Match(Console.WriteLine, _ => Assert.Fail("is string"));
        }

        [Test]
        public void New() {
            var a = FSharpResult.NewOk<int, string>(1);
            var b = FSharpResult<int, string>.NewOk(1);
            Assert.AreEqual(a, b);

            var c = FSharpResult.NewError<int, string>("a");
            var d = FSharpResult<int, string>.NewError("a");
            Assert.AreEqual(c, d);
        }

        [Test]
        public void Select() {
            var a = FSharpResult.NewOk<int, string>(5);
            var b = a.Select(i => i + 2);
            b.Match(i => Assert.AreEqual(7, i), _ => Assert.Fail("is string"));
        }

        [Test]
        public void Select2() {
            var a = FSharpResult.NewError<int, string>("hello");
            var b = a.Select(i => i + 2);
            b.Match(_ => Assert.Fail("is int"), s => Assert.AreEqual("hello", s));
        }

        [Test]
        public void Validation_Errors() {
            Person.TryNew("", -5)
                .Match(_ => Assert.Fail("should not have been ok"), 
                       err => {
                           Assert.AreEqual(2, err.Length);
                           Console.WriteLine(err);
                       });
        }

        [Test]
        public void Validation_OK() {
            Person.TryNew("Pepe", 40)
                .Match(p => {
                    Assert.AreEqual("Pepe", p.Name);
                    Assert.AreEqual(40, p.Age);
                }, 
                _ => Assert.Fail("should not have failed"));
        }

        [Test]
        public void Cast_OK() {
            object a = 40;
            FSharpResult.Cast<int>(a)
                .Match(i => Assert.AreEqual(40, i),
                       e => Assert.Fail(e.Message));
        }

        [Test]
        public void Cast_Exception() {
            object a = "hello";
            FSharpResult.Cast<int>(a)
                .Match(i => Assert.Fail("should not have succeeded with value {0}", i),
                       e => {});
        }

        [Test]
        public void ChoiceToOption() {
            object a = 40;
            const string b = "60";
            var r = from i in FSharpOption.ParseInt(b)
                    from j in FSharpResult.Cast<int>(a).ToFSharpOption()
                    select i + j;
            Assert.AreEqual(100.Some(), r);

        }

        [Test]
        public void OptionToChoice() {
            object a = 40;
            const string b = "60";
            var r = from i in FSharpOption.ParseInt(b).ToFSharpResult(new Exception())
                    from j in FSharpResult.Cast<int>(a)
                    select i + j;
            r.Match(i => Assert.AreEqual(100, i),
                    e => Assert.Fail(e.Message));
        }

        [Test]
        public void SelectSecond_OK() {
            object a = 40;
            const string b = "60";
            var r = from i in FSharpOption.ParseInt(b).ToFSharpResult("Invalid value b")
                    from j in FSharpResult.Cast<int>(a).SelectError(_ => "Invalid value a")
                    select i + j;
            r.Match(i => Assert.AreEqual(100, i),
                    Assert.Fail);
        }

        [Test]
        public void SelectSecond_Error() {
            object a = 40;
            const string b = "xx";
            var r = from i in FSharpOption.ParseInt(b).ToFSharpResult("Invalid value b")
                    from j in FSharpResult.Cast<int>(a).SelectError(_ => "Invalid value a")
                    select i + j;
            r.Match(i => Assert.Fail("should not have succeeded with value {0}", i),
                    e => Assert.AreEqual("Invalid value b", e));
        }
    }
}