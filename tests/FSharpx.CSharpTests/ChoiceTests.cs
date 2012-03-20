using System;
using Microsoft.FSharp.Core;
using NUnit.Framework;

namespace FSharpx.CSharpTests {
    [TestFixture]
    public class ChoiceTests {
        [Test]
        public void Match() {
            var a = FSharpChoice<int, string>.NewChoice1Of2(1);
            var c = a.Match(i => 0, _ => 1);
            Assert.AreEqual(0, c);
        }

        [Test]
        public void MatchAction() {
            var a = FSharpChoice<int, string>.NewChoice1Of2(1);
            a.Match(Console.WriteLine, _ => Assert.Fail("is string"));
        }

        [Test]
        public void New() {
            var a = FSharpChoice.New1Of2<int, string>(1);
            var b = FSharpChoice<int, string>.NewChoice1Of2(1);
            Assert.AreEqual(a, b);

            var c = FSharpChoice.New2Of2<int, string>("a");
            var d = FSharpChoice<int, string>.NewChoice2Of2("a");
            Assert.AreEqual(c, d);
        }

        [Test]
        public void Select() {
            var a = FSharpChoice.New1Of2<int, string>(5);
            var b = a.Select(i => i + 2);
            b.Match(i => Assert.AreEqual(7, i), _ => Assert.Fail("is string"));
        }

        [Test]
        public void Select2() {
            var a = FSharpChoice.New2Of2<int, string>("hello");
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
            FSharpChoice.Cast<int>(a)
                .Match(i => Assert.AreEqual(40, i),
                       e => Assert.Fail(e.Message));
        }

        [Test]
        public void Cast_Exception() {
            object a = "hello";
            FSharpChoice.Cast<int>(a)
                .Match(i => Assert.Fail("should not have succeeded with value {0}", i),
                       e => {});
        }

        [Test]
        public void ChoiceToOption() {
            object a = 40;
            const string b = "60";
            var r = from i in FSharpOption.ParseInt(b)
                    from j in FSharpChoice.Cast<int>(a).ToFSharpOption()
                    select i + j;
            Assert.AreEqual(100.Some(), r);

        }

        [Test]
        public void OptionToChoice() {
            object a = 40;
            const string b = "60";
            var r = from i in FSharpOption.ParseInt(b).ToFSharpChoice(new Exception())
                    from j in FSharpChoice.Cast<int>(a)
                    select i + j;
            r.Match(i => Assert.AreEqual(100, i),
                    e => Assert.Fail(e.Message));
        }

        [Test]
        public void SelectSecond_OK() {
            object a = 40;
            const string b = "60";
            var r = from i in FSharpOption.ParseInt(b).ToFSharpChoice("Invalid value b")
                    from j in FSharpChoice.Cast<int>(a).SelectSecond(_ => "Invalid value a")
                    select i + j;
            r.Match(i => Assert.AreEqual(100, i),
                    Assert.Fail);
        }

        [Test]
        public void SelectSecond_Error() {
            object a = 40;
            const string b = "xx";
            var r = from i in FSharpOption.ParseInt(b).ToFSharpChoice("Invalid value b")
                    from j in FSharpChoice.Cast<int>(a).SelectSecond(_ => "Invalid value a")
                    select i + j;
            r.Match(i => Assert.Fail("should not have succeeded with value {0}", i),
                    e => Assert.AreEqual("Invalid value b", e));
        }

        private static int ThisThrows(int a) {
            throw new Exception("bad");
        }

        [Test]
        public void Try() {
            FSharpChoice.Try<int, int>(ThisThrows)(2)
                .Match(a => Assert.Fail("Should have returned an exception"),
                       e => { });
        }

        [Test]
        public void Try_as_extension_method_creating_a_new_function() {
            Func<int, int> f = a => a + 2;
            var v = Choice.get(f.Try()(5));
            Assert.AreEqual(v,7);
        }

        [Test]
        public void Try_as_extension_method_applying_function() {
            Func<int, int> f = a => a + 2;
            var v = Choice.get(f.Try(5));
            Assert.AreEqual(v, 7);
        }
    }
}