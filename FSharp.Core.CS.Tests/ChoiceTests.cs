using System;
using System.Collections.Generic;
using Microsoft.FSharp.Collections;
using Microsoft.FSharp.Core;
using NUnit.Framework;
using Errors = Microsoft.FSharp.Collections.FSharpList<string>;

namespace FSharp.Core.CS.Tests {
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

        private static FSharpChoice<string, Errors> Mandatory(string s) {
            return FSharpChoice.Validator<string>(x => !string.IsNullOrEmpty(x), "Mandatory field")(s);
            //if (string.IsNullOrEmpty(s))
            //    return FSharpChoice.Error<string>("Mandatory field");
            //return FSharpChoice.Ok(s);
        }

        private static FSharpChoice<int, Errors> Positive(int a) {
            if (a <= 0)
                return FSharpChoice.Error<int>("Field must be positive");
            return FSharpChoice.Ok(a);
        }

        private class Person {
            private readonly string name;
            private readonly int age;

            public string Name {
                get { return name; }
            }

            public int Age {
                get { return age; }
            }

            private Person(string name, int age) {
                this.name = name;
                this.age = age;
            }

            private static readonly Func<string, int, Person> New = (name, age) => new Person(name, age);

            public static FSharpChoice<Person, Errors> TryNew(string name, int age) {
                return New.Curry().PureValidate()
                        .ApV(Mandatory(name))
                        .ApV(Positive(age));
            }
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

    }
}