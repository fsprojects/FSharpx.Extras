using System;
using Microsoft.FSharp.Core;
using Errors = FSharpx.Collections.NonEmptyList<string>;

namespace FSharpx.CSharpTests {
    public class Person {
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
            return New.Curry().ReturnValidation()
                .ApValidation(Mandatory(name))
                .ApValidation(Positive(age));
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

    }
}