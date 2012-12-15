using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Microsoft.FSharp.Core;
using Microsoft.FSharp.Collections;
using NUnit.Framework;
using FSharpx.Collections;

namespace FSharpx.CSharpTests.ValidationExample {
    // ported from original in Scalaz: https://gist.github.com/970717

    // First let's define a domain.

    enum Sobriety { Sober, Tipsy, Drunk, Paralytic, Unconscious }
    enum Gender { Male, Female }

    class Person {
        private readonly Gender gender;
        private readonly int age;
        private readonly FSharpSet<string> clothes;
        private readonly Sobriety sobriety;

        public Gender Gender {
            get { return gender; }
        }

        public int Age {
            get { return age; }
        }

        public FSharpSet<string> Clothes {
            get { return clothes; }
        }

        public Sobriety Sobriety {
            get { return sobriety; }
        }

        public Person(Gender gender, int age, FSharpSet<string> clothes, Sobriety sobriety) {
            this.gender = gender;
            this.age = age;
            this.clothes = clothes;
            this.sobriety = sobriety;
        }
    }

    // Let's define the checks that *all* nightclubs make!

    class Club {
        public static readonly Func<Person, FSharpChoice<Person, NonEmptyList<string>>>
            CheckAge =
                p => {
                    if (p.Age < 18)
                        return FSharpChoice.Error<Person>("Too young!");
                    if (p.Age > 40)
                        return FSharpChoice.Error<Person>("Too old!");
                    return FSharpChoice.Ok(p);
                };

        public static readonly Func<Person, FSharpChoice<Person, NonEmptyList<string>>>
            CheckClothes =
                p => {
                    if (p.Gender == Gender.Male && !p.Clothes.Contains("Tie"))
                        return FSharpChoice.Error<Person>("Smarten up!");
                    if (p.Gender == Gender.Female && p.Clothes.Contains("Trainers"))
                        return FSharpChoice.Error<Person>("Wear high heels!");
                    return FSharpChoice.Ok(p);
                };

        public static readonly Func<Person, FSharpChoice<Person, NonEmptyList<string>>> 
            CheckSobriety =
                p => {
                    if (new[] { Sobriety.Drunk, Sobriety.Paralytic, Sobriety.Unconscious }.Contains(p.Sobriety))
                        return FSharpChoice.Error<Person>("Sober up!");
                    return FSharpChoice.Ok(p);
                };
    }

    // Now let's compose some validation checks

    class ClubbedToDeath {
        // PERFORM THE CHECKS USING Monadic SUGAR (LINQ)
        public static FSharpChoice<decimal, NonEmptyList<string>> CostToEnter(Person p) {
            return from a in Club.CheckAge(p)
                   from b in Club.CheckClothes(a)
                   from c in Club.CheckSobriety(b)
                   select c.Gender == Gender.Female ? 0m : 5m;
        }
    }

    // Now let's see these in action

    [TestFixture]
    class Test1 {
        public static readonly Person Ken = new Person(
            gender: Gender.Male, 
            age: 28, 
            clothes: FSharpSet.Create("Tie", "Shirt"), 
            sobriety: Sobriety.Tipsy);

        public static readonly Person Dave = new Person(
            gender: Gender.Male,
            age: 41,
            clothes: FSharpSet.Create("Tie", "Jeans"),
            sobriety: Sobriety.Sober);

        public static readonly Person Ruby = new Person(
            gender: Gender.Female,
            age: 25,
            clothes: FSharpSet.Create("High heels"),
            sobriety: Sobriety.Tipsy);

        // let's go clubbing!

        [Test]
        public void Part1() {
            var costDave = ClubbedToDeath.CostToEnter(Dave);
            Assert.AreEqual(FSharpChoice.Error<decimal>("Too old!"), costDave);

            var costKen = ClubbedToDeath.CostToEnter(Ken);
            Assert.AreEqual(FSharpChoice.Ok(5m), costKen);

            var costRuby = ClubbedToDeath.CostToEnter(Ruby);
            Assert.AreEqual(FSharpChoice.Ok(0m), costRuby);

            var Ruby17 = new Person(
                age: 17,
                clothes: Ruby.Clothes,
                sobriety: Ruby.Sobriety,
                gender: Ruby.Gender);
            var costRuby17 = ClubbedToDeath.CostToEnter(Ruby17);
            Assert.AreEqual(FSharpChoice.Error<decimal>("Too young!"), costRuby17);

            var KenUnconscious = new Person(
                age: Ken.Age,
                clothes: Ken.Clothes,
                gender: Ken.Gender,
                sobriety: Sobriety.Unconscious);
            var costKenUnconscious = ClubbedToDeath.CostToEnter(KenUnconscious);
            Assert.AreEqual(FSharpChoice.Error<decimal>("Sober up!"), costKenUnconscious);

            /**
             * The thing to note here is how the Validations can be composed together in a computation expression.
             * The type system is making sure that failures flow through your computation in a safe manner.
             */
        }
    }

    /**
     * Part Two : Club Tropicana
     *
     * Part One showed monadic composition, which from the perspective of Validation is *fail-fast*.
     * That is, any failed check shortcircuits subsequent checks. This nicely models nightclubs in the
     * real world, as anyone who has dashed home for a pair of smart shoes and returned, only to be
     * told that your tie does not pass muster, will attest.
     *
     * But what about an ideal nightclub? One that tells you *everything* that is wrong with you.
     *
     * Applicative functors to the rescue!
     *
     */

    class ClubTropicana {
        //PERFORM THE CHECKS USING applicative functors, accumulating failure via a monoid
        // using LINQ sugar
        public static FSharpChoice<decimal, NonEmptyList<string>> CostToEnter(Person p) {
            return from c in Club.CheckAge(p)
                   join x in Club.CheckClothes(p) on 1 equals 1
                   join y in Club.CheckSobriety(p) on 1 equals 1
                   select c.Gender == Gender.Female ? 0m : 7.5m;
        }

        // or using regular functions:

        static readonly Func<Person, Person, Person, decimal> CostByGender =
            (p, x, y) => p.Gender == Gender.Female ? 0m : 7.5m;

        public static FSharpChoice<decimal, NonEmptyList<string>> CostToEnter2(Person p) {
            return CostByGender.Curry().ReturnValidation()
                .ApValidation(Club.CheckAge(p))
                .ApValidation(Club.CheckClothes(p))
                .ApValidation(Club.CheckSobriety(p));
        }
    }

    // And the use? Dave tried the second nightclub after a few more drinks in the pub

    [TestFixture]
    class Test2 {
        [Test]
        public void Part2() {
            var daveParalytic = new Person(
                age: Test1.Dave.Age,
                clothes: Test1.Dave.Clothes,
                gender: Test1.Dave.Gender,
                sobriety: Sobriety.Paralytic);
            var costDaveParalytic = ClubTropicana.CostToEnter(daveParalytic);
            Assert.AreEqual(FSharpChoice.Errors<decimal>("Too old!", "Sober up!"), costDaveParalytic);

            var costRuby = ClubTropicana.CostToEnter2(Test1.Ruby);
            Assert.AreEqual(FSharpChoice.Ok(0m), costRuby);
        }

        /**
         *
         * So, what have we done? Well, with a *tiny change* (and no changes to the individual checks themselves),
         * we have completely changed the behaviour to accumulate all errors, rather than halting at the first sign
         * of trouble. Imagine trying to do this using exceptions, with ten checks.
         *
         */

    }

    /**
     *
     * Part Three : Gay bar
     *
     * And for those wondering how to do this with a *very long list* of checks.
     *
     */

    class GayBar {
        public static readonly Func<Person, FSharpChoice<Person, NonEmptyList<string>>> 
            CheckGender =
                p => {
                    if (p.Gender == Gender.Male)
                        return FSharpChoice.Ok(p);
                    return FSharpChoice.Error<Person>("Men only");
                };

        public static FSharpChoice<decimal, NonEmptyList<string>> CostToEnter(Person p) {
            return 
                FSharpList.Create(Club.CheckAge, Club.CheckClothes, Club.CheckSobriety, CheckGender)
                    .SelectMValidation(check => check(p))
                    .Select(x => x[0].Age + 1.5m);
        }
    }

    [TestFixture]
    class Test3 {
        [Test]
        public void Part3() {
            var person = new Person(
                gender: Gender.Male,
                age: 59,
                clothes: FSharpSet.Create("Jeans"),
                sobriety: Sobriety.Paralytic);
            var cost = GayBar.CostToEnter(person);
            Assert.AreEqual(FSharpChoice.Errors<decimal>("Too old!", "Smarten up!", "Sober up!"), cost);
        }
    }
}
