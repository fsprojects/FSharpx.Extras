using System;
using System.Collections.Generic;
using System.Linq;
using Microsoft.FSharp.Collections;
using Microsoft.FSharp.Core;
using NUnit.Framework;

namespace FSharp.Core.CS.Tests {
    [TestFixture]
    public class IntegrationTests {
        int doSomething(int userID, int id) {
            // fetch some other entity, do "stuff"
            return userID + id;
        }

        void setError(string e) {
            Console.WriteLine("Error: {0}", e);
        }

        const string req_userID = "a123";
        const string req_otherID = "b999";

        [Test]
        public void Test1_Imperative() {

            int userID;
            var userID_ok = int.TryParse(req_userID, out userID);
            if (!userID_ok) {
                setError("Invalid User ID");
            } else {
                int id;
                var id_ok = int.TryParse(req_otherID, out id);
                if (!id_ok) {
                    setError("Invalid ID");
                } else {
                    Console.WriteLine(doSomething(userID, id));
                }
            }
        }

        [Test]
        public void Test1_option() {
            var userID = FSharpOption.TryParseInt(req_userID);
            if (!userID.HasValue()) {
                setError("Invalid User ID");
            } else {
                var otherID = FSharpOption.TryParseInt(req_otherID);
                if (!otherID.HasValue()) {
                    setError("Invalid ID");
                } else {
                    Console.WriteLine(doSomething(userID.Value, otherID.Value));
                }
            }
        }


        [Test]
        public void Test1_either() {

            var somethingOrError =
                from userID in FSharpOption.TryParseInt(req_userID).ToFSharpChoice("Invalid User ID")
                from id in FSharpOption.TryParseInt(req_otherID).ToFSharpChoice("Invalid ID")
                select doSomething(userID, id);

            somethingOrError.Match(Console.WriteLine, setError);

        }

        void setErrors(IEnumerable<string> errors) {
            foreach (var e in errors)
                Console.WriteLine("Error: {0}", e);
        }

        [Test]
        public void Test2_imperative() {
            var errors = new List<string>();

            int userID;
            var userID_ok = int.TryParse(req_userID, out userID);
            if (!userID_ok)
                errors.Add("Invalid user ID");

            int id;
            var id_ok = int.TryParse(req_otherID, out id);
            if (!id_ok)
                errors.Add("Invalid ID");

            if (errors.Count > 0)
                setErrors(errors);
            else
                Console.WriteLine(doSomething(userID, id));
        }

        [Test]
        public void Test2_either() {
            var userID = FSharpOption.TryParseInt(req_userID)
                .ToFSharpChoice(FSharpList.New("Invalid User ID"));
            var id = FSharpOption.TryParseInt(req_otherID)
                .ToFSharpChoice(FSharpList.New("Invalid ID"));

            var doSomethingFunc = L.F((int a, int b) => doSomething(a, b));
            var curriedDoSomething = doSomethingFunc.Curry();
            var result = curriedDoSomething.PureValidate()
                .ApV(userID)
                .ApV(id);

            //var result = L.F((int a, int b) => doSomething(a,b))
            //    .Curry().PureValidate()
            //    .ApV(userID)
            //    .ApV(id);

            result.Match(Console.WriteLine, setErrors);
        }

        [Test]
        public void Test2_either_LINQ() {
            var userID = FSharpOption.TryParseInt(req_userID)
                .ToFSharpChoice(FSharpList.New("Invalid User ID"));
            var id = FSharpOption.TryParseInt(req_otherID)
                .ToFSharpChoice(FSharpList.New("Invalid ID"));

            var result =
                from a in userID
                join b in id on 1 equals 1
                select doSomething(a, b);

            result.Match(Console.WriteLine, setErrors);
        }
    }

    public static class ValidationLINQ {
        public static FSharpChoice<R, FSharpList<string>> Join<T,I,K,R>(this FSharpChoice<T, FSharpList<string>> c, FSharpChoice<I, FSharpList<string>> inner, Func<T,K> outerKeySelector, Func<I,K> innerKeySelector, Func<T,I,R> resultSelector) {
            var ff = FSharpChoice.PureValidate(new Func<T, Func<I, Tuple<T, I>>>(a => b => Tuple.Create(a, b)));
            return ff.ApV(c).ApV(inner).Select(t => resultSelector(t.Item1, t.Item2));
        }
    }
}