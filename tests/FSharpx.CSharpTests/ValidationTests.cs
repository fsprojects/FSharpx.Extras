using System;
using System.Collections.Generic;
using System.Linq;
using Microsoft.FSharp.Collections;
using Microsoft.FSharp.Core;
using NUnit.Framework;
using Errors = FSharpx.Collections.NonEmptyList<string>;
using FSharpx.Collections;

namespace FSharpx.CSharpTests {
    [TestFixture]
    public class ValidationTests {
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
            var userID = FSharpOption.ParseInt(req_userID);
            if (!userID.HasValue()) {
                setError("Invalid User ID");
            } else {
                var otherID = FSharpOption.ParseInt(req_otherID);
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
                from userID in FSharpOption.ParseInt(req_userID).ToFSharpChoice("Invalid User ID")
                from id in FSharpOption.ParseInt(req_otherID).ToFSharpChoice("Invalid ID")
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
            var userID = FSharpOption.ParseInt(req_userID)
                .ToFSharpChoice(NonEmptyList.Singleton("Invalid User ID"));
            var id = FSharpOption.ParseInt(req_otherID)
                .ToFSharpChoice(NonEmptyList.Singleton("Invalid ID"));

            var doSomethingFunc = L.F((int a, int b) => doSomething(a, b));
            var curriedDoSomething = doSomethingFunc.Curry();
            var result = curriedDoSomething.ReturnValidation()
                .ApValidation(userID)
                .ApValidation(id);

            //var result = L.F((int a, int b) => doSomething(a,b))
            //    .Curry().PureValidate()
            //    .ApV(userID)
            //    .ApV(id);

            result.Match(Console.WriteLine, setErrors);
        }

        [Test]
        public void Test2_either_LINQ() {
            var userID = FSharpOption.ParseInt(req_userID)
                .ToFSharpChoice(NonEmptyList.Singleton("Invalid User ID"));
            var id = FSharpOption.ParseInt(req_otherID)
                .ToFSharpChoice(NonEmptyList.Singleton("Invalid ID"));

            var result =
                from a in userID
                join b in id on 1 equals 1
                select doSomething(a, b);

            result.Match(Console.WriteLine, setErrors);
        }

        static FSharpChoice<T, Errors> NonNull<T>(T value, string err) where T: class {
            return FSharpChoice.Validator<T>(x => x != null, err)(value);
        }

        static FSharpChoice<T, Errors> NotEqual<T>(T value, T other, string err) {
            return FSharpChoice.Validator<T>(v => !Equals(v, other), err)(value);
        }

        static FSharpChoice<Address, Errors> ValidateAddressLines(Address a) {
            if (a.Line1 != null || a.Line2 == null)
                return FSharpChoice.Ok(a);
            return FSharpChoice.Error<Address>("Line1 is empty but Line2 is not");
        }

        /// <summary>
        /// Same as FSharpChoice.Validator, for demo purposes only
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="pred"></param>
        /// <param name="err"></param>
        /// <returns></returns>
        static Func<T, FSharpChoice<T, Errors>> Validator<T>(Predicate<T> pred, string err) {
            return x => {
                if (pred(x))
                    return FSharpChoice.Ok(x);
                return FSharpChoice.Error<T>(err);
            };
        }

        static Func<Address, FSharpChoice<Address, Errors>> ValidateAddressLines2 = 
            Validator<Address>(x => x.Line1 != null || x.Line2 == null, "Line1 is empty but Line2 is not");

        static FSharpChoice<Address, Errors> ValidateAddress(Address a) {
            return from x in NonNull(a.Postcode, "Post code can't be null")
                   join y in ValidateAddressLines(a) on 1 equals 1
                   select a;
        }

        static FSharpChoice<T, Errors> GreaterThan<T>(T value, T other, string err) where T: IComparable<T> {
            var valueNull = Equals(null, value);
            var otherNull = Equals(null, other);
            if (valueNull && otherNull || valueNull != otherNull || value.CompareTo(other) > 0)
                return FSharpChoice.Ok(value);
            return FSharpChoice.Error<T>(err);
        }

        static FSharpChoice<T?, Errors> GreaterThan<T>(T? value, T? other, string err) where T: struct, IComparable<T> {
            if (!value.HasValue && !other.HasValue || value.HasValue != other.HasValue || value.Value.CompareTo(other.Value) > 0)
                return FSharpChoice.Ok(value);
            return FSharpChoice.Error<T?>(err);
        }

        static FSharpChoice<Order, Errors> ValidateOrder(Order o) {
            return
                from name in NonNull(o.ProductName, "Product name can't be null")
                from cost in GreaterThan(o.Cost, 0, string.Format("Cost for product '{0}' must be positive", name))
                select o;
        }

        static FSharpChoice<FSharpList<Order>, Errors> ValidateOrders(IEnumerable<Order> orders) {
            var zero = ListModule.Empty<Order>().ReturnValidation();
            return orders
                .Select(ValidateOrder)
                .Aggregate(zero, (e, c) => from a in e
                                           join b in c on 1 equals 1
                                           select a.Cons(b));
        }

        [Test]
        public void Test3() {
            var customer = new Customer {
                Address = new Address {
                    Postcode = "1424",
                },
                Orders = new[] {
                    new Order {
                        ProductName = "Foo",
                        Cost = 5,
                    },
                    new Order {
                        ProductName = "Bar",
                        Cost = -1,
                    },
                    new Order {
                        ProductName = null,
                        Cost = -1,
                    },
                }
            };
            var result =
                from surname in NonNull(customer.Surname, "Surname can't be null")
                join surname2 in NotEqual(customer.Surname, "foo", "Surname can't be foo") on 1 equals 1
                join address in ValidateAddress(customer.Address) on 1 equals 1
                join orders in ValidateOrders(customer.Orders) on 1 equals 1
                select customer;
            result.Match(c => Assert.Fail("Validation should have failed"),
                         Console.WriteLine);
        }

        [Test]
        public void AddressWithoutLine1() {
            var a = new Address { Postcode = "1000" };
            ValidateAddress(a)
                .Match(_ => { },
                       err => Assert.Fail("Validation should not have failed with errors {0}", err));
        }

        [Test]
        public void AddressWithLine1AndNoLine2() {
            var a = new Address { Postcode = "1000", Line1 = "Fake Street" };
            ValidateAddress(a)
                .Match(_ => { },
                       err => Assert.Fail("Validation should not have failed with errors {0}", err));
        }

        [Test]
        public void AddressWithLine1AndLine2() {
            var a = new Address { Postcode = "1000", Line1 = "Fake Street", Line2 = "123" };
            ValidateAddress(a)
                .Match(_ => { },
                       err => Assert.Fail("Validation should not have failed with errors {0}", err));
        }

        [Test]
        public void AddressWithLine2AndNoLine1() {
            var a = new Address { Postcode = "1000", Line2 = "123" };
            ValidateAddress(a)
                .Match(_ => Assert.Fail("Validation should have failed"),
                       err => {
                           Assert.AreEqual(1, err.Length);
                           Assert.AreEqual("Line1 is empty but Line2 is not", err.Head);
                       });
        }
    }
}