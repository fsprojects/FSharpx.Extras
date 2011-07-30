using System;
using Microsoft.FSharp.Core;
using NUnit.Framework;

namespace FSharp.Core.CS.Tests {
    [TestFixture]
    public class IntegrationTests {
        int doSomething(int userID, int id) {
            // fetch some other entity, do "stuff"
            return userID + id;
        }

        void setError(string e) {}

        const string req_userID = "123";
        const string req_otherID = "999";

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
        public void Test1_Functional() {

            var somethingOrError = 
                from userID in Opt.TryParseInt(req_userID).ToChoice("Invalid User ID")
                from id in Opt.TryParseInt(req_otherID).ToChoice("Invalid ID")
                select doSomething(userID, id);

            somethingOrError.Match(Console.WriteLine, setError);

        }
    }
}