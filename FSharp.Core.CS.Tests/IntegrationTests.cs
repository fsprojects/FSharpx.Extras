using System;
using System.Collections.Specialized;
using Microsoft.FSharp.Core;
using NUnit.Framework;

namespace FSharp.Core.CS.Tests {
    [TestFixture]
    public class IntegrationTests {
        private int doSomething(int userID, int id) {
            return userID + id;
        }

        private void setError(string e) {}

        [Test]
        public void Test1_Imperative() {
            // query string
            var nvc = new NameValueCollection { { "user_id", "123" } };
            // web forms textbox
            const string textboxContent = "999";

            int userID;
            var userID_ok = int.TryParse(nvc["user_id"], out userID);
            if (!userID_ok) {
                setError("Invalid User ID");
            } else {
                int id;
                var id_ok = int.TryParse(textboxContent, out id);
                if (!id_ok) {
                    setError("Invalid ID");
                } else {
                    Console.WriteLine(doSomething(userID, id));
                }
            }
        }

        [Test]
        public void Test1_Functional() {
            // query string
            var nvc = new NameValueCollection { { "user_id", "123" } };
            // web forms textbox
            const string textboxContent = "999";

            var somethingOrError = 
                from userID in Opt.TryParseInt(nvc["user_id"]).ToChoice("Invalid User ID")
                from id in Opt.TryParseInt(textboxContent).ToChoice("Invalid ID")
                select doSomething(userID, id);

            somethingOrError.Match(Console.WriteLine, setError);

        }
    }
}