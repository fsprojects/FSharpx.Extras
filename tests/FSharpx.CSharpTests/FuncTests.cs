using System;
using System.Linq;
using Microsoft.FSharp.Core;
using NUnit.Framework;

namespace FSharpx.CSharpTests {
    [TestFixture]
    public class FuncTests {
        [Test]
        public void FromAction() {
            int a = 0;
            FSharpFunc<Unit, Unit> ff = FSharpFunc.FromAction(() => a = 1);
            ff.Invoke(null);
            Assert.AreEqual(1, a);
        }

        [Test]
        public void FromActionEta() {
            int a = 0;
            Action action = () => a = 1;
            Action[] actions = new[] {action,action};
            var functions = actions.Select(FSharpFunc.FromAction);
        }
    }
}
