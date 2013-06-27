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
            Action action = () => a = a + 1;
            Action[] actions = new[] {action,action};
            var functions = actions.Select(FSharpFunc.FromAction);
        }

        [Test]
        public void ActionTuple() {
            int r = 0;
            Action<int, int> f = (a, b) => r = a + b;
            var g = f.Tuple();
            g(Tuple.Create(1, 2));
            Assert.AreEqual(3, r);
        }

        [Test]
        public void ActionUntuple() {
            int r = 0;
            Action<Tuple<int, int>> f = t => r = t.Item1 + t.Item2;
            var g = f.Untuple();
            g(1, 2);
            Assert.AreEqual(3, r);
        }

        [Test]
        public void Compose() {
            Func<int, long> plus2 = a => a + 2;
            Func<string, int> length = s => s.Length;
            Func<string, long> lengthPlus2 = plus2.Compose(length);
            Assert.AreEqual(5, lengthPlus2("abc"));
        }

        [Test]
        public void AndThen() {
            Func<int, long> plus2 = a => a + 2;
            Func<string, int> length = s => s.Length;
            Func<string, long> lengthPlus2 = length.AndThen(plus2);
            Assert.AreEqual(5, lengthPlus2("abc"));
        }

    }
}
