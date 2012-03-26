using System;
using NUnit.Framework;
using Microsoft.FSharp.Core;
using FSharpx;

namespace FSharpx.CSharpTests {
    [TestFixture]
    public class EnumerableTests {
        [Test]
        public void FirstOrNone_None() {
            var a = new int[0];
            Assert.AreEqual(FSharpOption<int>.None, a.FirstOrNone());
        }

        [Test]
        public void FirstOrNone_Some() {
            var a = new int[] {1,2,3};
            Assert.AreEqual(1.Some(), a.FirstOrNone());
        }

    }
}
