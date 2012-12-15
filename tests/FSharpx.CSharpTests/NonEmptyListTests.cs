using Microsoft.FSharp.Collections;
using NUnit.Framework;
using FSharpx.Collections;


namespace FSharpx.CSharpTests {
    [TestFixture]
    public class NonEmptyListTests {
        [Test]
        public void Create() {
            NonEmptyList<int> x = NonEmptyList.Create(1, FSharpList.Create(2, 3, 4));
            NonEmptyList<int> y = NonEmptyList.Create(1, 2, 3, 4);
            Assert.AreEqual(4, x.Length);
            Assert.AreEqual(x,y);
        }

        [Test]
        public void Singleton() {
            NonEmptyList<int> x = NonEmptyList.Singleton(1);
            Assert.AreEqual(1, x.Length);
        }

        [Test]
        public void HeadTail() {
            NonEmptyList<int> x = NonEmptyList.Singleton(1);
            Assert.AreEqual(1, x.Head);
            Assert.AreEqual(FSharpList<int>.Empty, x.Tail);
        }

        [Test]
        public void ToFSharpList() {
            NonEmptyList<int> x = NonEmptyList.Singleton(1);
            FSharpList<int> list = x.ToFSharpList();
            Assert.AreEqual(FSharpList.Create(1), list);
        }

        [Test]
        public void ToArray() {
            NonEmptyList<int> x = NonEmptyList.Singleton(1);
            int[] a = x.ToArray();
            CollectionAssert.AreEqual(new[] {1}, a);
        }

        [Test]
        public void Concat_FSharpList() {
            var x = NonEmptyList.Singleton(1);
            x = NonEmptyList.Concat(x, FSharpList.Create(2, 3, 4));
            CollectionAssert.AreEqual(new[] {1,2,3,4}, x);
        }

        [Test]
        public void Concat_NonEmptyList() {
            var x = NonEmptyList.Singleton(1);
            x = NonEmptyList.Concat(x, NonEmptyList.Singleton(2));
            CollectionAssert.AreEqual(new[] { 1, 2 }, x);
        }

        [Test]
        public void Concat_FSharpList_extension() {
            var x = NonEmptyList.Singleton(1);
            x = x.Concat(FSharpList.Create(2, 3, 4));
            CollectionAssert.AreEqual(new[] { 1, 2, 3, 4 }, x);
        }

        [Test]
        public void Concat_NonEmptyList_extension() {
            var x = NonEmptyList.Singleton(1);
            x = x.Concat(NonEmptyList.Singleton(2));
            CollectionAssert.AreEqual(new[] { 1, 2 }, x);
        }

        [Test]
        public void Cons() {
            var x = NonEmptyList.Singleton(1).Cons(2);
            CollectionAssert.AreEqual(new[] { 2, 1 }, x);
        }

        [Test]
        public void Reverse() {
            var x = NonEmptyList.Create(1, FSharpList.Create(2, 3, 4));
            x = x.Reverse();
            CollectionAssert.AreEqual(new[] { 4,3,2,1 }, x);
        }

        [Test]
        public void Select() {
            var list = NonEmptyList.Create(1, FSharpList.Create(2, 3, 4));
            list = list.Select(x => x + 2);
            CollectionAssert.AreEqual(new[] {3,4,5,6}, list);
            list = from x in list select x + 1;
            CollectionAssert.AreEqual(new[] { 4, 5, 6, 7 }, list);
        }
    }
}
