using System;
using System.Linq;
using FSharpx.Collections;
using FSharpx.Collections.Experimental;
using NUnit.Framework;

namespace FSharpx.CSharpTests {
    [TestFixture]
    public class RoseTreeTests {
        [Test]
        public void Create() {
            // without sugar
            var a = new RoseTree<int>(1, LazyListModule.ofArray(new[] {
                new RoseTree<int>(2, LazyListModule.empty<RoseTree<int>>()),
            }));

            // with sugar (but no laziness)
            var b = RoseTree.Create(1, RoseTree.Create(2));

            Assert.AreEqual(b, a);
        }

        [Test]
        public void Select() {
            var b = RoseTree.Create(1, RoseTree.Singleton(2));
            var c = from i in b select i + 1;
            var d = RoseTree.Create(2, RoseTree.Singleton(3));
            Assert.AreEqual(d, c);
        }

        [Test]
        public void SelectMany() {
            var b = RoseTree.Create(1, RoseTree.Singleton(2));
            var c = b.SelectMany(i => {
                if (i <= 1)
                    return RoseTree.Singleton(i);
                return RoseTree.Create(i, RoseTree.Singleton(i+1));
            });
            PrintTree(c, 0);
            var d = RoseTree.Create(1, RoseTree.Create(2, RoseTree.Singleton(3)));
            Assert.AreEqual(d, c);
        }

        [Test]
        public void SelectMany2() {
            // kind of useless?
            var b = RoseTree.Create(1, RoseTree.Singleton(2));
            var c = b.SelectMany(RoseTree.Singleton, (x,y) => y);
            PrintTree(c, 0);
            var d = RoseTree.Create(1, 
                        RoseTree.Singleton(2), 
                        RoseTree.Create(1, RoseTree.Singleton(2)));
            Assert.AreEqual(d, c);
        }

        [Test]
        public void SelectManyLinq() {
            // kind of useless?
            var b = RoseTree.Create(1, RoseTree.Singleton(2));
            var c = from i in b
                    let r = i <= 1 ? RoseTree.Singleton(i) : RoseTree.Create(i, RoseTree.Singleton(i + 1))
                    from j in r
                    select j;
            PrintTree(c, 0);
            var d = RoseTree.Create(1, 
                        RoseTree.Create(2, RoseTree.Create(3)),
                        RoseTree.Create(1,
                            RoseTree.Create(2, RoseTree.Create(3))));
            Assert.AreEqual(d, c);
        }

        private static RoseTree<T> R<T>(T root, params RoseTree<T>[] children) {
            return RoseTree.Create(root, children);
        }

        private static readonly RoseTree<string> atree =
            R("f",
                R("b",
                    R("a"),
                    R("d",
                        R("c"),
                        R("e"))),
                R("g",
                    R("i",
                        R("h"))));

        [Test]
        public void DFS() {
            var elements = atree.DfsPre().ToList();
            CollectionAssert.AreEqual(new[] {"f", "b", "a", "d","c","e","g","i","h"}, elements);
        }

        [Test]
        public void SelectAccum_trivial() {
            var b = atree.SelectAccum(0, Tuple.Create);
            Assert.AreEqual(0, b.Item1);
            Assert.AreEqual(atree, b.Item2);
        }

        [Test]
        public void SelectAccum() {
            var b = atree.SelectAccum(0, (i,e) => Tuple.Create(i+1, e + i));
            Assert.AreEqual(9, b.Item1);
            var expected =
                R("f0",
                    R("b1",
                        R("a2"),
                        R("d3",
                            R("c4"),
                            R("e5"))),
                    R("g6",
                        R("i7",
                            R("h8"))));

            Assert.AreEqual(expected, b.Item2);
        }

        private static void PrintTree<T>(RoseTree<T> tree, int level) {
            var tabs = new string('\t', level);
            Console.WriteLine(tabs + tree.Root);
            foreach (var c in tree.Children)
                PrintTree(c, level + 1);
        }
    }
}
