using System;
using NUnit.Framework;

namespace FSharpx.CSharpTests {
    [TestFixture]
    public class LazyTests {
        [Test]
        public void Create() {
            var i = 0;
            var l = FSharpLazy.Create(() => {
                i = i+1;
                return i;
            });
            Assert.AreEqual(1, l.Value);
            Assert.AreEqual(1, l.Value);
        }

        class MySingleton {
            private MySingleton() {}

            private static readonly Lazy<MySingleton> instance = 
                FSharpLazy.Create(() => new MySingleton());

            public static MySingleton Instance {
                get { return instance.Value; }
            }
        }

        [Test]
        public void Singleton() {
            var a = MySingleton.Instance;
            var b = MySingleton.Instance;
            Assert.AreSame(a,b);
        }
    }
}