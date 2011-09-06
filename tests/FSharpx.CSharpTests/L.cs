using System;

namespace FSharpx.CSharpTests {
    public static class L {
        public static Func<T> F<T>(Func<T> f) {
            return f;
        }

        public static Func<A,B> F<A,B>(Func<A,B> f) {
            return f;
        }

        public static Func<A, B, C> F<A, B, C>(Func<A, B, C> f) {
            return f;
        }
    }
}
