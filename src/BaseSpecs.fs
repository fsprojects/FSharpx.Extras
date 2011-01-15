module BaseSpecs
open NUnit.Framework

// NUnit helpers
let isTrue = Assert.IsTrue
let isFalse = Assert.IsFalse
let inline (==) (actual:#obj) (expected:#obj) = Assert.AreEqual(expected, actual)
let inline (!=) (actual:#obj) (expected:#obj) = Assert.AreNotEqual(expected, actual)
let inline (<->) (actual:#obj) expected = Assert.IsInstanceOf(expected, actual)
let inline (<!>) (actual:#obj) expected = Assert.IsNotInstanceOf(expected, actual)
let ``is null`` anObject = Assert.IsNull(anObject)
let ``is not null`` anObject = Assert.NotNull(anObject)