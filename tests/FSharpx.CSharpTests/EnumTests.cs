using System;
using NUnit.Framework;
using Microsoft.FSharp.Core;

namespace FSharpx.CSharpTests
{
    [TestFixture]
    public class EnumTests
    {
        enum LanguageOptions
        {
            FSharp = 0,
            CSharp = 1,
            VB = 2
        }

        [Test]
        public void tryParse_can_parse_value_and_return_Some_value() =>
            Assert.AreEqual(FSharpOption.Some(LanguageOptions.CSharp), Enum.TryParse<LanguageOptions>("CSharp"));
        [Test]
        public void tryParse_returns_None_if_it_fails_to_parse() =>
            Assert.AreEqual(FSharpOption<LanguageOptions>.None, Enum.TryParse<LanguageOptions>("English"));
        [Test]
        public void parse_returns_parsed_value()=>
            Assert.AreEqual(LanguageOptions.CSharp ,Enum.Parse<LanguageOptions>("CSharp"));

    }
}
