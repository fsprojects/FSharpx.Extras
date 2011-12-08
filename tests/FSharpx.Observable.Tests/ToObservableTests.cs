using System.Collections.Generic;
using FSharpx;
using NUnit.Framework;

[TestFixture]
public class ToObservableTests
{
    [Test]
    public void and_empty_sequence_converted_to_obserables_should_yield_no_values()
    {
        var items = new int[] {};
        var observable = items.ToObservable();
        var observed = new List<int>();
        var subscription = observable.Subscribe(observed.Add);
        subscription.Dispose();
        Assert.AreEqual(observed.Count,0);        
    }

    [Test]
    public void a_sequence_of_items_converted_to_an_obserable_should_yield_the_same_values()
    {
        var items = new[] {1,2,3};
        var observable = items.ToObservable();
        var observed = new List<int>();
        var subscription = observable.Subscribe(observed.Add);
        subscription.Dispose();
        Assert.AreEqual(items.Length, observed.Count);
        for (int i = 0; i < items.Length; i++)
            Assert.AreEqual(items[i], observed[i]);        
    }
}

