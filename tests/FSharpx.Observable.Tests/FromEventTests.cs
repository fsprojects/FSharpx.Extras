using System;
using FSharpx.Control;
using NUnit.Framework;

[TestFixture]
public class FromEventTests
{
    public delegate void TestEventHandler(object sender, EventArgs e);

    event TestEventHandler TestEvent;

    [Test]
    public void observable_generated_from_event_should_add_handler_on_subscribe()      
    {          
        bool added = false;
        var observable = 
            Observable.FromEvent<EventArgs,TestEventHandler>(
                f => new TestEventHandler((sender, args) => f(args)),
                handler => { added = true; TestEvent += handler; },
                handler => TestEvent -= handler);
        var disposable = observable.Subscribe(_ => { });
        disposable.Dispose();
        Assert.True(added);
    }

    [Test]
    public void observable_generated_from_event_should_remove_handler_on_dispose()      
    {          
        bool removed = false;
        var observable =
            Observable.FromEvent<EventArgs, TestEventHandler>(
                f => new TestEventHandler((sender, args) => f(args)),
                handler => TestEvent += handler,
                handler => { removed = true; TestEvent -= handler; });
        var disposable = observable.Subscribe(_ => { });
        disposable.Dispose();
        Assert.True(removed);
    }

    [Test]
    public void subscription_to_observable_generated_from_event_should_receive_events()
    {      
        var observable =
            Observable.FromEvent<EventArgs, TestEventHandler>(
                f => new TestEventHandler((sender, args) => f(args)),
                handler => TestEvent += handler,
                handler => TestEvent -= handler);
        int count = 0;
        var disposable = observable.Subscribe(_ => ++count);
        var e = TestEvent;
        if (e != null) e(this, new EventArgs());
        disposable.Dispose();
        Assert.AreEqual(count,1);
    }
}
