namespace FSharp.Control

open System
open System.Collections.Generic
open FSharp.Control

// ----------------------------------------------------------------------------

open System.Runtime.CompilerServices

[<Extension>]
type ObservableDispatcherExtensions private () =

    /// Execute Observer function on Dispatcher thread
    /// <Remarks>For WPF and Silverlight</remarks> 
    static let onDispatcher (observable:IObservable<'a>) =
    #if SILVERLIGHT
        let dispatcher = System.Windows.Deployment.Current.Dispatcher
    #else
        let dispatcher = System.Windows.Threading.Dispatcher.CurrentDispatcher
    #endif
        let f g =
            dispatcher.BeginInvoke(Action(fun _ -> g())) |> ignore
        Observable.invoke f observable

    [<Extension>]
    static member 
        OnDispatcher<'TSource>
            (source:IObservable<'TSource>) =
        source |> onDispatcher
