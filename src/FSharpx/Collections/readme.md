#FSharpx.Collections

##FSharpx.Collections Namespace is Purely Functional

This namespace is for Purely Functional (immutable) Data Structures. Mutable structures belong in FSharpx.Collections.Mutable. FSharpx.Collections and FSharpx.Collections.Mutable auto-open in client projects opening FSharpx.

##Structure Type and Module

The structure’s type should exist directly in the namespace, as well as a module with the same name as the type, but not the type under the module. The type should have all the members that operate on an instantiated type. The module should have let bindings to all the the type members as well as let bindings for all other "utility" functions on the type, like ofSeq and empty.

##Understand the difference between a property and a method

[Property Usage Guidelines](http://msdn.microsoft.com/en-us/library/bzwdh01d.aspx) Properties are usually O(1), sometimes O(log n), never O(n).

##Pattern Discriminator

Include any relevant pattern discriminators in the module. 

##Try to Avoid Exceptions

If a structure’s method can throw an exception, include a TryMethodName method that returns option.

##Necessary Members

Every data structure should have a length or count member (even if length is O(n)) and an IsEmpty member in the type and an ofSeq let binding in the module.

##Additional Members

Of course implement all the members central to the structure, but there can be some nice O(1) or O(log n) values just waiting to be implemented that perhaps were not directly addressed in the research paper because they were not interesting. Examples: the internal structure of the type allows O(1) rev or ofList.

##Necessary Interface

Implement IEnumerable in the type. It should sequence elements in the inherent order of the data structure. For instance if a structure implements the head/tail paradigm, IEnumerable should sequence elements in the order of unconsing the structure down to empty. Most every structure has some logical, if not physical, order to its elements when flattened. 

Implementing ofSeq and IEnumerable makes practically every Data Structure composable into every other.

##Hide unsafe underlying structures

Any publicly accessible interface to the type or module should withstand any conceivable test case without throwing exceptions, or have a corresponding try -> option interface.

##XML docs / Intellisense

Every public member and let binding should have XML documentation to make it user friendly and not require a literature search or inspection of the code to determine functionality. At the type level there should be intellisense documenting the purpose and use of the structure. Include URLs to additional literature where appropriate.

Data structures should give developers useful tools, not research projects.

##Time Complexity

Adopting a practice from Haskell documentation, put the time complexity for members and let bindings at the beginning of their XML documentation. It is OK to put O(1) on empty and singleton let bindings to accustom users to seeing the time complexity in the intellisense, even though strictly speaking time complexity for these bindings has no meaning.

##Take time to evaluate the naming standards of types and members

While it’s a good practice to keep the name a structure is best known as, don’t just unthinkingly adopt method and property names from a research paper or the implementation in another language. Consider renaming properties and methods if it makes sense to fit in with F# and .NET paradigms. Use ofList and ofArray not fromList or fromArray. Use the FSharp.Collections and FSharpx.Collections namepaces as models Example: the Heap members findMin and deleteMin renamed to head and tail because that conveys the head/tail paradigm like several other data structures (and Heap is implemented as ascending or descending, so “min” is inappropriate).