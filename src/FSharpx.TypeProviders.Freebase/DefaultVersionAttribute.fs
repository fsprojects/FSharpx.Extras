// Copyright (c) Microsoft Corporation 2005-2012.
// This sample code is provided "as is" without warranty of any kind. 
// We disclaim all warranties, either express or implied, including the 
// warranties of merchantability and fitness for a particular purpose. 

namespace DefaultVersionAttribute
// We use different assembly versions for different target platforms 
#if TYPE_PROVIDER_RUNTIME_FX_NET35
[<assembly: System.Reflection.AssemblyVersion("0.0.0.35")>]
#endif
#if TYPE_PROVIDER_RUNTIME_FX_NET40
[<assembly: System.Reflection.AssemblyVersion("0.0.0.4")>]
#endif
#if TYPE_PROVIDER_RUNTIME_FX_NET45
[<assembly: System.Reflection.AssemblyVersion("0.0.0.45")>]
#endif
#if TYPE_PROVIDER_RUNTIME_FX_PORTABLE47 
[<assembly: System.Reflection.AssemblyVersion("0.0.0.451")>]
#endif
#if TYPE_PROVIDER_RUNTIME_FX_SL5
[<assembly: System.Reflection.AssemblyVersion("0.0.0.452")>]
#endif
do()

