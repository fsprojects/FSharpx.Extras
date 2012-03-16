param($installPath, $toolsPath, $package, $project)
    $project.Object.References.Add("System.Configuration") | out-null
	$project.Object.References.Add("FSharp.Data.TypeProviders") | out-null