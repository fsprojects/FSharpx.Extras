param($installPath, $toolsPath, $package, $project)
    $project.Object.References | Where-Object { $_.Name -eq 'System.Configuration' } | ForEach-Object { $_.Remove() }
	$project.Object.References | Where-Object { $_.Name -eq 'FSharp.Data.TypeProviders' } | ForEach-Object { $_.Remove() }