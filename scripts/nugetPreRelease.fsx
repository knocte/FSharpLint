#load "nuget.fsx"

let args = fsi.CommandLineArgs

Fsdk.Network.GetNugetPrereleaseVersionFromBaseVersion args.[1]
|> System.Console.WriteLine
