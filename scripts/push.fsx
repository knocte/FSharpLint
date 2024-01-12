#load "nuget.fsx"

open System
open System.IO

open Fake.Core
open Fake.DotNet

// --------------------------------------------------------------------------------------
// Helpers
// --------------------------------------------------------------------------------------

let getBuildParam var =
    let value = Environment.GetEnvironmentVariable var
    if String.IsNullOrWhiteSpace value then
        None
    else
        Some value

// --------------------------------------------------------------------------------------
// Build variables
// --------------------------------------------------------------------------------------

let nugetDir  = "./out/"
let githubRef = Environment.GetEnvironmentVariable "GITHUB_REF"
let tagPrefix = "refs/tags/"
let isTag =
    if isNull githubRef then
        false
    else
        githubRef.StartsWith tagPrefix

// --------------------------------------------------------------------------------------
// Functions
// --------------------------------------------------------------------------------------

let Push() =
    let push key =
        Paket.push (fun p -> { p with WorkingDir = nugetDir; ApiKey = key; ToolType = ToolType.CreateLocalTool() })

    let key = getBuildParam "nuget-key"
    match getBuildParam "GITHUB_EVENT_NAME" with
    | None ->
        match key with
        | None ->
            let key = UserInput.getUserPassword "NuGet Key: "
            push key
        | Some key ->
            push key

    | Some "push" ->
        match key with
        | None ->
            Console.WriteLine "No nuget-key env var found, skipping..."
        | Some key ->
            if isTag then
                push key
            else
                match getBuildParam "GITHUB_SHA" with
                | None ->
                    failwith "GITHUB_SHA should have been populated"
                | Some commitHash ->
                    let gitArgs = sprintf "describe --exact-match --tags %s" commitHash
                    let proc =
                        CreateProcess.fromRawCommandLine "git" gitArgs
                        |> Proc.run
                    if proc.ExitCode <> 0 then
                        // commit is not a tag, so go ahead pushing a prerelease
                        push key
                    else
                        Console.WriteLine "Commit mapped to a tag, skipping pushing prerelease..."
    | _ ->
        Console.WriteLine "Github event name not 'push', skipping..."

// --------------------------------------------------------------------------------------
// Script
// --------------------------------------------------------------------------------------

Push()
