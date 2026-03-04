module FSharpLint.Rules.Utilities

open System.IO
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols
open FSharp.Compiler.Syntax

module TypedTree =
    let tryGetLastGenericArg (fSharpType: FSharpType) =
        Seq.tryLast fSharpType.GenericArguments

    [<TailCall>]
    let rec private getReturnType (fSharpType: FSharpType) =
        if fSharpType.IsFunctionType then
            match tryGetLastGenericArg fSharpType with
            | Some argType -> getReturnType argType
            | None -> fSharpType
        else
            fSharpType

    let getFunctionReturnType
        (checkInfo: FSharpCheckFileResults)
        (lines: array<string>)
        (funcIdent: SynLongIdent) : Option<FSharpType> =
        let maybeSymbolUse =
            match List.tryLast funcIdent.LongIdent with
            | Some ident ->
                checkInfo.GetSymbolUseAtLocation(
                    ident.idRange.EndLine,
                    ident.idRange.EndColumn,
                    lines.[ident.idRange.EndLine - 1],
                    List.singleton ident.idText)
            | None -> None
        match maybeSymbolUse with
        | Some symbolUse ->
            match symbolUse.Symbol with
            | :? FSharpMemberOrFunctionOrValue as func when func.IsFunction ->
                Some <| getReturnType func.FullType
            | _ -> None
        | _ -> None

module LibraryHeuristics =
    type LibraryHeuristicResult =
        | Likely
        | Unlikely
        | Uncertain

    let private projectNamesUnlikelyToBeLibraries =
        [
            "tests"
            "test"
            "testing"
            "console"
            "CLI"
            "TUI"
        ]
        |> Seq.map (fun name -> name.ToLowerInvariant())

    let private possibleProjectNameSegmentSeparators =
        [|
            '.'
            '_'
            '-'
        |]

    [<TailCall>]
    let rec howLikelyLintTargetIsInLibrary (fs: FileSystemInfo): LibraryHeuristicResult =
        let libraryAbbrev = "lib"
        let targetName = Path.GetFileNameWithoutExtension fs.FullName
        let nameSegments =
            Helper.Naming.QuickFixes.splitByCaseChange targetName
            |> Seq.map (fun segment -> segment.ToLowerInvariant())
        if nameSegments |> Seq.contains libraryAbbrev then
            Likely
        elif
            nameSegments
            |> Seq.exists (
                fun segment ->
                    let subSegments = segment.Split possibleProjectNameSegmentSeparators
                    subSegments
                    |> Seq.exists (fun subSegment ->
                        projectNamesUnlikelyToBeLibraries
                        |> Seq.exists (fun noLibName -> noLibName = subSegment)
                    )
            ) then
            Unlikely
        elif targetName.ToLowerInvariant().EndsWith libraryAbbrev then
            Likely
        else
            match fs with
            | :? FileInfo as file ->
                howLikelyLintTargetIsInLibrary file.Directory
            | :? DirectoryInfo as dir ->
                // some tests have fake paths, so we need this to not throw inside them
                if not dir.Exists then
                    Uncertain
                else
                    match dir.EnumerateFiles "*.fsproj" |> Seq.tryHead with
                    | Some _projFile -> Uncertain
                    | None ->
                        match Option.ofObj dir.Parent with
                        | None -> Uncertain
                        | Some parentDir ->
                            howLikelyLintTargetIsInLibrary parentDir
            | _ -> Uncertain
