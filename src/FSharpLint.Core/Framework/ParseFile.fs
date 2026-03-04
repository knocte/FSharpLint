namespace FSharpLint.Framework

open System

/// Provides functionality to parse F# files using `FSharp.Compiler.Service`.
module ParseFile =

    open System.IO
    open FSharpLint.Framework
    open FSharp.Compiler.CodeAnalysis
    open FSharp.Compiler.Diagnostics
    open FSharp.Compiler.Syntax
    open FSharp.Compiler.Text
    open Utilities

    /// Information for a file to be linted that is given to the analysers.
    [<NoEquality; NoComparison>]
    type FileParseInfo = {
        /// Contents of the file.
        Text:string

        /// File represented as an AST.
        Ast:ParsedInput

        /// Optional results of inferring the types on the AST (allows for a more accurate lint).
        TypeCheckResults:FSharpCheckFileResults option

        /// Optional results of project-wide type info (allows for a more accurate lint).
        ProjectCheckResults:FSharpCheckProjectResults option

        /// Path to the file.
        File:string
    }

    [<NoComparison>]
    type ParseFileFailure =
        | FailedToParseFile of failures: FSharpDiagnostic []
        | AbortedTypeCheck

    [<NoComparison>]
    type ParseFileResult<'Content> =
        | Failed of failure: ParseFileFailure
        | Success of result: 'Content

    let private parse file source (checker:FSharpChecker, options) = async {
        let sourceText = SourceText.ofString source
        let! parseResults, checkFileAnswer =
            checker.ParseAndCheckFileInProject(file, 0, sourceText, options)

        match checkFileAnswer with
        | FSharpCheckFileAnswer.Succeeded(typeCheckResults) ->
            return Success
                {
                    Text = source
                    Ast = parseResults.ParseTree
                    TypeCheckResults = Some(typeCheckResults)
                    ProjectCheckResults = None
                    File = file
                }
        | FSharpCheckFileAnswer.Aborted -> return Failed(AbortedTypeCheck)
    }

    let asyncGetProjectOptionsFromScript (checker:FSharpChecker) file (source:string) = async {
        let sourceText = SourceText.ofString source
        let assumeDotNetFramework = false
        let otherOpts = Array.singleton "--targetprofile:netstandard"

        let! options, _diagnostics =
            checker.GetProjectOptionsFromScript(file, sourceText, assumeDotNetFramework = assumeDotNetFramework, useSdkRefs = not assumeDotNetFramework, otherFlags = otherOpts)
        return options
    }

    let getProjectOptionsFromScriptAsync checker file source =
        Async.StartAsTask(asyncGetProjectOptionsFromScript checker file source)

    [<Obsolete "Use asyncGetProjectOptionsFromScript">]
    let getProjectOptionsFromScript = asyncGetProjectOptionsFromScript

    /// Parses a file using `FSharp.Compiler.Service`.
    let asyncParseFile file (checker:FSharpChecker) projectOptions = async {
        let source = File.ReadAllText(file)

        let! projectOptions =
            match projectOptions with
            | Some(existingOptions) -> async { return existingOptions }
            | None -> asyncGetProjectOptionsFromScript checker file source

        return! parse file source (checker, projectOptions)
    }

    let parseFileAsync file checker projectOptions = Async.StartAsTask(asyncParseFile file checker projectOptions)

    [<Obsolete "Use asyncParseFile">]
    let parseFile = asyncParseFile

    /// Parses source code using `FSharp.Compiler.Service`.
    let asyncParseSourceFile fileName source (checker:FSharpChecker) = async {
        let! options = asyncGetProjectOptionsFromScript checker fileName source

        return! parse fileName source (checker, options)
    }

    let parseSourceFileAsync fileName source checker = Async.StartAsTask(asyncParseSourceFile fileName source checker)

    [<Obsolete "Use asyncParseSourceFile">]
    let parseSourceFile = asyncParseSourceFile

    let asyncParseSource source (checker:FSharpChecker) =
        let fileName = Path.ChangeExtension(Path.GetTempFileName(), "fsx")
        asyncParseSourceFile fileName source checker

    let parseSourceAsync source checker = Async.StartAsTask(asyncParseSource source checker)

    [<Obsolete "Use asyncParseSource">]
    let parseSource = asyncParseSource
