module FSharpLint.Rules.NoAsyncRunSynchronouslyInLibrary

open System
open System.IO
open FSharp.Compiler.Syntax
open FSharp.Compiler.Symbols
open FSharp.Compiler.Text
open FSharp.Compiler.CodeAnalysis
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open FSharpLint.Framework.Utilities
open FSharpLint.Rules.Utilities.LibraryHeuristics

let runner args =
    match args.AstNode with
    | AstNode.Identifier(["Async"; "RunSynchronously"], range) ->
        let ruleIsApplicable = not (checkIfInLibrary args)
        if ruleIsApplicable then
            Array.singleton 
                { 
                    Range = range
                    Message = Resources.GetString "NoAsyncRunSynchronouslyInLibrary"
                    SuggestedFix = None
                    TypeChecks = List.Empty 
                }
        else
            Array.empty
    | _ -> Array.empty

let rule =
    AstNodeRule
        {
            Name = "NoAsyncRunSynchronouslyInLibrary"
            Identifier = Identifiers.NoAsyncRunSynchronouslyInLibrary
            RuleConfig =
                {
                    AstNodeRuleConfig.Runner = runner
                    Cleanup = ignore
                }
        }
