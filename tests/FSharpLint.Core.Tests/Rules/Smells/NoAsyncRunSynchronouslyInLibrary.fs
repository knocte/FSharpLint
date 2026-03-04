module FSharpLint.Core.Tests.Rules.Smells.NoAsyncRunSynchronouslyInLibrary

open System
open System.IO
open NUnit.Framework
open FSharpLint.Framework.Rules
open FSharpLint.Rules
open FSharpLint.Rules.NoAsyncRunSynchronouslyInLibrary
open FSharpLint.Rules.Utilities.LibraryHeuristics

[<TestFixture>]
type TestNoAsyncRunSynchronouslyInLibrary() =
    inherit FSharpLint.Core.Tests.TestAstNodeRuleBase.TestAstNodeRuleBase(NoAsyncRunSynchronouslyInLibrary.rule)

    [<Test>]
    member this.``Async.RunSynchronously should not be used in library code``() =
        this.Parse("""
module Program

async {
    return ()
}
|> Async.RunSynchronously""")

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.``Async.RunSynchronously may be used in code that declares entry point``() =
        this.Parse("""
module Program

[<EntryPoint>]
let main () =
    async {
        return ()
    }
    |> Async.RunSynchronously""")

        this.AssertNoWarnings()

    [<Test>]
    member this.``Async.RunSynchronously may be used in code module that has function with entry point``() =
        this.Parse("""
module Program

let foo () =
    async {
        return ()
    }
    |> Async.RunSynchronously

[<EntryPoint>]
let main () =
    0""")

        this.AssertNoWarnings()

    [<Test>]
    member this.``Async.RunSynchronously may be used in NUnit test code``() =
        this.Parse("""
module Program

[<TestFixture>]
type FooTest () =
    [<Test>]
    member this.Foo() =
        async {
            return ()
        }
        |> Async.RunSynchronously""")

        this.AssertNoWarnings()

    [<Test>]
    member this.``Async.RunSynchronously may be used in MSTest test code``() =
        this.Parse("""
module Program

[<TestClass>]
type FooTest () =
    [<TestMethod>]
    member this.Foo() =
        async {
            return ()
        }
        |> Async.RunSynchronously""")

        this.AssertNoWarnings()

    [<Test>]
    member this.``Async.RunSynchronously may be used in module with tests``() =
        this.Parse("""
module Program

let foo () =
    async {
        return ()
    }
    |> Async.RunSynchronously

[<TestClass>]
type FooTest () =
    [<TestMethod>]
    member this.Foo() =
        ()""")

        this.AssertNoWarnings()

    [<Test>]
    member this.``Async.RunSynchronously may be used in methods with Obsolete attribute``() =
        this.Parse("""
module Program

type FooTest () =
    [<Obsolete>]
    member this.Foo() =
        async {
            return ()
        }
        |> Async.RunSynchronously""")

        this.AssertNoWarnings()

    [<Test>]
    member this.``Async.RunSynchronously may be used in functions with Obsolete attribute``() =
        this.Parse("""
module Program

[<Obsolete>]
let Foo() =
    async {
        return ()
    }
    |> Async.RunSynchronously""")

        this.AssertNoWarnings()

[<TestFixture>]
type TestNoAsyncRunSynchronouslyInLibraryHeuristic() =

    [<Test>]
    member this.``Unlikely to be library if contains "tests" in name``() =
        Assert.AreEqual(
            LibraryHeuristicResult.Unlikely,
            howLikelyLintTargetIsInLibrary(FileInfo "/dummyPath/IntegrationTests.fsproj")
        )

    [<Test>]
    member this.``Unlikely to be library if contains "testing" in name``() =
        Assert.AreEqual(
            LibraryHeuristicResult.Unlikely,
            howLikelyLintTargetIsInLibrary(FileInfo "/dummyPath/UnitTesting.fsproj")
        )

    [<Test>]
    member this.``Unlikely to be library if contains "test" in name``() =
        Assert.AreEqual(
            LibraryHeuristicResult.Unlikely,
            howLikelyLintTargetIsInLibrary(FileInfo "/dummyPath/TestSuite.fsproj")
        )

    [<Test>]
    member this.``Unlikely to be library if contains "console" in name``() =
        Assert.AreEqual(
            LibraryHeuristicResult.Unlikely,
            howLikelyLintTargetIsInLibrary(FileInfo "/dummyPath/FooConsole.fsproj")
        )

    [<Test>]
    member this.``Likely to be library if contains Contains "Lib" as a PascalCase segment``() =
        Assert.AreEqual(
            LibraryHeuristicResult.Likely,
            howLikelyLintTargetIsInLibrary(FileInfo "/dummyPath/LibFoo.fsproj")
        )

    [<Test>]
    member this.``Uncertain if contains contains "Lib" but not as a PascalCase segment``() =
        Assert.AreEqual(
            LibraryHeuristicResult.Uncertain,
            howLikelyLintTargetIsInLibrary(FileInfo "/dummyPath/LibreOfficeProg.fsproj")
        )

    [<Test>]
    member this.``Likely to be library if contains ends with "lib" (case-insensitive)``() =
        Assert.AreEqual(
            LibraryHeuristicResult.Likely,
            howLikelyLintTargetIsInLibrary(FileInfo "/dummyPath/FooLib.fsproj")
        )

    [<Test>]
    member this.``Unlikely to be library if contains "CLI" in name``() =
        Assert.AreEqual(
            LibraryHeuristicResult.Unlikely,
            howLikelyLintTargetIsInLibrary(FileInfo "/dummyPath/FooCLI.fsproj")
        )

    [<Test>]
    member this.``Uncertain to be library if contains "cli" in name not related to CLI``() =
        Assert.AreEqual(
            LibraryHeuristicResult.Uncertain,
            howLikelyLintTargetIsInLibrary(FileInfo "/dummyPath/InclinedDriver")
        )

    [<Test>]
    member this.``Unlikely to be library if contains "TUI" in name``() =
        Assert.AreEqual(
            LibraryHeuristicResult.Unlikely,
            howLikelyLintTargetIsInLibrary(FileInfo "/dummyPath/FooTUI.fsproj")
        )

    [<Test>]
    member this.``Likely to be library if it starts with "lib", e.g. camelCase``() =
        Assert.AreEqual(
            LibraryHeuristicResult.Likely,
            howLikelyLintTargetIsInLibrary(FileInfo "/dummyPath/libFoo.fsproj")
        )

    [<Test>]
    member this.``Unlikely to be library if it contains "console", but segments are separated by dots``() =
        Assert.AreEqual(
            LibraryHeuristicResult.Unlikely,
            howLikelyLintTargetIsInLibrary(FileInfo "/dummyPath/foo.console.app.fsproj")
        )

    [<Test>]
    member this.``Unlikely to be library if it contains "console", but segments are separated by dashes``() =
        Assert.AreEqual(
            LibraryHeuristicResult.Unlikely,
            howLikelyLintTargetIsInLibrary(FileInfo "/dummyPath/foo-console-app.fsproj")
        )

    [<Test>]
    member this.``Unlikely to be library if it contains "console", but segments are separated by underscores``() =
        Assert.AreEqual(
            LibraryHeuristicResult.Unlikely,
            howLikelyLintTargetIsInLibrary(FileInfo "/dummyPath/foo_console_app.fsproj")
        )

    [<Test>]
    member this.``Unlikely to be library if path segment contains "console"``() =
        Assert.AreEqual(
            LibraryHeuristicResult.Unlikely,
            howLikelyLintTargetIsInLibrary(FileInfo "/dummyPath/Console/Foo.fs")
        )

    [<Test>]
    member this.``Likely to be library if path segment contains "Lib" as a PascalCase segment``() =
        Assert.AreEqual(
            LibraryHeuristicResult.Likely,
            howLikelyLintTargetIsInLibrary(FileInfo "/dummyPath/LibFoo/Foo.fs")
        )

    [<Test>]
    member this.``Uncertain if path segment don't indicate likelyhood being in library``() =
        Assert.AreEqual(
            LibraryHeuristicResult.Uncertain,
            howLikelyLintTargetIsInLibrary(FileInfo "/dummyPath/FooBar/Foo.fs")
        )

    [<Test>]
    member this.``Unlikely if one of the parent dirs contain fsproj file with 'Console'``() =
        let tempDir = Directory.CreateTempSubdirectory "libraryHeuristicTest"
        let srcDir = tempDir.CreateSubdirectory "src"
        let prjDir = srcDir.CreateSubdirectory "Console"
        let subDir = prjDir.CreateSubdirectory "Namespace"
        let file = FileInfo(Path.Combine(subDir.FullName, "Foo.fs"))
        File.WriteAllText(Path.Combine(prjDir.FullName, "SomeProject.fsproj"), String.Empty)
        let result =
            try
                howLikelyLintTargetIsInLibrary file
            finally
                tempDir.Delete true

        Assert.AreEqual(
            LibraryHeuristicResult.Unlikely,
            result
        )

    [<Test>]
    member this.``Likely if one of the parent dirs contain fsproj file with 'Console'``() =
        let tempDir = Directory.CreateTempSubdirectory "libraryHeuristicTest"
        let srcDir = tempDir.CreateSubdirectory "src"
        let prjDir = srcDir.CreateSubdirectory "LibFoo"
        let subDir = prjDir.CreateSubdirectory "Namespace"
        let file = FileInfo(Path.Combine(subDir.FullName, "Foo.fs"))
        File.WriteAllText(Path.Combine(prjDir.FullName, "SomeProject.fsproj"), String.Empty)
        let result =
            try
                howLikelyLintTargetIsInLibrary file
            finally
                tempDir.Delete true

        Assert.AreEqual(
            LibraryHeuristicResult.Likely,
            result
        )

    [<Test>]
    member this.``Uncertain if none of the parent dirs contain any hint``() =
        let tempDir = Directory.CreateTempSubdirectory "libraryHeuristicTest"
        let srcDir = tempDir.CreateSubdirectory "src"
        let prjDir = srcDir.CreateSubdirectory "Foo"
        let subDir = prjDir.CreateSubdirectory "Namespace"
        let file = FileInfo(Path.Combine(subDir.FullName, "Foo.fs"))
        File.WriteAllText(Path.Combine(prjDir.FullName, "SomeProject.fsproj"), String.Empty)
        let result =
            try
                howLikelyLintTargetIsInLibrary file
            finally
                tempDir.Delete true

        Assert.AreEqual(
            LibraryHeuristicResult.Uncertain,
            result
        )

    [<Test>]
    member this.``Still Uncertain if none of the RELEVANT parent dirs contain any hint (loop should not check until arriving to root / dir) cause 'librarian' shouldn't be a hint even if it starts with lib``() =
        let tempDir = Directory.CreateTempSubdirectory "libraryHeuristicTest"
        let homeDir = tempDir.CreateSubdirectory "home"
        let userDir = homeDir.CreateSubdirectory "librarian"
        let srcDir = userDir.CreateSubdirectory "src"
        let prjDir = srcDir.CreateSubdirectory "Foo"
        let subDir = prjDir.CreateSubdirectory "Namespace"
        let file = FileInfo(Path.Combine(subDir.FullName, "Foo.fs"))
        File.WriteAllText(Path.Combine(prjDir.FullName, "SomeProject.fsproj"), String.Empty)
        let result =
            try
                howLikelyLintTargetIsInLibrary file
            finally
                tempDir.Delete true

        Assert.AreEqual(
            LibraryHeuristicResult.Uncertain,
            result
        )

    [<Test>]
    member this.``Likely if parent of project dir is 'tests'``() =
        let tempDir = Directory.CreateTempSubdirectory "libraryHeuristicTest"
        let homeDir = tempDir.CreateSubdirectory "home"
        let userDir = homeDir.CreateSubdirectory "librarian"
        let srcDir = userDir.CreateSubdirectory "tests"
        let prjDir = srcDir.CreateSubdirectory "Foo"
        let subDir = prjDir.CreateSubdirectory "Namespace"
        let file = FileInfo(Path.Combine(subDir.FullName, "Foo.fs"))
        File.WriteAllText(Path.Combine(prjDir.FullName, "SomeProject.fsproj"), String.Empty)
        let result =
            try
                howLikelyLintTargetIsInLibrary file
            finally
                tempDir.Delete true

        Assert.AreEqual(
            LibraryHeuristicResult.Unlikely,
            result
        )

