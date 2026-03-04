namespace FSharpLint.Core

open System

[<AutoOpen>]
module Prelude =

    module Async =
        let asyncCombine operation job1 job2 = async {
            let! firstOperation = job1
            let! secondOperation = job2
            return operation firstOperation secondOperation }

        let combineAsync operation job1 job2 = Async.StartAsTask(asyncCombine operation job1 job2)

        [<Obsolete "Use asyncCombine">]
        let combine = asyncCombine

        let asyncMap operation job = async {
            let! jobResult = job
            return operation jobResult }

        let mapAsync operation job = Async.StartAsTask(asyncMap operation job)

        [<Obsolete "Use asyncMap">]
        let map = asyncMap
