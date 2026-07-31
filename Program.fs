namespace MathLibrary

open System
open System.IO
open System.Collections.Generic
open ClosedXML.Excel

open SemiProgram

module Program =

    [<EntryPoint>]
    let main argv =
        executar Inicio
        0 // código de saída