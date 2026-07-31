namespace MathLibrary 

open System
open System.IO
open System.Collections.Generic
open ClosedXML.Excel

open ExeFunctions

module SemiProgram =
    type Estado =
    | Inicio
    | Escolhendo
    | Computando of string
    
    let rec executar estado =
        match estado with
        | Inicio ->
                         Início() |>ignore
                         executar Escolhendo

        | Escolhendo ->  printfn"\t Com o quê você quer trabalhar?"
                         let operação = EscolhendoOperações() 
                         Console.WriteLine()
                         executar (Computando operação)

        | Computando operação ->
                         ComputandoOperação operação
                         maisUmaoperação ComputandoOperação operação             
                         Console.Clear()
                         executar Inicio
