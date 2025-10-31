#r "nuget: ClosedXML"
open ClosedXML.Excel
open System.IO

let PlanilhaParaMatriz (caminho: string) : int[,] =
    if not (File.Exists(caminho)) then
        failwithf "Arquivo não encontrado: %s" caminho

    let planilha = new XLWorkbook(caminho)
    let aba = planilha.Worksheet(1)

    let ultimaLinha = aba.LastRowUsed().RowNumber()
    let ultimaColuna = aba.LastColumnUsed().ColumnNumber()

    Array2D.init ultimaLinha ultimaColuna (fun linha coluna ->
        aba.Cell(linha + 1, coluna + 1).GetValue<int>()
    )

let caminhoN = Path.Combine(__SOURCE_DIRECTORY__, "naturais.xlsx")
let caminhoP = Path.Combine(__SOURCE_DIRECTORY__, "pares.xlsx")

let matrizNaturais = PlanilhaParaMatriz caminhoN
let matrizPares = PlanilhaParaMatriz caminhoP

printfn "Caminho usado: %s" caminhoN
//printfn "Matriz Naturais: %A" matrizNaturais
//printfn "Matriz Pares: %A" matrizPares
