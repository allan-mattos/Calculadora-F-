// Minha primeira calculadora em F# com 14 operações diferentes
//Autor: Allan Mattos
//Data: 21/09/2025 
//Em obras
//Transformando todos os loops em recursividade
//Amém!

//Após uma alteração, não esqueça de dar os dois comandos:
//1) git pull origin master
//2) git push origin master 

open System
open System.IO
open System.Collections.Generic
open ClosedXML.Excel
open MathLibrary

  //Função que trata entradas de inteiros incorretas
let ``éInt?`` (input: string) =

    match Int32.TryParse(input) with
    | (true, value) -> Some value
    | (false, _) -> None


let mutable entrada = ""
let mutable operação = ""
let mutable quantidade : int = 0

//Acaso o usuário digite "q" para sair, temos as funções confirmarSaida e theEnd(principal):
let confirmarSaida () =
    printf "Tem certeza que deseja sair? (S/N): "
    match Console.ReadLine() with
    | null -> None
    | valor ->
        match valor.Trim().ToUpper() with
        | "S" -> Some true
        | "N" -> Some false
        | _ -> None

let rec theEnd () =
    match confirmarSaida () with
    | Some true -> 
        printfn "Saindo do programa..."
        Environment.Exit(0)
    | Some false -> printfn "Ok"
    | None -> 
        printfn "Entrada inválida! Digite 'S' ou 'N'!"
        theEnd() 
        
let rec  maisUmaoperação função op =
    printfn$"Deseja efetuar mais uma {operação}? (S\N)"
    printfn"Ou então digite 'Q' para sair: "

    entrada <- Console.ReadLine ()

    match entrada.Trim().ToUpper() with
    |"Q"     ->   theEnd () 
    |"S"     ->   função op                                     
    |"N"     ->   printfn "Ok" 
    |_       ->   printfn "Entrada inesperada! Digite 'S' ou 'N'!"
                  maisUmaoperação função op
                  printfn""
        
let Início () =
    Console.WriteLine("CALCULADORA")

    printfn ""

    printfn "-------(Pressione \"q\" para sair!)-------"


let rec EscolhendoOperações () =

    printfn ""
    printfn "Com o quê você quer trabalhar?:  "
    printfn "A) Adição de grandes volumes | C) Conjuntos | D) Divisão | E) Exponenciação| S) Subtração | M) Multiplicação de grandes volumes |
    \nR) Raiz quadrada | !) Fatorial! | F) Fibonacci | Q) Sair: "

    let entrada1: option<string> =
        match Console.ReadLine() with
        | null  -> None
        | valor -> Some valor
     
    match entrada1 with
    | None ->       printfn "Digite uma entrada válida!:"
                    EscolhendoOperações ()

    | Some valor ->
                    match valor.Trim().ToUpper() with
                    | "Q"  -> theEnd ()  
                              EscolhendoOperações()
                    | "A"  -> operação <- "Adição"
                              
                    | "C"  -> operação <- "Conjuntos"
                    | "R"  -> operação <- "Raiz quadrada"
                    | "D"  -> operação <- "Divisão"
                    | "E"  -> operação <- "Exponenciação"
                    | "S"  -> operação <- "Subtração"
                    | "M"  -> operação <- "Multiplicação"
                    | "!"  -> operação <- "Fatorial"
                    | "F"  -> operação <- "Fibonacci"
                    | _    -> printfn "Entrada inesperada.. Digite uma inicial de operação válida!"
                              EscolhendoOperações ()

let rec ComputandoOperações operação =
    
    match operação with
    | "Adição" ->     printfn"Você escolheu \"Adição\""  

                      printf "Digite os valores a serem somados com espaços entre eles: "

                      entrada <- Console.ReadLine ()

                      let StringEntrada = string entrada
                      let separadores = [| ' '; ';'; ':' |]
                      let StringArrayValSeparados = StringEntrada.Split(separadores, StringSplitOptions.RemoveEmptyEntries)
                      let EntradaParaFloatArray = Array.map (float) StringArrayValSeparados
                      let soma = arraySoma EntradaParaFloatArray
                      let somaString = (string)soma
                      let textoConcatenado = String.concat ", " StringArrayValSeparados // Embora Split forme um array com separadores, para imprimirmos no Console necessitamos concatenar com ,
                  
                      printf $"O(s) valor(es) "   
                      printfn "%s" textoConcatenado
                      printfn "Gera(m) o somatório de: %s" somaString 
              
                      maisUmaoperação ComputandoOperações operação
                      EscolhendoOperações ()
                        
    | "Conjuntos" ->  printfn"Você escolheu Conjuntos!"
                      
                      printf "Com quantos conjuntos você quer trabalhar?: "      

                      let tryParseInt (s: string) =
                          match Int32.TryParse(s) with
                          | (true, value) -> Some value
                          | (false, _) -> None

                      let rec pedirNumero () =
                          printf "Com quantos conjuntos você quer trabalhar?: "
                          let input = Console.ReadLine()
                          match tryParseInt input with
                          | Some value -> quantidade <- value
                          | None       ->
                                          printfn "Por favor, digite um número válido."
                                          pedirNumero ()


                      printfn "Quantidade escolhida: %d" quantidade


                      let mutable conjunto : HashSet<double>[] = Array.init quantidade (fun _-> HashSet<double>())

                      let nomes = [|"A";"B";"C";"D";"E";"F";"G";"H";"I";"J";"K";"L";"M";"N"|]

                      let mapa = Dictionary<string, HashSet<double>>()

                      //Esse loop provavelmente vai virar uma função recursiva porém os foreach de uma linha acredito que não há porque mudar, a não ser
                      //que encontre algo que funcione mais rápido. Tem coisa que seja mais rápido que um foreach de uma linha?
                      for i = 0 to quantidade - 1 do
                          let nome =  if i < nomes.Length  then nomes.[i] else $"Conjunto{i+1}"

                          mapa.Add(nome, conjunto.[i])

                          printfn $"Adicione os elementos de seu conjunto {nome}, separados por espaços ou ponto e vírgula ou dois pontos: "
                          
                          printf "%s = { " nome
                          
                          entrada <- Console.ReadLine() 
                          
                          printf " }"
                      
                          
                          let valoresString = string entrada
                          
                          let separadores = [| ' '; ';'; ':' |]
                          
                          let valoresSeparados = valoresString.Split(separadores, StringSplitOptions.RemoveEmptyEntries)
                          
                          let valoresDouble = Array.map (double) valoresSeparados
                          
                          conjunto.[i] <- HashSet<double>(valoresDouble)
            
                          mapa.Add(nome, conjunto.[i])
                          
                      printfn"Seus conjuntos são: "

                      let EscrevaOconjunto Cj =

                          let agrupeSeqElementos = conjunto|> Seq.map string|> String.concat ", "
                          
                          printfn$"{Cj} = {{{agrupeSeqElementos}}}"

                      printfn ""
        
                      for i = 0 to quantidade - 1 do

                      EscrevaOconjunto mapa.[nomes.[i]]


                      
                      
                      //Tentando encontrar a melhor forma de trabalhar com conjuntos numéricos em F#:


                      let Ni = Seq.initInfinite id (*Se N,R,Z,Q são infinitos então a melhor maneira de trabalhar com eles em programação, deve ser usando
                                                    Seq.initInfinite*)
                      let primeiros2mil = Ni |> Seq.take 2000|> Seq.map string |> String.concat ", "

                      let escrevaN = printfn $"N = {{{primeiros2mil}}}"

                      let NaturaisCSV =
                          Ni
                          |> Seq.take 10000
                          |> Seq.map string
                          |> String.concat ", "
    
                      File.WriteAllText("naturais.csv", NaturaisCSV)
    
                      let N = seq {1..2..1000000000}
                      let Npar = seq {2..4..1000000000}
                      let Nímpar = seq {1..3..999999999}

                      //Parei aqui: Tem coisa para fazer daqui para a frente!
                      let união =  Seq.append Nímpar Npar |> Seq.distinct
                      let AmB = N |> Seq.except Npar
                      let AInterB = N|> Seq.filter (fun x -> Seq.contains x Npar)
                      let númerosbons = seq {1,2,3,5,7,9,11,14,20,22,30,35,42,52,77,100}


                      (*
                        Agora, como essas funções se comportam com um grande volume de dados?
                        Vamos transformar uma planilha Excel(Com mil ou 2mil valores) em uma matriz
                       (para depois a convertermos em um conjunto HashSet):
                        open ClosedXML.Excel
                      *)
                      //#r "nuget: ClosedXML"

                      //Função que converte uma planilha Excel (passando seu caminho como parâmetro) em uma matriz 2D do tipo double
                      let PlanilhaParaMatriz (caminho: string) : double[,] =
                         if not (File.Exists(caminho)) then
                             failwithf "Arquivo não encontrado: %s" caminho
                         let planilha = new XLWorkbook(caminho)
                         let aba = planilha.Worksheet(1)

                         let ultimaLinha = aba.LastRowUsed().RowNumber()
                         let ultimaColuna = aba.LastColumnUsed().ColumnNumber()

                         Array2D.init ultimaLinha ultimaColuna (fun linha coluna ->
                             aba.Cell(linha + 1, coluna + 1).GetValue<double>()
                         )

                    // Definindo os caminhos das planilhas Excel
                      let caminhoN = Path.Combine(__SOURCE_DIRECTORY__, "naturais.xlsx")
                      let caminhoP = Path.Combine(__SOURCE_DIRECTORY__, "pares.xlsx")

                      let matriznaturais = PlanilhaParaMatriz caminhoN
                      let matrizpares = PlanilhaParaMatriz caminhoP

                      printfn "Caminho usado: %s" caminhoN
                      

                      //Função que converte uma matriz 2D em um conjunto HashSet
                      let matrizParaHashSet (matriz: double[,]) : HashSet<double> =
                          let linhas = Array2D.length1 matriz
                          let colunas = Array2D.length2 matriz

                          let elementos =
                              seq {
                                  for i in 0 .. linhas - 1 do
                                      for j in 0 .. colunas - 1 do
                                          yield matriz.[i, j]
                              }

                          HashSet<double>(elementos)


                      let HSN = matrizParaHashSet matriznaturais
                      let HSP = matrizParaHashSet matrizpares
                      

                      let NUP = AUB HSN HSP
                      printfn $"A união do conjunto dos naturais até mil com o conjunto dos números pares até dois mil é: {EscrevaOconjunto NUP}"
                      printfn ""
                      let NIP = AIB HSN HSP
                      printfn $"A interseção do conjunto dos naturais até mil, com o conjunto dos números pares até 2000 é: {EscrevaOconjunto NIP}"
                      printfn ""
                      let NdifP = AdifB HSN HSP
                      printfn $"O conjunto dos naturais até mil, menos o conjunto dos números pares até 2000 é: {EscrevaOconjunto NdifP}"
                      printfn ""
                      let HSP_ = A_ HSP HSN
                      printfn $"O complementar dos números pares até dois mil em relação ao conjunto dos números naturais até mil é: {EscrevaOconjunto HSP_}"
                     
                     
                     //Função para converter HashSet em lista:
                      
                      let HSToList (A: HashSet<'T>) : 'T list =
                          let Tolist = A |> Seq.toList
                          Tolist

                      let meuSet = HashSet<double>([1.0; 2.0; 3.0; 4.0; 5.0])
  
                      let comoLista = HSToList meuSet       
                      
                      let minhaLista = meuSet |> Seq.toList // Função para converter Set em lista

                      //Convertendo lista em HashSet:
                      let ListToHS (lista: 'T list) : HashSet<'T> =
                          let toHS = HashSet<'T>(lista)
                          toHS

                      let listaExemplo = [1.0; 2.0; 3.0; 4.0; 5.0]
                      let A = ListToHS listaExemplo
         

                      //Função recursiva de escolha entre várias operações de conjuntos diferentes
                      let rec OperaçõesDeConjuntos()=
                          printfn "" 
                          printfn "Que operação você quer calcular com os seus conjuntos?"
                          printfn "P)Pertence ou não pertence (∈, ∉)| U)União (∪)| I)Intersecção (∩)| C)Complementar (C\u0304)| D)Diferença (-)| E) Conjunto Das Partes (Pa) | q) Sair"
                          printfn "Digite a letra inicial da operação que deseja efetuar: "
                          
                          let entrada2 : option<string> =
                              match Console.ReadLine() with
                              | null -> None
                              | valor -> Some valor

                          match entrada2 with
                          |None       ->   
                                            printfn"Entrada nula! Digite uma entrada válida!"
                                            OperaçõesDeConjuntos ()
                          |Some valor -> 

                                            match valor with                     
                                            |"Q"|"q" -> 
                                                         theEnd ()
                                                         OperaçõesDeConjuntos ()

                                            |"U"|"u" ->
                                                         let mutable união = HashSet<double>()
                                                         for i = 0 to quantidade - 2 do
                                                          união <- AUB (conjunto.[i]) (conjunto.[i+1])
                                                         printfn $"A união dos conjuntos é: {EscrevaOconjunto união}" 

                                            |"I"|"i" -> 
                                                         let mutable intersecção = HashSet<double>()
                                                         for i = 0 to quantidade - 2 do
                                                          intersecção <- AIB (conjunto.[i]) (conjunto.[i+1])
                                                         printfn $"A interseção dos conjuntos é: {EscrevaOconjunto intersecção}"

                                            |"D"|"d" -> 
                                                         let mutable diferença = HashSet<double>()
                                                         for i = 0 to quantidade - 2 do
                                                          diferença <- AdifB (conjunto.[i]) (conjunto.[i+1])
                                                         printfn $"A diferença dos conjuntos é: {EscrevaOconjunto diferença}"

                                            |"P"|"p" ->  printf "Para qual conjunto você quer testar pertinência? Digite a letra do conjunto: "
                                                         entrada <- Console.ReadLine ()
                                                         let letraDoconjunto = entrada

                                                         printf "Agora, qual elemento você quer saber se pertence a %s?: " letraDoconjunto
                                                         entrada <- Console.ReadLine ()
                     
                                                         let elemento = (double)entrada
                                                         let conjuntoFormatado =  mapa.[letraDoconjunto] |> Seq.toList |> List.map string |> String.concat ", "
                     
                                                         if mapa.ContainsKey(letraDoconjunto) then
                                          
                                                          if apA elemento mapa.[letraDoconjunto] then
                                                              printfn $"O elemento {elemento} pertence a {letraDoconjunto} de fato, pois {letraDoconjunto} = {{{conjuntoFormatado}}}"       
                                                          else
                                                             printfn $"O elemento {elemento} NÃO pertence a {letraDoconjunto}, pois {letraDoconjunto} = {{{conjuntoFormatado}}}"
                                                         else
                                                          printfn $"O conjunto {conjunto} não existe no mapa"

                          

                                            |_       ->  printfn "Entrada inválida. Tente novamente." 
                                                         OperaçõesDeConjuntos ()

                          
                          let rec outraOp () =
                              printfn "Deseja fazer outra operação com os mesmos conjuntos?(S/N)"
                              let entrada3 : option<string> =
                                  match Console.ReadLine() with
                                  | null -> None
                                  | valor -> Some valor

                              match entrada3 with
                              |None   -> failwith "Entrada nula! Digite uma entrada válida!"
                                         outraOp ()
                              |Some valor -> 

                                             match valor.Trim().ToUpper() with
                                             |"Q" ->   theEnd()
                                                       OperaçõesDeConjuntos ()
                                             |"N" ->   printfn "Ok"
                                             |"S" ->   OperaçõesDeConjuntos ()
                                             |_   ->   printfn"Entrada inesperada! Digite novamente: "
                                                       outraOp ()
                          printfn""

                      printfn ""

        //Permitir o usuário fazer, por exemplo,( A U B inter C) dif D //Vou fazer uma calculadora de conjuntos a parte
        //Permitir ao usuário passar conjuntos de forma sequencial através do Console, ex: {1..10..100}
        //Repassar para o usuário todo o poder do F#
              
                     
    | "Multiplicação"        ->  printfn "Você escolheu Multiplicação!" //Copiar e colar o código da Adição acima. Trocar apenas a operação
                                 printf "Digite os valores a serem multiplicados com espaços entre eles: "

                                 entrada <- Console.ReadLine ()

                                 let StringEntrada = string entrada
                                 let separadores = [| ' '; ';'; ':' |]
                                 let StringArrayValSeparados = StringEntrada.Split(separadores, StringSplitOptions.RemoveEmptyEntries)
                                 let EntradaParaFloatArray = Array.map (float) StringArrayValSeparados
                                 let produto = arrayProduto EntradaParaFloatArray
                                 let produtoString = (string) produto
                                 let textoConcatenado = String.concat ", " StringArrayValSeparados // Embora Split forme um array com separadores, para imprimirmos no Console necessitamos concatenar com ,
                  
                                 printf $"O(s) valor(es) "   
                                 printfn "%s" textoConcatenado
                                 printfn "Gera(m) o total de: %s" produtoString
              
                                 maisUmaoperação ComputandoOperações operação

    | "Divisão"              -> printfn "Você escolheu \"Divisão\"!"
                                printf "Digite o valor 1: "

                                entrada <- Console.ReadLine()

                                let valor1 = decimal entrada
        
                                printf "Digite o valor 2: "

                                entrada <- Console.ReadLine()

                                let valor2 = decimal entrada

                                let divisão = valor1/valor2

                                printfn $"O valor {valor1}, dividido pelo valor {valor2} é: |{divisão}|" 

                                printfn ""

    | "Exponenciação"        -> printfn "Você escolheu \"Exponenciação\"!"
                                printf "Digite a base da potência: "

                                entrada <- Console.ReadLine()

                                printfn ""

                                let valor1 = int entrada

                                printf "Agora digite o expoente: "

                                entrada <- Console.ReadLine()

                                printfn ""

                                let valor2 = int entrada

                                let potência = Pow valor1 valor2

                                printfn $"A base {valor1}, elevada à potência de valor {valor2} é: |{potência}|"
        
                                printfn ""

    | "Raiz quadrada"        -> printfn"Você escolheu |Raiz quadrada|!"

                                printf "Digite o número: "

                                entrada <- Console.ReadLine()

                                let valor = float entrada
       
                                let resultado = raiz valor

                                printfn $"A raiz quadrada do valor {valor}, é: |{resultado}|"
        
                                printfn ""

    | "Fatorial"             -> printfn"Você escolheu \"Fatorial\"!"

                                printf "Digite o número inteiro!: "

                                entrada <- Console.ReadLine()

                                let valor = int entrada
        
                                let rec fatorial n =
                                    if n <= 1 then 
                                        1
                                    else
                                        n * fatorial (n-1)

                                let resultado = fatorial valor

                                printfn $"O fatorial de {valor} ({valor}!), é: {resultado}"
        
                                printfn ""    

    | "Fibonacci"            -> printfn"Você escolheu |Fibonacci|!"

                                let valor = int entrada

                                let  valorAnterior = valor - 1

                               // Função recursiva para calcular Fibonacci
                                let rec fib x =
                                    match x with
                                    | 1 -> 1
                                    | 2 -> 1
                                    | x -> fib (x - 1) + fib (x - 2)

                                let resultado = fib valor

                                let resultadoAnterior = fib valorAnterior

                                let resultadoDouble = double resultado

                                let resultadoAnteriorDouble = double resultadoAnterior

                                let phiAproximado =  (resultadoDouble/resultadoAnteriorDouble)

                                let phi = 1.61803398874989484820

                                printfn $"O {valor}º termo da sequência de Fibonacci é o número {resultado}"
        
                                printfn ""

                                printfn $"A razão áurea aproximada para esse termo é: {phiAproximado}"

                                printfn $"O valor real de phi é de {phi}" 

                                printfn "O desvio entre o valor real e o valor aproximado de phi é de: %f" (phi - phiAproximado)

    |_                       -> printfn"Entrada inválida. Tente novamente."
                                ComputandoOperações operação
    printfn""

[<EntryPoint>]
    let main argv =
        Início ()
        EscolhendoOperações ()
        ComputandoOperações operação
        0 // código de saída


  





     




    

    

    





