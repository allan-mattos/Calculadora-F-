
namespace MathLibrary 

open System
open System.IO
open System.Collections.Generic
open ClosedXML.Excel

open Conjuntos
open Functions  
    
[<AutoOpen>]
module ExeFunctions =
        
    let rec ComputandoOperação operação = 
        
        match operação with
                        
        |"Adição" ->    
                        Console.Clear()
                        let inline Adição () : string =
                            let texto = "-------==========//////////////////////ADIÇÃO\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\==========-------"
  
                            ImprimirCentralizado texto |> ignore
  
                            Console.WriteLine()

                            ImprimirCentralizado "+++Pressione qualquer tecla para continuar!+++" |> ignore

                            Console.WriteLine()

                            Console.ReadKey(true) |>ignore
                            " "

                        Adição() |> ignore

                        Console.WriteLine()
                        printfn"\t Digite os valores a serem somados, com espaços entre eles: (espaços ou \",\" ou \";\" ou \":\")"
                        printfn"\t (Para números com casas decimais, utilize o ponto: \".\".Ex: 3.14 1.67 = 3,14 + 1,67 !)"

                        Console.WriteLine()       

                        printf"\t "

                        let entrada = LendoEntrada ()
     
                        let separadores = [| ' ' ; ',' ; ';'; ':' |]
                        let StringArrayValSeparados = entrada.Split(separadores, StringSplitOptions.RemoveEmptyEntries) 

                        let EntradaParaDoubleArray =
                             StringArrayValSeparados
                             |> Array.choose ``ÉDouble?``

                     
                        match EntradaParaDoubleArray with
                        | [||] -> 
                                                printfn "Nenhum valor válido foi digitado! Digite apenas números!"
                                                ComputandoOperação operação
                                 

                        | [| unicoValor |] -> 
                                                printfn "R: %f" unicoValor |> ignore

                        | valores -> 
                                                let ValoresParaString = valores |> Array.map string
            
                                                let arraySoma arr = Array.fold (fun acc x -> acc + x) 0.0 arr
                                                let soma = arraySoma valores
                                                let MostrandoAdiçãoDeValoresNaTela = String.concat " + " ValoresParaString
                                                 
                                                Console.WriteLine()

                                                printf "\t %s" MostrandoAdiçãoDeValoresNaTela
                                                printfn " = %f" soma |> ignore
                        
        |"Conjuntos"->    
                        let texto = @"-------//////////////////////ADIÇÃO\\\\\\\\\\\\\\\\\\\\\\-------"

                        ImprimirCentralizado texto |> ignore

                        printfn"Você escolheu Operações De Conjuntos!"
                        
                        let rec pedirNumero  () : int =
                            printfn "Com quantos conjuntos você quer trabalhar?"
                            let entrada = LendoEntrada ()
                            let qtd = PxLinhaSóSeInteiro entrada
                            qtd
                            

                        printfn""
                      
                        let quantidade = pedirNumero()
                        printfn "Quantidade escolhida: %d" quantidade

                            //variável importantíssima, porém esquecida: mutable conjunto!
                        let mutable conjuntos : HashSet<double>[] = Array.init quantidade (fun _-> HashSet<double>())
                        let mutable sets : Set<double>[] = Array.init quantidade (fun _-> Set.empty)

                        let nomes = [|"A";"B";"C";"D";"E";"F";"G";"H";"I";"J";"K";"L";"M";"N";"O";"P";"Q";"R";"S";"T";"U";"V";"W";"X";"Y";"Z"|]
                                  //[| 0 ; 1 ; 2 ; 3 ; 4 ; 5 ; 6 ; 7 ; 8 ; 9 ;10 ;11 ;12 ;13 ;14 ;15 ;16 ;17 ;18 ;19 ;20 ;21 ;22 ;23 ;24 ;25 |]

                        let mapa1 = Dictionary<string, HashSet<double>>()
                        let mapa2 = Dictionary<string, Set<double>>()
                       
                        let rec loopConjuntos (i : int) : unit =

                            match i <= (quantidade - 1) with
                            | true  ->
                            
                                let nome =  if i < nomes.Length  then nomes.[i] else $"Conjunto{i+1}"
 
                                mapa1.Add(nome, conjuntos.[i])
                                mapa2.Add(nome, sets.[i])

                                let rec pedirConjunto nome =
                                    let text = $"Digite os elementos do seu conjunto {nome}, separados por espaços ou ';' ou ':'\n
                                    {nome} = {{ "    
                                    
                                    let entrada = LendoEntrada ()
                                                          
                                    Console.WriteLine()
                                  
                                    let separadores = [| ' ' ; ',' ; ';'; ':' |]
                                    let StringArrayValSeparados = entrada.Split(separadores, StringSplitOptions.RemoveEmptyEntries)

                                    let EntradaParaDoubleArray =
                                        StringArrayValSeparados
                                        |> Array.choose ``ÉDouble?``

                                     
                                    match EntradaParaDoubleArray with
                                    | [||] -> 
                                             printfn "Nenhum valor válido foi digitado! Digite apenas números!"
                                             pedirConjunto nome
                                 

                                    | [| unicoValor |] -> conjuntos.[i].Clear()
                                                          conjuntos.[i].Add unicoValor |> ignore
                                                          sets.[i] <- Set.singleton unicoValor
                                                         
                                                            

                                    | valores ->      let set = valores |> Set.ofArray
                                                      sets.[i] <- set
                                                      
                                                      conjuntos.[i].Clear()
                                                      conjuntos.[i].UnionWith(set)
                                    
                                pedirConjunto nome 
                                
                                printf "Agora "
                                
                                loopConjuntos (i + 1)    
                             | false ->
                               printfn "TODOS os conjuntos foram definidos!"
                        printfn""  
                        
                        loopConjuntos 0

                        printfn "Seus conjuntos são: "    
                        
                        let loopEscreveConjuntos (i : int) : unit =
                            match i <= (quantidade - 1) with
                            |true  ->
                                       EscrevaOconjunto nomes.[i] mapa1.[nomes.[i]]

                            |false ->  printfn "Fim da listagem de conjuntos!"

                        loopEscreveConjuntos  0

                                  //Tentando encontrar a melhor forma de trabalhar com conjuntos numéricos em F#:

                        let N = Seq.initInfinite id (*Se N,R,Z,Q são infinitos então a melhor maneira de trabalhar com eles em programação, deve ser usando
                                                                Seq.initInfinite*)
                        let Doismil = N |> Seq.take 2000|> Seq.map string |> String.concat ", "
                               

                        let DoismilHSet: HashSet<double> = A (N |> Seq.take 2000 |> Seq.toList)
                        printfn "A seguir temos os primeiros dois mil números do conjunto N: "
                        EscrevaOconjunto nomes.[12] DoismilHSet

                        let NaturaisCSV =
                            N
                            |>Seq.take 10000
                            |> Seq.map string
                            |> String.concat ", "
                        File.WriteAllText("naturais.csv", NaturaisCSV)
    
                        let NE9 = seq {1..2..1000000000}
                        let Npar = seq {2..4..1000000000}
                        let Nímpar = seq {1..3..999999999}

                        //Parei aqui: Tem coisa para fazer daqui para a frente!
                        let união =  Seq.append Nímpar Npar |> Seq.distinct
                        let AmB = N |> Seq.except Npar
                        let AInterB = N|> Seq.filter (fun x -> Seq.contains x Npar)
                        let númerosbons = seq {1,2,3,5,7,9,11,14,20,22,25,31,32,35,41,42,43,49,51,52,53,61,71,72,77,84,96,100}


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
                                printfn "Arquivo não encontrado: %s" caminho
                            let planilha = new XLWorkbook(caminho)
                            let aba = planilha.Worksheet(1)

                            let ultimaLinha = aba.LastRowUsed().RowNumber()
                            let ultimaColuna = aba.LastColumnUsed().ColumnNumber()

                            Array2D.init ultimaLinha ultimaColuna (fun linha coluna ->
                            aba.Cell(linha + 1, coluna + 1).GetValue<double>()
                            )
                        printfn""

                        // Definindo os caminhos das planilhas Excel
                        let caminhoN = Path.Combine(__SOURCE_DIRECTORY__, "naturais.xlsx")
                        let caminhoP = Path.Combine(__SOURCE_DIRECTORY__, "pares.xlsx")

                        let matriznaturais = PlanilhaParaMatriz caminhoN
                        let matrizpares = PlanilhaParaMatriz caminhoP

                        printfn "Caminho usado: %s" caminhoN
                      
                        //Função que converte uma matriz 2D em um conjunto HashSet:
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
                      
                        let HSN  = // => HSN vai até 1000
                            matrizParaHashSet matriznaturais
                            |>Seq.take 1000
                            |>Seq.toList
                            |>HashSet
                      
                        let HSP = // => HSP vai até 2000
                            matrizParaHashSet matrizpares
                            |>Seq.take 1000
                            |>Seq.toList
                            |>HashSet

                          // ∪  ∩  ∈  ∉  ∅  ≠  ≡  ∀  ∃

                          //Calculando Naturais até 1000 União pares até 2000:
                        let NUP = U HSN HSP
                        printfn $"A união do conjunto dos naturais até mil com o conjunto dos números pares até dois mil é:"
                        EscrevaOconjunto nomes.[20] NUP
                        printfn ""

                        //Calculando Naturais até 1000 Interseção Pares até 2000:
                        let ``N ∩ P`` = Y HSN HSP // r = {0,2,4,6,...1000}
                        printfn $"A interseção do conjunto dos naturais até mil, com o conjunto dos números pares até 2000 é:" 
                        EscrevaOconjunto nomes.[8] ``N ∩ P``
                        printfn ""

                            //Calculando Naturais até 1000 Diferença Pares até 2000:
                        let ``N - P`` = M HSN HSP // r = {1,3,5,...,999}
                        printfn $"O conjunto dos naturais até mil, menos o conjunto dos números pares até 2000 é:"
                        EscrevaOconjunto nomes.[3] ``N - P``
                        printfn ""

                        
                        let N_ = C HSN HSP // r = {1002,1004,...,2000}
                        printfn $"O complementar dos números naturais até mil com relação ao conjunto dos pares até dois mil"
                        EscrevaOconjunto nomes.[2] N_
                     
                     
                        //Função para converter HashSet em lista:
                      
                        let HSToList (A: HashSet<'T>) : 'T list =
                            let Tolist = A |> Seq.toList
                            Tolist

                        let meuSet = HashSet<double>([1.0; 2.0; 3.0; 4.0; 5.0])
  
                        let comoLista = HSToList meuSet       
                      
                        let minhaLista = meuSet |> Seq.toList // Função para converter Set em lista

                        //Convertendo lista em HashSet:
                              
  
                        let listaExemplo = [1.0; 2.0; 3.0; 4.0; 5.0]
                        let hashSet = A listaExemplo
         
                    //Função recursiva de escolha entre várias operações de conjuntos diferentes
                        let rec ComputandoOperaçõesDeConjuntos ()  =
                            printfn "" 
                            printfn "Que operação você quer calcular com os seus conjuntos?"
                            printfn "P)Pertence ou não pertence (∈, ∉)| U)União (∪)| I)Intersecção (∩)| C)Complementar (C\u0304)| D)Diferença (-)| E) Conjunto Das Partes (Pa) | q) Sair"
                            printfn "Digite a letra inicial da operação que deseja efetuar: "
                          
                            let entrada2 : option<string> =
                                match Console.ReadLine() with
                                |null -> None
                                |valor -> Some valor

                                     
                            match entrada2 with
                            |None     ->   
                            printfn"Entrada nula! Digite uma entrada válida!"
                            ComputandoOperaçõesDeConjuntos ()

                            |Some valor -> 

                                match valor with               
                                |"Q"|"q" -> 
                                            theEnd ()
                                            ComputandoOperaçõesDeConjuntos ()

                                |"U"|"u" ->   //Obras: Lembrando que união é A com B
                                            //Enquanto Uni é A com B com C com D com E...até o infinito
                                            //Logo:

                                            let rec UniãoDeConjuntos () =

                                                printfn "Você escolheu União De Conjuntos!"
                                                printfn""

                                                let texto = "-------===========================|União De Conjuntos|========================-------"

                                                let imprimirCentralizado (text:string) =
                                                    let larguraConsole = Console.WindowWidth
                                                    let posicaoInicial = (larguraConsole - text.Length) / 2
                                                    Console.WriteLine(String(' ', posicaoInicial) + text)
                                                printfn ""

                                                imprimirCentralizado texto

                                                Console.WriteLine()

                                                printfn"Digite que tipo de União você quer calcular: "
                                                printfn"2) União de dois conjuntos (A ∪ B)"
                                                printfn"3) União de três conjuntos (A ∪ B ∪ C)"
                                                printfn"4) União de n conjuntos (A ∪ B ∪ C ∪ D ∪ E...)"
                                                printfn"5)Conjunto Univero (Uni): União de todos os conjuntos listados!"
                                                printfn""

                                                let inp: option<string> =
                                                    match Console.ReadLine() with
                                                    |null -> None
                                                    |valor -> Some valor
                                                match inp with
                                                |None     ->   
                                                                printfn"Entrada nula! Digite uma entrada válida!"
                                                                UniãoDeConjuntos ()
                                                |Some valor -> 
                                                    match valor with
                                                    |"2" -> let rec UniãoDeDoisConjuntos () =
                                                                printfn "Você escolheu União de dois conjuntos (A ∪ B)!"
                                                                printf "Digite a letra do primeiro conjunto (já definido anteriormente!): "

                                                                let mutable cjA = HashSet<double>()
                                                                
                                                                let  entradaA : option<string> = 
                                                                    match Console.ReadLine() with
                                                                    |null -> None
                                                                    |valor -> Some (valor.Trim().ToUpper())

                                                                match entradaA with
                                                                |None -> printfn"Entrada nula! Digite uma entrada válida!"
                                                                         UniãoDeDoisConjuntos ()    
                                                                |Some valor -> 
                                                                               if mapa1.ContainsKey(valor) then
                                                                                   cjA <- mapa1.[valor]
                                                                                   printfn $"{valor} foi o primeiro dos dois conjuntos escolhidos dentre os conjuntos do mapa."
                                                                                   Console.WriteLine()
                                                                               else
                                                                                   printfn $"O conjunto {valor} não existe no mapa! Tente novamente."
                                                                                   UniãoDeDoisConjuntos ()  

                                                                let mutable cjB = HashSet<double>()
                                                                let mutable entradaB : option<string> = None
                                                                printf"AGORA "

                                                                let rec LeiaoConjuntoB () =
                                                                    printfn"Digite a letra do segundo conjunto (já definido anteriormente!): "
                                                                    entradaB <-
                                                                        match Console.ReadLine() with
                                                                        |null -> None
                                                                        |valor -> Some (valor.Trim().ToUpper())

                                                                    match entradaB with
                                                                    |None -> printfn"Entrada nula! Digite uma entrada válida!"
                                                                             LeiaoConjuntoB ()

                                                                    |Some valor ->   if mapa1.ContainsKey(valor) then
                                                                                         cjB <- mapa1.[valor]
                                                                                         printfn $"{valor} foi o segundo dos dois conjuntos escolhidos dentre os conjuntos do mapa."
                                                                                         Console.WriteLine()
                                                                                     else
                                                                                         printfn $"O conjunto {valor} não existe no mapa! Tente novamente."
                                                                                         LeiaoConjuntoB ()  
                                                               
                                                                let união = U cjA cjB
                                                                   
                                                                printfn$"A união dos conjuntos {entradaA} e {entradaB} é: " 
                                                                EscrevaOconjunto nomes.[20] união
                                                                printfn""
                                                            printfn""

                                                    |"3" ->   let rec UniãoDeTrêsConjuntos () =

                                                                  printfn"Você escolheu União de três conjuntos (A ∪ B ∪ C)!"
                                                                  printfn "Digite a letra do primeiro conjunto (já digitado anteriormente!): "
                                                                  let mutable cjA = HashSet<double>() 
                                                                  let mutable cjB = HashSet<double>()
                                                                  let mutable cjC = HashSet<double>()
                                                                  let mutable entradaA =  Console.ReadLine () 
                                                                  if mapa1.ContainsKey(entradaA) then
                                                                      cjA <- mapa1.[entradaA]
                                                                  else 
                                                                      printfn $"O conjunto {entradaA} não existe no mapa! Tente novamente."
                                                                      UniãoDeTrêsConjuntos ()     
                                                                      
                                                                  printfn"Agora digite a letra do segundo conjunto: "
                                                                  let mutable entradaB = Console.ReadLine ()
                                                                  while not (mapa1.ContainsKey(entradaB)) do
                                                                      printfn $"O conjunto {entradaB} não existe no mapa! Tente novamente."
                                                                      entradaB <- Console.ReadLine ()
                                                                  if mapa1.ContainsKey(entradaB) then
                                                                      cjB <- mapa1.[entradaB]
                                                                 
                                                                  printfn"Agora digite a letra do terceiro conjunto: "
                                                                  let mutable entradaC = Console.ReadLine ()
                                                                  while not (mapa1.ContainsKey(entradaC)) do
                                                                      printfn $"O conjunto {entradaC} não existe no mapa! Tente novamente."
                                                                      entradaC <- Console.ReadLine ()
                                                                  if mapa1.ContainsKey(entradaC) then
                                                                      cjB <- mapa1.[entradaC]
                                                                  let união = U (U cjA cjB)  cjC//A recursividade das pequenas coisas!
                                                                  printfn"A união dos conjuntos %s, %s e %s é: " entradaA entradaB entradaC
                                                                  EscrevaOconjunto nomes.[20] união
                                                              printfn""
                                                    |"4" ->        //União de n conjuntos: //
                                                                //let rec UniãoDeNConjuntos () =
                                                                    let te = "Você escolheu União de N Conjuntos (4 ou mais!)!\n
                                                                    *Para quais conjuntos você quer calcular ∪?\n
                                                                    Digite a letra inicial de cada um dos 4 conjuntos para os quais você quer calcular a união, separados por espaços ou vírgulas ou ponto e vírgula ou dois pontos: "
                                                                    
                                                                    PrinTex te

                                                                    let Entrada = LendoEntrada ()
                                                  
                                                                    let separadores = [| ' '; ','; ';'; ':' |]
                          
                                                                    let mutable StringArrayDeValSeparados = Entrada.Split(separadores, StringSplitOptions.RemoveEmptyEntries)
                          
                                                                    let mutable união = HashSet<double>()
                                                                    for i= 0 to StringArrayDeValSeparados.Length - 1 do
                                                                        if mapa1.ContainsKey(StringArrayDeValSeparados.[i]) then
                                                                            união <- U (união)(mapa1.[StringArrayDeValSeparados.[i]])
                                                                        else
                                                                            StringArrayDeValSeparados <- Array.removeAt i StringArrayDeValSeparados
                                                                            printfn $"O conjunto {StringArrayDeValSeparados.[i]} não existe no mapa!"

                                                                    printf "A união dos conjuntos "    
                                                                    StringArrayDeValSeparados|> Array.iter (fun X ->  printf "%s, " X )
                                                                    printf " é igual a:"
                                                                    EscrevaOconjunto nomes.[20] união 
                                                    
                                                    |"5" ->         printfn"Você escolheu o conjunto Universo (Uni), isto é, a União de Todos os Conjuntos Listados!"
                                                                    printfn"A união de todos os conjuntos listados é: "
                                                                    Uni conjuntos |> EscrevaOconjunto nomes.[20] 
                       
                                                    |_       ->  
                                                                    printfn "Entrada inválida. Tente novamente." 
                                                                    ComputandoOperaçõesDeConjuntos ()
                                            printfn""
                    
                                |"C"|"c" -> 
                                                     let mutable complementar = HashSet<double>()
                                                     for i = 0 to quantidade - 2 do
                                                         complementar <- C (conjuntos.[i]) (conjuntos.[i+1])
                                                     printfn "O complementar dos conjuntos..." 
                                                     for i = 0 to quantidade - 1 do   
                                                         EscrevaOconjunto nomes.[i] mapa1.[nomes.[i]]
                                                     printfn"...É:"
                                                     EscrevaOconjunto nomes.[2] complementar

                                |"I"|"i" -> 
                                                     let mutable intersecção = HashSet<double>()
                                                     for i = 0 to quantidade - 2 do
                                                         intersecção <- Y (conjuntos.[i]) (conjuntos.[i+1])
                                                     printfn "A interseção dos conjuntos..." 
                                                     for i = 0 to quantidade - 1 do   
                                                         EscrevaOconjunto nomes.[i] mapa1.[nomes.[i]]
                                                     printfn"...É:"
                                                     EscrevaOconjunto nomes.[8] intersecção

                                |"D"|"d" -> 
                                                     let mutable diferença = HashSet<double>()
                                                     for i = 0 to quantidade - 2 do
                                                         diferença <- M (conjuntos.[i]) (conjuntos.[i+1])
                                                     printfn "A diferença dos conjuntos...:"
                                                     for i = 0 to quantidade - 1 do   
                                                         EscrevaOconjunto nomes.[i] mapa1.[nomes.[i]]
                                                     printfn"...É:"
                                                     EscrevaOconjunto nomes.[3]diferença
                                |"E"|"e" ->  
                                                     let rec PedirletraConjunto () =
                                                         printfn"Digite a letra do conjunto para o qual você quer calcular o conjunto das partes (Pa): "
                                                         let cj: option<string> = 
                                                             match Console.ReadLine ()with
                                                             |null ->  None
                                                             |valor -> Some (valor.Trim().ToUpper()) 
                                                         match cj with
                                                         |None -> printfn"Entrada nula! Digite uma entrada válida!"
                                                                  PedirletraConjunto ()
                                                         |Some valor ->
                                                             if mapa1.ContainsKey(valor) then
                                                                 let Par = Pa (mapa1.[valor] |> Seq.toList)
                                                                 printfn "O conjunto das partes de %s é: " (valor)
                                                                 EscrevaPa Par
                                                             else
                                                             printfn $"O conjunto {valor} não foi definido!"
                                                             printfn"Tente outra vez!"
                                                             PedirletraConjunto ()
                                                     printfn""
                                |"P"|"p" -> 
                                                     let texto1 = "Para qual conjunto você quer testar pertinência? Digite a letra do conjunto: "

                                                     PrinTex texto1

                                                     let Entrada1 = LendoEntrada ()
                                                     let letraDoconjunto = Entrada1.Trim().ToUpper() 

                                                     let texto2 =  $"Agora, qual elemento você quer saber se pertence a {letraDoconjunto}?: " 

                                                     PrinTex texto2

                                                     let Entrada2 = LendoEntrada ()
                     
                                                     let elemento = (double)Entrada2
                                                     let conjuntoFormatado =  mapa1.[letraDoconjunto] |> Seq.toList |> List.map string |> String.concat ", "
                     
                                                     if mapa1.ContainsKey(letraDoconjunto) then
                                          
                                                         if p elemento mapa1.[letraDoconjunto] then
                                                             printfn $"O elemento {elemento} pertence a {letraDoconjunto} de fato, pois {letraDoconjunto} = {{{conjuntoFormatado}}}"       
                                                             else
                                                             printfn $"O elemento {elemento} NÃO pertence a {letraDoconjunto}, pois {letraDoconjunto} = {{{conjuntoFormatado}}}"
                                                     else
                                                         printfn $"O conjunto {letraDoconjunto} não foi definido!"

                          

                                |_       ->  
                                                     printfn "Entrada inválida. Tente novamente." 
                                                     ComputandoOperaçõesDeConjuntos ()

                             
                             
                            let rec  OutraOp()= //Tradução Compute outra operação de conjunto. Caso contrário, volte para ComputandoOperação operação (conjuntos). Ou então escolha a nova operação 
                                let textil = "Se deseja fazer outras operações com os mesmos conjuntos, digite a letra 'O'.\n
                                Se deseja digitar novos conjuntos, digite a letra 'C'.\n
                                Se deseja escolher outros tipos de operações (soma, multiplicação,divisão,etc), digite a letra 'E'.\n
                                Para reiniciar a aplicação do início, digite a letra 'I'.\n
                                Ou então digite 'Q' para sair: "

                                PrinTex textil

                                let entrada = LendoEntrada ()
       
                                match entrada with
                                |"Q"|"q"    ->   theEnd()
                                                 EscolhendoOperações ()|>ignore
                         

                                |"O"|"o"    ->   printfn "Você escolheu calcular outra(s) operação(ões) com os mesmos conjuntos!"
                                                 ComputandoOperaçõesDeConjuntos ()
                                                 
                         

                                |"C"|"c"    ->   ComputandoOperação operação 
                                                 

                                |"E"|"e"    ->   EscolhendoOperações ()|>ignore
                         

                                |"I"|"i"    ->   Início()|>ignore
                         
                     
                                |_          ->   printfn"Entrada inesperada! Digite uma das letras a seguir: "
                                                 OutraOp ()
                            
                            printfn""

                            OutraOp()
                        printfn""

                //Permitir o usuário fazer, por exemplo,( A U B inter C) dif D //Vou fazer uma calculadora de conjuntos a parte
                //Permitir ao usuário passar conjuntos de forma sequencial através do Console, ex: {1..10..100}
                //Repassar para o usuário todo o poder do F#
              
                      
        |"Multiplicação"        ->                    
                                            Console.WriteLine()

                                            let Doistextos =  "Você escolheu Multiplicação!\n 
                                                              Digite os valores a serem multiplicados com espaços entre eles: "
                                            PrinTex Doistextos
                                            let Entrada = LendoEntrada ()

                                        
                                            let separadores = [| ' '; ';'; ':' |]
                                            let StringArrayValSeparados = Entrada.Split(separadores, StringSplitOptions.RemoveEmptyEntries)
                                            let EntradaParaFloatArray = Array.map (float) StringArrayValSeparados
                                            let inline arrayProduto array = Array.fold (fun acc x -> acc * x) 1.0 array
                                            let produto = arrayProduto EntradaParaFloatArray
                                            let produtoString = (string) produto
                                            let textoConcatenado = String.concat ", " StringArrayValSeparados // Embora Split forme um array com separadores, para imprimirmos no Console necessitamos concatenar com ,
                  
                                            printf $"O(s) valor(es) "   
                                            printfn "%s" textoConcatenado
                                            printfn "Gera(m) o total de: %s" produtoString
                   

        |"Divisão"              ->  
                                            Console.WriteLine()

                                            let texto1 = "Você escolheu \"Divisão\"!\n
                                                        Digite o primeiro valor: "

                                            PrinTex texto1
                                            let entrada1 = LendoEntrada ()

                                            let valor1 = PxLinhaSóSeDouble entrada1
        
                                            let texto2 = "Digite o segundo valor: "

                                            PrinTex texto2

                                            let entrada2 = LendoEntrada ()

                                            let valor2 = PxLinhaSóSeDouble entrada2

                                            let divisão = valor1/valor2

                                            printfn $"O valor {valor1}, dividido pelo valor {valor2} é: |{divisão}|" 

                                            printfn ""

                            
        |"Exponenciação"        ->          Console.WriteLine()

                                            let tex1 = "Você escolheu \"Exponenciação\"!\n
                                                      Digite a base da potência: "

                                            PrinTex tex1

                                            let Entrada1 = LendoEntrada ()          

                                            let Int1 = PxLinhaSóSeInteiro Entrada1

                                            let tex2 = "Agora digite o expoente: "

                                            PrinTex tex2

                                            let Entrada2 = LendoEntrada ()
                                            printfn ""

                                            let Int2 = PxLinhaSóSeInteiro Entrada2
                                            
                                            let potência = Pow Int1 Int2

                                            printfn $"A base {Int1}, elevada à potência de valor {Int2} é: |{potência}|"
        
                                            printfn ""                    


        |"Raiz quadrada"        ->          Console.WriteLine()
                                
                                            let texto = "Você escolheu |Raiz quadrada|!\n

                                                        Digite o número: "

                                            PrinTex texto

                                            let Entrada = LendoEntrada ()

                                            let X = PxLinhaSóSeDouble Entrada
                                            let RaízDeX = raiz X

                                            printfn $"A raiz quadrada do valor {X}, é: |{RaízDeX}|"
        
                                            printfn ""

        |"Fatorial"             ->          Console.WriteLine()
                                
                                            let texto = "Você escolheu \"Fatorial\"!\n

                                                         Digite o número inteiro!: "

                                            let Entrada = LendoEntrada ()

                                            let valor = PxLinhaSóSeInteiro Entrada
        
                                            let rec fatorial n =
                                                if n <= 1 then 
                                                    1
                                                else
                                                    n * fatorial (n-1)

                                            let resultado = fatorial valor

                                            printfn $"O fatorial de {valor} ({valor}!), é: {resultado}"
        
                                            printfn ""    

                                   

        |"Fibonacci"            ->          Console.WriteLine()
           
                                            let texto = "Você escolheu |Fibonacci|!"
                                            PrinTex texto
                                            let Entrada = LendoEntrada ()
                                            let valor = PxLinhaSóSeInteiro Entrada

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

                                  

        |_                    ->            printfn"Operação não reconhecida. Por favor, escolha uma operação válida!"
                                            ComputandoOperação operação    
                                            printfn""

       
       
        printfn""
            