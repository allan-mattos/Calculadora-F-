// Minha primeira calculadora em F# com 14 operações diferentes
//Autor: Allan Mattos
//Data: 21/09/2025 (Comecei em janeiro de 2025 mas só fiz push agora em setembro)
//Estamos em obras
//Transformando todos os loops em recursividade
//Amém!

//Após uma alteração, não esqueça de dar os dois comandos:
//1) git pull origin master
//2) git push origin master 

open System
open System.IO
open System.Collections.Generic
open ClosedXML.Excel

Console.WriteLine("CALCULADORA")

printfn ""

printfn "-------(Pressione \"q\" para sair!)-------"

//Função que trata entradas de inteiros incorretas
let tryParseInt (input: string) =

    match Int32.TryParse(input) with
    | (true, value) -> Some value
    | (false, _) -> None

let mutable entrada = ""

let mutable operação = ""

let mutable quantidade : int = 0

let rec EscolhendoOperações () =

    printfn ""
    printfn "Com qual operação você quer trabalhar?:  "
    printfn "C) Conjuntos | A) Adição de grandes volumes | S) Subtração | M) Multiplicação de grandes volumes | D) Divisão | E) Exponenciação |
    \nR) Raiz quadrada | !) Fatorial! | F) Fibonacci | q) Sair: "

    let entrada1: option<string> =
        match Console.ReadLine() with
        | null -> None
        | valor -> Some valor
     
    match entrada1 with
    | None ->       failwith "Digite uma entrada válida!:"
                    EscolhendoOperações ()

    | Some valor ->
                    match valor with
                    | "Q" | "q" -> Environment.Exit(0)
                    | "C" | "c" -> operação <- "Conjuntos"
                    | "A" | "a" -> operação <- "Adição"
                    | "S" | "s" -> operação <- "Subtração"
                    | "M" | "m" -> operação <- "Multiplicação"
                    | "D" | "d" -> operação <- "Divisão"
                    | "E" | "e" -> operação <- "Exponenciação"
                    | "R" | "r" -> operação <- "Raiz quadrada"
                    | "!" -> operação <- "Fatorial"
                    | "F" | "f" -> operação <- "Fibonacci"
                    | _ -> failwith "Oooops, input inesperado.. Digite uma inicial de operação válida!"
                           EscolhendoOperações ()


let rec ComputandoOperações operação =
    
    match operação with
    | "Conjuntos" ->  printfn"Você escolheu Conjuntos!"
                      
                      printf "Com quantos conjuntos você quer trabalhar?: "

                      

                      printfn ""

                    (*
                      ->Sempre buscar funções puras e evitar efeitos colaterais

                      ->Minimizar a colateralidade separando funções puras da
                        entrada do usuário e da saída para o console

                      ->Assim fica mais fácil identificar bugs e testar o código
                        "Se ocorrer um erro, o problema quase nunca está
                        na função pura"   
                    *) 
                        //Função recursiva que pede um número até que o usuário digite um valor válido
                      let rec pedirNúmero () =
                          let input = Console.ReadLine()
                          match tryParseInt input with       //Tratando entrada incorreta funcional:
                          | Some value -> quantidade <-value
                          | None -> printf "Por favor, digite um número válido: "
                                    pedirNúmero ()
                      printfn""
                      
                     (*Tratando entrada incorreta imperativa e Csharpiana
                      while not (double.TryParse(entrada, &quantidade)) do 

                          Console.WriteLine("Por favor, digite um Número!: ")
     
                          entrada <- Console.ReadLine()

                          Console.WriteLine()
                      *)

                      let mutable conjunto : HashSet<double>[] = Array.init quantidade (fun _-> HashSet<double>())
                      let nomes = [|"A";"B";"C";"D";"E";"F";"G";"H";"I";"J";"K";"L";"M";"N"|]
                      let mapa = Dictionary<string, HashSet<double>>()

                      for i = 0 to quantidade - 1 do
                          let nome =  if i < nomes.Length  then nomes.[i] else $"Conjunto{i+1}"
                          mapa.Add(nome, conjunto.[i])

                          printfn $"Adicione os elementos de seu conjunto {nome}, separados por espaços ou ponto e vírgula ou dois pontos: "
                          printf "%s = { " (nome)
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


                      //Definindo a função apa (pertence) que verifica se um elemento a pertence ao conjunto A 
                      let apA (a: double) (A: HashSet<double>) : bool =
                           if A.Contains (a) then
                               true
                           else
                               false

                      //Definindo a função AUB que processa a união de dois conjuntos dados como parâmetros
                      let AUB (A: HashSet<double>) (B: HashSet<double>) : HashSet<double> =
                          let união= HashSet<double>(A)
                          união.UnionWith(B)
                          união

                      //Definindo a função AIB que processa a interseção de dois conjuntos dados como parâmetros
                      let AIB (A: HashSet<double>) (B: HashSet<double>) : HashSet<double> =
                          let interseção = HashSet<double>(A)
                          interseção.IntersectWith(B)
                          interseção

                      //Definindo a função AdifB (A-B) que processa a diferença de dois conjuntos dados como parâmetros
                      let AdifB (A: HashSet<double>) (B: HashSet<double>) : HashSet<double> =
                          let diferença = HashSet<double>(A)
                          diferença.ExceptWith(B)
                          diferença

                      //Definindo a função A_, que processa o complementar de um conjunto A em relação ao universo Uni
                      let A_ (A: HashSet<double>) (Uni: HashSet<double>) : HashSet<double> =
                          let Acomplementar = HashSet<double>(Uni)
                          Acomplementar.ExceptWith(A)
                          Acomplementar
                      
                      (*
                        Agora, como essas funções se comportam com um grande volume de dados?
                        Vamos transformar uma planilha Excel(Com mil ou 2mil valores) em uma matriz
                       (para depois a convertermos em um conjunto HashSet):
                        open ClosedXML.Excel
                      *)
                      //#r "nuget: ClosedXML"

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
                      let caminhoN = Path.Combine(__SOURCE_DIRECTORY__, "naturais.xlsx")
                      let caminhoP = Path.Combine(__SOURCE_DIRECTORY__, "pares.xlsx")

                      let matriznaturais = PlanilhaParaMatriz caminhoN
                      let matrizpares = PlanilhaParaMatriz caminhoP

                      printfn "Caminho usado: %s" caminhoN
                      


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
                      

                      let união = AUB HSN HSP
                     // printfn $"A união dos conjuntos é: {EscrevaOconjunto união}"

                      //Função para converter HashSet em lista:
                      
                      let HSToList (A: HashSet<'T>) : 'T list =
                          let Tolist = A |> Seq.toList
                          Tolist

                      let meuSet = HashSet<double>([1.0; 2.0; 3.0; 4.0; 5.0])
  
                      let comoLista = HSToList meuSet       //Temos 2 opções para converter HashSet em lista
                      
                      let minhaLista = meuSet |> Seq.toList

                      //Convertendo lista em HashSet:
                      let ListToHS (lista: 'T list) : HashSet<'T> =
                          let toHS = HashSet<'T>(lista)
                          toHS

                      let listaExemplo = [1.0; 2.0; 3.0; 4.0; 5.0]
                      let A = ListToHS listaExemplo



                      let rec P Alist =
                          match Alist with
                          | [] -> [[]] // O conjunto potência de [] é [[]] (lista com a lista vazia)
                          | head::tail ->
                              // 1. Encontra todos os subconjuntos do restante da lista (tail)
                              let subSetsOfTail = P tail 
        
                              // 2. Cria novos subconjuntos adicionando 'head' a cada um deles
                              let subSetsWithHead = 
                                  subSetsOfTail 
                                  |> List.map (fun subList -> head :: subList)
            
                              // 3. Concatena os subconjuntos sem 'head' e os subconjuntos com 'head'
                              subSetsOfTail @ subSetsWithHead

         

                      //Função recursiva de escolha entre várias operações de conjuntos diferentes
                      let rec OperaçõesDeConjuntos()=
                          printfn "" 
                          printfn "O que você quer calcular?"
                          printfn "P)Pertence| U)União| I)Intersecção| C)Complementar| D)Diferença| E) Conjunto Das Partes | q)Sair"
                          printfn "Digite a letra inicial da operação que deseja efetuar: "
                          entrada <- Console.ReadLine()

                          match entrada with
                          |null    -> failwith "Entrada nula! Digite uma entrada válida!"
                                      OperaçõesDeConjuntos ()

                          |"U"|"u" -> 
                                      for i = 0 to quantidade - 2 do
                                      let união = U (conjunto.[i]) (conjunto.[i+1])
                                      printfn $"A união dos conjuntos é: {EscrevaOconjunto união}"
                                      //Deseja fazer outra operação com os mesmos conjuntos?(s/n)
                                      //entrada <- Console.ReadLine()
                                      //match entrada with
                                      //|"S"|"s" -> OperaçõesDeConjuntos
                                      //|"N"|"n" -> printfn "Ok"

                          |"I"|"i" -> 
                                      for i = 0 to quantidade - 2 do
                                      let inter = AIB (conjunto.[i]) (conjunto.[i+1])
                                      printfn $"A interseção dos conjuntos é: {EscrevaOconjunto inter}"

                          |"D"|"d" ->                      
                                      for i = 0 to quantidade - 2 do
                                      let diferença = AdifB (conjunto.[i]) (conjunto.[i+1])
                                      printfn $"A diferença dos conjuntos é: {EscrevaOconjunto diferença}"

                          |"P"|"p" -> printf "Para qual conjunto você quer testar pertinência? Digite a letra do conjunto: "
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

                          |"Q"|"q" -> Environment.Exit(0)

                          |_       -> failwith "Entrada inválida. Tente novamente." 
                                      OperaçõesDeConjuntos ()

                      printfn ""

        //Permitir o usuário fazer, por exemplo,( A U B inter C) dif D //Vou fazer uma calculadora de conjuntos a parte
               
    | "Adição" ->     printfn"Você escolheu \"Adição\""  
                      printf "Digite os valores a serem somados com espaços entre eles: "

                      entrada <- Console.ReadLine ()

                      let valoresEntrada = string entrada
                      let separadores = [| ' '; ';'; ':' |]
        
                      let valoresSeparados = valoresEntrada.Split(separadores, StringSplitOptions.RemoveEmptyEntries)

                      let floatArray = Array.map (float) valoresSeparados
                      let soma = Array.sum floatArray

                      let valoresString = Array.map(string)floatArray

                      let somaString = (string)soma

                      let textoConcatenado = String.concat ", " valoresString
                  
                      printf $"O(s) valor(es) "
                      
                      printfn "%s" textoConcatenado

                      printfn "gera(m) o somatório de: %s" somaString 

                      printfn"Deseja efetuar mais uma adição? (s\n)"
              
                      let rec maisUmaadição () =
                          entrada <- Console.ReadLine ()
                          match entrada with
                          |"S"|"s" -> ComputandoOperações operação
                          |"N"|"n" -> printfn "Ok"
                          |"Q"|"q" -> printfn"Tem certeza que deseja sair? (S/N): "
                                      let rec theEnd () = 
                                          
                                          entrada <- Console.ReadLine()
                                          if entrada ="S" || entrada ="s" then
                                              Environment.Exit(0)
                                          elif entrada = "N" || entrada = "n" then
                                              printfn"Ok"
                                          else failwith "Entrada inesperada! Digite novamente (S/N): "
                                               theEnd ()
                                      printfn ""

                          |_ -> failwith "Entrada inesperada! Digite novamente: "
                                maisUmaadição ()

                      printfn""       
    
    | "Multiplicação" -> printfn "Você escolheu Multiplicação!" //Copiar e colar o código da Adição acima. Trocar apenas a operação

                      
    | "Divisão" -> printfn "Você escolheu \"Divisão\"!"
    | "Exponenciação" -> printfn "Você escolheu \"Exponenciação\"!"
    | "Raiz quadrada" -> printfn"VocE escolheu \"Raiz quadrada \"!"
    | "Fatorial" -> printfn"Você escolheu \"Fatorial\"!"
    | "Fibonacci" ->printfn"Você escolheu \"Fibonatti\"!"

EscolhendoOperações ()
ComputandoOperações operação
printfn ""

  

let booleano = true

printfn ""

while booleano do

    if operação = "0" then

        printf "Com quantos conjuntos você quer trabalhar?: "
        let quantidade = int (Console.ReadLine())
        
        let mutable conjunto : HashSet<double>[] = Array.init quantidade (fun _-> HashSet<double>())
        let nomes = [|"A";"B";"C";"D";"E";"F";"G";"H";"I";"J";"K";"L";"M";"N"|]
        let mapa = Dictionary<string, HashSet<double>>()
        for i = 0 to quantidade - 1 do
            let nome =  if i < nomes.Length  then nomes.[i] else $"Conjunto{i+1}"
            mapa.Add(nome, conjunto.[i])

           
            printfn $"Adicione os elementos de seu conjunto {nome}, separados por espaços ou ponto e vírgula ou dois pontos: "
            printf "%s = { " (nome)
            entrada <- Console.ReadLine() 
            printf " }"
            theEnd entrada

            let valoresString = string entrada
            let separadores = [| ' '; ';'; ':' |]
            let valoresSeparados = valoresString.Split(separadores, StringSplitOptions.RemoveEmptyEntries)

            let valoresDouble = Array.map (double) valoresSeparados
            conjunto.[i] <- HashSet<double>(valoresDouble)
            
            mapa.Add(nome, conjunto.[i])


        let U (A: HashSet<double>) (B: HashSet<double>) : HashSet<double> =
            let resultado = HashSet<double>(A)
            resultado.UnionWith(B)
            resultado


        let I (A: HashSet<double>) (B: HashSet<double>) : HashSet<double> =
            let resultado = HashSet<double>(A)
            resultado.IntersectWith(B)
            resultado

        let D (A: HashSet<double>) (B: HashSet<double>) : HashSet<double> =
            let resultado = HashSet<double>(A)
            resultado.ExceptWith(B)
            resultado

        //próximo: Pertence ou não pertence 

        let pertence (A: HashSet<double>) (n: int): bool =
            if A.Contains (n) then
                true
            else
                false

        printfn"Seus conjuntos são: "

       
        let EscrevaOconjunto conjunto =

            let agrupeSeqElementos =
                conjunto|> Seq.map string|> String.concat ", "

            printfn$"{nomes.[i]}= {{{agrupeSeqElementos conjunto}}}"
            printfn ""
        
        for i = 0 to quantidade - 1 do

            EscrevaOconjunto mapa.[nomes.[i]]


       
        let rec OperaçõesDeConjuntos =
            printfn "" 
            printfn "O que você quer calcular?: "
            printfn"P) Pertence U) União  I) Intersecção C) Complementar D) Diferença q) Sair"
            entrada <- Console.ReadLine()

            theEnd entrada

            match entrada with
                | null -> failwith "Entrada nula"
                          OperaçõesDeConjuntos// trata entrada nula
              //| "U"|"u" ->
              //   let mutable união
              //   for i = 0 to quantidade -1 do
                | s when s.Length = 1 && Char.IsLetter(s.[0]) && Char.ToUpper(s.[0]) = Char.ToUpper("U") ->
                    let mutable união: HashSet<double>= conjunto.[0]
                    for i = 0 to quantidade - 1 do
                         U união conjunto.[i+1]//Vou ter que clonar os conjuntos para não alterá-los
                | s when s.Length = 1 && Char.IsLetter(s.[0]) && Char.ToUpper(s.[0]) = Char.ToUpper("I") ->
                    let interseção: HashSet<double>= conjunto.[0]
                    for i = 0 to quantidade - 1 do
                         I interseção conjunto.[i+1]//Vou ter que clonar os conjuntos para não alterá-los
                | s when s.Length = 1 && Char.IsLetter(s.[0]) && Char.ToUpper(s.[0]) = Char.ToUpper("D") ->
                    let diferença: HashSet<double>= conjunto.[0]
                    for i = 0 to quantidade - 1 do
                         D diferença conjunto.[i+1]//Vou ter que clonar os conjuntos para não alterá-los
                | s when s.Length = 1 && Char.IsLetter(s.[0]) && Char.ToUpper(s.[0]) = Char.ToUpper("P") ->
                     printf "Para qual conjunto você quer testar pertinência? Digite a letra do conjunto: "
                     entrada <- Console.ReadLine
                     theEnd entrada
                     let letraDoconjunto = entrada
                     

                     printf "Agora, qual elemento você quer saber se pertence a %s?: " letraDoconjunto
                     entrada <- Console.ReadLine
                     theEnd entrada
                     let elemento = entrada
                     let conjunto =  mapa.[letraDoconjunto] |> Seq.toList |> List.map string |> String.concat ", "
                     

                     if mapa.ContainsKey(letraDoconjunto) then
                         pertence mapa.[letraDoconjunto] elemento
                         if pertence then
                             printfn $"O elemento {elemento} pertence a {letraDoconjunto} de fato, pois {letraDoconjunto} = {{{conjunto}}}"       
                         else
                             printfn $"O elemento {elemento} NÃO pertence a {conjunto}, pois {letraDoconjunto} = {{{conjunto}}}"
                     else
                         printfn $"O conjunto {conjunto} não existe no mapa"


                 | _ -> printfn "Entrada inválida. Tente novamente." OperaçõesDeConjuntos

                
            
                

        



        //Permitir o usuário fazer, por exemplo,( A U B inter C) dif D 

        



    if operação = "1" then

    //lendo as entradas do console e somando
        printf "Digite os valores a serem somados com espaços entre eles: "
        entrada <- Console.ReadLine ()

        theEnd entrada

        let valoresString = string entrada
        let separadores = [| ' '; ';'; ':' |]
        let valoresSeparados = valoresString.Split(separadores, StringSplitOptions.RemoveEmptyEntries)

        let floatArray = Array.map (float) valoresSeparados
        let soma = Array.sum floatArray

        printf $"O(s) valor(es) "

        for valor in floatArray do
            printf $"<{valor}>, "

        printfn $"gera(m) o somatório de: {soma} "

        printfn ""

     elif operação = "2" then

        printf "Digite o valor 1: "
        entrada <- Console.ReadLine()

        theEnd entrada

        let valor1= int entrada

        printf "Digite o valor 2: "
        entrada <- Console.ReadLine()

        theEnd entrada

        let valor2 = int entrada

        let subtração = valor1 - valor2

        printfn $"O valor {valor1}, subtraído do valor {valor2} é: |{subtração}|" 

        printfn ""

    elif operação = "3" then 

        printf "Digite o valor 1: "

        entrada <- Console.ReadLine()

        theEnd entrada

        let valor1 = int entrada
       
        printf "Digite o valor 2: "

        entrada <- Console.ReadLine()

        theEnd entrada

        let valor2 = int entrada


        let operaçãoPonto = valor1 * valor2

        printfn $"O valor {valor1}, multiplicado pelo valor {valor2} é: |{operaçãoPonto}|"

        printfn ""

    elif operação = "4" then

        printf "Digite o valor 1: "

        entrada <- Console.ReadLine()

        theEnd entrada

        let valor1 = decimal entrada
        
        printf "Digite o valor 2: "

        entrada <- Console.ReadLine()

        theEnd entrada

        let valor2 = decimal entrada

        let divisão = valor1/valor2

        printfn $"O valor {valor1}, dividido pelo valor {valor2} é: |{divisão}|" 

        printfn ""

    elif operação = "5" then //parei aqui
    
        printf "Digite a base da potência: "

        entrada <- Console.ReadLine()

        printfn ""

        theEnd entrada

        let valor1 = int entrada

        printf "Digite o expoente: "

        entrada <- Console.ReadLine()

        printfn ""

        theEnd entrada
    
        let valor2 = int entrada

        let potência = Math.Pow(valor1, valor2)

        printfn $"A base {valor1}, elevada à potência de valor {valor2} é: |{potência}|"
        
        printfn ""

    elif operação = "6" then
    
        printf "Digite o número: "
        entrada <- Console.ReadLine()

        theEnd entrada

        let valor = float entrada
        

        let raiz x = Math.Sqrt(x)

        let resultado = raiz valor

        printfn $"A raiz quadrada do valor {valor}, é: |{resultado}|"
        
        printfn ""

    elif operação = "7" then
    
        printf "Digite o número: "
        entrada <- Console.ReadLine()

        theEnd entrada

        let valor = int entrada
        

        let rec fatorial n =
            if n <= 1 then 
               1
            else
               n * fatorial (n-1)

        let resultado = fatorial valor

        printfn $"O fatorial de {valor} ({valor}!), é: {resultado}"
        
        printfn ""    

    elif operação = "8" then
     
        printf "Qual número da sequência Fibonacci você quer calcular junto com o valor aproximado de phi?: "
        entrada <- Console.ReadLine()

        printf ""

        theEnd entrada

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

        printfn ""

    printfn "Qual a próxima operação?: "
    printf "%s" Operações

    operação <- Console.ReadLine()

    theEnd operação

    printfn ""








        




        




        //E se eu quiser utilizar recursividade em vez de while?

        //vamos tentar:

        //




    

    

    





