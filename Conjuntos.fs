namespace MathLibrary 

open System.Collections.Generic
open System

[<AutoOpen>]
module Conjuntos =
   
    type Coleção<'T when 'T : comparison> = 
    |Sq of seq<'T>
    |Li of 'T list
    |CjS of Set<'T>
    |CjH of HashSet<'T>

    let inline A (cl) = // pega qualquer tipo de coleção e retorna um conjunto Set.
        match cl with
        |Sq  sq ->   sq  |> set         // Ex: let AS = A (Sq ( Seq.ofList [1;2;3]) ) ou let AS = A( S ( seq {1;2;3} )
        |Li  l  ->   l   |> set
        |CjS s  ->   s  
        |CjH h  ->   h   |> set

    let inline H (cl) = // pega qualquer tipo de coleção e retorna um conjuntoHashSet.
        match cl with
        |Sq  sq ->   sq  |> HashSet     // let A = H( S (Seq.ofList [1;2;3]))
        |Li  l  ->   l   |> HashSet     //Exemplo de uso: let Conjunto = H( Li [1;2;3] ) let A = H( S(Seq.ofList [1;2;3]))
        |CjS s  ->   s   |> HashSet
        |CjH h  ->   h   

    let inline L (cl) = // pega qualquer tipo de coleção e retorna uma lista.
        match cl with
        |Sq  sq ->   sq  |> List.ofSeq
        |Li  l  ->   l   
        |CjS s  ->   s   |> List.ofSeq
        |CjH h  ->   h   |> List.ofSeq

    let inline S (cl) = // pega qualquer tipo de coleção e retorna uma sequência Seq.
        match cl with
        |Sq  sq ->   sq  
        |Li  l  ->   l   :> seq<_>
        |CjS s  ->   s   :> seq<_>
        |CjH h  ->   h   :> seq<_>


  //Função que calcula a união de quaisquer dois conjuntos de dados A e B seja de qual tipo for  e retorna a união deles em forma de um conjunto Set.
    let inline  Un A B =  
        
        let setA = match A with
                   |Sq  sq ->   sq  |> set     
                   |Li  l  ->   l   |> set
                   |CjS s  ->   s  
                   |CjH h  ->   h   |> set
       
        let setB = match B with
                   |Sq sq ->   sq  |> set    
                   |Li l  ->   l   |> set
                   |CjS s  ->   s
                   |CjH h  ->   h   |> set

        Set.union setA setB  // união. Ex.: let AuB = U' (CjS(A)) (CjS (B))
        
    let Sunion A B = Un (CjS(A)) (CjS (B)) // Calcula a união de dois conjuntos A e B do tipo Set. Ex.: let PuA = Sunion P A

    let inline U' A B = Set.union A B     // união. Ex.: let AuB = U' A B

    let inline  Y' A B = Set.intersect A B     // intersecção. Ex. de uso: let AnB = Y' A B

    let inline  D' A B = Set.difference A B   //abrev diferença. Ex.: let ``A-B`` = D' A B

    let inline  C' A Uni = D' Uni A  //abrev complementar de um conjunto A em relação ao Universo. Ex.: let Ac = C' A Uni

    let inline Uni' (cjs: seq<Set<double>>) : Set<double> = //Calcula o Universo. . Ela recebe uma coleção ou array de conjuntos e retorna a união de todos eles de uma vez.
        Set.unionMany cjs
    
    
    let uniHash (cjs: seq<HashSet<double>>) : HashSet<double> =
        let resultado = HashSet<double>()
        for conjunto in cjs do
            resultado.UnionWith(conjunto)
        resultado

//Função que testa se um elemento a pertence ao conjunto A ou não... 
    let inline  p' a A = Set.contains a A //abrev pert Ex.: let apA = p' a A

   //Criando a função que escreve o nome e cada elemento de um conjunto (HashSet) dado como parâmetro:
    let inline EscrevaOconjunto (nome : string) (Conjunto : HashSet<double>) =

        let agrupeSeqElementos = Conjunto|> Seq.map string|> String.concat ", "
        printfn$"{nome} = {{{agrupeSeqElementos}}}"
        printfn ""

   //Definindo a função U que processa a união de dois conjuntos dados como parâmetros
    let inline U (A: HashSet<double>)  (B: HashSet<double>) : HashSet<double> =
        let AUB = HashSet<double>(A)
        AUB.UnionWith(B)
        AUB
    // Exemplo de uso: let união = U A B 
    
   //Definindo a função Y que processa a intersecção de dois conjuntos (Hash) dados como parâmetros
    let inline Y (A: HashSet<double>) (B: HashSet<double>) : HashSet<double> =
        let AIB = HashSet<double>(A)
        AIB.IntersectWith(B)
        AIB
    //Exemplo de uso: let intersecção = Y A B

     //Definindo a função  D(A-B) que processa a diferença de dois conjuntos (Hash)dados como parâmetros
    let inline D (A: HashSet<double>) (B: HashSet<double>) : HashSet<double> =
        let ``A-B`` = HashSet<double>(A)
        ``A-B``.ExceptWith(B)
        ``A-B``
    //Exemplo de uso: let diferença = D A B ou let ``A-B`` = D A B

     //Definindo a função C, que processa o complementar de um conjunto Hash  A em relação ao universo Uni
    let inline C (A: HashSet<double>) (Uni: HashSet<double>) : HashSet<double> =
        let AC = HashSet<double>(Uni)
        AC.ExceptWith(A)
        AC
    //Exemplo de uso: let Acomplementar = C A Uni

    //Função A que transforma uma lista de tipo genérico de elementos numéricos em um conjunto HashSet<double>
    let inline A'' (L: seq<'T>) : HashSet<double> =
        L
        |> Seq.map double
        |> HashSet
    

    //Definindo a função pertence  (p) que verifica se um elemento a pertence ao conjunto A 
    let inline  p (a: double) (A: HashSet<double>) : bool =
        A.Contains(a)

    type ``P(A)`` =
        
        static member Calcular (Alist: 'T list) =
    //Definindo a função Pa que calcula o conjunto das partes (lista de listas) de um conjunto passado como parâmetro em  forma de uma lista
            let rec Pa Alist =
                match Alist with
                | [] -> [[]] // O conjunto potência de [] é [[]] (lista com a lista vazia)
                | head::tail ->
        // 1. Encontra todos os subconjuntos do restante da lista (tail)
                    let subSetsOfTail = Pa tail 
        
        // 2. Cria novos subconjuntos adicionando 'head' a cada um deles
                    let subSetsWithHead = subSetsOfTail |> List.map (fun subList -> head :: subList)
            
        // 3. Concatena os subconjuntos sem 'head' e os subconjuntos com 'head'
                    subSetsOfTail @ subSetsWithHead

            Pa Alist

   
        static member Calcular(ASet: Set<'T> when 'T : comparison) =    
            let estadoInicial = Set.singleton Set.empty
            ASet
            |> Set.fold (fun subconjuntosAtuais elemento ->
                let novosSubconjuntos = subconjuntosAtuais |> Set.map (Set.add elemento)
                Set.union subconjuntosAtuais novosSubconjuntos
            ) estadoInicial

        
     
        static member Calcular(ASet: HashSet<'T>) =
        
            // O "segredo" para conjuntos de conjuntos no .NET funcionarem matematicamente
            let comparador = HashSet<'T>.CreateSetComparer()
        
            // 1. Estado inicial passando o comparador
            let estadoInicial = new HashSet<HashSet<'T>>(comparador)
            estadoInicial.Add(new HashSet<'T>()) |> ignore
        
            // 2. Anotamos os tipos no fold para o compilador não se perder na sobrecarga
            ASet
            |> Seq.fold (fun (subconjuntosAtuais: HashSet<HashSet<'T>>) (elemento: 'T) ->
            
                // Passamos a coleção e o comparador estrutural
                let uniao = new HashSet<HashSet<'T>>(subconjuntosAtuais, comparador)
            
                for subconjunto in subconjuntosAtuais do
                    let novoSubconjunto = new HashSet<'T>(subconjunto) 
                    novoSubconjunto.Add(elemento) |> ignore
                    uniao.Add(novoSubconjunto) |> ignore
                
                uniao 
            
            ) estadoInicial


        static member Escrever (Lista: 'T list list) =
            let subconjuntos = 
                Lista
                |> List.map (fun sub -> 
                    sub |> List.map string |> String.concat ", " |> sprintf "{%s}"
                )
                |> String.concat "; "
            printfn "%A = {%s}" Lista subconjuntos
            printfn ""
        
     
        static member Escrever (ASet: Set<Set<'T>> when 'T : comparison) =
            let subconjuntos = 
                ASet
                |> Set.toList
                |> List.map (fun sub -> 
                    sub |> Set.toList |> List.map string |> String.concat ", " |> sprintf "{%s}"
                )
                |> String.concat "; "
            printfn "%A = {%s}" ASet subconjuntos
            printfn ""

        static member Escrever (ASet: HashSet<HashSet<'T>>) =
            let comparador = HashSet<'T>.CreateSetComparer()
            let subconjuntos = 
                ASet
                |> Seq.toList
                |> List.map (fun sub -> 
                    sub |> Seq.toList |> List.map string |> String.concat ", " |> sprintf "{%s}"
                )
                |> String.concat ", "
            printfn "%A = {%s}" ASet subconjuntos
            printfn ""

//Exemplos:
    let  P = ``P(A)``.Calcular([1,2])

    let E = ``P(A)``.Escrever([[1,2]])
        