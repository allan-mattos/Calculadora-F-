namespace MathLibrary 

open System.Collections.Generic
open System

[<AutoOpen>]
module Conjuntos =
    //Lembrar de dar um jeito de criar snippets para essas funções
    //TEORIA DOS CONJUNTOS
    let inline  U' A B = Set.union A B             // união. Ex.: let AUB = U' A B

    let inline  Y' A B = Set.intersect A B     // intersecção. Ex.: let AnB = Y' A B

    let inline  M' A B = Set.difference A B   //abrev diferença. Ex.: let AmB = M' A B

    let inline  C' A Uni = M' Uni A  //abrev complementar de um conjunto A em relação ao Universo. Ex.: let Ac = C' A Uni

    let inline A' (L: seq<'T>) : Set<double> =  // Ex.: let A = A' L
        L
        |> Seq.map double
        |> Set

    let inline Uni' (cjs: Set<double>[]) : Set<double>  = //Função Uni que calcula o Universo a partir de um array de conjuntos (Set<double>)
        let mutable Universo : Set<double> = Set.empty 
        for i = 0 to (cjs.Length) - 1 do   
          Universo <-  Set.union Universo cjs.[i] 
        Universo   

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

     //Definindo a função  M(A-B) que processa a diferença de dois conjuntos (Hash)dados como parâmetros
    let inline M (A: HashSet<double>) (B: HashSet<double>) : HashSet<double> =
        let AMB = HashSet<double>(A)
        AMB.ExceptWith(B)
        AMB
    //Exemplo de uso: let diferença = M A B

     //////////////////////Parei aqui:
     //Definindo a função C, que processa o complementar de um conjunto Hash  A em relação ao universo Uni
    let inline C (A: HashSet<double>) (Uni: HashSet<double>) : HashSet<double> =
        let AC = HashSet<double>(Uni)
        AC.ExceptWith(A)
        AC
    //Exemplo de uso: let Acomplementar = C A Uni

    //Função A que transforma uma lista de tipo genérico de elementos numéricos em um conjunto HashSet<double>
    let inline A (L: seq<'T>) : HashSet<double> =
        L
        |> Seq.map double
        |> HashSet


    let inline Uni (cj: HashSet<double>[]) : HashSet<double>  =
        let Universo = HashSet<double>()
        for i = 0 to (cj.Length) - 1 do   
            Universo.UnionWith(cj.[i]) 
        Universo           

    //Definindo a função pertence  (p) que verifica se um elemento a pertence ao conjunto A 
    let inline  p (a: double) (A: HashSet<double>) : bool =
        A.Contains(a)


    //Definindo a função Pa que calcula o conjunto das partes (lista de listas) de um conjunto passado como parâmetro em  forma de uma lista
    let rec Pa Alist =
        match Alist with
        | [] -> [[]] // O conjunto potência de [] é [[]] (lista com a lista vazia)
        | head::tail ->
        // 1. Encontra todos os subconjuntos do restante da lista (tail)
        let subSetsOfTail = Pa tail 
        
        // 2. Cria novos subconjuntos adicionando 'head' a cada um deles
        let subSetsWithHead = 
            subSetsOfTail 
            |> List.map (fun subList -> head :: subList)
            
        // 3. Concatena os subconjuntos sem 'head' e os subconjuntos com 'head'
        subSetsOfTail @ subSetsWithHead

    let EscrevaPa N=
     
        let subconjuntos = 
            N
            |> List.map (fun sub -> 
                sub |> List.map string |> String.concat ", " |> sprintf "{%s}"
            )
            |> String.concat ", "

        printfn "%A = {%s}" N subconjuntos
        printfn ""
