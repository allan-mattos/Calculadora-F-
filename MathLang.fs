

namespace MathLibrary

open System.Collections.Generic
open System

// COLA RÁPIDA:
// ∪  ∩  ∈  ∉  ∅  ≠  ≡  ∀  ∃

[<AutoOpen>]
module MathLang =
    //Lembrar de dar um jeito de criar snippets para essas funções
    //TEORIA DOS CONJUNTOS
    let inline  U' A B = Set.union A B             //abrev uni

    let inline  ``∩'`` A B = Set.intersect A B     //abrev inter

    let inline  ``∈'``  a A = Set.contains a A    //abrev pert

    let inline  ``-'`` A B = Set.difference A B   //abrev dif

    let inline  A_' A Uni = Set.difference Uni A  //abrev comp

   //Definindo a função pertence  (∈  ∉ ) que verifica se um elemento a pertence ao conjunto A 
    let inline  ``∈`` (a: double) (A: HashSet<double>) : bool =
        A.Contains(a)

   //Definindo a função AUB que processa a união de dois conjuntos dados como parâmetros
    let inline U (A: HashSet<double>)  (B: HashSet<double>) : HashSet<double> =
        let união = HashSet<double>(A)
        união.UnionWith(B)
        união
    
   //Definindo a função ∩ que processa a intersecção de dois conjuntos dados como parâmetros
    let inline ``∩`` (A: HashSet<double>) (B: HashSet<double>) : HashSet<double> =
        let interseção = HashSet<double>(A)
        interseção.IntersectWith(B)
        interseção

     //Definindo a função ``-`` (A-B) que processa a diferença de dois conjuntos dados como parâmetros
    let inline ``-`` (A: HashSet<double>) (B: HashSet<double>) : HashSet<double> =
        let diferença = HashSet<double>(A)
        diferença.ExceptWith(B)
        diferença

     //Definindo a função A_, que processa o complementar de um conjunto A em relação ao universo Uni
    let inline A_ (A: HashSet<double>) (Uni: HashSet<double>) : HashSet<double> =
        let Acomplementar = HashSet<double>(Uni)
        Acomplementar.ExceptWith(A)
        Acomplementar

//Definindo a função Pa que calcula o conjunto das partes de um conjunto transformado em uma lista
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

    //Cálculo aritmético

    let inline Pow v1 v2 = Math.Pow ( v1, v2) 

    let inline raiz x = Math.Sqrt(x)

    //Definindo a função arraySoma que calcula a soma dos elementos de um array
    let inline arraySoma array = Array.fold (fun acc x -> acc + x) 0.0 array

    //Definindo a função arrayProduto que calcula o produto dos elementos de um array
    let inline arrayProduto array = Array.fold (fun acc x -> acc * x) 1.0 array
