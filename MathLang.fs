

namespace MathLibrary

open System.Collections.Generic

// COLA RÁPIDA:
// ∪  ∩  ∈  ∉  ∅  ≠  ≡  ∀  ∃

[<AutoOpen>]
module MathLang =

    //TEORIA DOS CONJUNTOS
    let inline  AUB' A B = Set.union A B

    let inline  AIB' A B = Set.intersect A B

    let inline  apA'  a A = Set.contains a A

    let inline  AdifB' A B = Set.difference A B

    let inline  A_' A Uni = Set.difference Uni A

   //Definindo a função apA (pertence  ∈  ∉ ) que verifica se um elemento a pertence ao conjunto A 
    let inline apA (a: double) (A: HashSet<double>) : bool =
        A.Contains(a)

   //Definindo a função AUB que processa a união de dois conjuntos dados como parâmetros
    let inline AUB (A: HashSet<double>) (B: HashSet<double>) : HashSet<double> =
        let união = HashSet<double>(A)
        união.UnionWith(B)
        união
    
   //Definindo a função AIB que processa a intersecção de dois conjuntos dados como parâmetros
    let inline AIB (A: HashSet<double>) (B: HashSet<double>) : HashSet<double> =
        let interseção = HashSet<double>(A)
        interseção.IntersectWith(B)
        interseção

     //Definindo a função AdifB (A-B) que processa a diferença de dois conjuntos dados como parâmetros
    let inline AdifB (A: HashSet<double>) (B: HashSet<double>) : HashSet<double> =
        let diferença = HashSet<double>(A)
        diferença.ExceptWith(B)
        diferença

     //Definindo a função A_, que processa o complementar de um conjunto A em relação ao universo Uni
    let inline A_ (A: HashSet<double>) (Uni: HashSet<double>) : HashSet<double> =
        let Acomplementar = HashSet<double>(Uni)
        Acomplementar.ExceptWith(A)
        Acomplementar

    let rec inline Pa Alist =
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
