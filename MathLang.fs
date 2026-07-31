namespace MathLibrary 

open System.Collections.Generic
open System

open Functions
// COLA RÁPIDA:
// ∪  ∩  ∈  ∉  ∅  ≠  ≡  ∀  ∃ ∫



[<AutoOpen>]
module MathLang =
    //Lembrar de dar um jeito de criar snippets para essas funções

  

    //Cálculo aritmético

    let inline Pow v1 v2 = Math.Pow ( v1, v2) 

    let inline raiz x = Math.Sqrt(x)

    //Definindo a função arraySoma que calcula a soma dos elementos de um array
    let inline arraySoma array = Array.fold (fun acc x -> acc + x) 0.0 array

    //Definindo a função arrayProduto que calcula o produto dos elementos de um array
    let inline arrayProduto array = Array.fold (fun acc x -> acc * x) 1.0 array
