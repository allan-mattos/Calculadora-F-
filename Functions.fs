// Minha primeira calculadora em F# com 14 operações diferentes
//Autor: Allan Mattos
//Data: 21/09/2025 
//Em obras
//Transformando todos os loops em recursividade
//Amém!

//Após uma alteração, não esqueça de dar os dois comandos:
//1) git pull origin master
//2) git push origin master 

namespace MathLibrary 

open System
open System.IO
open System.Collections.Generic
open ClosedXML.Excel

open Conjuntos



[<AutoOpen>]
module Functions = 
     
     let PrinTex text = printfn $"{text}" 

     let inline ImprimirCentralizado (text : string) : string =

         let larguraConsole = Console.WindowWidth
         let posicaoInicial = (larguraConsole - text.Length) / 2
         Console.WriteLine(String(' ', posicaoInicial) + text)
         ""

     printfn""
         
         //Função que lê a entrada e retorna o mesmíssimo valor caso não seja nulo. Acaso retorne None, pedimos para o usuário digitar uma entrada não-nula:
     let rec  LendoEntrada (): string =
       
         let imput : option<string> =
             match Console.ReadLine() with
             |null -> None
             |valor -> Some valor
                       
         match imput with
         |None     -> 
                        printfn"Entrada nula! Digite uma entrada válida!(Apenas números!)"
                        LendoEntrada () 
         |Some valor -> valor


  //Função que tenta converter a entrada para inteiro:
     let inline ``éInt?`` (Entrada: string) =
         match Int32.TryParse(Entrada) with
         | (true, value) -> Some value
         | (false, _) -> None
     printfn""

     // Função que só deixa passar para a próxima linha de código caso a entrada represente um algarismo inteiro
     let rec PxLinhaSóSeInteiro Entrada  =
         let ``EntradaÉInteiro?`` = ``éInt?`` Entrada
         match ``EntradaÉInteiro?`` with
         |None -> 
                         printfn"Entrada Inválida! Digite um número inteiro! "
                         let Entradai = LendoEntrada ()
                         PxLinhaSóSeInteiro Entradai 

         |Some value ->  value

    //Função que tenta efetuar a conversão para o tipo double option
     let inline ``ÉDouble?`` (s: string)  : double option =
         match Double.TryParse(s, System.Globalization.CultureInfo.InvariantCulture) with
         | true, valor -> Some valor
         | false, _    -> None        

     // Função que só deixa passar para a próxima linha de código caso a entrada represente um número que possa ser convertido para o tipo double
     let rec  PxLinhaSóSeDouble Entrada =
         let ``EntradaÉDouble?`` = ``ÉDouble?`` Entrada
         match ``EntradaÉDouble?`` with
         |None -> 
                         printfn"Entrada Inválida! Digite um número inteiro! "
                         
                         let Entradai = LendoEntrada ()
                         PxLinhaSóSeDouble Entradai 

         |Some value ->  value

                       

//Caso o usuário digite "q" para sair, temos as funções confirmarSaida e theEnd(principal):
     let inline confirmarSaida () =
         printfn ""
         printf "Tem certeza que deseja sair? (S/N): "
         match Console.ReadLine() with
         |null -> None
         |valor ->
             match valor.Trim().ToUpper() with
             | "S" -> Some true
             | "N" -> Some false
             | _ -> None            
     printfn""

//Função principal que trata a saída do programa, caso o usuário digite "q" para sair:
     let rec theEnd () =
         match confirmarSaida () with
         | Some true -> 
             printfn "Saindo do programa..."
             Environment.Exit(0)
         | Some false -> printfn "Ok, retornando!"
         | None -> 
             printfn "Entrada inválida! Digite 'S' ou 'N'!"
             theEnd() 
         printfn""
    
     let inline Início () : string =
         let texto = "-------===========================|CALCULADORA|========================-------"
  
         ImprimirCentralizado texto |> ignore
  
         Console.WriteLine()

         ImprimirCentralizado "+++Pressione qualquer tecla para continuar!+++" |> ignore

         Console.WriteLine()

         Console.ReadKey()|>ignore
         " "
    //Função que pergunta ao usuário se deseja efetuar mais uma operação, caso sim, chama a função comput() que é a função que efetua a operação escolhida, 
    //a qual é passada como parâmetro para a função maisUmaoperação, caso não, a função termina e o programa volta para
    //a função EscolhendoOperações()

     let rec  EscolhendoOperações  ()  =
       
         Console.WriteLine()
         let text = @"
         A) Adição
         L) Lógica                      
         C) Conjuntos
         R) Raiz quadrada                     
         D) Divisão
         E) Exponenciação
         S) Subtração 
         M) Multiplicação
         !) Fatorial
         F) Fibonacci
         Q) Sair!"

         Console.WriteLine(text)

         Console.WriteLine()
         printf@"         "

         let entrada = LendoEntrada ()

         let ``|Entrada|`` = entrada.Trim().ToUpper()

         match ``|Entrada|`` with   
         | "Q"  -> theEnd ()  
                   EscolhendoOperações()
         | "A"  -> "Adição"    
               
         | "L"  -> "Lógica"                     //Em construção: Cálculo de lógica proposicional, com tabelas verdade indicando se  a proposição é tautologia ou não.
         | "C"  -> "Conjuntos"
         | "R"  -> "Raiz quadrada"
         | "D"  -> "Divisão"
         | "E"  -> "Exponenciação"
         | "S"  -> "Subtração"  // Futuro ∫
         | "M"  -> "Multiplicação"
         | "!"  -> "Fatorial"
         | "F"  -> "Fibonacci"
         | _    -> 
                   printfn"Entrada inesperada.. Digite uma inicial de operação válida!"
                   EscolhendoOperações ()
                      
            
     let rec  maisUmaoperação comput (oper: string) =
         
         Console.WriteLine()

         printf"\t "
         let texto = $"Deseja efetuar mais uma operação de {oper}? (S\N)"

         PrinTex texto
         printfn"\t (Ou então digite 'Q' para sair): "

         let entrada = LendoEntrada ()
         let ``|Entrada|`` = entrada.Trim().ToUpper()

         printfn ""
         match ``|Entrada|`` with
         |"Q"     ->   theEnd () 
         |"S"     ->   comput oper
                     
         |"N"     ->   printfn "Ok" 
                      
                      
         |_       ->   printfn "Entrada inesperada! Digite 'S' ou 'N'!"
                       maisUmaoperação comput oper 
                       printfn""
         Console.WriteLine()
     printfn""
           

    
                         
        

         
 
   

    


