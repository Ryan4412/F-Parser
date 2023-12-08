(*

Name: Kraft, Ryan
Name: Singh, Arjun (2nd Student, if applicable. You may work with ONE partner, but both must submit identical copies.)
 
program ⟶ stmt_list 
stmt_list ⟶ stmt stmt_list | ε 
stmt ⟶ assignment | read_stmt | write_stmt | for_loop | if_stmt 
expr ⟶ term term_tail 
term_tail ⟶ add_op term term_tail | ε 
term ⟶ factor factor_tail 
factor_tail ⟶ mult_op factor factor_tail | ε 
factor ⟶ ( expr ) | id 
add_op ⟶ - | + 
mult_op ⟶ * | / 
rel_oper ⟶ > | < | == 
cond ⟶ expr rel_oper expr 
assignment ⟶ id := expr 
read_stmt ⟶ read id
write_stmt ⟶ write expr 
if_stmt ⟶ if cond then stmt_list else_stmt 
else_stmt ⟶ else stmt_list fi | fi  
for_loop ⟶ for id = id to id step_stmt do stmt_list done 
step_stmt ⟶ step id | ε 
id ⟶ <any lexeme/token that's not already expressed as a terminal above>
*)

// Tokens
type Token =
    | OpenParen of string
    | ClosedParen of string
    | AddOp of string
    | MultOp of string
    | RelOp of string
    | Assign of string
    | ForAssign of string
    | If of string
    | Fi of string
    | Then of string 
    | Else of string
    | For of string
    | To of string 
    | Do of string
    | Step of string
    | Done of string
    | Read of string
    | Write of string 
    | ID of string

    with static member tokenFromLexeme str = // Function to get a token from a lexeme (String)
            match str with
            | "(" -> OpenParen str
            | ")" -> ClosedParen str
            | "+" | "-" -> AddOp str
            | "*" | "/" -> MultOp str 
            | ">" | "<" | "==" -> RelOp str
            | ":=" -> Assign str
            | "=" -> ForAssign str
            | "if" -> If str
            | "fi" -> Fi str
            | "then" -> Then str
            | "else" -> Else str
            | "for" -> For str
            | "to" -> To str
            | "do" -> Do str
            | "step" -> Step str
            | "done" -> Done str
            | "read" -> Read str
            | "write" -> Write str
            | x -> ID str

// NOTES:
// The |> operator sends (pipes) the output of one function directly to the next one in line.
// "and" just allows multiple, mutually recursive functions to be defined under a single "let"

 
let rec parse theList = program theList

// program ⟶ stmt_list
and  program lst  =
    lst |> stmt_list

// stmt_list ⟶ stmt stmt_list | ε 
and stmt_list lst =
    match lst with
    | ID _ :: _ 
    | Read _ :: _ 
    | Write _ :: _ 
    | If _ :: _
    | For _ :: _ -> lst |> stmt |> stmt_list
    | xs -> xs

// stmt ⟶ assignment | read_stmt | write_stmt | for_loop | if_stmt 
and stmt = function
    | ID id :: xs -> (ID id :: xs) |> assignment 
    | Read r :: xs -> (Read r :: xs) |> read_stmt
    | Write w :: xs -> (Write w :: xs) |> write_stmt
    | For f :: xs -> (For f :: xs) |> for_loop
    | If i :: xs -> (If i :: xs) |> if_stmt
    | x :: xs -> failwithf $"Expected stmt, but found: %A{x}"
    | [] -> failwith "Unexpected end of input while processing stmt."


// assignment ⟶ id := expr
and assignment = function
    | ID id :: xs -> xs |> assign |> expr 
    | x :: xs -> failwithf $"Expected assignment, but found: %A{x}"
    | [] -> failwith "Unexpected end of input while processing assignment."

// if_stmt ⟶ if cond then stmt_list else_stmt
and if_stmt = function 
    | If i :: xs -> xs |> cond |> then_stmt |> stmt_list |> else_stmt
    | x :: xs -> failwithf $"Expected if_stmt, but found: %A{x}"
    | [] -> failwith "Unexpected end of input while processing if_stmt."

// else_stmt ⟶ else stmt_list fi | fi
and else_stmt = function 
    | Else e :: xs -> xs |> stmt_list |> fi 
    | Fi fi :: xs -> xs
    | x :: xs -> failwithf $"Expected else_stmt, but found: %A{x}"
    | [] -> failwith "Unexpected end of input while processing else_stmt."

// read_stmt ⟶ read id
and read_stmt = function 
    | Read r :: xs -> xs |> Id
    | x :: xs -> failwithf $"Expected read_stmt, but found: %A{x}"
    | [] -> failwith "Unexpected end of input while processing read_stmt."

// write_stmt ⟶ write expr
and write_stmt = function 
    | Write w :: xs -> xs |> expr
    | x :: xs -> failwithf $"Expected write_stmt, but found: %A{x}"
    | [] -> failwith "Unexpected end of input while processing write_stmt."

// for_loop ⟶ for id = id to id step_stmt do stmt_list done
and for_loop = function
    | For f :: xs -> xs |> Id |> for_assign |> Id |> to_stmt |> Id |> step_stmt |> do_stmt |> stmt_list |> done_stmt
    | x :: xs -> failwithf $"Expected for_loop, but found: %A{x}"
    | [] -> failwith "Unexpected end of input while processing for_loop."

// cond ⟶ expr rel_oper expr
and cond = function 
    | xs -> xs |> expr |> rel_oper |> expr

// step_stmt ⟶ step id | ε
and step_stmt = function
    | Step s :: xs -> xs |> Id 
    | xs -> xs


// process fi 
and fi = function 
    | Fi fi :: xs -> xs
    | x -> failwithf $"Expected Fi, but found: %A{x}"
// process then 
and then_stmt = function 
    | Then t :: xs -> xs
    | x -> failwithf $"Expected Then, but found: %A{x}"

// process rel_oper 
and rel_oper = function 
    | RelOp ro :: xs -> xs 
    | x -> failwithf $"Expected RelOp, but found: %A{x}"
// processes Assign
and assign = function 
    | Assign  n :: xs -> xs
    | x -> failwithf $"Expected Assign, but found: %A{x}"

// process ID
and Id = function
    | ID id :: xs -> xs
    | x -> failwithf $"Expected ID, but found: %A{x}"

// process ForAssign
and for_assign = function 
    | ForAssign fa :: xs -> xs 
    | x -> failwithf $"Expected ForAssign, but found: %A{x}"

// process to
and to_stmt = function 
    | To t :: xs -> xs
    | x -> failwithf $"Expected To, but found: %A{x}"

// process do
and do_stmt = function
    | Do d :: xs -> xs
    | x -> failwithf $"Expected Do, but found: %A{x}"

// process done 
and done_stmt = function
    | Done d :: xs -> xs 
    | x -> failwithf $"Expected Done, but found: %A{x}"
// expr ⟶ term term_tail 
and expr = function
    | xs -> xs |> term |> term_tail 

// term ⟶ factor factor_tail
and term = function 
    | xs -> xs |> factor |> factor_tail 

// term_tail ⟶ add_op term term_tail | ε 
and term_tail = function 
    | AddOp ao :: xs -> xs |> term |> term_tail
    | xs -> xs 

// factor ⟶ ( expr ) | id
and factor = function
    | OpenParen par :: xs -> xs |> expr |> closedParen
    | ID id :: xs -> xs
    | x :: xs -> failwithf $"Expected factor, but found: %A{x}"
    | [] -> failwith "Unexpected end of input while processing factor." 

// factor_tail ⟶ mult_op factor factor_tail | ε
and factor_tail = function
    | MultOp mo :: xs -> xs |> factor |> factor_tail
    | xs -> xs

// processes ClosedParen
and closedParen = function 
    |ClosedParen cp :: xs -> xs
    |x -> failwithf $"Expected ClosedParen, but found: %A{x}"


(* Begin Parsing Process *)
let startParsing (str:string) =
    // Split the String (which creates an Array) -> convert the Array to a List -> MAP the list of strings into a list of Tokens.
    // (Note, though arrays are a lot like lists, lists are a bit easier to use for the pattern matching.)
    let tokenList =
        str.Split ' ' |>
        Array.toList |>
        List.map Token.tokenFromLexeme

    // Display our list of tokens... just for fun.
    printfn $"The initial String: %s{str}"
    printfn $"The initial List: %A{tokenList}\n"

    // Work the magic...
    try
        let parsedList = program tokenList
        in printfn $"The original string %s{str}\n is a valid input. inputThe Final List:\n\t%A{parsedList}"

    with
        Failure msg -> printfn $"Error: %s{msg}";  System.Console.ReadLine () |> ignore




(* Get the user input and start parsing *)
// NOTE: To make the let assihnment be a function that accepts no parameters,
// an "empty tuple" must be accepted.
let promptAndGo () =
    (* TEST DATA *)
    // let userInput = "the fast , fast dog chases the fast cat ."
    // let userInput = "the noun chases the adj cat and the adj , adj , fast dog adv chases the cat prep a adj noun ."

    let userInput =
        printf "Enter String: ";
        // A case where it's easier to use the .NET ReadLine as opposed to the more restrictive OCaml native variant.
        System.Console.ReadLine ()

    in startParsing userInput


// RUN INTERACTIVELY AS A SCRIPT
promptAndGo ()

// Uncomment the following to pause at the end if it's running in a terminal which dissapears upon running.
// System.Console.ReadKey(true) |> ignore
