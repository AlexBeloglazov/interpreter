fun interpreter(infile : string, outfile : string) =
    let
        val streamIn  = TextIO.openIn infile;
        val streamOut = TextIO.openOut outfile;
        val listLines = String.tokens (fn ch => ch = #"\n") (TextIO.inputAll streamIn);
        datatype VAL =  INT of int | STRING of string | NAME of string |
                        BOOL of bool | UNIT | ERROR | NOVAL;
        datatype CMD =  PUSH of VAL | POP | ADD | SUB | MUL | DIV | REM |
                        NEG | SWAP | QUIT | AND | OR | NOT | EQUAL | LESSTHAN |
                        BIND | IF | LET | END;
        val digitsMap = Char.contains "0123456789";
        val specChMap = Char.contains "`~!@#$%^&*()_+-=[]{}|\\:;'\"<>?,./";

        fun tokenizeQuotes (str: string) = String.tokens (fn ch => ch = #"\"") str;
        fun tokenizeSpace (str: string) = String.tokens (fn ch => ch = #" ") str;
        fun tokenize (str: string) = case (tokenizeQuotes str) of
            "push "::s::nil => ["push", "\"" ^ s ^ "\""]
            | _ => tokenizeSpace str;

        fun int2str num = if num < 0 then "-"^(Int.toString(~num)) else Int.toString num;

        fun has_only_digits (arg: char list) = case arg of
            ch::rest => (if digitsMap ch then has_only_digits rest else false)
            | _ => true;

        fun has_sp_ch (arg: char list) = case arg of
            ch::rest => (if specChMap ch then true else has_sp_ch rest)
            | _ => false;

        fun bind (name: string, value: VAL, scope) =
            fn x => if (x = name) then value else scope x;

        fun parse_arg (arg: string) =
            if has_only_digits(explode arg)
                then INT (valOf(Int.fromString arg))
            else
                if (String.isPrefix "\"" arg) andalso (String.isSuffix "\"" arg)
                    then STRING (substring (arg, 1, (size arg)-2))
                else
                    if (String.isPrefix "-" arg)
                        then (if has_only_digits(tl(explode arg)) then INT (valOf(Int.fromString arg))
                        else ERROR)
                    else
                        if has_sp_ch(explode arg) then ERROR else NAME arg;

        fun parse_instruction strInstruction =
            case strInstruction of
                "push"::arg::nil => PUSH (parse_arg arg)
                | "pop"::nil => POP
                | ":true:"::nil => PUSH (BOOL true)
                | ":false:"::nil => PUSH (BOOL false)
                | ":error:"::nil => PUSH (ERROR)
                | "add"::nil => ADD
                | "sub"::nil => SUB
                | "mul"::nil => MUL
                | "div"::nil => DIV
                | "rem"::nil => REM
                | "neg"::nil => NEG
                | "swap"::nil => SWAP
                | "quit"::nil => QUIT
                | "and"::nil => AND
                | "or"::nil => OR
                | "not"::nil => NOT
                | "equal"::nil => EQUAL
                | "lessThan"::nil => LESSTHAN
                | "bind"::nil => BIND
                | "if"::nil => IF
                | "let"::nil => LET
                | "end"::nil => END
                | _ => PUSH (ERROR);

        fun get_instructions listLines =
            case listLines of
                line::rest => (parse_instruction(tokenize line))::get_instructions(rest)
                | _ => nil;

        fun print_stack (strStack: string, stack: VAL list) =
            case stack of
                (INT num)::rest => print_stack (strStack^(int2str num)^"\n", rest)
                | (STRING str)::rest => print_stack (strStack^str^"\n", rest)
                | (NAME name)::rest => print_stack (strStack^name^"\n", rest)
                | (BOOL true)::rest => print_stack (strStack^":true:\n", rest)
                | (BOOL false)::rest => print_stack (strStack^":false:\n", rest)
                | UNIT::rest => print_stack (strStack^":unit:\n", rest)
                | ERROR::rest => print_stack (strStack^":error:\n", rest)
                | _ => (TextIO.output(streamOut, strStack);
                        TextIO.flushOut streamOut;
                        TextIO.closeIn streamIn;
                        TextIO.closeOut streamOut);

        fun execute (instruction: CMD, stack: VAL list, stackS: (VAL list) list,
                    scope: string -> VAL, scopeS: (string -> VAL) list) =
            let
                fun test (returnedStack: VAL list, returnedstackS: (VAL list) list,
                            returnedScope: string -> VAL, returnedScopeS: (string -> VAL) list) =
                    case returnedStack of
                        ERROR::rest => (ERROR::stack, stackS, scope, scopeS)
                        | _ => (returnedStack, returnedstackS, returnedScope, returnedScopeS);
                fun binary_ariphmetic () = case stack of
                    (INT f)::(NAME s)::rest =>
                        test(execute(instruction, (INT f)::(scope s)::rest, stackS, scope, scopeS))
                    | (NAME f)::(INT s)::rest =>
                        test(execute(instruction, (scope f)::(INT s)::rest, stackS, scope, scopeS))
                    | (NAME f)::(NAME s)::rest =>
                        test(execute(instruction, (scope f)::(scope s)::rest, stackS, scope, scopeS))
                    | _ => (ERROR::stack, stackS, scope, scopeS);
                fun binary_logic () = case stack of
                    (BOOL f)::(NAME s)::rest =>
                        test(execute(instruction, (BOOL f)::(scope s)::rest, stackS, scope, scopeS))
                    | (NAME f)::(BOOL s)::rest =>
                        test(execute(instruction, (scope f)::(BOOL s)::rest, stackS, scope, scopeS))
                    | (NAME f)::(NAME s)::rest =>
                        test(execute(instruction, (scope f)::(scope s)::rest, stackS, scope, scopeS))
                    | _ => (ERROR::stack, stackS, scope, scopeS)
            in
                (case instruction of
                    PUSH (INT arg) => ((INT arg)::stack, stackS, scope, scopeS)
                    | PUSH (STRING arg) => ((STRING arg)::stack, stackS, scope, scopeS)
                    | PUSH (NAME arg) => ((NAME arg)::stack, stackS, scope, scopeS)
                    | PUSH (BOOL true) => ((BOOL true)::stack, stackS, scope, scopeS)
                    | PUSH (BOOL false) => ((BOOL false)::stack, stackS, scope, scopeS)
                    | PUSH (ERROR) => (ERROR::stack, stackS, scope, scopeS)
                    | POP => (case stack of
                                value::rest => (rest, stackS, scope, scopeS)
                                | _ => (ERROR::stack, stackS, scope, scopeS))
                    | ADD => (case stack of
                                (INT f)::(INT s)::rest => (INT(s+f)::rest, stackS, scope, scopeS)
                                | _ => binary_ariphmetic())
                    | SUB => (case stack of
                                (INT f)::(INT s)::rest => (INT(s-f)::rest, stackS, scope, scopeS)
                                | _ => binary_ariphmetic())
                    | MUL => (case stack of
                                (INT f)::(INT s)::rest => (INT(s*f)::rest, stackS, scope, scopeS)
                                | _ => binary_ariphmetic())
                    | DIV => (case stack of
                                (INT 0)::s::rest => (ERROR::stack, stackS, scope, scopeS)
                                | (INT f)::(INT s)::rest => (INT(s div f)::rest, stackS, scope, scopeS)
                                | _ => binary_ariphmetic())
                    | REM => (case stack of
                                (INT 0)::s::rest => (ERROR::stack, stackS, scope, scopeS)
                                | (INT f)::(INT s)::rest => (INT(s mod f)::rest, stackS, scope, scopeS)
                                | _ => binary_ariphmetic())
                    | NEG => (case stack of
                                (INT f)::rest => (INT(~f)::rest, stackS, scope, scopeS)
                                | (NAME f)::rest => test(execute(instruction, (scope f)::rest, stackS, scope, scopeS))
                                | _ => (ERROR::stack, stackS, scope, scopeS))
                    | SWAP => (case stack of
                                f::s::rest => (s::f::rest, stackS, scope, scopeS)
                                | _ => (ERROR::stack, stackS, scope, scopeS))
                    | AND => (case stack of
                                (BOOL f)::(BOOL s)::rest => (BOOL(s andalso f)::rest, stackS, scope, scopeS)
                                | _ => binary_logic())
                    | OR => (case stack of
                                (BOOL f)::(BOOL s)::rest => (BOOL(s orelse f)::rest, stackS, scope, scopeS)
                                | _ => binary_logic())
                    | NOT => (case stack of
                                (BOOL f)::rest => (BOOL(not f)::rest, stackS, scope, scopeS)
                                | (NAME f)::rest => test(execute(instruction, (scope f)::rest, stackS, scope, scopeS))
                                | _ => (ERROR::stack, stackS, scope, scopeS))
                    | EQUAL => (case stack of
                                (INT f)::(INT s)::rest => (BOOL(s = f)::rest, stackS, scope, scopeS)
                                | _ => binary_ariphmetic())
                    | LESSTHAN => (case stack of
                                (INT f)::(INT s)::rest => (BOOL(s < f)::rest, stackS, scope, scopeS)
                                | _ => binary_ariphmetic())
                    | IF => (case stack of
                                f::s::(BOOL t)::rest => if t
                                    then (f::rest, stackS, scope, scopeS)
                                    else (s::rest, stackS, scope, scopeS)
                                | f::s::(NAME t)::rest => test(execute(instruction, f::s::(scope t)::rest, stackS, scope, scopeS))
                                | _ => (ERROR::stack, stackS, scope, scopeS))
                    | BIND => (case stack of
                                ERROR::(NAME s)::rest => (ERROR::stack, stackS, scope, scopeS)
                                | (NAME f)::(NAME s)::rest => (case scope f of
                                        NOVAL => (ERROR::stack, stackS, scope, scopeS)
                                        | boundv => (UNIT::rest, stackS, bind(s, boundv, scope), scopeS))
                                | value::(NAME s)::rest => (UNIT::rest, stackS, bind(s, value, scope), scopeS)
                                | _ => (ERROR::stack, stackS, scope, scopeS))
                    | LET => ([], stack::stackS, scope, scope::scopeS)
                    | END => ((hd stack)::(hd stackS), tl stackS, hd scopeS, tl scopeS)
                    | _ => (ERROR::stack, stackS, scope, scopeS))
                end;

        fun main (instructions: CMD list, (stack: VAL list, stackS: (VAL list) list,
                    scope: string -> VAL, scopeS: (string -> VAL) list)) =
            case instructions of
                QUIT::rest => print_stack ("", stack)
                | instruction::rest =>
                    main (rest, execute (instruction, stack, stackS, scope, scopeS))
                | _ => print_stack ("", stack);
        val cmdInstructions = get_instructions listLines
    in
        main (cmdInstructions, ([], [], (fn x => NOVAL), []))
    end
