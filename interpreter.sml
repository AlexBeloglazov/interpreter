fun interpreter(infile : string, outfile : string) =
    let
        val streamIn  = TextIO.openIn infile;
        val streamOut = TextIO.openOut outfile;
        val listLines = String.tokens (fn ch => ch = #"\n") (TextIO.inputAll streamIn);
        datatype VAL =  INT of int | STRING of string | NAME of string |
                        TRUE | FALSE | ERROR;
        datatype CMD =  PUSH of VAL | POP | ADD | SUB | MUL | DIV | REM |
                        NEG | SWAP | QUIT;
        fun tokenize str = String.tokens (fn ch => ch = #" ") str;
        fun int2str num = if num < 0 then "-"^(Int.toString(~num)) else Int.toString num;
        fun parse_arg arg =
            if (Char.contains arg #".") then ERROR
            else if (String.isPrefix "\"" arg) then NAME (substring (arg, 1, (size arg)-2))
            else (case Int.fromString arg of
                SOME num => INT num
                | NONE => STRING arg);

        fun parse_instruction strInstruction =
            case strInstruction of
                "push"::arg::nil => PUSH (parse_arg arg)
                | "pop"::nil => POP
                | ":true:"::nil => PUSH (TRUE)
                | ":false:"::nil => PUSH (FALSE)
                | ":error:"::nil => PUSH (ERROR)
                | "add"::nil => ADD
                | "sub"::nil => SUB
                | "mul"::nil => MUL
                | "div"::nil => DIV
                | "rem"::nil => REM
                | "neg"::nil => NEG
                | "swap"::nil => SWAP
                | "quit"::nil => QUIT
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
                | TRUE::rest => print_stack (strStack^":true:\n", rest)
                | FALSE::rest => print_stack (strStack^":false:\n", rest)
                | ERROR::rest => print_stack (strStack^":error:\n", rest)
                | _ => (TextIO.output(streamOut, strStack);
                        TextIO.flushOut streamOut;
                        TextIO.closeIn streamIn;
                        TextIO.closeOut streamOut);
        fun execute (instruction: CMD, stack: VAL list) =
            case instruction of
                PUSH (INT arg) => (INT arg)::stack
                | PUSH (STRING arg) => (STRING arg)::stack
                | PUSH (NAME arg) => (NAME arg)::stack
                | PUSH (TRUE) => TRUE::stack
                | PUSH (FALSE) => FALSE::stack
                | PUSH (ERROR) => ERROR::stack
                | POP => (case stack of
                            value::rest => rest
                            | _ => ERROR::stack)
                | ADD => (case stack of
                            (INT f)::(INT s)::rest => INT(s+f)::rest
                            | _ => ERROR::stack)
                | SUB => (case stack of
                            (INT f)::(INT s)::rest => INT(s-f)::rest
                            | _ => ERROR::stack)
                | MUL => (case stack of
                            (INT f)::(INT s)::rest => INT(s*f)::rest
                            | _ => ERROR::stack)
                | DIV => (case stack of
                            (INT 0)::(INT s)::rest => ERROR::stack
                            | (INT f)::(INT s)::rest => INT(s div f)::rest
                            | _ => ERROR::stack)
                | REM => (case stack of
                            (INT 0)::(INT s)::rest => ERROR::stack
                            | (INT f)::(INT s)::rest => INT(s mod f)::rest
                            | _ => ERROR::stack)
                | NEG => (case stack of
                            (INT f)::rest => INT(~f)::rest
                            | _ => ERROR::stack)
                | SWAP => (case stack of
                            f::s::rest => s::f::rest
                            | _ => ERROR::stack)
                | _ => ERROR::stack
        fun main (instructions: CMD list, stack: VAL list) = case instructions of
            QUIT::rest => print_stack ("", stack)
            | instruction::rest => main (rest, execute (instruction, stack))
            | _ => print_stack ("", stack);
        val cmdInstructions = get_instructions listLines
    in
        main (cmdInstructions, [])
    end
