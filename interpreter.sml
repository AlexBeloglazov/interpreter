fun interpreter(infile : string, outfile : string) =
    let
        val streamIn  = TextIO.openIn infile;
        val streamOut = TextIO.openOut outfile;
        val listLines = String.tokens (fn ch => ch = #"\n") (TextIO.inputAll streamIn);
        datatype VAL =  INT of int | STRING of string | NAME of string |
                        BOOL of bool | UNIT | ERROR | NOVAL | USUAL | INOUT |
                        (* ftype param body env *)
                        CLOSURE of VAL * string * CMD list * (string -> VAL)
        and      CMD =  PUSH of VAL | POP | ADD | SUB | MUL | DIV | REM |
                        NEG | SWAP | QUIT | AND | OR | NOT | EQUAL | LESSTHAN |
                        BIND | IF | LET | END  | FUNEND | RETURN | CALL |
                        (* fname ftype param var_names body *)
                        FUN of string * VAL * string * string list * CMD list |
                        F of VAL * string * string;

        fun int2str num = if num < 0 then "-"^(Int.toString(~num)) else Int.toString num;

        fun bind (name: string, value: VAL, env) =
            fn x => if (x = name) then value else env x;

        fun get_instructions (listLines, resultInstructions, namespace) =
            let
                val digitsMap = Char.contains "0123456789";
                val specChMap = Char.contains "`~!@#$%^&*()_+-=[]{}|\\:;'\"<>?,./";
                fun has_only_digits (arg: char list) = case arg of
                    ch::rest => (if digitsMap ch then has_only_digits rest else false)
                    | _ => true;
                fun has_sp_ch (arg: char list) = case arg of
                    ch::rest => (if specChMap ch then true else has_sp_ch rest)
                    | _ => false;
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
                fun tokenize (str: string) =
                    let
                        fun tokenizeQuotes (str: string) = String.tokens (fn ch => ch = #"\"") str;
                        fun tokenizeSpace (str: string) = String.tokens (fn ch => ch = #" ") str;
                    in
                        (case (tokenizeQuotes str) of
                            "push "::s::nil => ["push", "\"" ^ s ^ "\""]
                            | _ => tokenizeSpace str)
                    end;
                fun parse(line: string) = case tokenize(line) of
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
                    | "fun"::name::param::nil => F (USUAL, name, param)
                    | "inOutFun"::name::param::nil => F (INOUT, name, param)
                    | "funEnd"::nil => FUNEND
                    | "return"::nil => RETURN
                    | "call"::nil => CALL
                    | _ => PUSH (ERROR);
            in
                (case listLines of
                    line::rest => (case parse(line) of
                        PUSH (NAME n) => get_instructions(rest, resultInstructions @ [PUSH (NAME n)], n::namespace)
                        | F (t, name, param) => (
                            let
                                val (leftOvers, body, names) = get_instructions(rest, [], []);
                            in
                                get_instructions(leftOvers, resultInstructions @ [FUN (name, t, param, names, body)], name::namespace)
                            end)
                        | FUNEND => (rest, resultInstructions, namespace)
                        | othercmd => get_instructions(rest, resultInstructions @ [othercmd], namespace))
                    | _ => (listLines, resultInstructions, namespace))
            end;

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
                    env: string -> VAL, envS: (string -> VAL) list) =
            let
                fun test (returnedStack: VAL list, returnedstackS: (VAL list) list,
                            returnedScope: string -> VAL, returnedScopeS: (string -> VAL) list) =
                    case returnedStack of
                        ERROR::rest => (ERROR::stack, stackS, env, envS)
                        | _ => (returnedStack, returnedstackS, returnedScope, returnedScopeS);
                fun binary_ariphmetic () = case stack of
                    (INT f)::(NAME s)::rest =>
                        test(execute(instruction, (INT f)::(env s)::rest, stackS, env, envS))
                    | (NAME f)::(INT s)::rest =>
                        test(execute(instruction, (env f)::(INT s)::rest, stackS, env, envS))
                    | (NAME f)::(NAME s)::rest =>
                        test(execute(instruction, (env f)::(env s)::rest, stackS, env, envS))
                    | _ => (ERROR::stack, stackS, env, envS);
                fun binary_logic () = case stack of
                    (BOOL f)::(NAME s)::rest =>
                        test(execute(instruction, (BOOL f)::(env s)::rest, stackS, env, envS))
                    | (NAME f)::(BOOL s)::rest =>
                        test(execute(instruction, (env f)::(BOOL s)::rest, stackS, env, envS))
                    | (NAME f)::(NAME s)::rest =>
                        test(execute(instruction, (env f)::(env s)::rest, stackS, env, envS))
                    | _ => (ERROR::stack, stackS, env, envS);
                fun create_env (names: string list, map: string -> VAL, new_env: string -> VAL) = case names of
                    name::rest => (case map(name) of
                        NOVAL => create_env(rest, map, new_env)
                        | someval => create_env(rest, map, bind(name, someval, new_env)))
                    | _ => new_env;
                fun exec_fun (funtype: VAL, body: CMD list, actualP: string, formalP: string, fenv: string -> VAL, pSt: VAL list) =
                    let
                        fun exec_body (fInstructions, (s, ss, e, es)) = case fInstructions of
                            RETURN::restBody => (s, ss, e, es)
                            | fInstruction::restBody => exec_body(restBody, execute(fInstruction, s, ss, e, es))
                            | nil => ([], ss, e, es);
                        val (rs, rss, re, res) = exec_body(body, ([], [], fenv, []));
                        val stackToReturn = case rs of
                            [] => pSt
                            | (NAME n)::tail => (case (re n) of
                                CLOSURE c => (NAME n)::pSt
                                | _ => (re n)::pSt)
                            | head::tail => head::pSt;
                    in
                        (case funtype of
                            USUAL => (stackToReturn, stackS, env, envS)
                            | _ => (stackToReturn, stackS, bind(actualP, re(formalP), env), envS))
                    end;
            in
                (case instruction of
                    PUSH (INT arg) => ((INT arg)::stack, stackS, env, envS)
                    | PUSH (STRING arg) => ((STRING arg)::stack, stackS, env, envS)
                    | PUSH (NAME arg) => ((NAME arg)::stack, stackS, env, envS)
                    | PUSH (BOOL true) => ((BOOL true)::stack, stackS, env, envS)
                    | PUSH (BOOL false) => ((BOOL false)::stack, stackS, env, envS)
                    | PUSH (ERROR) => (ERROR::stack, stackS, env, envS)
                    | POP => (case stack of
                                value::rest => (rest, stackS, env, envS)
                                | _ => (ERROR::stack, stackS, env, envS))
                    | ADD => (case stack of
                                (INT f)::(INT s)::rest => (INT(s+f)::rest, stackS, env, envS)
                                | _ => binary_ariphmetic())
                    | SUB => (case stack of
                                (INT f)::(INT s)::rest => (INT(s-f)::rest, stackS, env, envS)
                                | _ => binary_ariphmetic())
                    | MUL => (case stack of
                                (INT f)::(INT s)::rest => (INT(s*f)::rest, stackS, env, envS)
                                | _ => binary_ariphmetic())
                    | DIV => (case stack of
                                (INT 0)::s::rest => (ERROR::stack, stackS, env, envS)
                                | (INT f)::(INT s)::rest => (INT(s div f)::rest, stackS, env, envS)
                                | _ => binary_ariphmetic())
                    | REM => (case stack of
                                (INT 0)::s::rest => (ERROR::stack, stackS, env, envS)
                                | (INT f)::(INT s)::rest => (INT(s mod f)::rest, stackS, env, envS)
                                | _ => binary_ariphmetic())
                    | NEG => (case stack of
                                (INT f)::rest => (INT(~f)::rest, stackS, env, envS)
                                | (NAME f)::rest => test(execute(instruction, (env f)::rest, stackS, env, envS))
                                | _ => (ERROR::stack, stackS, env, envS))
                    | SWAP => (case stack of
                                f::s::rest => (s::f::rest, stackS, env, envS)
                                | _ => (ERROR::stack, stackS, env, envS))
                    | AND => (case stack of
                                (BOOL f)::(BOOL s)::rest => (BOOL(s andalso f)::rest, stackS, env, envS)
                                | _ => binary_logic())
                    | OR => (case stack of
                                (BOOL f)::(BOOL s)::rest => (BOOL(s orelse f)::rest, stackS, env, envS)
                                | _ => binary_logic())
                    | NOT => (case stack of
                                (BOOL f)::rest => (BOOL(not f)::rest, stackS, env, envS)
                                | (NAME f)::rest => test(execute(instruction, (env f)::rest, stackS, env, envS))
                                | _ => (ERROR::stack, stackS, env, envS))
                    | EQUAL => (case stack of
                                (INT f)::(INT s)::rest => (BOOL(s = f)::rest, stackS, env, envS)
                                | _ => binary_ariphmetic())
                    | LESSTHAN => (case stack of
                                (INT f)::(INT s)::rest => (BOOL(s < f)::rest, stackS, env, envS)
                                | _ => binary_ariphmetic())
                    | IF => (case stack of
                                f::s::(BOOL t)::rest => if t
                                    then (f::rest, stackS, env, envS)
                                    else (s::rest, stackS, env, envS)
                                | f::s::(NAME t)::rest => test(execute(instruction, f::s::(env t)::rest, stackS, env, envS))
                                | _ => (ERROR::stack, stackS, env, envS))
                    | BIND => (case stack of
                                ERROR::(NAME s)::rest => (ERROR::stack, stackS, env, envS)
                                | (NAME f)::(NAME s)::rest => (case env f of
                                        NOVAL => (ERROR::stack, stackS, env, envS)
                                        | boundv => (UNIT::rest, stackS, bind(s, boundv, env), envS))
                                | value::(NAME s)::rest => (UNIT::rest, stackS, bind(s, value, env), envS)
                                | _ => (ERROR::stack, stackS, env, envS))
                    | LET => ([], stack::stackS, env, env::envS)
                    | END => ((hd stack)::(hd stackS), tl stackS, hd envS, tl envS)
                    | FUN (name, ftype, param, names, body) =>
                            (UNIT::stack, stackS, bind(name, CLOSURE(ftype, param, body, create_env(names, env, (fn x => NOVAL))), env), envS)
                    | CALL => (case stack of
                                _::ERROR::rest => (ERROR::stack, stackS, env, envS)
                                | (NAME f)::arg::rest => (case env(f) of
                                    CLOSURE (ftype, formal_p, body, prevenv) => (case arg of
                                        NAME actual_p => (case env(actual_p) of
                                            NOVAL => (ERROR::stack, stackS, env, envS)
                                            | x => exec_fun(ftype, body, actual_p, formal_p, bind(f, env(f), bind(formal_p, x, prevenv)), rest))
                                        | someval => exec_fun(ftype, body, "", formal_p, bind(f, env(f), bind(formal_p, someval, prevenv)), rest))
                                    | _ => (ERROR::stack, stackS, env, envS))
                                | _ => (ERROR::stack, stackS, env, envS))
                    | RETURN => (stack, stackS, env, envS)
                    | _ => (ERROR::stack, stackS, env, envS))
                end;

        fun main (instructions: CMD list, (stack: VAL list, stackS: (VAL list) list,
                    env: string -> VAL, envS: (string -> VAL) list)) =
            case instructions of
                QUIT::rest => print_stack("", stack)
                | x::rest => main(rest, execute(x, stack, stackS, env, envS))
                | nil => print_stack("", stack);
        val (_, cmdInstructions, _) = get_instructions(listLines, [], []);
    in
        main (cmdInstructions, ([], [], (fn x => NOVAL), []))
    end

fun run () = let
    val names = ["input_1.txt", "input_2.txt", "input_3.txt", "input_4.txt", "input_5.txt", "input_6.txt", "input_7.txt", "input_8.txt", "input_9.txt", "input_10.txt", "input_11.txt", "input_12.txt", "input_13.txt", "input_14.txt", "input_15.txt", "input_16.txt", "input_17.txt", "input_18.txt", "input_19.txt", "input_20.txt", "input_21.txt", "input_22.txt"];
    fun loop l = case l of
        name::rest => (interpreter("tst3/"^name, "rslt3/"^name ); loop rest)
        | _ => ();
    in
        loop names
    end
