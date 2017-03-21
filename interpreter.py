
class ReturnException(Exception):
    pass

#### DATA TYPES ####

class Bool:
    def __init__(self, value):
        self.bool = value
    def __str__(self):
        return ":true:" if self.bool else ":false:"

class Name:
    def __init__(self, name):
        self.name = name
    def __str__(self):
        return self.name

class Error:
    def __str__(self):
        return ":error:"

class Unit:
    def __str__(self):
        return ":unit:"

#### MIXINS ####

class BinaryAriphmetic:
    def get_operands(stack, env):
        s, f = stack[-2:]
        s = int(env[s.name]) if isinstance(s, Name) else int(s)
        f = int(env[f.name]) if isinstance(f, Name) else int(f)
        return s, f

class BinaryLogic:
    def get_operands(stack, env):
        s, f = stack[-2:]
        s = env[s.name].bool if isinstance(s, Name) else s.bool
        f = env[f.name].bool if isinstance(f, Name) else f.bool
        stack.pop()
        stack.pop()
        return s, f

#### COMMANDS ####

class CMDPush:
    def __init__(self, v):
        self.value = v
    def execute(self, stack, env):
        stack.append(self.value)

class CMDPop:
    def execute(self, stack, env):
        try:
            stack.pop()
        except:
            stack.append(Error())

class CMDTrue:
    def execute(self, stack, env):
        stack.append(Bool(True))

class CMDFalse:
    def execute(self, stack, env):
        stack.append(Bool(False))

class CMDError:
    def execute(self, stack, env):
        stack.append(Error())

class CMDAdd(BinaryAriphmetic):
    def execute(self, stack, env):
        try:
            s,f = BinaryAriphmetic.get_operands(stack, env)
            stack.pop()
            stack.pop()
            stack.append(s + f)
        except:
            stack.append(Error())

class CMDSub(BinaryAriphmetic):
    def execute(self, stack, env):
        try:
            s,f = BinaryAriphmetic.get_operands(stack, env)
            stack.pop()
            stack.pop()
            stack.append(s - f)
        except:
            stack.append(Error())

class CMDMul(BinaryAriphmetic):
    def execute(self, stack, env):
        try:
            s,f = BinaryAriphmetic.get_operands(stack, env)
            stack.pop()
            stack.pop()
            stack.append(s * f)
        except:
            stack.append(Error())

class CMDDiv(BinaryAriphmetic):
    def execute(self, stack, env):
        try:
            s,f = BinaryAriphmetic.get_operands(stack, env)
            f = s // f
            stack.pop()
            stack.pop()
            stack.append(f)
        except:
            stack.append(Error())

class CMDRem(BinaryAriphmetic):
    def execute(self, stack, env):
        try:
            s,f = BinaryAriphmetic.get_operands(stack, env)
            f = s % f
            stack.pop()
            stack.pop()
            stack.append(f)
        except:
            stack.append(Error())

class CMDNeg:
    def execute(self, stack, env):
        try:
            f = stack[-1]
            f = int(env[f.name]) if isinstance(f, Name) else int(f)
            stack.pop()
            stack.append(-f)
        except:
            stack.append(Error())

class CMDSwap:
    def execute(self, stack, env):
        if len(stack) < 2:
            stack.append(Error())
        else:
            f = stack.pop()
            s = stack.pop()
            stack.append(f)
            stack.append(s)

class CMDAnd(BinaryLogic):
    def execute(self, stack, env):
        try:
            s, f = BinaryLogic.get_operands(stack, env)
            stack.append(Bool(s and f))
        except:
            stack.append(Error())

class CMDOr(BinaryLogic):
    def execute(self, stack, env):
        try:
            s, f = BinaryLogic.get_operands(stack, env)
            stack.append(Bool(s or f))
        except:
            stack.append(Error())

class CMDNot:
    def execute(self, stack, env):
        try:
            f = stack[-1]
            f = Bool(not (env[f.name].bool if isinstance(f, Name) else f.bool ))
            stack.pop()
            stack.append(f)
        except:
            stack.append(Error())

class CMDEqual(BinaryAriphmetic):
    def execute(self, stack, env):
        try:
            s,f = BinaryAriphmetic.get_operands(stack, env)
            stack.pop()
            stack.pop()
            stack.append(Bool(s == f))
        except:
            stack.append(Error())

class CMDLessThan(BinaryAriphmetic):
    def execute(self, stack, env):
        try:
            s,f = BinaryAriphmetic.get_operands(stack, env)
            stack.pop()
            stack.pop()
            stack.append(Bool(s < f))
        except:
            stack.append(Error())

class CMDBind:
    def execute(self, stack, env):
        try:
            s, f = stack[-2:]
            if isinstance(f, Error):
                return stack.append(f)
            if isinstance(f, Name):
                f = env[f.name]
            env[s.name] = f
            stack.pop()
            stack.pop()
            stack.append(Unit())
        except:
            stack.append(Error())

class CMDIf:
    def execute(self, stack, env):
        try:
            t, s, f = stack[-3:]
            t = env[t.name].bool if isinstance(t, Name) else t.bool
            stack.pop()
            stack.pop()
            stack.pop()
            stack.append(f if t else s)
        except:
            stack.append(Error())

class CMDQuit:
    pass

class CMDEnd:
    pass

class CMDFunEnd:
    pass

class CMDReturn:
    def execute(self, stack, env):
        raise ReturnException()

class CMDLet():
    def __init__(self):
        self.body = []
    def append(self, instr):
        self.body.append(instr)
    def execute(self, stack, env):
        lstack = []
        lenv = env.copy()
        for cmd in self.body:
            cmd.execute(lstack, lenv)
        if lstack:
            stack.append(lstack[-1])

class CMDFun:
    def __init__(self, typ, name, param):
        self.type = typ
        self.name = name
        self.param = param
        self.body = []
    def append(self, instr):
        self.body.append(instr)
    def set_arg(self, arg):
        self.env[self.param] = arg
    def execute(self, stack, env):
        env[self.name] = self
        self.env = env.copy()
        stack.append(Unit())

class CMDCall():
    def execute(self, stack, env):
        try:
            arg, fun = stack[-2:]
            fun = env[fun.name]
            if isinstance(arg, Error):
                return stack.append(arg)
            if isinstance(arg, Name):
                fun.set_arg(env[arg.name])
            else:
                fun.set_arg(arg)
                arg = None
            fstack, fenv = [], fun.env.copy()
            for cmd in fun.body:
                cmd.execute(fstack, fenv)
            stack.pop()
            stack.pop()
            if fun.type == "inOutFun" and arg:
                env[arg.name] = fenv[fun.param]
        except ReturnException:
            stack.pop()
            stack.pop()
            top = None
            if fstack:
                top = fstack[-1]
            if isinstance(top, Name):
                refers = fenv.get(top.name, top)
                stack.append(refers if not isinstance(refers, CMDFun) else top)
            else:
                stack.append(top)
            if fun.type == "inOutFun" and arg:
                env[arg.name] = fenv[fun.param]
        except:
            stack.append(Error())

class Interpreter:
    def __init__(self,):
        self.cmdMap = {'pop': CMDPop(), ':true:': CMDTrue(), ':false:': CMDFalse(), ':error:': CMDError(), \
                        'add': CMDAdd(), 'sub': CMDSub(), 'mul': CMDMul(), 'div': CMDDiv(), 'rem': CMDRem(), \
                        'neg': CMDNeg(), 'swap': CMDSwap(), 'quit': CMDQuit(), 'and': CMDAnd(), 'or': CMDOr(), \
                        'not': CMDNot(), 'equal': CMDEqual(), 'lessThan': CMDLessThan(), 'bind': CMDBind(), \
                        'if': CMDIf(), 'end': CMDEnd(), 'funEnd': CMDFunEnd(), 'return': CMDReturn(), 'call': CMDCall()}
    def parse_line(self, line):
        if (line.startswith("push")):
            arg = line.split("push")[-1].strip()
            if arg.isdigit() or (arg[0] == '-' and arg[1:].isdigit()):
                return CMDPush(int(arg))
            elif arg.isalnum():
                return CMDPush(Name(arg))
            elif arg.startswith('"') and arg.endswith('"'):
                return CMDPush(arg[1:-1])
            return self.cmdMap[':error:']
        elif (line.startswith("fun ") or line.startswith("inOutFun")):
            t, n, p = line.split()
            return CMDFun(t, n, p)
        elif line == "let":
            return CMDLet()
        return self.cmdMap.get(line, CMDError())
    def run(self, strIn, strOut):
        inFile = open(strIn, mode='r')
        listInstructions = [[]]
        for line in inFile.readlines():
            instruction = self.parse_line(line.strip())
            if isinstance(instruction, CMDFun) or isinstance(instruction, CMDLet):
                listInstructions[-1].append(instruction)
                listInstructions.append(instruction)
            elif isinstance(instruction, CMDEnd) or isinstance(instruction, CMDFunEnd):
                listInstructions.pop()
            elif isinstance(instruction, CMDQuit):
                break
            else:
                listInstructions[-1].append(instruction)
        inFile.close()
        stack, environment = [], {}
        for cmd in listInstructions[-1]:
            cmd.execute(stack, environment)
        with open(strOut, mode='w') as out:
            stack.reverse()
            for i in stack:
                out.write(str(i) + "\n")

__interpreter = Interpreter()

def interpreter(strIn, strOut):
    __interpreter.run(strIn, strOut)
