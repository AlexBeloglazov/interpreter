
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

class CMDAdd():
    def execute(self, stack, env):
        try:
            s, f = stack[-2:]
            s = int(env[s.name]) if isinstance(s, Name) else int(s)
            f = int(env[f.name]) if isinstance(f, Name) else int(f)
            stack.pop()
            stack.pop()
            stack.append(s + f)
        except:
            stack.append(Error())

class CMDSub():
    def execute(self, stack, env):
        try:
            s, f = stack[-2:]
            s = int(env[s.name]) if isinstance(s, Name) else int(s)
            f = int(env[f.name]) if isinstance(f, Name) else int(f)
            stack.pop()
            stack.pop()
            stack.append(s - f)
        except:
            stack.append(Error())

class CMDMul():
    def execute(self, stack, env):
        try:
            s, f = stack[-2:]
            s = int(env[s.name]) if isinstance(s, Name) else int(s)
            f = int(env[f.name]) if isinstance(f, Name) else int(f)
            stack.pop()
            stack.pop()
            stack.append(s * f)
        except:
            stack.append(Error())

class CMDDiv():
    def execute(self, stack, env):
        try:
            s, f = stack[-2:]
            s = int(env[s.name]) if isinstance(s, Name) else int(s)
            f = int(env[f.name]) if isinstance(f, Name) else int(f)
            f = s // f
            stack.pop()
            stack.pop()
            stack.append(f)
        except:
            stack.append(Error())

class CMDRem():
    def execute(self, stack, env):
        try:
            s, f = stack[-2:]
            s = int(env[s.name]) if isinstance(s, Name) else int(s)
            f = int(env[f.name]) if isinstance(f, Name) else int(f)
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

class CMDQuit:
    pass

class Interpreter:
    def __init__(self,):
        self.cmdMap = {'pop': CMDPop(), ':true:': CMDTrue(), ':false:': CMDFalse(), ':error:': CMDError(), \
                        'add': CMDAdd(), 'sub': CMDSub(), 'mul': CMDMul(), 'div': CMDDiv(), 'rem': CMDRem(), \
                        'neg': CMDNeg(), 'swap': CMDSwap(), 'quit': CMDQuit()}
    def parse_line(self, line):
        if (line.startswith("push")):
            arg = line.split("push")[-1].strip()
            if arg.isdigit() or (arg[0] == '-' and arg[1:].isdigit()):
                return CMDPush(int(arg))
            elif arg.isalnum():
                return CMDPush(Name(arg))
            elif arg.startswith('"') and arg.endswith('"'):
                return CMDPush(arg[1:-1])
            return CMDError()
        return self.cmdMap.get(line, CMDError())
    def run(self, strIn, strOut):
        inFile = open(strIn, mode='r')
        listInstructions = [[]]
        for line in inFile.readlines():
            instruction = self.parse_line(line.strip())
            if isinstance(instruction, CMDQuit):
                break
            listInstructions[-1].append(instruction)
        inFile.close()
        stack, environment = [], {}
        for cmd in listInstructions[-1]:
            cmd.execute(stack, environment)
        with open(strOut, mode='w', buffering=1000) as out:
            stack.reverse()
            for i in stack:
                out.write(str(i) + "\n")

__interpreter = Interpreter()

def interpreter(strIn, strOut):
    __interpreter.run(strIn, strOut)
