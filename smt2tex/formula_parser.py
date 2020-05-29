
import re

mapped_symbols = {
    "add": "+",
    "leq": r"\le",
    "app": r"\append",
    "or": r"\lor",
    "select": r"\mathit{read}",
    "store": r"\mathit{write}",
}

# value means parenthesis is
# potentially needed around args
infix_symbols = {
    "+": True,
    "-": True,
    "=": False,
    r"\le": False,
    r"\append": True,
    r"\lor": False,
}

def prettify_var(var):
    m = re.match(r".*?([0-9]+)$", var)
    if not m:
        return var
    ind = m.start(1)
    if ind == 0:
        return var
    else:
        return var[:ind]+"_{"+var[ind:]+"}"

def get_symbol(token):
    if token in mapped_symbols:
        return mapped_symbols[token]
    return token

def handle_forall(vars):
    res = r"\forall "
    for v in vars[:-1]:
        res += prettify_var(v[0]) + ","
    res += prettify_var(vars[-1][0]) + "."
    return res

def parse_terms(terms):
    res = ""
    for t in terms[:-1]:
        res += parse(t)[0] + ","
    res += parse(terms[-1])[0]
    return res

def parse(expr):
    result = ""
    p_outside = False
    if not isinstance(expr, list):
        return prettify_var(expr), False
    symbol = get_symbol(expr[0])
    if len(expr) == 1:
        result = symbol
    elif symbol == "forall":
        block, p_in = parse(expr[2])
        if p_in:
            result = "{}({})".format(handle_forall(expr[1]), block)
        else:
            result = handle_forall(expr[1])+block
    elif symbol == "not":
        assert len(expr) == 2
        expr = expr[1]
        symbol = get_symbol(expr[0])
        if symbol == "=":  # maybe generalize to negated_infix_symbols?
            block1, p_in1 = parse(expr[1])
            block2, p_in2 = parse(expr[2])
            if p_in1 and infix_symbols[symbol]:
                block1 = "(" + block1 + ")"
            if p_in2 and infix_symbols[symbol]:
                block2 = "(" + block2 + ")"
            result = "{} {} {}".format(block1, r" \neq ", block2)
            p_outside = True
        else:
            block, p_in = parse(expr)
            if p_in:
                block = "(" + block + ")"
            result = "\lnot {}".format(block)
            p_outside = False
    elif symbol in infix_symbols:
        block1, p_in1 = parse(expr[1])
        block2, p_in2 = parse(expr[2])
        if p_in1 and infix_symbols[symbol]:
            block1 = "(" + block1 + ")"
        if p_in2 and infix_symbols[symbol]:
            block2 = "(" + block2 + ")"
        result = "{} {} {}".format(block1, symbol, block2)
        p_outside = True
    else:
        result = "{}({})".format(symbol, parse_terms(expr[1:]))
    return result, p_outside
