
import re

tokenizer = r'''\s*(,@|[('`,)]|"(?:[\\].|[^\\"])*"|;.*|[^\s('"`,;)]*)(.*)'''

def get_tokens(string):
    tokens = []
    while string:
        token, string = re.match(tokenizer, string).groups()
        if token and not token.startswith(';'):
            tokens.append(token)
    return tokens

def get_lists(tokens):
    lists = []
    i = 0
    while i < len(tokens):
        if tokens[i] == '(':
            l, end = get_lists(tokens[i+1:])
            i += end+2
            lists.append(l)
        elif tokens[i] == ")":
            return lists, i
        else:
            lists.append(tokens[i])
            i += 1
    return lists, len(tokens)

def parse_file(filename):
    tokens = []
    with open(filename) as file:
        for line in file:
            tokens.extend(get_tokens(line))
    lists, _ = get_lists(tokens)
    return lists
