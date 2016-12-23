#!/usr/bin/python
# An implementation of the evaluation parsing challenge from the Twitter
# coding challenge

class tree:
    def __init__(self, root, *children):
        self.root = root
        self.children = children
    def __str__(self):
        result = str(self.root)
        for child in self.children:
            assert type(child) == tree
            result += ('(%s)' if child.children else '%s') % str(child)
        return result

def match_paren(s, n=0):
    parens = 1
    for i in range(n+1, len(s)):
        if s[i] == '(': parens += 1
        if s[i] == ')': parens -= 1
        if parens == 0: return i
    else:
        raise ValueError('No matching parenthesis found')

# there is definitely a shorter way with regexes, but those make my head
# hurt if i try too long
def make_tree(s):
    s = s.replace(' ', '')
    i = 0
    children = []
    if s[0] == '(':
        i = match_paren(s)
        root = make_tree(s[1:i])
    else: root = tree(s[0])
    i += 1
    while i < len(s):
        if s[i] == '(':
            j = match_paren(s, i)
            children.append(make_tree(s[i+1:j]))
            i = j+1
        else:
            children.append(make_tree(s[i]))
            i += 1
    return tree(root, *children)

def main(args):
    if len(args) < 2:
        print('Usage: ./binding.py STRING')
        return
    print(make_tree(''.join(args[1:])))

if __name__ == '__main__':
    import sys
    main(sys.argv)

