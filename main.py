import os
from Parser import Parser

prog = '''
    
    var i SlavaString = 5;
'''

prog2 = '''
var b SlavaString = 10;
    do {
        if(i >= 7)
            logprint(i);
        i++;
    } while(i < 10)
    var c SlavaString = "10";
'''
parser = Parser()
res = parser.parse(prog)
print(*res.tree, sep=os.linesep)
