import os
from Parser import Parser

prog = '''
    /*
    comment
    */
    
    //another comment
    
    var i SlavaInt = 5;
    var b SlavaInt = 10;
    { slava_write(a); }
    whilava(i < 10) {
        if(i >= 7)
            slava_write(i);
        i++;
    }
    var c SlavaString = "10";
'''

parser = Parser()
res = parser.parse(prog)
print(*res.tree, sep=os.linesep)
