"""SPI - Simple Pascal Interpreter. Part 19"""

import argparse
import sys
#import sympy
#from sympy import symbols, simplify
from enum import Enum
from sympy import *
import copy
import threading

_SHOULD_LOG_SCOPE = False  # see '--scope' command line option
_SHOULD_LOG_STACK = False  # see '--stack' command line option
_SHOULD_LOG_ALL = False # see --all comand line option
# tm=threading.Event()
# t1=threading.Event()

class ProcessedPath():
    # Masīvs apzīmē, vai path jāiet ifā jāiet pa true vai false zaru. 0 - true, 1 - false
    p = []
    # i norāda, cik tālu ifu struktūrā konkrētajā patha iziešanas reizē ir tikts
    i = 0

    def __init__(self):
        self.p.append("0")
        self.i = 1
        if _SHOULD_LOG_ALL:
            print(f'init: self._path[0]: {self.p[0]}')

    def isDone(self):
        #print(f'isDone: self._path[0]: {self.p[0]}')
        if self.p[0] == "0":
            return False
        else:
            return True

    def nextPath(self):
        # Izdzēš no beigām visus masīva elementus, kas satur 1, pirmo, kas nesatur 1 uzstādā uz 1
        if _SHOULD_LOG_ALL:
            print(f'nextPath: self._path[0]: {self.p[0]}')
            print(f'nextPath: len(self._path): {len(self.p)}')
        while self.p[len(self.p)-1] == "1":
            self.p.pop(len(self.p)-1)
        self.p[len(self.p)-1] = "1"
        if _SHOULD_LOG_ALL:
            print(f'nextPath: self._path[0]: {self.p[0]}')
            print(f'nextPath: len(self._path): {len(self.p)}')
        self.i = 1

    def nextIf(self):
        if len(self.p) == self.i:
            # nepieciešams pielikt vēl vienu elementu un uzstādīt to uz 0
            if _SHOULD_LOG_ALL:
                print(f'ejam pa true i: {self.i}')
            self.p.append("0")
            self.i = self.i + 1
            return True
        elif self.p[self.i] == "1":
            if _SHOULD_LOG_ALL:
                print(f'nextIf ejam pa false 2, i: {self.i}')
            self.i = self.i + 1
            return False
        else:
            if _SHOULD_LOG_ALL:
                print(f'nextIf ejam pa true 1, i: {self.i}')
            if len(self.p) == self.i + 1:
                self.p[self.i] = "1"
            self.i = self.i + 1
            return True

    def ival(self):
        return str(self.i)

    def printPath(self):
        return(' '.join(self.p)) 



class Path():
    def __init__(self):      
        self._path = ''

    def addLine(self, lineno):
        if _SHOULD_LOG_ALL:
            print(f'Path.addLine: {str(lineno)}')  
        if self._path=='' or self._path[-1]=='(':
            self._path = self._path + lineno
        else:
            self._path = self._path + "," + lineno

    def addProc(self, name):
        if _SHOULD_LOG_ALL:
            print(f'Path.addProc: {name}')        
        if self._path=='':
            self._path = name + "("
        else:
            self._path = self._path + "," + name + "("   

    def addTransaction(self, name):
        if self._path=='' or self._path[-1]=='(':
            self._path = self._path + name + "("
        else:
            self._path = self._path + "," + name + "("   

    def addClosPar(self):
        self._path=self._path+")"



class Criteria():
    def __init__(self):      
        self._criteria = ""

    def addTrueCriteria(self, crit):
        if _SHOULD_LOG_ALL:
            print(f'Criteria.addTrueCriteria: {crit}')        
        if self._criteria == "":
            self._criteria =  crit
        else:          
            self._criteria = "(" + str(self._criteria) + ") and (" + str(crit) + ")"

    def addFalseCriteria(self, crit):

        if self._criteria == "":
            self._criteria =  "not (" + str(crit) + ")"
        else:          
            self._criteria = "(" + str(self._criteria) + ") and not (" + str(crit) + ")"

class arCounter():
    def __init__(self):
        self.count = 1

    def get(self):
        return self.count

    def increase(self):
        self.count = self.count + 1

class ErrorCode(Enum):
    UNEXPECTED_TOKEN = 'Unexpected token'
    ID_NOT_FOUND     = 'Identifier not found'
    DUPLICATE_ID     = 'Duplicate id found'


class Error(Exception):
    def __init__(self, error_code=None, token=None, message=None):
        self.error_code = error_code
        self.token = token
        # add exception class name before the message
        self.message = f'{self.__class__.__name__}: {message}'


class LexerError(Error):
    pass


class ParserError(Error):
    pass


class SemanticError(Error):
    pass


###############################################################################
#                                                                             #
#  LEXER                                                                      #
#                                                                             #
###############################################################################


class TokenType(Enum):
    # single-character token types
    PLUS          = '+'
    MINUS         = '-'
    MUL           = '*'
    FLOAT_DIV     = '/'
    LPAREN        = '('
    RPAREN        = ')'
    SEMI          = ';'
    DOT           = '.'
    COLON         = ':'
    COMMA         = ','
    QMARK         = '"'
    LESS          = '<'
    MORE          = '>'
    AMPERSAND     = '&'    
    # block of reserved words
    PROGRAM       = 'PROGRAM'  # marks the beginning of the block
    ENDPROG       = 'ENDPROG'
    STARTPROC     = 'STARTPROC'
    ENDPROC       = 'ENDPROC'
    BEGINTRAN     = 'BEGINTRAN'
    COMMITTRAN    = 'COMMITTRAN'
    CONCURRENT    = 'CONCURRENT'
    IF            = 'IF'
    THEN          = 'THEN'
    ELSE          = 'ELSE'
    ENDIF         = 'ENDIF'
    PRINT         = 'PRINT'
    INTEGER       = 'INTEGER'
    REAL          = 'REAL'
    INTEGER_DIV   = 'DIV'
    VAR           = 'VAR'
    PROCEDURE     = 'PROCEDURE'
    BEGIN         = 'BEGIN'
    END           = 'END'      # marks the end of the block
    # misc
    ID            = 'ID'
    INTEGER_CONST = 'INTEGER_CONST'
    REAL_CONST    = 'REAL_CONST'
    EQUAL         = '=='
    ASSIGN        = '='
    EOF           = 'EOF'



class Symval:
    def __init__(self):
        self.symbols={}


    def add(self, name, value):
        pass

class Token:
    def __init__(self, type, value, lineno=None, column=None):
        self.type = type
        self.value = value
        self.lineno = lineno
        self.column = column

    def __str__(self):
        """String representation of the class instance.

        Example:
            >>> Token(TokenType.INTEGER, 7, lineno=5, column=10)
            Token(TokenType.INTEGER, 7, position=5:10)
        """
        return 'Token({type}, {value}, position={lineno}:{column})'.format(
            type=self.type,
            value=repr(self.value),
            lineno=self.lineno,
            column=self.column,
        )

    def __repr__(self):
        return self.__str__()


def _build_reserved_keywords():
    """Build a dictionary of reserved keywords.

    The function relies on the fact that in the TokenType
    enumeration the beginning of the block of reserved keywords is
    marked with PROGRAM and the end of the block is marked with
    the END keyword.

    Result:
        {'PROGRAM': <TokenType.PROGRAM: 'PROGRAM'>,
         'INTEGER': <TokenType.INTEGER: 'INTEGER'>,
         'REAL': <TokenType.REAL: 'REAL'>,
         'DIV': <TokenType.INTEGER_DIV: 'DIV'>,
         'VAR': <TokenType.VAR: 'VAR'>,
         'PROCEDURE': <TokenType.PROCEDURE: 'PROCEDURE'>,
         'BEGIN': <TokenType.BEGIN: 'BEGIN'>,
         'END': <TokenType.END: 'END'>}
    """
    # enumerations support iteration, in definition order
    tt_list = list(TokenType)
    start_index = tt_list.index(TokenType.PROGRAM)
    end_index = tt_list.index(TokenType.END)
    reserved_keywords = {
        token_type.value: token_type
        for token_type in tt_list[start_index:end_index + 1]
    }
    return reserved_keywords


RESERVED_KEYWORDS = _build_reserved_keywords()


class Lexer:
    def __init__(self, text):
        # client string input, e.g. "4 + 2 * 3 - 6 / 2"
        self.text = text
        # self.pos is an index into self.text
        self.pos = 0
        self.current_char = self.text[self.pos]
        # token line number and column number
        self.lineno = 1
        self.column = 1

    def error(self):
        s = "Lexer error on '{lexeme}' line: {lineno} column: {column}".format(
            lexeme=self.current_char,
            lineno=self.lineno,
            column=self.column,
        )
        raise LexerError(message=s)

    def advance(self):
        """Advance the `pos` pointer and set the `current_char` variable."""
        if self.current_char == '\n':
            self.lineno += 1
            self.column = 0

        self.pos += 1
        if self.pos > len(self.text) - 1:
            self.current_char = None  # Indicates end of input
        else:
            self.current_char = self.text[self.pos]
            self.column += 1

    def peek(self):
        peek_pos = self.pos + 1
        if peek_pos > len(self.text) - 1:
            return None
        else:
            return self.text[peek_pos]

    def skip_whitespace(self):
        while self.current_char is not None and self.current_char.isspace():
            self.advance()

    def skip_comment(self):
        while self.current_char != '}':
            self.advance()
        self.advance()  # the closing curly brace

    def number(self):
        """Return a (multidigit) integer or float consumed from the input."""

        # Create a new token with current line and column number
        token = Token(type=None, value=None, lineno=self.lineno, column=self.column)

        result = ''
        while self.current_char is not None and self.current_char.isdigit():
            result += self.current_char
            self.advance()

        if self.current_char == '.':
            result += self.current_char
            self.advance()

            while self.current_char is not None and self.current_char.isdigit():
                result += self.current_char
                self.advance()

            token.type = TokenType.REAL_CONST
            # token.value = float(result)
            token.value = result
        else:
            token.type = TokenType.INTEGER_CONST
            # token.value = int(result)
            token.value = result


        return token

    def string(self):
        """Return a (multicharacter) string consumed from the input."""

        # Create a new token with current line and column number
        token = Token(type=None, value=None, lineno=self.lineno, column=self.column)
        result = ''
        while self.current_char is not None and self.current_char != '"':
            result += self.current_char
            self.advance()

        token.type = TokenType.INTEGER_CONST
        token.value = result
        self.advance()

        return token

    def _id(self):
        """Handle identifiers and reserved keywords"""

        # Create a new token with current line and column number
        token = Token(type=None, value=None, lineno=self.lineno, column=self.column)

        value = ''
        while self.current_char is not None and self.current_char.isalnum():
            value += self.current_char
            self.advance()

        token_type = RESERVED_KEYWORDS.get(value.upper())

        if _SHOULD_LOG_ALL:
            print(f'Lexer._id.value:{value}, token_type:{token_type}')

        if token_type is None:
            token.type = TokenType.ID
            token.value = value
        else:
            # reserved keyword
            token.type = token_type
            token.value = value.upper()

        return token

    def get_next_token(self):
        """Lexical analyzer (also known as scanner or tokenizer)

        This method is responsible for breaking a sentence
        apart into tokens. One token at a time.
        """
        while self.current_char is not None:
            if self.current_char.isspace():
                self.skip_whitespace()
                continue

            if self.current_char == '{':
                self.advance()
                self.skip_comment()
                continue

            if self.current_char == '"':
                self.advance()
                return self.string()

            if self.current_char.isalpha():
                return self._id()

            if self.current_char.isdigit():
                return self.number()

            if self.current_char == '=' and self.peek() == '=':
                token = Token(
                    type=TokenType.EQUAL,
                    value=TokenType.EQUAL.value,  # '='
                    lineno=self.lineno,
                    column=self.column,
                )
                self.advance()
                self.advance()
                return token

            if self.current_char == '=':
                token = Token(
                    type=TokenType.ASSIGN,
                    value=TokenType.ASSIGN.value,  # '='
                    lineno=self.lineno,
                    column=self.column,
                )
                self.advance()
                return token

            # single-character token
            try:
                # get enum member by value, e.g.
                # TokenType(';') --> TokenType.SEMI
                token_type = TokenType(self.current_char)
            except ValueError:
                # no enum member with value equal to self.current_char
                self.error()
            else:
                # create a token with a single-character lexeme as its value
                token = Token(
                    type=token_type,
                    value=token_type.value,  # e.g. ';', '.', etc
                    lineno=self.lineno,
                    column=self.column,
                )
                self.advance()
                return token

        # EOF (end-of-file) token indicates that there is no more
        # input left for lexical analysis
        return Token(type=TokenType.EOF, value=None)


###############################################################################
#                                                                             #
#  PARSER                                                                     #
#                                                                             #
###############################################################################
class AST:
    pass


class BinOp(AST):
    def __init__(self, left, op, right):
        self.left = left
        self.token = self.op = op
        self.right = right
        if _SHOULD_LOG_ALL:
            print(f'AST.BinOp__init__:{op}')


class Num(AST):
    def __init__(self, token):
        self.token = token
        self.value = token.value
        if _SHOULD_LOG_ALL:
            print(f'AST.Num.__init__:{token}')

class UnaryOp(AST):
    def __init__(self, op, expr):
        self.token = self.op = op
        self.expr = expr
        if _SHOULD_LOG_ALL:
            print(f'AST.UnaryOp.__init__:{op}')

class Compound(AST):
    """Represents a 'BEGIN ... END' block"""
    def __init__(self):
        self.children = []
        if _SHOULD_LOG_ALL:
            print(f'AST.Compound.__init__')

class tCompound(AST):            
    """Represents a 'BEGINTRAN ... COMMITTRAN' block"""
    def __init__(self, name):
        self.children = []
        self.name = name
        if _SHOULD_LOG_ALL:
            print(f'AST.Compound.__init__')

class Assign(AST):
    def __init__(self, left, op, right):
        self.left = left
        self.token = self.op = op
        self.right = right
        if _SHOULD_LOG_ALL:
            print(f'AST.Assign.__init__:{op}')

class Logic_Expr(AST):
    def __init__(self, left, op, right):
        self.left = left
        self.token = self.op = op
        self.right = right
        if _SHOULD_LOG_ALL:
            print(f'AST.Logic_Expr.__init__:{op}')

class Var(AST):
    """The Var node is constructed out of ID token."""
    def __init__(self, token):
        self.token = token
        self.value = token.value
        if _SHOULD_LOG_ALL:
            print(f'AST.Var.__init__:{token}')


class NoOp(AST):
    pass


class Program(AST):
    def __init__(self, name, block):
        self.name = name
        self.block = block
        if _SHOULD_LOG_ALL:
            print(f'AST.Program.__init__:{name}')


class Block(AST):
    def __init__(self, declarations, compound_statement):
        self.declarations = declarations
        self.compound_statement = compound_statement
        if _SHOULD_LOG_ALL:
            print('AST.Block.__init__')

class sBlock(AST):
    def __init__(self, declarations, compound_statement):
        self.declarations = declarations
        self.compound_statement = compound_statement
        if _SHOULD_LOG_ALL:
            print('AST.sBlock.__init__')

class TransactionDeclaration(AST):
    def __init__(self,  transactionBlock):
        self.transactionBlock = transactionBlock
        if _SHOULD_LOG_ALL:
            print('AST.TransactionDeclaration.__init__')


class IFDeclaration(AST):
    def __init__(self,  logic_expr, true_nodes, false_nodes):
        self.logic_expr = logic_expr
        self.true_nodes = true_nodes
        self.false_nodes = false_nodes
        if _SHOULD_LOG_ALL:
            print('AST.IFDeclaration.__init__')

class PrintStr(AST):
    def __init__(self, string_expr, variable):
        self.string_expr = string_expr
        self.variable = variable
        if _SHOULD_LOG_ALL:
            print('AST.PrintStr.__init__')

class VarDecl(AST):
    def __init__(self, var_node, type_node):
        self.var_node = var_node
        self.type_node = type_node
        if _SHOULD_LOG_ALL:
            print(f'AST.VarDecl.__init__:{var_node}')

class Type(AST):
    def __init__(self, token):
        self.token = token
        self.value = token.value


class Param(AST):
    def __init__(self, var_node, type_node):
        self.var_node = var_node
        self.type_node = type_node


class ProcedureDecl(AST):
    def __init__(self, proc_name, formal_params, block_node):
        self.proc_name = proc_name
        self.formal_params = formal_params  # a list of Param nodes
        self.block_node = block_node
        if _SHOULD_LOG_ALL:
            print(f'AST.ProcedureDecl.__init__:{proc_name}')

class ProcedureCall(AST):
    def __init__(self, proc_name, actual_params, token):
        self.proc_name = proc_name
        self.actual_params = actual_params  # a list of AST nodes
        self.token = token
        # a reference to procedure declaration symbol
        self.proc_symbol = None
        if _SHOULD_LOG_ALL:
            print(f'AST.ProcedureCall.__init__:{proc_name}')

class ConcurrentCall(AST):
    def __init__(self, left, right):
        self.left = left
        self.right = right

        if _SHOULD_LOG_ALL:
            print(f'AST.ConcurrentCall')

class Parser:
    def __init__(self, lexer):
        self.lexer = lexer
        # set current token to the first token taken from the input
        self.current_token = self.get_next_token()

    def get_next_token(self):
        return self.lexer.get_next_token()

    def error(self, error_code, token):
        raise ParserError(
            error_code=error_code,
            token=token,
            message=f'{error_code.value} -> {token}',
        )

    def eat(self, token_type):
        # compare the current token type with the passed token
        # type and if they match then "eat" the current token
        # and assign the next token to the self.current_token,
        # otherwise raise an exception.
        if self.current_token.type == token_type:
            self.current_token = self.get_next_token()
        else:
            print('expected token type:')
            print(token_type)
            self.error(
                error_code=ErrorCode.UNEXPECTED_TOKEN,
                token=self.current_token,
            )
            

    def program(self):
        """program : PROGRAM variable SEMI block DOT"""
        self.eat(TokenType.PROGRAM)
        var_node = self.variable()
        prog_name = var_node.value
        # self.eat(TokenType.SEMI)
        sblock_node = self.block()
        program_node = Program(prog_name, sblock_node)
        # self.eat(TokenType.DOT)
        if _SHOULD_LOG_ALL:
            print(f'Parser.program:{prog_name}')
        return program_node

    def block(self):
        """block : declarations compound_statement"""
        declaration_nodes = self.declarations()
        compound_statement_node = self.compound_statement()
        node = Block(declaration_nodes, compound_statement_node)
        if _SHOULD_LOG_ALL:
            print('Parser.block')
        return node

    def ifblock(self):
        """ifblock : IF logic_expr THEN statement_list ENDIF"""
        logic_expr = self.logic_expr()
        true_nodes = self.statement_list()
        false_nodes = self.statement_list()
        node = IFDeclaration(logic_expr, true_nodes, false_nodes)
        if _SHOULD_LOG_ALL:
            print('Parser.block')
        return node            

    def printStr(self):
        """printStr : Print(STRING & Var)"""
        if _SHOULD_LOG_ALL:
            print('Parser.printStr')
        self.eat(TokenType.PRINT)
        self.eat(TokenType.LPAREN)
        string_expr = self.current_token.value
        if _SHOULD_LOG_ALL:
            print(f'String:{string_expr}')        
        self.eat(TokenType.INTEGER_CONST)
        self.eat(TokenType.AMPERSAND) 
        variable = self.variable()
        if _SHOULD_LOG_ALL:
            print(f'Variable:{variable}')        
        self.eat(TokenType.RPAREN)
        node = PrintStr(string_expr, variable)
        return node


    def transactionBlock(self):
        """transactionBlock :  statement_list"""
        #nodes = self.statement_list()

    def sblock(self):
        """sblock : declarations statement_list"""
        declaration_nodes = self.declarations()
        nodes = self.statement_list()

        root = Compound()
        for node in nodes:
            root.children.append(node)
        node = sBlock(declaration_nodes, root)
        if _SHOULD_LOG_ALL:
            print('Parser.sBlock')        
        return node


    def declarations(self):
        """
        declarations : (VAR (variable_declaration SEMI)+)? procedure_declaration*
        """
        declarations = []

        if self.current_token.type == TokenType.VAR:
            self.eat(TokenType.VAR)
            while self.current_token.type == TokenType.ID:
                var_decl = self.variable_declaration()
                declarations.extend(var_decl)
    #            self.eat(TokenType.SEMI)

        while self.current_token.type == TokenType.PROCEDURE:
            proc_decl = self.procedure_declaration()
            declarations.append(proc_decl)

        while self.current_token.type == TokenType.STARTPROC:
            sproc_decl = self.sprocedure_declaration()
            declarations.append(sproc_decl)

        if _SHOULD_LOG_ALL:
            print('Parser.declarations')
        return declarations

    def formal_parameters(self):
        """ formal_parameters : ID (COMMA ID)* COLON type_spec """
        param_nodes = []

        param_tokens = [self.current_token]
        self.eat(TokenType.ID)
        while self.current_token.type == TokenType.COMMA:
            self.eat(TokenType.COMMA)
            param_tokens.append(self.current_token)
            self.eat(TokenType.ID)

        self.eat(TokenType.COLON)
        type_node = self.type_spec()

        for param_token in param_tokens:
            param_node = Param(Var(param_token), type_node)
            param_nodes.append(param_node)

        if _SHOULD_LOG_ALL:
            print('formal_parameters')

        return param_nodes

    def formal_parameter_list(self):
        """ formal_parameter_list : formal_parameters
                                  | formal_parameters SEMI formal_parameter_list
        """
        # procedure Foo();
        if not self.current_token.type == TokenType.ID:
            return []

        param_nodes = self.formal_parameters()

        while self.current_token.type == TokenType.SEMI:
            self.eat(TokenType.SEMI)
            param_nodes.extend(self.formal_parameters())

        if _SHOULD_LOG_ALL:
            print('Parser.formal_parameters_list')

        return param_nodes

    def variable_declaration(self):
        """variable_declaration : ID (COMMA ID)* COLON type_spec"""
        var_nodes = [Var(self.current_token)]  # first ID
        self.eat(TokenType.ID)

        while self.current_token.type == TokenType.COMMA:
            self.eat(TokenType.COMMA)
            var_nodes.append(Var(self.current_token))
            self.eat(TokenType.ID)

        self.eat(TokenType.COLON)

        type_node = self.type_spec()
        var_declarations = [
            VarDecl(var_node, type_node)
            for var_node in var_nodes
        ]

        if _SHOULD_LOG_ALL:
            print('Parser.variable_declaration')

        return var_declarations

    def procedure_declaration(self):
        """procedure_declaration :
             PROCEDURE ID (LPAREN formal_parameter_list RPAREN)? SEMI block SEMI
        """
        self.eat(TokenType.PROCEDURE)
        proc_name = self.current_token.value
        self.eat(TokenType.ID)
        formal_params = []

        if self.current_token.type == TokenType.LPAREN:
            self.eat(TokenType.LPAREN)
            formal_params = self.formal_parameter_list()
            self.eat(TokenType.RPAREN)

        self.eat(TokenType.SEMI)
        block_node = self.block()
        proc_decl = ProcedureDecl(proc_name, formal_params, block_node)
        self.eat(TokenType.SEMI)
        if _SHOULD_LOG_ALL:
            print(f'Parser.procedure_declaration:{self.current_token}')
        return proc_decl

    def sprocedure_declaration(self):
        """sprocedure_declaration :
             STARTPROC ID (LPAREN formal_parameter_list RPAREN)? block
        """
        self.eat(TokenType.STARTPROC)
        sproc_name = self.current_token.value
        self.eat(TokenType.ID)
        formal_params = []

        if self.current_token.type == TokenType.LPAREN:
            self.eat(TokenType.LPAREN)
            formal_params = self.formal_parameter_list()
            self.eat(TokenType.RPAREN)

        "self.eat(TokenType.SEMI)"
        block_node = self.sblock()
        proc_decl = ProcedureDecl(sproc_name, formal_params, block_node)
        self.eat(TokenType.ENDPROC)
        if _SHOULD_LOG_ALL :
            print(f'Parser.sprocedure_declaration:{sproc_name}')
        return proc_decl

    def type_spec(self):
        """type_spec : INTEGER
                     | REAL
        """

        token = self.current_token
        #if _SHOULD_LOG_ALL:

        if self.current_token.type == TokenType.INTEGER:
            self.eat(TokenType.INTEGER)
        else:
            self.eat(TokenType.REAL)
        node = Type(token)



        return node

    def compound_statement(self):
        """
        compound_statement: BEGIN statement_list END
        """
        self.eat(TokenType.BEGIN)
        nodes = self.statement_list()
        self.eat(TokenType.END)

        root = Compound()
        for node in nodes:
            root.children.append(node)
            if _SHOULD_LOG_ALL:
                print(f'Compound statement:{node}')            

        if _SHOULD_LOG_ALL:
            print('Parser.compound_statement')
        return root

    def transaction_statement(self):
        """
        compound_statement: BEGINTRAN statement_list COMMITTRAN
        """
        self.eat(TokenType.BEGINTRAN)
        trans_name = self.current_token.value
        self.eat(TokenType.ID)
        if _SHOULD_LOG_ALL:
            print('Parser.Transaction_statement.begin:')         
        nodes = self.statement_list()
        self.eat(TokenType.COMMITTRAN)

        root = tCompound(trans_name)
        for node in nodes:
            root.children.append(node)

        if _SHOULD_LOG_ALL:
            print('Parser.transaction_statement.end:')

        return root

    def if_statement(self):
        """
        compound_statement: IF logic_expr THEN statement_list ENDIF
        """
        self.eat(TokenType.IF)
        if _SHOULD_LOG_ALL:
            print('Parser.IF.begin')
        logic_expr = self.logic_expr()
        self.eat(TokenType.THEN)
        if _SHOULD_LOG_ALL:
            print('Parser.visitTrueNodes')
        true_nodes = self.statement_list()
        if self.current_token.type == TokenType.ELSE :
            if _SHOULD_LOG_ALL:
                print('Parser.visitFalseNodes')
            self.eat(TokenType.ELSE)
            false_nodes = self.statement_list()
        else :
            false_nodes = []
        self.eat(TokenType.ENDIF)

        root = IFDeclaration(logic_expr, true_nodes, false_nodes)
        if _SHOULD_LOG_ALL:
            print('Parser.IF.end')

        return root

    def statement_list(self):
        """
        statement_list : statement
                       | statement SEMI statement_list
        """
        node = self.statement()

        results = [node]

        while self.current_token.type in (
                TokenType.BEGIN,
                TokenType.BEGINTRAN,
                TokenType.IF,
                TokenType.PRINT,
                TokenType.CONCURRENT,                
                TokenType.ID
        ):
            # self.eat(TokenType.SEMI)
            if _SHOULD_LOG_ALL:
                print(f'Parser.Statement_list.addstetement:{self.current_token}') 
            results.append(self.statement())
          

        if _SHOULD_LOG_ALL:
            print('Parser.statement_list')

        return results

    def statement(self):
        """
        statement : compound_statement
                  | transaction_statement
                  | if_statement
                  | proccall_statement
                  | assignment_statement
                  | printStr
                  | empty
        """
        if _SHOULD_LOG_ALL:      
            print(f'Parser.statement:{self.current_token} ,{self.current_token.type}')
        if self.current_token.type == TokenType.BEGIN:
            node = self.compound_statement()
        elif self.current_token.type == TokenType.BEGINTRAN:
            node = self.transaction_statement()
            if _SHOULD_LOG_ALL:
                print('Parser.statement:BEGINTRAN')
        elif self.current_token.type == TokenType.CONCURRENT:
            node = self.concurrentcall_statement()
            if _SHOULD_LOG_ALL:
                print('Parser.statement:CONCURRENT')
        elif self.current_token.type == TokenType.IF:
            node = self.if_statement()
            if _SHOULD_LOG_ALL:
                print('Parser.statement:IF')

        elif (self.current_token.type == TokenType.ID and
              self.lexer.current_char == '('
        ):
            node = self.proccall_statement()
        elif self.current_token.type == TokenType.ID:
            node = self.assignment_statement()
            if _SHOULD_LOG_ALL:
                print(f'Parser.statement:ID')            
        elif self.current_token.type == TokenType.PRINT:
            node = self.printStr()
            if _SHOULD_LOG_ALL:
                print(f'Parser.printStr')            
        else:
            node = self.empty()

        if _SHOULD_LOG_ALL:
            print(f'Parser.statement:{node}')

        return node

    def proccall_statement(self):
        """proccall_statement : ID LPAREN (expr (COMMA expr)*)? RPAREN"""
        token = self.current_token

        proc_name = self.current_token.value
        self.eat(TokenType.ID)
        self.eat(TokenType.LPAREN)
        actual_params = []
        if self.current_token.type != TokenType.RPAREN:
            node = self.expr()
            actual_params.append(node)

        while self.current_token.type == TokenType.COMMA:
            self.eat(TokenType.COMMA)
            node = self.expr()
            actual_params.append(node)

        self.eat(TokenType.RPAREN)

        node = ProcedureCall(
            proc_name=proc_name,
            actual_params=actual_params,
            token=token,
        )

        if _SHOULD_LOG_ALL:
            print(f'Parser.proccall_statement:{proc_name}')        

        return node

    def concurrentcall_statement(self):
        """concurrentcall_statement :    CONCURRENT LPAREN ID LPAREN (expr (COMMA expr)*)? RPAREN COMMA ID LPAREN (expr (COMMA expr)*)? RPAREN RPAREN"""

        if _SHOULD_LOG_ALL:
            print(f'Enter Parser.concurrentcall_statement')  


        token = self.current_token
        self.eat(TokenType.CONCURRENT)
        self.eat(TokenType.LPAREN)

        # 1. concurr procedūras sākums
        left=self.proccall_statement()
        self.eat(TokenType.COMMA)
        right=self.proccall_statement()
        self.eat(TokenType.RPAREN)
        node = ConcurrentCall(
            left=left,
            right=right
        )

        if _SHOULD_LOG_ALL:
            print(f'Exit Parser.concurrentcall_statement')        

        return node

    def assignment_statement(self):
        """
        assignment_statement : variable ASSIGN expr
        """
        left = self.variable()
        token = self.current_token
        self.eat(TokenType.ASSIGN)
        right = self.expr()
        node = Assign(left, token, right)

        if _SHOULD_LOG_ALL:
            print(f'Parser.assignment_statement: Variable{left}')

        return node

    def variable(self):
        """
        variable : ID
        """
        node = Var(self.current_token)
        self.eat(TokenType.ID)
        return node

    def empty(self):
        """An empty production"""
        return NoOp()

    def expr(self):
        """
        expr : term ((PLUS | MINUS) term)*
        """
        node = self.term()

        while self.current_token.type in (TokenType.PLUS, TokenType.MINUS):
            token = self.current_token
            if _SHOULD_LOG_ALL:
                print(f'Expr:{self.current_token}')
            if token.type == TokenType.PLUS:
                self.eat(TokenType.PLUS)
            elif token.type == TokenType.MINUS:
                self.eat(TokenType.MINUS)

            node = BinOp(left=node, op=token, right=self.term())

        return node

    def logic_expr(self):
        """
        logic_expr : expr (EQUAL | LESS | MORE) expr
        """
        if _SHOULD_LOG_ALL:
            print(f'logic_expr1')
 

        node = self.expr()

        if _SHOULD_LOG_ALL:
            print(f'logic_expr1.token:{self.current_token.type}')

        if self.current_token.type in (TokenType.EQUAL, TokenType.MORE, TokenType.LESS):
            token = self.current_token
            if _SHOULD_LOG_ALL:
                print(f'Expr:{self.current_token}')
            if token.type == TokenType.EQUAL:
                self.eat(TokenType.EQUAL)
            if token.type == TokenType.MORE:
                self.eat(TokenType.MORE)
            elif token.type == TokenType.LESS:
                self.eat(TokenType.LESS)

            node = Logic_Expr(left=node, op=token, right=self.expr())

        return node

    def term(self):
        """term : factor ((MUL | INTEGER_DIV | FLOAT_DIV) factor)*"""
        node = self.factor()
        if _SHOULD_LOG_ALL:
            print(f'term:{self.current_token}')
        while self.current_token.type in (
                TokenType.MUL,
                TokenType.INTEGER_DIV,
                TokenType.FLOAT_DIV,
        ):
            token = self.current_token
            if token.type == TokenType.MUL:
                self.eat(TokenType.MUL)
            elif token.type == TokenType.INTEGER_DIV:
                self.eat(TokenType.INTEGER_DIV)
            elif token.type == TokenType.FLOAT_DIV:
                self.eat(TokenType.FLOAT_DIV)

            node = BinOp(left=node, op=token, right=self.factor())

        return node

    def factor(self):
        """factor : PLUS factor
                  | MINUS factor
                  | INTEGER_CONST
                  | REAL_CONST
                  | LPAREN expr RPAREN
                  | variable
        """
        token = self.current_token
        if token.type == TokenType.PLUS:
            self.eat(TokenType.PLUS)
            node = UnaryOp(token, self.factor())
            return node
        elif token.type == TokenType.MINUS:
            self.eat(TokenType.MINUS)
            node = UnaryOp(token, self.factor())
            return node
        elif token.type == TokenType.INTEGER_CONST:
            self.eat(TokenType.INTEGER_CONST)
            return Num(token)
        elif token.type == TokenType.REAL_CONST:
            self.eat(TokenType.REAL_CONST)
            return Num(token)
        elif token.type == TokenType.LPAREN:
            self.eat(TokenType.LPAREN)
            node = self.expr()
            self.eat(TokenType.RPAREN)
            return node
        else:
            node = self.variable()
            return node

    def parse(self):
        """
        program : PROGRAM variable SEMI block DOT

        block : declarations compound_statement

        declarations : (VAR (variable_declaration SEMI)+)? procedure_declaration*

        variable_declaration : ID (COMMA ID)* COLON type_spec

        procedure_declaration :
             PROCEDURE ID (LPAREN formal_parameter_list RPAREN)? SEMI block SEMI

        formal_params_list : formal_parameters
                           | formal_parameters SEMI formal_parameter_list

        formal_parameters : ID (COMMA ID)* COLON type_spec

        type_spec : INTEGER | REAL

        compound_statement : BEGIN statement_list END

        statement_list : statement
                       | statement SEMI statement_list

        statement : compound_statement
                  | proccall_statement
                  | assignment_statement
                  | empty

        proccall_statement : ID LPAREN (expr (COMMA expr)*)? RPAREN

        concurrentcall_statement :    CONCURRENT LPAREN ID LPAREN (expr (COMMA expr)*)? RPAREN COMMA ID LPAREN (expr (COMMA expr)*)? RPAREN RPAREN

        assignment_statement : variable ASSIGN expr

        empty :

        expr : term ((PLUS | MINUS) term)*

        term : factor ((MUL | INTEGER_DIV | FLOAT_DIV) factor)*

        factor : PLUS factor
               | MINUS factor
               | INTEGER_CONST
               | REAL_CONST
               | LPAREN expr RPAREN
               | variable

        variable: ID
        """
        node = self.program()
        if self.current_token.type != TokenType.EOF:
            self.error(
                error_code=ErrorCode.UNEXPECTED_TOKEN,
                token=self.current_token,
            )

        return node


###############################################################################
#                                                                             #
#  AST visitors (walkers)                                                     #
#                                                                             #
###############################################################################

class NodeVisitor:
    def visit(self, node):
        method_name = 'visit_' + type(node).__name__
        if _SHOULD_LOG_ALL:
            print(f'Visit:{method_name}')
        visitor = getattr(self, method_name, self.generic_visit)
        return visitor(node)

    def generic_visit(self, node):
        raise Exception(f'No visit_{node.__name__} method')


###############################################################################
#                                                                             #
#  SYMBOLS, TABLES, SEMANTIC ANALYSIS                                         #
#                                                                             #
###############################################################################

class Symbol1:
    def __init__(self, name, type=None):
        self.name = name
        self.type = type
        self.scope_level = 0


class VarSymbol(Symbol1):
    def __init__(self, name, type):
        super().__init__(name, type)

    def __str__(self):
        return "<{class_name}(name='{name}', type='{type}')>".format(
            class_name=self.__class__.__name__,
            name=self.name,
            type=self.type,
        )

    __repr__ = __str__


class BuiltinTypeSymbol(Symbol1):
    def __init__(self, name):
        super().__init__(name)

    def __str__(self):
        return self.name

    def __repr__(self):
        return "<{class_name}(name='{name}')>".format(
            class_name=self.__class__.__name__,
            name=self.name,
        )


class ProcedureSymbol(Symbol1):
    def __init__(self, name, formal_params=None):
        super().__init__(name)
        # a list of VarSymbol objects
        self.formal_params = [] if formal_params is None else formal_params
        # a reference to procedure's body (AST sub-tree)
        self.block_ast = None

    def __str__(self):
        return '<{class_name}(name={name}, parameters={params})>'.format(
            class_name=self.__class__.__name__,
            name=self.name,
            params=self.formal_params,
        )

    __repr__ = __str__


class ScopedSymbolTable:
    def __init__(self, scope_name, scope_level, enclosing_scope=None):
        self._symbols = {}
        self.scope_name = scope_name
        self.scope_level = scope_level
        self.enclosing_scope = enclosing_scope

        #mainīgo siboli priekš sympy
        self._var_symbols = {}

    def _init_builtins(self):
        self.insert(BuiltinTypeSymbol('INTEGER'))
        self.insert(BuiltinTypeSymbol('REAL'))

    def __str__(self):
        h1 = 'SCOPE (SCOPED SYMBOL TABLE)'
        lines = ['\n', h1, '=' * len(h1)]
        for header_name, header_value in (
            ('Scope name', self.scope_name),
            ('Scope level', self.scope_level),
            ('Enclosing scope',
             self.enclosing_scope.scope_name if self.enclosing_scope else None
            )
        ):
            lines.append(f'{header_name:<15}: {header_value}')
        h2 = 'Scope (Scoped symbol table) contents'
        lines.extend([h2, '-' * len(h2)])
        lines.extend(
            f'{key:>7}: {value}'
            for key, value in self._symbols.items()
        )
        lines.append('\n')
        s = '\n'.join(lines)
        return s

    __repr__ = __str__

    def log(self, msg):
        if _SHOULD_LOG_SCOPE:
            print(msg)

    def insert(self, symbol):
        self.log(f'Insert: {symbol.name}')
        symbol.scope_level = self.scope_level
        self._symbols[symbol.name] = symbol

        #pievienojam mainīgo kā simbolu priekš sympy
        self._var_symbols[symbol.name]=symbols(symbol.name)

    def lookup(self, name, current_scope_only=False):
        self.log(f'Lookup: {name}. (Scope name: {self.scope_name})')
        # 'symbol' is either an instance of the Symbol class or None
        symbol = self._symbols.get(name)
        if _SHOULD_LOG_ALL:
            print(f'Lookup: {name}. (Scope name: {self.scope_name})')
            print(f'Value:{symbol}')

        if symbol is not None:
            return symbol

        if current_scope_only:
            return None

        # recursively go up the chain and lookup the name
        if self.enclosing_scope is not None:
            return self.enclosing_scope.lookup(name)


class SemanticAnalyzer(NodeVisitor):
    def __init__(self):
        self.current_scope = None

    def log(self, msg):
        if _SHOULD_LOG_SCOPE:
            print(msg)

    def error(self, error_code, token):
        raise SemanticError(
            error_code=error_code,
            token=token,
            message=f'{error_code.value} -> {token}',
        )

    def visit_Block(self, node):
        for declaration in node.declarations:
            self.visit(declaration)
        self.visit(node.compound_statement)

    def visit_sBlock(self, node):
        for declaration in node.declarations:
            self.visit(declaration)
        self.visit(node.compound_statement)

    def visit_Program(self, node):
        self.log('ENTER scope: global')
        global_scope = ScopedSymbolTable(
            scope_name='global',
            scope_level=1,
            enclosing_scope=self.current_scope,  # None
        )
        global_scope._init_builtins()
        self.current_scope = global_scope

        # visit subtree
        self.visit(node.block)

        self.log(global_scope)

        self.current_scope = self.current_scope.enclosing_scope
        self.log('LEAVE scope: global')

    def visit_Compound(self, node):
        for child in node.children:
            self.visit(child)

    def visit_Logic_Expr(self, node):
        if _SHOULD_LOG_ALL:
            print('Visit_Logic_Expr1 ')         
        self.visit(node.left)
        self.visit(node.right)
 

    def visit_tCompound(self, node):
        for child in node.children:
            self.visit(child)

    def visit_IFDeclaration(self, node):
        if _SHOULD_LOG_ALL:
            print(f'Visit_IFDeclaration: Begin17')
        exec_criteria = self.visit(node.logic_expr)
        if _SHOULD_LOG_ALL:
           print(f'Visit_IFDeclaration.exec_criteria 17: {exec_criteria}')
 
        for child in node.true_nodes:
            self.visit(child)
        for child in node.false_nodes:
            self.visit(child)

    def visit_NoOp(self, node):
        pass

    def visit_BinOp(self, node):
        self.visit(node.left)
        self.visit(node.right)

    def visit_PrintStr(self, node):
        self.visit(node.variable)

    def visit_ProcedureDecl(self, node):
        proc_name = node.proc_name
        proc_symbol = ProcedureSymbol(proc_name)
        self.current_scope.insert(proc_symbol)

        self.log(f'ENTER scope: {proc_name}')
        # Scope for parameters and local variables
        procedure_scope = ScopedSymbolTable(
            scope_name=proc_name,
            scope_level=self.current_scope.scope_level + 1,
            enclosing_scope=self.current_scope
        )
        self.current_scope = procedure_scope

        # Insert parameters into the procedure scope
        for param in node.formal_params:
            param_type = self.current_scope.lookup(param.type_node.value)
            param_name = param.var_node.value
            var_symbol = VarSymbol(param_name, param_type)
            self.current_scope.insert(var_symbol)
            proc_symbol.formal_params.append(var_symbol)

        self.visit(node.block_node)
        self.log(procedure_scope)

        self.current_scope = self.current_scope.enclosing_scope
        self.log(f'LEAVE scope: {proc_name}')

        # accessed by the interpreter when executing procedure call
        proc_symbol.block_ast = node.block_node

    def visit_VarDecl(self, node):
        type_name = node.type_node.value
        type_symbol = self.current_scope.lookup(type_name)

        # We have all the information we need to create a variable symbol.
        # Create the symbol and insert it into the symbol table.
        var_name = node.var_node.value
        var_symbol = VarSymbol(var_name, type_symbol)

        # izveidojam simbolisko mainīgo un tā vērtību
        # Symval.add(var_name, var_name+'0')

        # Signal an error if the table already has a symbol
        # with the same name
        if self.current_scope.lookup(var_name, current_scope_only=True):
            self.error(
                error_code=ErrorCode.DUPLICATE_ID,
                token=node.var_node.token,
            )

        self.current_scope.insert(var_symbol)

    def visit_Assign(self, node):
        # right-hand side
        self.visit(node.right)
        # left-hand side
        self.visit(node.left)
        if _SHOULD_LOG_ALL:
            print(f'Visit_Assign:')

    def visit_Var(self, node):
        var_name = node.value
        var_symbol = self.current_scope.lookup(var_name)
        if _SHOULD_LOG_ALL:
            print(f'Visit_VAR Value:{var_symbol}')
            print(f'Visit_Var Name:{var_name} Value:{var_symbol}')
        if var_symbol is None:
            self.error(error_code=ErrorCode.ID_NOT_FOUND, token=node.token)

    def visit_Num(self, node):
        pass

    def visit_UnaryOp(self, node):
        pass

    def visit_ProcedureCall(self, node):
        for param_node in node.actual_params:
            self.visit(param_node)

        proc_symbol = self.current_scope.lookup(node.proc_name)
        # accessed by the interpreter when executing procedure call
        node.proc_symbol = proc_symbol

    def visit_ConcurrentCall(self, node):

        if _SHOULD_LOG_ALL:
            print(f'Enter Visit_ConcurrentCall:')

        self.visit(node.left)
        self.visit(node.right)
        # accessed by the interpreter when executing procedure call

        if _SHOULD_LOG_ALL:
            print(f'Exit Visit_ConcurrentCall:')        




###############################################################################
#                                                                             #
#  INTERPRETER                                                                #
#                                                                             #
###############################################################################


class ARType(Enum):
    PROGRAM   = 'PROGRAM'
    PROCEDURE = 'PROCEDURE'


class CallStack:
    def __init__(self):
        self._records = []

    def push(self, ar):
        self._records.append(ar)

    def pop(self):
        return self._records.pop()

    def peek(self):
        return self._records[-1]

    def __str__(self):
        s = '\n'.join(repr(ar) for ar in reversed(self._records))
        s = f'CALL STACK\n{s}\n\n'
        return s

    def __repr__(self):
        return self.__str__()


class ActivationRecord:
    def __init__(self, name, type, nesting_level, enclosing_ar):
        self.name = name
        self.type = type
        self.nesting_level = nesting_level
        self.enclosing_ar = enclosing_ar
        self.arNumber = arCount.get()
        self.members = {}
        arCount.increase()

    def __setitem__(self, key, value):
        if key in self.members:
            self.members[key] = value
        else: 
            self.enclosing_ar[key] = value

    def __getitem__(self, key):
        return self.members[key]

    def get(self, key):
        if _SHOULD_LOG_ALL:
            print(f'get key:{key}')
        if key in self.members:
            return self.members.get(key)
        else:
            return self.enclosing_ar.get(key)

    def create(self, key):
        if _SHOULD_LOG_ALL:
            print(f'create key:{key}')
        self.members[key] = None

    def __str__(self):
        lines = [
            '{level}: {type} {name}'.format(
                level=self.nesting_level,
                type=self.type.value,
                name=self.name,
            )
        ]
        for name, val in self.members.items():
            lines.append(f'   {name:<20}: {val}')

        s = '\n'.join(lines)
        return s

    def __repr__(self):
        return self.__str__()


class Interpreter(NodeVisitor):
    def __init__(self, tree):
        self.tree = tree
        self.call_stack = CallStack()

    def log(self, msg):
        if _SHOULD_LOG_STACK:
            print(msg)

    def visit_Program(self, node):
        program_name = node.name
        self.log(f'ENTER: PROGRAM {program_name}')

        ar = ActivationRecord(
            name=program_name,
            type=ARType.PROGRAM,
            nesting_level=1,
            enclosing_ar=None
        )
        self.call_stack.push(ar)

        self.log(str(self.call_stack))

        self.visit(node.block)

        self.log(f'LEAVE: PROGRAM {program_name}')
        self.log(str(self.call_stack))

        self.call_stack.pop()

    def visit_Block(self, node):
        for declaration in node.declarations:
            self.visit(declaration)
        self.visit(node.compound_statement)

    def visit_sBlock(self, node):
        for declaration in node.declarations:
            self.visit(declaration)
        self.visit(node.compound_statement)

    def visit_VarDecl(self, node):
        # create variable with value None
        ar = self.call_stack.peek()
        var_name = node.var_node.value
        if _SHOULD_LOG_ALL:
            print(f'visit_VarDecl Var_name:{var_name}')
        ar.create(var_name)

    def visit_Type(self, node):
        # Do nothing
        pass

    def visit_BinOp(self, node):
        if node.op.type == TokenType.PLUS:
            # return self.visit(node.left) + self.visit(node.right)
            a = self.visit(node.left)
            b = self.visit(node.right)
            if _SHOULD_LOG_ALL:
                print(f'a: {a}')            
                print(f'b: {b}')
            return a+ "+(" + b + ")"
            #return self.visit(node.left) + "+(" + self.visit(node.right) + ")"
        elif node.op.type == TokenType.MINUS:
            # return self.visit(node.left) - self.visit(node.right)
            return self.visit(node.left) + "-(" + self.visit(node.right) + ")"
        elif node.op.type == TokenType.MUL:
            return self.visit(node.left) * self.visit(node.right)
        elif node.op.type == TokenType.INTEGER_DIV:
            return self.visit(node.left) // self.visit(node.right)
        elif node.op.type == TokenType.FLOAT_DIV:
            return float(self.visit(node.left)) / float(self.visit(node.right))

    def visit_PrintStr(self, node):
        val = self.visit(node.variable)
        strVal = node.string_expr+val
        print(strVal)



    def visit_Num(self, node):
        return node.value

    def visit_UnaryOp(self, node):
        op = node.op.type
        if op == TokenType.PLUS:
            # return +self.visit(node.expr)
            return "+(" + self.visit(node.expr) + ")"
        elif op == TokenType.MINUS:
            # return -self.visit(node.expr)
            return "-(" + self.visit(node.expr) + ")"

    def visit_Compound(self, node):
        for child in node.children:
            self.visit(child)

    def visit_tCompound(self, node):
        ExecPath.addTransaction(node.name)        
        for child in node.children:
            self.visit(child)
        ExecPath.addClosPar()

    def visit_IFDeclaration(self, node):
        #print(f'Visit_IFDeclaration2: Begin')   
        exec_criteria = simplify(self.visit(node.logic_expr))
        #print(f'Visit_IFDeclaration.logic_expr:{exec_criteria}')         
        if ProcPath.nextIf():         
            ExecCriteria.addTrueCriteria(exec_criteria)
            for child in node.true_nodes:
                #print(f'visit True nodes:')
                self.visit(child)
        else:
            ExecCriteria.addFalseCriteria(exec_criteria)
            for child in node.false_nodes:
                #print(f'visit False nodes:')
                self.visit(child)            
        

    def visit_Assign(self, node):
        var_name = node.left.value
        var_value = self.visit(node.right)
        ExecPath.addLine(str(node.token.lineno))        

        ar = self.call_stack.peek()
        if _SHOULD_LOG_ALL:
            print(f'Visit_Assign1 Name:{var_name} value:{var_value}')              
        ar[var_name] = str(simplify(var_value))
        if _SHOULD_LOG_ALL:
            print(f'Visit_Assign2 Name:{var_name} value:{var_value}')        

    def visit_Logic_Expr(self, node):
        if _SHOULD_LOG_ALL:
            print(f'Visit_Logic_Expr2 ')          
        left = self.visit(node.left)
        right = self.visit(node.right)
        op = node.op.value
        if _SHOULD_LOG_ALL:
            print(f'Visit_Logic_Expr left:{left} op:{op} right:{right}')              
        return left + op + right

    def visit_Var(self, node):
        var_name = node.value
        if _SHOULD_LOG_ALL:
            print(f'visit_Var Name:{var_name}')
        ar = self.call_stack.peek()
        if _SHOULD_LOG_ALL:
            print(f'ar:{ar}')
        var_value = ar.get(var_name)
        if _SHOULD_LOG_ALL:
            print(f'visit_Var value:{var_value}')
        return var_value

    def visit_NoOp(self, node):
        pass

    def visit_ProcedureDecl(self, node):
        pass

    def visit_ProcedureCall(self, node):
        proc_name = node.proc_name
        proc_symbol = node.proc_symbol
        ExecPath.addProc(proc_name)

        ar = ActivationRecord(
            name=proc_name,
            type=ARType.PROCEDURE,
            nesting_level=proc_symbol.scope_level + 1,
            enclosing_ar=self.call_stack.peek()
        )

        formal_params = proc_symbol.formal_params
        actual_params = node.actual_params

        for param_symbol, argument_node in zip(formal_params, actual_params):
            ar[param_symbol.name] = self.visit(argument_node)

        self.call_stack.push(ar)

        self.log(f'ENTER: PROCEDURE {proc_name}')
        self.log(str(self.call_stack))

        # evaluate procedure body
        self.visit(proc_symbol.block_ast)

        self.log(f'LEAVE: PROCEDURE {proc_name}')
        self.log(str(self.call_stack))

        ExecPath.addClosPar()
        self.call_stack.pop()

    def visit_ConcurrentCall(self, node):
        if _SHOULD_LOG_ALL:
            print(f'Enter1 visit_ConcurrentCall')
        left = self.visit(node.left)
        right = self.visit(node.right)
        if _SHOULD_LOG_ALL:
            print(f'Exit1 visit_ConcurrentCall')            

    def interpret(self):
        tree = self.tree
        if tree is None:
            return ''
        return self.visit(tree)


def main():
    global arCount 
    arCount = arCounter()
    # symval = Symval()
    parser = argparse.ArgumentParser(
        description='SPI - Simple Pascal Interpreter'
    )
    parser.add_argument('inputfile', help='Pascal source file')
    parser.add_argument(
        '--scope',
        help='Print scope information',
        action='store_true',
    )
    parser.add_argument(
        '--stack',
        help='Print call stack',
        action='store_true',
    )
    parser.add_argument(
        '--all',
        help='Print all function calls',
        action='store_true',
    )
    args = parser.parse_args()

    global _SHOULD_LOG_SCOPE, _SHOULD_LOG_STACK, _SHOULD_LOG_ALL
    _SHOULD_LOG_SCOPE, _SHOULD_LOG_STACK, _SHOULD_LOG_ALL = args.scope, args.stack, args.all
    global ProcPath, ExecPath, ExecCriteria
    global tm, t1, t2
 

    ProcPath = ProcessedPath()
    ExecPath=Path()
    ExecCriteria=Criteria()
    tm=threading.Event()
    t1=threading.Event()
    t2=threading.Event()

    text = open(args.inputfile, 'r').read()

    lexer = Lexer(text)
    try:
        parser = Parser(lexer)
        tree = parser.parse()
    except (LexerError, ParserError) as e:
        print(e.message)
        sys.exit(1)
    semantic_analyzer = SemanticAnalyzer()
 
    try:
        semantic_analyzer.visit(tree)
    except SemanticError as e:
        print(e.message)
        sys.exit(1)

    if _SHOULD_LOG_ALL:
        print(f'ProcPath.isDone: {ProcPath.isDone()}')    
    while not(ProcPath.isDone()):
        # print(f'ProcPath pirms 1. deepcopy: {ProcPath.printPath()}')          
        # print(f'ProcPath.i pirms 1. deepcopy: {ProcPath.ival()}')  
        interpreter = Interpreter(tree)
        ExecCriteria.__init__()
        ExecPath.__init__()
        interpreter.interpret()
        print(f'Path: {ExecPath._path}')
        print(f'Executing criteria: {ExecCriteria._criteria}')
        print()
        # print(f'ProcPath pēc interpreter: {ProcPath.printPath()}')   
        # print(f'ProcPath.i pēc interpreter: {ProcPath.ival()}') 
#        print(f'ProcPath1.i pēc interpreter: {ProcPath1.printPath()}')   
        ProcPath.nextPath()



if __name__ == '__main__':
    main()
