import java_cup.runtime.*;

%%
%class Lexer
%unicode
%cup
%line
%column
%states IN_COMMENT
%{
  private Symbol symbol(int type) {
    return new Symbol(type, yyline, yycolumn);
  }
  private Symbol symbol(int type, Object value) {
    return new Symbol(type, yyline, yycolumn, value);
  }
%}

// WhiteSpace 
LineTerminator = \r|\n|\r\n|" "|"\t"
InputCharacter = [^\r\n]
WhiteSpace = {LineTerminator} | [\t\f]

// Comment
Comment = {MultiLineComment} | {SingleLineComment}
MultiLineComment = "/#" [^#]* ~"#/"
SingleLineComment = "#" {InputCharacter}* {LineTerminator}?

// BasicRegex
Digit = [0-9]
Digits = {Digit}{Digit}*
Letter = [a-zA-Z] 
Letter_ = {Letter} | "_"

Identifier = {Letter}({Letter_}|{Digit})*
Integer = (0|[1-9]{Digit}*)
Float = {Integer}[.]{Digits}
Rational = ({Integer}+"_")?{Integer}"/"[1-9]{Digit}*
Character = '[\x00-\x7F]'
String = \" ([^\\\"]|\\.)* \"

%%
<YYINITIAL> {

	// Data Types
	"bool"		{ return symbol(sym.BOOL);	}
	"int"			{ return symbol(sym.INT);	}
  "float"		{ return symbol(sym.FLOAT); }
	"rat"			{ return symbol(sym.RAT);	}
	"char"		{ return symbol(sym.CHAR);	}
  "str"     { return symbol(sym.STR); }
	"seq"			{ return symbol(sym.SEQ);	}
  "dict"    { return symbol(sym.DICT); }
	
  // Key Words
  "top"			{ return symbol(sym.TOP);	}
	"thread"			{ return symbol(sym.THREAD);	}  
  "for"			{ return symbol(sym.FOR);	}  
  "of"			{ return symbol(sym.OF);	}  
  "len"			{ return symbol(sym.LEN);	}  
  "range"			{ return symbol(sym.RANGE);	}  
  "tdef"			{ return symbol(sym.TDEF);	}  
  "fdef"			{ return symbol(sym.FDEF);	}  
  "alias"			{ return symbol(sym.ALIAS);	} 
  "return"			{ return symbol(sym.RETURN);	} 
  "break"			{ return symbol(sym.BREAK);	} 
  "while"			{ return symbol(sym.WHILE);	} 
  "if"			{ return symbol(sym.IF);	} 
  "else"			{ return symbol(sym.ELSE);	} 
  "elif"			{ return symbol(sym.ELIF);	} 
  "read"			{ return symbol(sym.READ);	} 
  "print"			{ return symbol(sym.PRINT);	} 
  "main"			{ return symbol(sym.MAIN);	} 
  "in"        { return symbol(sym.IN); }

  // Symbols
  "T"			{ return symbol(sym.TRUE);	} 
  "F"			{ return symbol(sym.FALSE);	} 
  "("				{ return symbol(sym.LPAREN);		}
	")"				{ return symbol(sym.RPAREN);		}
	"["				{ return symbol(sym.LBRACKET);		}
	"]"				{ return symbol(sym.RBRACKET);		}
	"{"				{ return symbol(sym.LBRACE);		}
	"}"				{ return symbol(sym.RBRACE);		}
	":"				{ return symbol(sym.COLON);		}
	";"				{ return symbol(sym.SEMICOLON);		}
	","				{ return symbol(sym.COMMA);		}
	"."				{ return symbol(sym.DOT);		}

  // Operators
  "&&"			{ return symbol(sym.AND);		}
	"||"			{ return symbol(sym.OR);		}
  "!"				{ return symbol(sym.NOT);		}
	"+"				{ return symbol(sym.PLUS);		}
	"-"				{ return symbol(sym.MINUS);		}
	"*"				{ return symbol(sym.TIMES);		}
	"/"				{ return symbol(sym.DIVIDE);	}
	"^"				{ return symbol(sym.POWER);		}
	"<"				{ return symbol(sym.LESS);	}
	"<="			{ return symbol(sym.LESSEQ);	}
  ">"       { return symbol(sym.GREATER);	}
  ">="      { return symbol(sym.GREATEREQ);	}
	"="				{ return symbol(sym.EQUAL);		}
	"!="			{ return symbol(sym.NOTEQUAL);	}
	":="			{ return symbol(sym.ASSIGN);	}

  // Others
  {Identifier}  { return symbol(sym.ID); }
  {Integer}     { return symbol(sym.INTLITERAL); }
  {Float}       { return symbol(sym.FLOATLITERAL); }
  {Rational}    { return symbol(sym.RATLITERAL); }
  {Character}   { return symbol(sym.CHARLITERAL); }
  {String}      { return symbol(sym.STRLITERAL);}


  // Ignore
  {Comment}		  { /* do nothing */	}
  {WhiteSpace}  { /* do nothing */  }

}

/* error fallback */
[^]  {
    System.out.println("Error in line "
        + (yyline+1) +": Invalid input '" + yytext()+"'");
    return symbol(sym.ILLEGAL_CHARACTER);
}
