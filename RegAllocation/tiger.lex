type svalue = Tokens.svalue
type pos = int
type ('a, 'b) token = ('a, 'b) Tokens.token
type lexresult = (svalue,pos) token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
fun err(p1,p2) = ErrorMsg.error p1

val stringTokenContent = ref ""
val stringStartPosition = ref 0
val commentCount = ref 0
fun eof() = let val pos = hd(!linePos) in (if (!commentCount=0) then () else ErrorMsg.error pos ("Unclosed comment"); Tokens.EOF(pos,pos)) end


%% 
%header (functor TigerLexFun(structure Tokens: Tiger_TOKENS));
%s STRING COMMENT FORMATTING; 

alpha=[A-Za-z];
digit=[0-9];
ws = [\ \t];

%%
<INITIAL> "type" => (Tokens.TYPE(yypos, yypos+4));
<INITIAL> "var" => (Tokens.VAR(yypos, yypos+3));
<INITIAL> "function" => (Tokens.FUNCTION(yypos, yypos+8));
<INITIAL> "break" => (Tokens.BREAK(yypos, yypos+5));
<INITIAL> "of" => (Tokens.OF(yypos, yypos+2));
<INITIAL> "end" => (Tokens.END(yypos, yypos+3));
<INITIAL> "in" => (Tokens.IN(yypos, yypos+2));
<INITIAL> "nil" => (Tokens.NIL(yypos, yypos+3));
<INITIAL> "let" => (Tokens.LET(yypos, yypos+3));
<INITIAL> "do" => (Tokens.DO(yypos, yypos+2));
<INITIAL> "to" => (Tokens.TO(yypos, yypos+2));
<INITIAL> "for" => (Tokens.FOR(yypos, yypos+3));
<INITIAL> "while" => (Tokens.WHILE(yypos, yypos+5));
<INITIAL> "else" => (Tokens.ELSE(yypos, yypos+4));
<INITIAL> "then" => (Tokens.THEN(yypos,yypos+4));
<INITIAL> "if" => (Tokens.IF(yypos,yypos+2));
<INITIAL> "array" => (Tokens.ARRAY(yypos, yypos+5));

<INITIAL> ":=" => (Tokens.ASSIGN(yypos, yypos+2));
<INITIAL> "|" => (Tokens.OR(yypos, yypos+1));
<INITIAL> "&" => (Tokens.AND(yypos, yypos+1));
<INITIAL> ">=" => (Tokens.GE(yypos, yypos+2));
<INITIAL> ">" => (Tokens.GT(yypos, yypos+1));
<INITIAL> "<=" => (Tokens.LE(yypos, yypos+2));
<INITIAL> "<" => (Tokens.LT(yypos, yypos+1));
<INITIAL> "<>" => (Tokens.NEQ(yypos, yypos+2));
<INITIAL> "=" => (Tokens.EQ(yypos, yypos+1));
<INITIAL> "/" => (Tokens.DIVIDE(yypos, yypos+1));
<INITIAL> "*" => (Tokens.TIMES(yypos, yypos+1));
<INITIAL> "-" => (Tokens.MINUS(yypos, yypos+1));
<INITIAL> "+" => (Tokens.PLUS(yypos, yypos+1));
<INITIAL> "." => (Tokens.DOT(yypos, yypos+1));
<INITIAL> "}" => (Tokens.RBRACE(yypos, yypos+1));
<INITIAL> "{" => (Tokens.LBRACE(yypos, yypos+1));
<INITIAL> "]" => (Tokens.RBRACK(yypos, yypos+1));
<INITIAL> "[" => (Tokens.LBRACK(yypos, yypos+1));
<INITIAL> ")" => (Tokens.RPAREN(yypos, yypos+1));
<INITIAL> "(" => (Tokens.LPAREN(yypos, yypos+1));
<INITIAL> ";" => (Tokens.SEMICOLON(yypos, yypos+1));
<INITIAL> ":" => (Tokens.COLON(yypos, yypos+1));
<INITIAL> "," => (Tokens.COMMA(yypos, yypos+1));

<INITIAL> \n	=> (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
<INITIAL> {ws}+ => (continue());

<INITIAL> {alpha}[a-zA-Z0-9_]* => (Tokens.ID(yytext, yypos, yypos+size(yytext)));
<INITIAL> {digit}+             => (Tokens.INT(Option.valOf(Int.fromString yytext), yypos, yypos+size(yytext)));

<INITIAL> \" => (YYBEGIN STRING; stringTokenContent:=""; stringStartPosition:=yypos; continue());
<STRING> \" => (YYBEGIN INITIAL; Tokens.STRING(!stringTokenContent, !stringStartPosition, yypos+size(!stringTokenContent)));

<STRING> \\ => (YYBEGIN FORMATTING; continue());
<FORMATTING> \\ => (YYBEGIN STRING; continue());
<FORMATTING> [\n\r] => (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
<FORMATTING> [\ \t\f]+ => (continue());
<FORMATTING> .    => (ErrorMsg.error yypos ("Illegal character in format region: " ^ yytext); continue());

<STRING> \\n => (stringTokenContent := !stringTokenContent ^ "\n"; continue());
<STRING> \\t => (stringTokenContent := !stringTokenContent ^ "\t"; continue());
<STRING> \\{digit}{3} => (let val ascii = Option.valOf(Int.fromString (String.substring (yytext, 1, size(yytext)-1))) 
			    in 
				if (ascii < 128) 
				then stringTokenContent := !stringTokenContent ^ String.str (Char.chr ascii) 
				else ErrorMsg.error yypos ("not an ascii character " ^ yytext)
		            end; continue());
<STRING> \\\" => (stringTokenContent := !stringTokenContent ^ "\""; continue());
<STRING> \\\\ => (stringTokenContent := !stringTokenContent ^ "\\"; continue());
<STRING> \n => (ErrorMsg.error yypos ("Unclosed string."); continue());
<STRING> .     => (stringTokenContent := !stringTokenContent ^ yytext; continue());

<INITIAL> "/*" => (YYBEGIN COMMENT; commentCount := 1; continue());
<COMMENT> "*/" => (commentCount := !commentCount-1; if !commentCount=0 then YYBEGIN INITIAL else () ; continue());
<COMMENT> "/*" => (commentCount := !commentCount + 1; continue());
<COMMENT> \n => (lineNum := !lineNum+1; continue());
<COMMENT> . => (continue());

<INITIAL> .    => (ErrorMsg.error yypos ("Illegal character: " ^ yytext); continue());

