
structure T = Sym

type pos = int
type lexresult = T.t

val pos = ref 0
fun eof () = T.Eof
fun error (errmsg, char, _) =
    print (String.concat ["Char ", Int.toString char, ": ", errmsg, "\n"])

val commentLevel = ref 0

%%

%structure IndentLexer

%s STRING COMMENT;

ws=[\t\ \n];
lc=[a-z];
uc=[A-Z_];
num=[0-9];
idchars={lc}|{uc}|{num}|[.<>#=+*^?@!$|&/%`~'-];
id={idchars}+|\(\);
cstart=\(\*+;
cend=\*+\);
char=\#\"..?\";
%%

<INITIAL>";COMMENT;"        => ( YYBEGIN COMMENT; lex () );
<INITIAL>";STRING;"         => ( YYBEGIN STRING; lex () );

<INITIAL>{ws}+              => ( lex () );

<INITIAL>{char}             => ( T.Char );
<INITIAL>"="                => ( T.Eq yypos );
<INITIAL>"{"                => ( T.Lbrace yypos );
<INITIAL>"["                => ( T.Lbrack yypos );
<INITIAL>"("                => ( T.Lparen yypos );

<INITIAL>")"                => ( T.Rparen );
<INITIAL>"()"               => ( T.Id yypos );
<INITIAL>"}"                => ( T.Rbrace );
<INITIAL>"]"                => ( T.Rbrack );
<INITIAL>"\\"               => ( T.Backslash );
<INITIAL>","                => ( T.Comma );
<INITIAL>":>"               => ( T.Opaque );
<INITIAL>":"                => ( T.Colon );
<INITIAL>";"                => ( T.Semi );
<INITIAL>"|"                => ( T.Bar );
<INITIAL>"=>"               => ( T.Darr );
<INITIAL>"->"               => ( T.Arr );
<INITIAL>"*"                => ( T.Star );

<INITIAL>"fn"{ws}+{id}{ws}+"=>" => ( T.FnArr yypos );
<INITIAL>"where"{ws}+"type" => ( T.Wheretype yypos );
<INITIAL>"and"{ws}+"type"   => ( T.Andtype );
<INITIAL>"withtype"         => ( T.Withtype );

<INITIAL>"and"              => ( T.And );
<INITIAL>"else"{ws}+"if"    => ( T.ElseIf );
<INITIAL>"andalso"          => ( T.Andalso );
<INITIAL>"case"             => ( T.Case yypos );
<INITIAL>"datatype"         => ( T.Datatype yypos );
<INITIAL>"else"             => ( T.Else );
<INITIAL>"end"              => ( T.End );
<INITIAL>"eqtype"           => ( T.Type yypos );
<INITIAL>"exception"        => ( T.Exception yypos );
<INITIAL>"fn"               => ( T.Fn yypos );
<INITIAL>"fun"              => ( T.Fun yypos );
<INITIAL>"functor"          => ( T.Functor yypos );
<INITIAL>"handle"           => ( T.Handle yypos );
<INITIAL>"before"           => ( T.Handle yypos );
<INITIAL>"if"               => ( T.If yypos );
<INITIAL>"in"               => ( T.In );
<INITIAL>"include"          => ( T.Include yypos );
<INITIAL>"infix"r?          => ( T.Infix yypos );
<INITIAL>"let"              => ( T.Let );
<INITIAL>"local"            => ( T.Local yypos );
<INITIAL>"of"               => ( T.Of );
<INITIAL>"open"             => ( T.Open yypos );
<INITIAL>"orelse"           => ( T.Orelse);
<INITIAL>"sig"              => ( T.Sig );
<INITIAL>"signature"        => ( T.Signature yypos );
<INITIAL>"struct"           => ( T.Struct yypos );
<INITIAL>"structure"        => ( T.Structure yypos );
<INITIAL>"then"             => ( T.Then );
<INITIAL>"type"             => ( T.Type yypos );
<INITIAL>"val"              => ( T.Val yypos );
<INITIAL>"withtype"         => ( T.Withtype );

<INITIAL>{id}        => ( T.Id yypos );

<INITIAL>\"          => ( YYBEGIN STRING; T.DoubleQuote yypos );
<STRING>\\\"         => ( lex () );
<STRING>\"           => ( YYBEGIN INITIAL; T.DoubleQuote yypos );
<STRING>.            => ( lex () );

<INITIAL>{cstart}    => ( Ref.incr commentLevel;
                          YYBEGIN COMMENT;
                          T.Cstart yypos );
<INITIAL>{cend}      => ( T.Cend );
<COMMENT>{cstart}    => ( Ref.incr commentLevel;
                          T.Cstart yypos );
<COMMENT>{cend}      => ( Ref.decr commentLevel;
                          (if !commentLevel = 0 then YYBEGIN INITIAL else ());
                          T.Cend );
<COMMENT>.           => ( lex () );

<INITIAL>.           => ( error ("ignoring illegal character" ^ yytext,
                                  yypos, yypos + size yytext); lex () );
