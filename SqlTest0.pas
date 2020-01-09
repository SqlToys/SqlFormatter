(* $Header: /SQL Toys/SqlFormat/SqlTest0.pas 23    19-03-10 18:05 Tomek $
   (c) Tomasz Gierka, github.com/SqlToys, 2015.05.17                          *)
{--------------------------------------  --------------------------------------}
unit SqlTest0;

interface

{---------------------------- Query test functions ----------------------------}

var TestQueryCount: Integer = 0;
    TestQueryErrors: Integer = 0;

function QueryTextCompact       ( aQuery: String ) : String;
function QueryTextRemoveComments( aQuery: String ) : String;
function QueryParseFormat       ( aQuery: String ) : String;

function TestQuery              ( aQuery: String ) : Boolean;

{------------------------------- Test functions -------------------------------}
procedure GtSqlTestRun;

implementation

uses Classes, Dialogs, SysUtils, SqlCommon, SqlParser, SqlLister;

{---------------------------- Query test functions ----------------------------}

{ compacts query with text replace function }
function QueryTextCompact;
begin
  Result := Trim(
            StringReplace( StringReplace(
            StringReplace( StringReplace(
            StringReplace( StringReplace(
            StringReplace(
            StringReplace( StringReplace(
            AnsiLowerCase(
            StringReplace( StringReplace(
            StringReplace( StringReplace(
            StringReplace( StringReplace(
            StringReplace( StringReplace(
            StringReplace( StringReplace(
            StringReplace( StringReplace(
            StringReplace( StringReplace(
            StringReplace( StringReplace(
            StringReplace( StringReplace(
            StringReplace( StringReplace(
            StringReplace( StringReplace(
            StringReplace( StringReplace(
            StringReplace( StringReplace(
            { 2^10 - da radê z 1024 spacjami pod rz¹d }
            StringReplace( StringReplace( StringReplace( StringReplace( StringReplace(
            StringReplace( StringReplace( StringReplace( StringReplace( StringReplace(
            StringReplace( StringReplace( StringReplace(
            aQuery
          , #9,     #32, [rfReplaceAll] ), #13#10, #32, [rfReplaceAll] ), ';',    #32, [rfReplaceAll] )
          , #32#32, #32, [rfReplaceAll] ), #32#32, #32, [rfReplaceAll] ), #32#32, #32, [rfReplaceAll] ), #32#32, #32, [rfReplaceAll] ), #32#32, #32, [rfReplaceAll] )
          , #32#32, #32, [rfReplaceAll] ), #32#32, #32, [rfReplaceAll] ), #32#32, #32, [rfReplaceAll] ), #32#32, #32, [rfReplaceAll] ), #32#32, #32, [rfReplaceAll] )
          , ' (', '(',   [rfReplaceAll] ), '( ', '(',   [rfReplaceAll] )
          , ' )', ')',   [rfReplaceAll] ), ') ', ')',   [rfReplaceAll] )
          , ' ,', ',',   [rfReplaceAll] ), ', ', ',',   [rfReplaceAll] )
          , ' =', '=',   [rfReplaceAll] ), '= ', '=',   [rfReplaceAll] )
          , ' !=','!=',  [rfReplaceAll] ), '!= ','!=',  [rfReplaceAll] )
          , ' <', '<',   [rfReplaceAll] ), '< ', '<',   [rfReplaceAll] )
          , ' >', '>',   [rfReplaceAll] ), '> ', '>',   [rfReplaceAll] )
          , ' +', '+',   [rfReplaceAll] ), '+ ', '+',   [rfReplaceAll] )
          , ' -', '-',   [rfReplaceAll] ), '- ', '-',   [rfReplaceAll] )
          , ' *', '*',   [rfReplaceAll] ), '* ', '*',   [rfReplaceAll] )
          , ' /', '/',   [rfReplaceAll] ), '/ ', '/',   [rfReplaceAll] )
          , ' %', '%',   [rfReplaceAll] ), '% ', '%',   [rfReplaceAll] )
          , ' ||','||',  [rfReplaceAll] ), '|| ','||',  [rfReplaceAll] )
          )
          { !!! temporary, should be removed !!! }
          , ' as ', ' ', [rfReplaceAll] ), ')as ', ')', [rfReplaceAll] )
          , 'integer', 'int', [rfReplaceAll] )
          , 'ascending', '', [rfReplaceAll] ), 'asc', '', [rfReplaceAll] )
          , ' ascending', '', [rfReplaceAll] ), ' asc', '', [rfReplaceAll] )
          , 'ascending ', '', [rfReplaceAll] ), 'asc ', '', [rfReplaceAll] )
          )
          ;
end;

{ compacts query with text replace function }
{ TODO: comments inside strings are removed !! }
function QueryTextRemoveComments;
var EOLC, CRLF, CB, CE: Integer;
begin
  Result := aQuery;

  EOLC := Pos('--', Result);
  while EOLC > 0 do begin
    CRLF := Pos(#13#10, Result, EOLC);

    if CRLF > 0
      then Result := Copy(Result, 1, EOLC -1) + ' ' + Copy(Result, CRLF + 2, Length(Result))
      else Result := Copy(Result, 1, EOLC -1);

    EOLC := Pos('--', Result);
  end;

  CB := Pos('/*', Result);
  CE := Pos('*/', Result, CB);
  while (CB > 0) and (CE > 0) do begin

    Result := Copy(Result, 1, CB -1) + ' ' + Copy(Result, CE + 2, Length(Result));

    CB := Pos('/*', Result);
    CE := Pos('*/', Result, CB);
  end;
end;

{ formats query, with lister }
function QueryParseFormat;
var lSL: TStringList;
    Parser: TGtSqlParser;
    ScriptLister: TGtSqlFormatLister;
begin
  Result := '';

  lSL := TStringList.Create;
  try
    lSL.Text := aQuery;

    Parser := TGtSqlParser.Create( nil, lSL );
    try
      Parser.BuildTokenList := False;
      Parser.Tokenize_Script;
      Parser.Parse_Statements;

      ScriptLister := TGtSqlFormatLister.Create;
      try
        ScriptLister.FormattingMode  := gtfoText;
        // SetScriptListerOptions(ScriptLister);
        // SetScriptFormatOptions(ScriptLister, True);

        ScriptLister.List_SqlParser(Parser);

        Result := ScriptLister.SL.Text;
      finally
        ScriptLister.Free;
      end;
    finally
      Parser.Free;
    end;
  except
    lSL.Free;
    raise;
  end;
end;

{ tests query, format compacted by text replace function must return same query }
function TestQuery;
var lQuery: String;
begin
  try
    Inc(TestQueryCount);

    lQuery := QueryTextCompact( QueryTextRemoveComments( QueryParseFormat( aQuery ) ) );
    aQuery := QueryTextCompact( QueryTextRemoveComments( aQuery ) );

    Result := aQuery = lQuery;
  except
    Result := False;
  //raise;
  end;

  if not Result then begin
    Inc(TestQueryErrors);
    ShowMessage('Test Query ' + IntToStr(TestQueryCount) + ' Error !!!' + #13#10 +
                'INPUT:'#13#10 + aQuery + #13#10 +
                'OUTPUT:'#13#10 + lQuery );
  end;
end;

{------------------------------- Test functions -------------------------------}
procedure GtSqlTestRun;
begin
  { basic SELECT tests first -----------------------------------------------------------------------------------------}
//TestQuery( 'SELECT a-1 FROM DUAL;' ); { ** SAME AS a - b ** }

  TestQuery( 'select 1 from dual' );
  TestQuery( 'select -1 from dual' );
//TestQuery( 'select +1 from dual' ); { ** MUST BE WRONG ** }

  { date: 2012-10-26, file: format - minus number.sql }
  TestQuery( 'select -1 from dual' );

  { date: 2013-01-08, file: Basic Queries - Expressions.sql }
  TestQuery( '/* ALGEBRA - proste operacje */' );
  TestQuery( '/* dodaæ operacje ze zmian¹ znaku - minus/reverse */' );   { TODO !!! }
  TestQuery( '/* dodaæ operacje z nawiasami */' );
  TestQuery( 'SELECT a + b ;' );
  TestQuery( 'SELECT a - b ;' );
  TestQuery( 'SELECT a * b ;' );
  TestQuery( 'SELECT a / b ;' );
  TestQuery( 'SELECT a % b ;' );
  TestQuery( 'SELECT a || b;' );

  TestQuery( '/* zmiana poziomu pojedyncza i podwójna */' );
  TestQuery( 'SELECT a + b + c;' );
  TestQuery( 'SELECT a + b - c;' );
  TestQuery( 'SELECT a + b * c;' );
  TestQuery( 'SELECT a + b / c;' );
  TestQuery( 'SELECT a + b % c;' );
  TestQuery( 'SELECT a + b || c;' );
  TestQuery( 'SELECT a - b + c;' );
  TestQuery( 'SELECT a - b - c;' );
  TestQuery( 'SELECT a - b * c;' );
  TestQuery( 'SELECT a - b / c;' );
  TestQuery( 'SELECT a - b % c;' );
  TestQuery( 'SELECT a - b || c;' );
  TestQuery( 'SELECT a * b + c;' );
  TestQuery( 'SELECT a * b - c;' );
  TestQuery( 'SELECT a * b * c;' );
  TestQuery( 'SELECT a * b / c;' );
  TestQuery( 'SELECT a * b % c;' );
  TestQuery( 'SELECT a * b || c;' );
  TestQuery( 'SELECT a / b + c;' );
  TestQuery( 'SELECT a / b - c;' );
  TestQuery( 'SELECT a / b * c;' );
  TestQuery( 'SELECT a / b / c;' );
  TestQuery( 'SELECT a / b % c;' );
  TestQuery( 'SELECT a / b || c;' );
  TestQuery( 'SELECT a % b + c;' );
  TestQuery( 'SELECT a % b - c;' );
  TestQuery( 'SELECT a % b * c;' );
  TestQuery( 'SELECT a % b / c;' );
  TestQuery( 'SELECT a % b % c;' );
  TestQuery( 'SELECT a % b || c;' );
  TestQuery( 'SELECT a || b + c;' );
  TestQuery( 'SELECT a || b - c;' );
  TestQuery( 'SELECT a || b * c;' );
  TestQuery( 'SELECT a || b / c;' );
  TestQuery( 'SELECT a || b % c;' );
  TestQuery( 'SELECT a || b || c;' );

  TestQuery( '/* czy po zmianach poziomu prawid³owe parsowanie */' );
  TestQuery( 'SELECT a + b + c + d;' );
  TestQuery( 'SELECT a + b + c - d;' );
  TestQuery( 'SELECT a + b + c * d;' );
  TestQuery( 'SELECT a + b + c / d;' );
  TestQuery( 'SELECT a + b + c % d;' );
  TestQuery( 'SELECT a + b + c || d;' );
  TestQuery( 'SELECT a + b - c + d;' );
  TestQuery( 'SELECT a + b - c - d;' );
  TestQuery( 'SELECT a + b - c * d;' );
  TestQuery( 'SELECT a + b - c / d;' );
  TestQuery( 'SELECT a + b - c % d;' );
  TestQuery( 'SELECT a + b - c || d;' );
  TestQuery( 'SELECT a + b * c + d;' );
  TestQuery( 'SELECT a + b * c - d;' );
  TestQuery( 'SELECT a + b * c * d;' );
  TestQuery( 'SELECT a + b * c / d;' );
  TestQuery( 'SELECT a + b * c % d;' );
  TestQuery( 'SELECT a + b * c || d;' );
  TestQuery( 'SELECT a + b / c + d;' );
  TestQuery( 'SELECT a + b / c - d;' );
  TestQuery( 'SELECT a + b / c * d;' );
  TestQuery( 'SELECT a + b / c / d;' );
  TestQuery( 'SELECT a + b / c % d;' );
  TestQuery( 'SELECT a + b / c || d;' );
  TestQuery( 'SELECT a + b % c + d;' );
  TestQuery( 'SELECT a + b % c - d;' );
  TestQuery( 'SELECT a + b % c * d;' );
  TestQuery( 'SELECT a + b % c / d;' );
  TestQuery( 'SELECT a + b % c % d;' );
  TestQuery( 'SELECT a + b % c || d;' );
  TestQuery( 'SELECT a + b || c + d;' );
  TestQuery( 'SELECT a + b || c - d;' );
  TestQuery( 'SELECT a + b || c * d;' );
  TestQuery( 'SELECT a + b || c / d;' );
  TestQuery( 'SELECT a + b || c % d;' );
  TestQuery( 'SELECT a + b || c || d;' );
  TestQuery( 'SELECT a - b + c + d;' );
  TestQuery( 'SELECT a - b + c - d;' );
  TestQuery( 'SELECT a - b + c * d;' );
  TestQuery( 'SELECT a - b + c / d;' );
  TestQuery( 'SELECT a - b + c % d;' );
  TestQuery( 'SELECT a - b + c || d;' );
  TestQuery( 'SELECT a - b - c + d;' );
  TestQuery( 'SELECT a - b - c - d;' );
  TestQuery( 'SELECT a - b - c * d;' );
  TestQuery( 'SELECT a - b - c / d;' );
  TestQuery( 'SELECT a - b - c % d;' );
  TestQuery( 'SELECT a - b - c || d;' );
  TestQuery( 'SELECT a - b * c + d;' );
  TestQuery( 'SELECT a - b * c - d;' );
  TestQuery( 'SELECT a - b * c * d;' );
  TestQuery( 'SELECT a - b * c / d;' );
  TestQuery( 'SELECT a - b * c % d;' );
  TestQuery( 'SELECT a - b * c || d;' );
  TestQuery( 'SELECT a - b / c + d;' );
  TestQuery( 'SELECT a - b / c - d;' );
  TestQuery( 'SELECT a - b / c * d;' );
  TestQuery( 'SELECT a - b / c / d;' );
  TestQuery( 'SELECT a - b / c % d;' );
  TestQuery( 'SELECT a - b / c || d;' );
  TestQuery( 'SELECT a - b % c + d;' );
  TestQuery( 'SELECT a - b % c - d;' );
  TestQuery( 'SELECT a - b % c * d;' );
  TestQuery( 'SELECT a - b % c / d;' );
  TestQuery( 'SELECT a - b % c % d;' );
  TestQuery( 'SELECT a - b % c || d;' );
  TestQuery( 'SELECT a - b || c + d;' );
  TestQuery( 'SELECT a - b || c - d;' );
  TestQuery( 'SELECT a - b || c * d;' );
  TestQuery( 'SELECT a - b || c / d;' );
  TestQuery( 'SELECT a - b || c % d;' );
  TestQuery( 'SELECT a - b || c || d;' );
  TestQuery( 'SELECT a * b + c + d;' );
  TestQuery( 'SELECT a * b + c - d;' );
  TestQuery( 'SELECT a * b + c * d;' );
  TestQuery( 'SELECT a * b + c / d;' );
  TestQuery( 'SELECT a * b + c % d;' );
  TestQuery( 'SELECT a * b + c || d;' );
  TestQuery( 'SELECT a * b - c + d;' );
  TestQuery( 'SELECT a * b - c - d;' );
  TestQuery( 'SELECT a * b - c * d;' );
  TestQuery( 'SELECT a * b - c / d;' );
  TestQuery( 'SELECT a * b - c % d;' );
  TestQuery( 'SELECT a * b - c || d;' );
  TestQuery( 'SELECT a * b * c + d;' );
  TestQuery( 'SELECT a * b * c - d;' );
  TestQuery( 'SELECT a * b * c * d;' );
  TestQuery( 'SELECT a * b * c / d;' );
  TestQuery( 'SELECT a * b * c % d;' );
  TestQuery( 'SELECT a * b * c || d;' );
  TestQuery( 'SELECT a * b / c + d;' );
  TestQuery( 'SELECT a * b / c - d;' );
  TestQuery( 'SELECT a * b / c * d;' );
  TestQuery( 'SELECT a * b / c / d;' );
  TestQuery( 'SELECT a * b / c % d;' );
  TestQuery( 'SELECT a * b / c || d;' );
  TestQuery( 'SELECT a * b % c + d;' );
  TestQuery( 'SELECT a * b % c - d;' );
  TestQuery( 'SELECT a * b % c * d;' );
  TestQuery( 'SELECT a * b % c / d;' );
  TestQuery( 'SELECT a * b % c % d;' );
  TestQuery( 'SELECT a * b % c || d;' );
  TestQuery( 'SELECT a * b || c + d;' );
  TestQuery( 'SELECT a * b || c - d;' );
  TestQuery( 'SELECT a * b || c * d;' );
  TestQuery( 'SELECT a * b || c / d;' );
  TestQuery( 'SELECT a * b || c % d;' );
  TestQuery( 'SELECT a * b || c || d;' );
  TestQuery( 'SELECT a / b + c + d;' );
  TestQuery( 'SELECT a / b + c - d;' );
  TestQuery( 'SELECT a / b + c * d;' );
  TestQuery( 'SELECT a / b + c / d;' );
  TestQuery( 'SELECT a / b + c % d;' );
  TestQuery( 'SELECT a / b + c || d;' );
  TestQuery( 'SELECT a / b - c + d;' );
  TestQuery( 'SELECT a / b - c - d;' );
  TestQuery( 'SELECT a / b - c * d;' );
  TestQuery( 'SELECT a / b - c / d;' );
  TestQuery( 'SELECT a / b - c % d;' );
  TestQuery( 'SELECT a / b - c || d;' );
  TestQuery( 'SELECT a / b * c + d;' );
  TestQuery( 'SELECT a / b * c - d;' );
  TestQuery( 'SELECT a / b * c * d;' );
  TestQuery( 'SELECT a / b * c / d;' );
  TestQuery( 'SELECT a / b * c % d;' );
  TestQuery( 'SELECT a / b * c || d;' );
  TestQuery( 'SELECT a / b / c + d;' );
  TestQuery( 'SELECT a / b / c - d;' );
  TestQuery( 'SELECT a / b / c * d;' );
  TestQuery( 'SELECT a / b / c / d;' );
  TestQuery( 'SELECT a / b / c % d;' );
  TestQuery( 'SELECT a / b / c || d;' );
  TestQuery( 'SELECT a / b % c + d;' );
  TestQuery( 'SELECT a / b % c - d;' );
  TestQuery( 'SELECT a / b % c * d;' );
  TestQuery( 'SELECT a / b % c / d;' );
  TestQuery( 'SELECT a / b % c % d;' );
  TestQuery( 'SELECT a / b % c || d;' );
  TestQuery( 'SELECT a / b || c + d;' );
  TestQuery( 'SELECT a / b || c - d;' );
  TestQuery( 'SELECT a / b || c * d;' );
  TestQuery( 'SELECT a / b || c / d;' );
  TestQuery( 'SELECT a / b || c % d;' );
  TestQuery( 'SELECT a / b || c || d;' );
  TestQuery( 'SELECT a % b + c + d;' );
  TestQuery( 'SELECT a % b + c - d;' );
  TestQuery( 'SELECT a % b + c * d;' );
  TestQuery( 'SELECT a % b + c / d;' );
  TestQuery( 'SELECT a % b + c % d;' );
  TestQuery( 'SELECT a % b + c || d;' );
  TestQuery( 'SELECT a % b - c + d;' );
  TestQuery( 'SELECT a % b - c - d;' );
  TestQuery( 'SELECT a % b - c * d;' );
  TestQuery( 'SELECT a % b - c / d;' );
  TestQuery( 'SELECT a % b - c % d;' );
  TestQuery( 'SELECT a % b - c || d;' );
  TestQuery( 'SELECT a % b * c + d;' );
  TestQuery( 'SELECT a % b * c - d;' );
  TestQuery( 'SELECT a % b * c * d;' );
  TestQuery( 'SELECT a % b * c / d;' );
  TestQuery( 'SELECT a % b * c % d;' );
  TestQuery( 'SELECT a % b * c || d;' );
  TestQuery( 'SELECT a % b / c + d;' );
  TestQuery( 'SELECT a % b / c - d;' );
  TestQuery( 'SELECT a % b / c * d;' );
  TestQuery( 'SELECT a % b / c / d;' );
  TestQuery( 'SELECT a % b / c % d;' );
  TestQuery( 'SELECT a % b / c || d;' );
  TestQuery( 'SELECT a % b % c + d;' );
  TestQuery( 'SELECT a % b % c - d;' );
  TestQuery( 'SELECT a % b % c * d;' );
  TestQuery( 'SELECT a % b % c / d;' );
  TestQuery( 'SELECT a % b % c % d;' );
  TestQuery( 'SELECT a % b % c || d;' );
  TestQuery( 'SELECT a % b || c + d;' );
  TestQuery( 'SELECT a % b || c - d;' );
  TestQuery( 'SELECT a % b || c * d;' );
  TestQuery( 'SELECT a % b || c / d;' );
  TestQuery( 'SELECT a % b || c % d;' );
  TestQuery( 'SELECT a % b || c || d;' );
  TestQuery( 'SELECT a || b + c + d;' );
  TestQuery( 'SELECT a || b + c - d;' );
  TestQuery( 'SELECT a || b + c * d;' );
  TestQuery( 'SELECT a || b + c / d;' );
  TestQuery( 'SELECT a || b + c % d;' );
  TestQuery( 'SELECT a || b + c || d;' );
  TestQuery( 'SELECT a || b - c + d;' );
  TestQuery( 'SELECT a || b - c - d;' );
  TestQuery( 'SELECT a || b - c * d;' );
  TestQuery( 'SELECT a || b - c / d;' );
  TestQuery( 'SELECT a || b - c % d;' );
  TestQuery( 'SELECT a || b - c || d;' );
  TestQuery( 'SELECT a || b * c + d;' );
  TestQuery( 'SELECT a || b * c - d;' );
  TestQuery( 'SELECT a || b * c * d;' );
  TestQuery( 'SELECT a || b * c / d;' );
  TestQuery( 'SELECT a || b * c % d;' );
  TestQuery( 'SELECT a || b * c || d;' );
  TestQuery( 'SELECT a || b / c + d;' );
  TestQuery( 'SELECT a || b / c - d;' );
  TestQuery( 'SELECT a || b / c * d;' );
  TestQuery( 'SELECT a || b / c / d;' );
  TestQuery( 'SELECT a || b / c % d;' );
  TestQuery( 'SELECT a || b / c || d;' );
  TestQuery( 'SELECT a || b % c + d;' );
  TestQuery( 'SELECT a || b % c - d;' );
  TestQuery( 'SELECT a || b % c * d;' );
  TestQuery( 'SELECT a || b % c / d;' );
  TestQuery( 'SELECT a || b % c % d;' );
  TestQuery( 'SELECT a || b % c || d;' );
  TestQuery( 'SELECT a || b || c + d;' );
  TestQuery( 'SELECT a || b || c - d;' );
  TestQuery( 'SELECT a || b || c * d;' );
  TestQuery( 'SELECT a || b || c / d;' );
  TestQuery( 'SELECT a || b || c % d;' );
  TestQuery( 'SELECT a || b || c || d;' );

  { date: 2013-01-15, file: minus expressions.sql }
//TestQuery( 'SELECT 1 - -1 FROM DUAL;' );     { ** MUST BE WRONG ** }

  { date: 2013-01-18, file: where to or.sql }
  TestQuery( 'SELECT  1 FROM  a WHERE a = 1 OR b IN ( 2, 3 ) ;' );

  { date: 2014-01-22, file: old BRACKETS condition 1.sql }
  TestQuery( 'SELECT 1 FROM tab WHERE (((a)+b) > c)' );

  { date: 2014-01-30, file: old BRACKETS condition 2.sql }
  TestQuery( '/* po FORMAT jest problem z niew³aœciwym listowaniem nawiasów */'#13#10 +
             'SELECT  *'#13#10 +
             'FROM    tab'#13#10 +
             'WHERE   ((a=a AND b=b)  OR  (c=c AND d=d))'#13#10 +
             'AND     ((e=e AND f=f)  OR  (g=g AND h=h)  OR  (i=i))'#13#10 +
             'OR      (j=j);' );

  { date: 2014-02-04, file: old BRACKETS condition 3.sql }
  TestQuery( 'SELECT * FROM tab WHERE (a=b) AND (c=d) ' );

  { date: 2013-12-24, file: FUNCTION param name.sql }
  TestQuery( 'SELECT fun(param_name=>1)' );

  { date: 2013-11-29, file: old EXPR double quoted.sql }
  TestQuery( 'SELECT xxx."aaa]asas"' );

  { date: 2013-06-15, file: SELECT INTO mssql.sql }
  TestQuery( 'SELECT   a'#13#10 +
             '     ,   b'#13#10 +
             '  INTO   table1'#13#10 +
             '  FROM   dual ;' );

  { date: 2013-06-15, file: SELECT INTO oracle.sql }
  TestQuery( 'SELECT   a'#13#10 +
             '     ,   b'#13#10 +
             '  INTO   c'#13#10 +
             '     ,   d'#13#10 +
             '  FROM   dual ;' );

  { date: 2013-05-31, file: LIMIT.sql }
  TestQuery( 'SELECT   *  FROM   dual  limit 5, 2' );

  { date: 2013-03-29, file: non existed alias.sql }
  TestQuery( 'SELECT   A.a'#13#10 +
             '     ,   B.b'#13#10 +
             '     ,   c.c'#13#10 +
             '  FROM   tab_a A'#13#10 +
             '     ,   tab_b B' );

  { date: 2013-03-18, file: todo select into.sql }
  TestQuery( 'select 1 into a from dual' );

  { date: 2013-03-09, file: space inside brackets.sql }
  TestQuery( 'SELECT   NVL( U.WYMIAR, 1)' );

  { date: 2014-10-21, file: parse sql_rowcount.sql }
  TestQuery( 'select sql%rowcount from dual' );

  { date: 2013-03-01, file: CRLF inside comment 2.sql }
  TestQuery( 'select 1 /*'#13#10 +
             'from dual */' );

  { date: 2014-05-27, file: parse WHERE conditions.sql }
  TestQuery( '/* too much brackets */'#13#10 +
             'SELECT * FROM tab WHERE a=a AND b=b OR c=c;' );
  TestQuery( 'SELECT * FROM tab WHERE (a=a AND b=b OR c=c);' );
  TestQuery( 'SELECT  *'#13#10 +
             'FROM    tab'#13#10 +
             'WHERE   ((a=a AND b=b)  OR  c=c);' );
  TestQuery( 'SELECT 1 FROM tab WHERE (((a)+b) > c);' );
  TestQuery( '/* po FORMAT jest problem z niew³aœciwym listowaniem nawiasów */'#13#10 +
             'SELECT  *'#13#10 +
             'FROM    tab'#13#10 +
             'WHERE   ((a=a AND b=b)  OR  c=c);' );
  TestQuery( '/* po FORMAT jest problem z niew³aœciwym listowaniem nawiasów */'#13#10 +
             'SELECT  *'#13#10 +
             'FROM    tab'#13#10 +
             'WHERE   ((a=a AND b=b)  OR  (c=c AND d=d))'#13#10 +
             'AND     ((e=e AND f=f)  OR  (g=g AND h=h)  OR  (i=i))'#13#10 +
             'OR      (j=j);' );
  TestQuery( 'SELECT * FROM tab WHERE (a=b) AND (c=d);' );

  { date: 2014-05-07, file: BRACKETS expression.sql }
  TestQuery( '         SELECT  1 + ( 2 )'#13#10 +
             '           FROM  dual ' );

  { date: 2014-05-08, file: BRACKETS expression 2.sql }
  TestQuery( 'SELECT 1;' );
  TestQuery( 'SELECT (1);' );
  TestQuery( 'SELECT ((1));' );
  TestQuery( 'SELECT 1+2+3;' );
  TestQuery( 'SELECT 1+(2+3); /* lost brackets, semicolon orphant */' );
  TestQuery( 'SELECT (1+(2+3)); /* lost brackets, semicolon orphant */' );
  TestQuery( 'SELECT ((1)+((2)+(3))); /* lost brackets, semicolon orphant */' );

  { date: 2014-05-11, file: wrong COND escalation.sql }
  TestQuery( 'SELECT * FROM tab WHERE a=a AND b=b OR c=c;' );
  TestQuery( 'SELECT * FROM tab WHERE (a=a AND b=b OR c=c);' );
  TestQuery( 'SELECT  *'#13#10 +
             'FROM    tab'#13#10 +
             'WHERE   ((a=a AND b=b)  OR  c=c);' );

  { date: 2014-04-02, file: format DISTINCT star.sql }
  TestQuery( ' SELECT   DISTINCT*'#13#10 +
             '   FROM   TAB' );

  TestQuery( 'SELECT xxx."aaa]asas";' );

  { date: 2014-05-11, file: parse EXPR misc.sql }
  TestQuery( 'select ''a''''''''b'''''' from dual;' );

  { date: 2013-01-18, file: old UNION order by.sql }
  TestQuery( 'select 1 from dual'#13#10 +
             'union'#13#10 +
             'select 2 from dual'#13#10 +
             'order by 1' );

  { date: 2012-12-30, file: Basic Queries - Conditions.sql }
  TestQuery( '/* (Line: 15) WARUNKI - operatory porównania */' );
  TestQuery( 'SELECT * FROM a WHERE a = 1;' );
  TestQuery( 'SELECT * FROM a WHERE a < 1;' );
  TestQuery( 'SELECT * FROM a WHERE a > 1;' );
  TestQuery( 'SELECT * FROM a WHERE a <= 1; /* NOT > */' );
  TestQuery( 'SELECT * FROM a WHERE a >= 1; /* NOT < */' );
  TestQuery( 'SELECT * FROM a WHERE a <> 1; /* NOT = */' );
  TestQuery( 'SELECT * FROM a WHERE a != 1; /* NOT = */' );
  TestQuery( '/* WARUNKI - podstawowe wyra¿enia z³o¿one */' );
  TestQuery( 'SELECT * FROM b WHERE a=1 OR b=2;' );
  TestQuery( 'SELECT * FROM b WHERE a=1 AND b=2;' );
  TestQuery( 'SELECT * FROM b WHERE a=1 OR b=2 OR c=3;' );
  TestQuery( 'SELECT * FROM b WHERE a=1 AND b=2 OR c=3;' );
  TestQuery( 'SELECT * FROM b WHERE a=1 OR b=2 AND c=3;' );
  TestQuery( 'SELECT * FROM b WHERE a=1 AND b=2 AND c=3;' );
  TestQuery( 'SELECT * FROM b WHERE ( a=1 OR b=2 ) OR c=3;' );
  TestQuery( 'SELECT * FROM b WHERE ( a=1 AND b=2 ) OR c=3;' );
  TestQuery( 'SELECT * FROM b WHERE ( a=1 OR b=2 ) AND c=3;' );
  TestQuery( 'SELECT * FROM b WHERE ( a=1 AND b=2 ) AND c=3;' );
  TestQuery( 'SELECT * FROM b WHERE a=1 OR ( b=2 OR c=3 );' );
  TestQuery( 'SELECT * FROM b WHERE a=1 AND ( b=2 OR c=3 );' );
  TestQuery( 'SELECT * FROM b WHERE a=1 OR ( b=2 AND c=3 );' );
  TestQuery( 'SELECT * FROM b WHERE a=1 AND ( b=2 AND c=3 );' );
  TestQuery( 'SELECT * FROM c WHERE a=1 AND ( b=2 OR c=3 ) OR d=4 AND e=5 AND f=6 OR g=7 OR ( h=8 AND i=9 );' );
  TestQuery( 'SELECT * FROM c WHERE NOT a=1 AND NOT( NOT b=2 OR NOT c=3 ) OR NOT d=4 AND NOT e=5 AND NOT f=6 OR NOT g=7 OR NOT ( NOT h=8 AND NOT i=9 );' );
  TestQuery( '/* WARUNKI - klauzule IN, BETWEEN, EXISTS, SUBQUERY, LIKE, IS NULL */' );
  TestQuery( 'SELECT * FROM d WHERE a IN (1,2);' );
  TestQuery( 'SELECT * FROM d WHERE a NOT IN (1,2);' );
  TestQuery( 'SELECT * FROM d WHERE NOT a IN (1,2);' );
  TestQuery( 'SELECT * FROM e WHERE a BETWEEN 1 AND 2;' );
  TestQuery( 'SELECT * FROM e WHERE NOT a BETWEEN 1 AND 2;' );
  TestQuery( 'SELECT * FROM e WHERE a NOT BETWEEN 1 AND 2;' );
  TestQuery( 'SELECT * FROM f WHERE EXISTS(SELECT * FROM nic);' );
  TestQuery( 'SELECT * FROM f WHERE NOT EXISTS(SELECT * FROM nic);' );
  TestQuery( 'SELECT * FROM a WHERE (SELECT a FROM b) = 1;' );
  TestQuery( 'SELECT * FROM a WHERE NOT (SELECT a FROM b) = 1;' );
  TestQuery( 'SELECT * FROM h WHERE a LIKE b;' );
  TestQuery( 'SELECT * FROM h WHERE NOT a LIKE b;' );
  TestQuery( 'SELECT * FROM h WHERE a NOT LIKE b;' );
  TestQuery( 'SELECT * FROM i WHERE a IS NULL;' );
  TestQuery( 'SELECT * FROM i WHERE NOT a IS NULL;' );
  TestQuery( 'SELECT * FROM i WHERE a IS NOT NULL;' );
  TestQuery( '/* klauzule IN, BETWEEN, EXISTS, SUBQUERY, LIKE z innymi warunkami */' );
  TestQuery( 'SELECT * FROM d WHERE b=1 OR a IN (1,2);' );
  TestQuery( 'SELECT * FROM d WHERE b=1 AND a IN (1,2);' );
  TestQuery( 'SELECT * FROM d WHERE a IN (1,2) OR b=1;' );
  TestQuery( 'SELECT * FROM d WHERE a IN (1,2) AND b=1;' );
  TestQuery( 'SELECT * FROM d WHERE b=1 OR a NOT IN (1,2);' );
  TestQuery( 'SELECT * FROM d WHERE b=1 AND a NOT IN (1,2);' );
  TestQuery( 'SELECT * FROM d WHERE a NOT IN (1,2) OR b=1;' );
  TestQuery( 'SELECT * FROM d WHERE a NOT IN (1,2) AND b=1;' );
  TestQuery( 'SELECT * FROM d WHERE b=1 OR NOT a IN (1,2);' );
  TestQuery( 'SELECT * FROM d WHERE b=1 AND NOT a IN (1,2);' );
  TestQuery( 'SELECT * FROM d WHERE NOT a IN (1,2) OR b=1;' );
  TestQuery( 'SELECT * FROM d WHERE NOT a IN (1,2) AND b=1;' );
  TestQuery( 'SELECT * FROM e WHERE c=1 OR a BETWEEN 1 AND 2;' );
  TestQuery( 'SELECT * FROM e WHERE c=1 AND a BETWEEN 1 AND 2;' );
  TestQuery( 'SELECT * FROM e WHERE a BETWEEN 1 AND 2 OR c=1;' );
  TestQuery( 'SELECT * FROM e WHERE a BETWEEN 1 AND 2 AND c=1;' );
  TestQuery( 'SELECT * FROM e WHERE c=1 OR NOT a BETWEEN 1 AND 2;' );
  TestQuery( 'SELECT * FROM e WHERE c=1 AND NOT a BETWEEN 1 AND 2;' );
  TestQuery( 'SELECT * FROM e WHERE NOT a BETWEEN 1 AND 2 OR c=1;' );
  TestQuery( 'SELECT * FROM e WHERE NOT a BETWEEN 1 AND 2 AND c=1;' );
  TestQuery( 'SELECT * FROM e WHERE c=1 OR a NOT BETWEEN 1 AND 2;' );
  TestQuery( 'SELECT * FROM e WHERE c=1 AND a NOT BETWEEN 1 AND 2;' );
  TestQuery( 'SELECT * FROM e WHERE a NOT BETWEEN 1 AND 2 OR c=1;' );
  TestQuery( 'SELECT * FROM e WHERE a NOT BETWEEN 1 AND 2 AND c=1;' );
  TestQuery( 'SELECT * FROM f WHERE c=1 OR EXISTS(SELECT * FROM nic);' );
  TestQuery( 'SELECT * FROM f WHERE c=1 AND EXISTS(SELECT * FROM nic);' );
  TestQuery( 'SELECT * FROM f WHERE EXISTS(SELECT * FROM nic) OR c=1;' );
  TestQuery( 'SELECT * FROM f WHERE EXISTS(SELECT * FROM nic) AND c=1;' );
  TestQuery( 'SELECT * FROM f WHERE c=1 OR NOT EXISTS(SELECT * FROM nic);' );
  TestQuery( 'SELECT * FROM f WHERE c=1 AND NOT EXISTS(SELECT * FROM nic);' );
  TestQuery( 'SELECT * FROM f WHERE NOT EXISTS(SELECT * FROM nic) OR c=1;' );
  TestQuery( 'SELECT * FROM f WHERE NOT EXISTS(SELECT * FROM nic) AND c=1;' );
  TestQuery( 'SELECT * FROM g WHERE c=1 OR (SELECT a FROM b) = 1;' );
  TestQuery( 'SELECT * FROM g WHERE c=1 AND (SELECT a FROM b) = 1;' );
  TestQuery( 'SELECT * FROM g WHERE (SELECT a FROM b) = 1 OR c=1;' );
  TestQuery( 'SELECT * FROM g WHERE (SELECT a FROM b) = 1 AND c=1;' );
  TestQuery( 'SELECT * FROM g WHERE c=1 OR NOT (SELECT a FROM b) = 1;' );
  TestQuery( 'SELECT * FROM g WHERE c=1 AND NOT (SELECT a FROM b) = 1;' );
  TestQuery( 'SELECT * FROM g WHERE NOT (SELECT a FROM b) = 1 OR c=1;' );
  TestQuery( 'SELECT * FROM g WHERE NOT (SELECT a FROM b) = 1 AND c=1;' );
  TestQuery( 'SELECT * FROM h WHERE c=1 OR a LIKE b;' );
  TestQuery( 'SELECT * FROM h WHERE c=1 AND a LIKE b;' );
  TestQuery( 'SELECT * FROM h WHERE a LIKE b OR c=1;' );
  TestQuery( 'SELECT * FROM h WHERE a LIKE b AND c=1;' );
  TestQuery( 'SELECT * FROM h WHERE c=1 OR NOT a LIKE b;' );
  TestQuery( 'SELECT * FROM h WHERE c=1 AND NOT a LIKE b;' );
  TestQuery( 'SELECT * FROM h WHERE NOT a LIKE b OR c=1;' );
  TestQuery( 'SELECT * FROM h WHERE NOT a LIKE b AND c=1;' );
  TestQuery( 'SELECT * FROM h WHERE c=1 OR a NOT LIKE b;' );
  TestQuery( 'SELECT * FROM h WHERE c=1 AND a NOT LIKE b;' );
  TestQuery( 'SELECT * FROM h WHERE a NOT LIKE b OR c=1;' );
  TestQuery( 'SELECT * FROM h WHERE a NOT LIKE b AND c=1;' );
  TestQuery( 'SELECT * FROM i WHERE c=1 OR a IS NULL;' );
  TestQuery( 'SELECT * FROM i WHERE c=1 AND a IS NULL;' );
  TestQuery( 'SELECT * FROM i WHERE a IS NULL OR c=1;' );
  TestQuery( 'SELECT * FROM i WHERE a IS NULL AND c=1;' );
  TestQuery( 'SELECT * FROM i WHERE c=1 OR NOT a IS NULL;' );
  TestQuery( 'SELECT * FROM i WHERE c=1 AND NOT a IS NULL;' );
  TestQuery( 'SELECT * FROM i WHERE NOT a IS NULL OR c=1;' );
  TestQuery( 'SELECT * FROM i WHERE NOT a IS NULL AND c=1;' );
  TestQuery( 'SELECT * FROM i WHERE c=1 OR a IS NOT NULL;' );
  TestQuery( 'SELECT * FROM i WHERE c=1 AND a IS NOT NULL;' );
  TestQuery( 'SELECT * FROM i WHERE a IS NOT NULL OR c=1;' );
  TestQuery( 'SELECT * FROM i WHERE a IS NOT NULL AND c=1;' );

  { date: 2010-08-12, file: 2010 First Test.sql }
  TestQuery( 'USE comit ;' );
  TestQuery( 'GO ;' );

  TestQuery( 'SELECT pole FROM tabela;' );
  TestQuery( 'SELECT 1;' );
  TestQuery( 'SELECT ''string'';' );
  TestQuery( 'SELECT pole1, pole2, pole3 FROM tabela;' );
  TestQuery( 'SELECT ISNULL(pole,'''') FROM tabela;' );

  TestQuery( 'SELECT pole1 /*, pole2*/ FROM tabela;' );
  TestQuery( 'SELECT pole1 --, pole2'#13#10'FROM tabela;' );

  TestQuery( 'SELECT 1+1;' );
  TestQuery( 'SELECT ISNULL (pole1 /*, 0*/,1 ) + ISNULL (pole2,0);' );

  TestQuery( 'SELECT * FROM tabela;' );
  TestQuery( 'SELECT * FROM tab1 , tab2;' );
  TestQuery( 'SELECT * FROM tab1 JOIN tab2;' );
  TestQuery( 'SELECT * FROM tab1 LEFT /*comment*/ JOIN tab2;' );
  TestQuery( 'SELECT * FROM tab1 LEFT JOIN tab2 USING(id);' );
  TestQuery( 'SELECT * FROM tab1 LEFT JOIN tab2 ON tab1.id = tab2.id AND tab1.b=1 WHERE tab1.a=3 AND tab2.c=4 GROUP BY tab1.a;' );
  TestQuery( 'SELECT * FROM tab1, tab2;' );
  TestQuery( 'SELECT * FROM ( SELECT * FROM tab1 );' );
  TestQuery( 'SELECT * FROM ( SELECT * FROM tab1 ) AS tab2;' );
  TestQuery( 'SELECT * FROM dbo.tab1;' );
  TestQuery( 'SELECT a+1 AS a1 FROM tab1;' );
  TestQuery( 'UPDATE tab1 SET a=1, b=2;' );
  TestQuery( 'UPDATE tab1 SET a=x FROM tab2;' );
  TestQuery( 'INSERT INTO tab1 (a, b) VALUES(1,''a'');' );
  TestQuery( 'INSERT INTO tab1 (a, b) SELECT a, b FROM tab2;' );
  TestQuery( 'SELECT 1 AS X FROM DUAL UNION ALL SELECT 2 FROM DUAL;' );

  TestQuery( 'SELECT 1;SELECT 2;' );

  { date: 2012-01-28, file: MinQueries.sql }
  TestQuery( '/* Demo pe³nego b³êdów parsera SQL v0.3 */' );
  TestQuery( '/* aktualnie do dzia³ania wymaga podania DDL-i dla tabel.*/' );
  TestQuery( '/* Highlighter dedykowany dla PlusMemo - do poprawki.*/' );

  TestQuery( 'CREATE TABLE emp_table ( deptno INT, empname VARCHAR(100) );' );
  TestQuery( 'CREATE TABLE dept_table ( deptno INT, deptname VARCHAR(100) );' );

  TestQuery( 'SELECT empname Employee, deptname Department, emp_table.deptno AS DepartamentNo, deptno AmbigousDeptNo FROM emp_table, dept_table WHERE emp_table.deptno (+) = dept_table.deptno;' );
  TestQuery( 'SELECT empname Employee, deptname Department, deptno Ambigous FROM emp_table, dept_table T6 WHERE emp_table.deptno (+) = T6.deptno;' );

  TestQuery( 'CREATE TABLE sm ( id INT, name VARCHAR(100) );' );
  TestQuery( 'SELECT * FROM sm;' );

  TestQuery( 'SELECT * FROM Dict;' );

  { date: 2012-02-13, file: AdvQueries.sql }
  TestQuery( 'CREATE TABLE tabelka1 ( pole1 INTEGER, pole2 VARCHAR(11) );' );
  TestQuery( 'CREATE TABLE tabelka2 ( pole1 INTEGER, pole2 VARCHAR(11), pole3 DECIMAL(10,5) );' );
  TestQuery( 'ALTER TABLE tabelka2 ADD pole4 INT, pole5 INT NULL;' );
  TestQuery( 'ALTER TABLE tabelka2 ADD WHERE INT;' );

  TestQuery( 'ALTER TABLE tabelka1 ADD CONSTRAINT tabelka1_PK PRIMARY KEY (pole1);' );
  TestQuery( 'ALTER TABLE tabelka1 DROP COLUMN pole1;' );
  TestQuery( 'ALTER TABLE tabelka2 ADD CONSTRAINT tabelka2_PK PRIMARY KEY (pole1);' );
  TestQuery( 'ALTER TABLE tabelka2 DROP COLUMN pole1;' );
  TestQuery( 'ALTER TABLE tabelka2 ADD CONSTRAINT tabelka2_FK FOREIGN KEY (pole2) REFERENCES tabelka1 (pole1) ON DELETE CASCADE ON UPDATE SET NULL;' );
  TestQuery( 'ALTER TABLE tabelka2 DROP COLUMN pole2;' );
  TestQuery( 'ALTER TABLE tabelka2 ADD CONSTRAINT tabelka2_CK CHECK (pole5 > 0);' );
  TestQuery( 'ALTER TABLE tabelka2 DROP COLUMN pole5;' );
  TestQuery( 'ALTER TABLE tabelka2 ADD CONSTRAINT tabelka2_UK UNIQUE (pole5);' );
  TestQuery( 'ALTER TABLE tabelka2 DROP COLUMN pole5;' );
  TestQuery( 'ALTER TABLE tabelka2 MODIFY pole2 /*5*/ VARCHAR(10) NOT NULL;' );
  TestQuery( 'ALTER TABLE tabelka2 MODIFY pole5 VARCHAR(10) NOT NULL;' );
  TestQuery( 'ALTER TABLE tabelka2 ALTER COLUMN pole5 VARCHAR(20) NULL;' );

  TestQuery( 'ALTER TABLE tabelka2 ADD CONSTRAINT tabelka2_CK2 CHECK (pole6 > 0);' );

  TestQuery( 'ALTER TABLE tabelka2 ADD ( pole6 INT, pole7 INT );' );
  TestQuery( 'ALTER TABLE tabelka2 MODIFY ( pole6 VARCHAR(10), pole7 VARCHAR(10) );' );
  TestQuery( 'ALTER TABLE tabelka2 DROP ( pole6, pole7 );' );

  TestQuery( 'CREATE INDEX IX_tabelka_pole4 ON tabelka2 ( pole4 );' );
  TestQuery( 'CREATE UNIQUE INDEX IX_tabelka_pole5 ON tabelka2 ( pole5 );' );
  TestQuery( 'DROP INDEX IX_tabelka_pole5;' );

  TestQuery( 'ALTER TABLE tabelka1 DROP CONSTRAINT tabelka1_PK;' );
  TestQuery( 'ALTER TABLE tabelka2 DROP CONSTRAINT tabelka2_FK;' );
  TestQuery( 'ALTER TABLE tabelka2 DROP CONSTRAINT tabelka2_CK;' );
  TestQuery( 'ALTER TABLE tabelka2 DROP CONSTRAINT tabelka2_UK;' );

  TestQuery( 'ALTER TABLE tabelka2 DROP pole4;' );
  TestQuery( 'ALTER TABLE tabelka2 DROP COLUMN pole5;' );

  TestQuery( 'ALTER TABLE tabelka2 RENAME TO tabelka3;' );
  TestQuery( 'ALTER TABLE tabelka2 RENAME COLUMN pole3 TO pole9;' );
  TestQuery( 'ALTER TABLE tabelka3 RENAME COLUMN pole3 TO pole9;' );

  TestQuery( 'SELECT A.pole2 FROM tabelka2 A JOIN tabelka1 AS B ON B.pole1 = A.pole2;' );
  TestQuery( 'SELECT * FROM tabelka2 A JOIN tabelka1 AS B ON B.pole1 = A.pole2;' );

  TestQuery( 'DROP TABLE tabelka1;' );
  TestQuery( 'DROP TABLE tabelka2;' );

  TestQuery( 'CREATE TABLE emp ( deptno INT, empno INT, empname VARCHAR(50) );' );
  TestQuery( 'CREATE TABLE dept ( deptno INT, deptname VARCHAR(50) );' );
  TestQuery( 'SELECT 1, deptno, * FROM emp;' );
  TestQuery( 'SELECT e.* FROM emp e, dept d;' );
  TestQuery( 'INSERT INTO emp ( deptno ) VALUES ( 1 );' );
  TestQuery( 'INSERT INTO emp ( deptno ) SELECT deptno FROM emp;' );
  TestQuery( 'DELETE FROM emp;' );

  TestQuery( 'UPDATE emp SET deptno = 1, empname = empname + ''DEPT=2'' WHERE deptno = 2;' );
  TestQuery( 'SELECT deptno FROM emp;' );
  TestQuery( 'UPDATE emp SET deptno = deptno FROM dept WHERE deptno = 1;  --Ambigous' );
  TestQuery( 'DELETE FROM emp JOIN dept ON dept.deptno = emp.deptno;' );

  TestQuery( 'SELECT * FROM emp LEFT JOIN dept ON emp.deptno = dept.deptno;' );
  TestQuery( 'SELECT T1.deptno, T1.deptname, T2.empno, T2.empname FROM dept T1, emp T2 WHERE T1.deptno=T2.deptno;' );
  TestQuery( 'SELECT deptname				--, COUNT(empno)'#13#10 +
             'FROM   dept D'#13#10 +
             'JOIN   emp E ON E.deptno = D.deptno'#13#10 +
             'WHERE  deptname != ''R&D'' AND deptname != ''IT'' -- ERROR - disabled reference for class.'#13#10 +
             'GROUP BY deptname'#13#10 +
             'HAVING deptname <> ''''			---COUNT(empno) > 0'#13#10 +
             'UNION ALL'#13#10 +
             'SELECT ''all'''#13#10 +
             'FROM DUAL'#13#10 +
             'ORDER BY deptname			---COUNT(empno) DESC;' );

  { date: 2012-02-23, file: Subqueries.sql }
  TestQuery( 'SELECT  (SELECT Name FROM Dept WHERE Dept.ID = Emp.ID_Dept)'#13#10'FROM    Emp;' );
  TestQuery( 'SELECT  (SELECT Name FROM Dept D WHERE D.ID = E.ID_Dept) AS Dept_Name'#13#10'FROM    Emp E;' );
  TestQuery( 'SELECT  *'#13#10'FROM    Emp  '#13#10'WHERE   EXISTS (SELECT Name FROM Dept WHERE Dept.ID = Emp.ID_Dept);' );
  TestQuery( 'SELECT  *'#13#10'FROM    Emp E'#13#10'WHERE   NOT EXISTS (SELECT * FROM Dept D WHERE D.ID = E.ID_Dept);' );
  TestQuery( 'SELECT  *'#13#10'FROM    Emp  '#13#10'WHERE   ID_Dept IN (SELECT ID FROM Dept WHERE Dept.ID = Emp.ID_Dept);' );
  TestQuery( 'SELECT  *'#13#10'FROM    Emp E'#13#10'WHERE   E.ID_Dept NOT IN (SELECT ID FROM Dept D WHERE D.ID = E.ID_Dept);' );
  TestQuery( 'SELECT  *'#13#10'FROM    Emp  '#13#10'WHERE   (SELECT ID FROM Dept WHERE Dept.ID = Emp.ID_Dept) = 1;' );
  TestQuery( 'SELECT  *'#13#10'FROM    Emp E'#13#10'WHERE   (SELECT ID FROM Dept D WHERE D.ID = E.ID_Dept) = 1;' );
  TestQuery( 'SELECT	*'#13#10'FROM	(SELECT * FROM Emp E);' );
  TestQuery( 'SELECT	*'#13#10'FROM	(SELECT * FROM Emp);' );

  { date: 2012-05-29, file: DDL Operations.sql }
  TestQuery( '/* DDL test */' );
  TestQuery( '/* 1. NON CREATE OPERATIONS */' );
  TestQuery( 'ALTER TABLE tab1 DROP col;' );
  TestQuery( 'ALTER TABLE tab1 DROP COLUMN col;' );
  TestQuery( 'ALTER TABLE tab1 DROP ( col1, col2 );' );
  TestQuery( 'ALTER TABLE tab1 RENAME COLUMN col1 TO col2;' );
  TestQuery( '/* 2. COLUMN NAMES */' );
  TestQuery( '/* 2.1. VARIOUS COLUMN DEFS - ALTER TABLE ADD COLUMN */' );
  TestQuery( 'CREATE TABLE tab ( col INT );' );
  TestQuery( 'ALTER TABLE tab ADD col INT;' );
  TestQuery( 'ALTER TABLE tab ADD col INTEGER;' );
  TestQuery( 'ALTER TABLE tab ADD col DECIMAL ;' );
  TestQuery( 'ALTER TABLE tab ADD col DECIMAL(6) ;' );
  TestQuery( 'ALTER TABLE tab ADD col DECIMAL(99999999) ;' );
  TestQuery( 'ALTER TABLE tab ADD col DECIMAL(8,2) ;' );
  TestQuery( 'ALTER TABLE tab ADD col CHAR ;' );
  TestQuery( 'ALTER TABLE tab ADD col CHAR(5) ;' );
  TestQuery( 'ALTER TABLE tab ADD col CHAR(999999) ;' );
  TestQuery( 'ALTER TABLE tab ADD col VARCHAR ;' );
  TestQuery( 'ALTER TABLE tab ADD col VARCHAR(5) ;' );
  TestQuery( 'ALTER TABLE tab ADD col VARCHAR(999999) ;' );
  TestQuery( '/* 3. MULTIPLE COLUMNS */' );
  TestQuery( 'CREATE TABLE emp_table ( deptno INT, empno INT, empname VARCHAR(100), Salary DECIMAL(10,2) );' );
  TestQuery( 'CREATE TABLE dept_table ( deptno INT, deptname VARCHAR(100) );' );

  TestQuery( 'CREATE TABLE Dept	(	ID INT IDENTITY NOT NULL PRIMARY KEY,'#13#10'Name VARCHAR(10) NOT NULL UNIQUE );' );
  TestQuery( 'CREATE TABLE Emp 	(	ID INT IDENTITY(1,1) NOT NULL PRIMARY KEY,'#13#10 +
                                 'ID_Dept INT NOT NULL REFERENCES Dept,'#13#10 +
                                 'Salary DECIMAL(10,2) DEFAULT 1.99,'#13#10 +
                                 'CONSTRAINT Emp_FK_Dept FOREIGN KEY (ID_Dept) REFERENCES Dept (ID),'#13#10 +
                                 'Name VARCHAR(100) NOT NULL );' );
  TestQuery( 'ALTER TABLE tabelka2 ADD pole4 INT, pole5 INT NULL;' );
  TestQuery( 'ALTER TABLE tabelka2 ADD WHERE INT;' );
  TestQuery( 'ALTER TABLE tabelka2 MODIFY pole5 VARCHAR(10) NOT NULL;' );
  TestQuery( 'ALTER TABLE tabelka2 ALTER COLUMN pole5 VARCHAR(20) NULL;' );
  TestQuery( 'ALTER TABLE tabelka2 ADD ( pole6 INT, pole7 INT );' );
  TestQuery( 'ALTER TABLE tabelka2 MODIFY ( pole6 VARCHAR(10), pole7 VARCHAR(10) );' );

  { date: 2012-06-28, file: Dashboard Query.sql }
  TestQuery( 'INSERT	INTO	dbo.rhi_targety ('#13#10 +
             '      okres_kod, target_obrot, klient_kod, odbiorca_kod,'#13#10 +
             '      handlowiec_kod, handl_dzial1_kod, handl_dzial2_kod, handl_dzial3_kod )'#13#10 +
             'SELECT			TAB.okres_kod, Kwota, klient_kod, odb_kod,'#13#10 +
             '      CASE WHEN handl_kod='''' THEN ''-'' ELSE handl_kod END,'#13#10 +
             '      ISNULL(D.handl_dzial1_kod, ''-''), ISNULL(D.handl_dzial2_kod, ''-''), ISNULL(D.handl_dzial3_kod, ''-'')'#13#10 +
             'FROM	('#13#10 +
             '  SELECT		O.okres_kod,'#13#10 +
             '      CASE M.czas_mies'#13#10 +
             '      WHEN 1 THEN BRT.TargetThYr_CSF WHEN 2 THEN TargetThYr_CSF2_ WHEN 3 THEN TargetThYr_CSF3_'#13#10 +
             '      WHEN 4 THEN TargetThYr_CSF4_ WHEN 5 THEN TargetThYr_CSF5_ WHEN 6 THEN TargetThYr_CSF6_'#13#10 +
             '      WHEN 7 THEN TargetThYr_CSF7_ WHEN 8 THEN TargetThYr_CSF8_ WHEN 9 THEN TargetThYr_CSF9_'#13#10 +
             '      WHEN 10 THEN TargetThYr_CSF10_ WHEN 11 THEN TargetThYr_CSF11_ WHEN 12 THEN TargetThYr_CSF12_ ELSE 0.0 END AS Kwota,'#13#10 +
             '      UPPER(BusRelAccount) AS klient_kod, ''-'' AS odb_kod,'#13#10 +
             '      LOWER(MainContact) AS handl_kod,'#13#10 +
             '      SalesUnitId_CSF, SalesUnitParentId_CSF'#13#10 +
             '  FROM		Axapta.smmBusRelTable AS BRT,'#13#10 +
             '      dbo.rwm_czas_mies AS M,'#13#10 +
             '      dbo.rhi_okresy_rozl AS O'#13#10 +
             '  WHERE		BRT.DataAreaId = ''WSP'''#13#10 +
             '  AND	(	M.czas_mies >= datepart(month, getdate()) /* bez historycznych */'#13#10 +
             '    OR	(SELECT COUNT(*) FROM dbo.rhi_targety) = 0	)'#13#10 +
             '  AND		dateadd(month, M.czas_mies -1, ''2008-01-01'') BETWEEN O.data_pocz AND O.data_konc'#13#10 +
             ') AS TAB'#13#10 +
             'LEFT JOIN	dbo.rpr_dzialy_cx AS D ON D.okres_kod = TAB.okres_kod'#13#10 +
             '          AND D.parent_unit_cx = UPPER(TAB.SalesUnitParentId_CSF) COLLATE Polish_CI_AI'#13#10 +
             '          AND D.sales_unit_cx = UPPER(TAB.SalesUnitId_CSF) COLLATE Polish_CI_AI;' );

  { date: 2012-09-28, file: buchal query.sql }
//  TestQuery( 'UPDATE PRODUKT SET CENA_MINIMALNA = ZP.CENA FROM ('#13#10 +
//             '  SELECT P.ID, SYM.CENA'#13#10 +
//             '  FROM'#13#10 +
//             '  PRODUKT P'#13#10 +
//             '                 JOIN PRODUCENT PR ON PR.ID=P.ID_PRODUCENT'#13#10 +
//             '                 JOIN      ('#13#10 +
//             '  SELECT'#13#10 +
//             '    MIN( P.CENA) AS CENA,'#13#10 +
//             '    P.KOD'#13#10 +
//             '  FROM'#13#10 +
//             '    SRV_A2012.A2012.DBO.DK AS D,'#13#10 +
//             '    SRV_A2012.A2012.DBO.DP AS P'#13#10 +
//             '  WHERE'#13#10 +
//             '        D.ID=P.SUPER'#13#10 +
//             '    AND D.TYP=0'#13#10 +
//             '    AND D.RODZAJ=3300'#13#10 +
//             '    AND D.AKTYWNY=1'#13#10 +
//             '    AND          P.SUBTYP=0'#13#10 +
//             '    AND D.DATA >= CONVERT(DATETIME, ''2011-01-01 00:00:00'', 120)'#13#10 +
//             '    AND D.DATA <= CONVERT(DATETIME, ''2011-03-30 23:59:59'', 120)'#13#10 +
//             '  GROUP BY P.KOD ) AS Z ON Z.KOD = PR.SYMBOL || '' '' || P.NAZWA_KODOWA'#13#10 +
//             '  ) AS ZP WHERE ZP.ID=PRODUKT.ID' );

  { date: 2012-10-02, file: background queries.sql }
  TestQuery( '     CREATE TABLE  dept_table'#13#10 +
             '                (  deptno INTEGER NULL CONSTRAINT pk_dept PRIMARY KEY'#13#10 +
             '                ,  deptname VARCHAR (100) NULL CONSTRAINT uq_dept UNIQUE'#13#10 +
             '                )  ;' );
  TestQuery( '      ALTER TABLE  tabelka1'#13#10 +
             '   ADD CONSTRAINT  tabelka1_PK'#13#10 +
             '      PRIMARY KEY  ( pole1 );' );
  TestQuery( '            BEGIN  TRAN tgi_tran_101;' );
  TestQuery( '      INSERT INTO  emp_table'#13#10 +
             '                (  deptno, empname )'#13#10 +
             '         VALUES (  1, ''Scott'' );' );
  TestQuery( '           UPDATE  emp_table'#13#10 +
             '              SET  deptno = 2'#13#10 +
             '            WHERE  empname = ''Scott''; ');
  TestQuery( '           SELECT  empname AS Employee, deptname AS Department,'#13#10 +
             '           emp_table.deptno AS DepartamentNo, deptno AS AmbigousDeptNo'#13#10 +
             '             FROM  emp_table'#13#10 +
             '                ,  dept_table'#13#10 +
             '            WHERE  emp_table.deptno (+) = dept_table.deptno;' );
  TestQuery( '      DELETE FROM  emp_table'#13#10 +
             '            WHERE  empname = ''Scott'';' );
  TestQuery( '           COMMIT  TRAN tgi_tran_101;' );
  TestQuery( '       DROP TABLE  dept_table;');

  { date: 2012-10-02, file: parsing condition.sql }
  TestQuery( '-- zamiana zapasów jeœli wskazuj¹ wêz³y odwrotnie do kierunku punktów charakterystycznych.'#13#10 +
             'UPDATE    WEZEL_KABEL'#13#10 +
             'SET       ID_WEZEL_1 = ID_WEZEL_2'#13#10 +
             '  ,       ID_WEZEL_2 = ID_WEZEL_1'#13#10 +
             '  ,       ZAPAS_1    = ZAPAS_2'#13#10 +
             '  ,       ZAPAS_2    = ZAPAS_1'#13#10 +
             'WHERE     ID IN ( SELECT    WK.ID'#13#10 +
             '                  FROM      WEZEL_KABEL WK'#13#10 +
             '                  LEFT JOIN WEZEL_KABEL WK1 ON WK1.ID_TYP_ELEMENTU = WK.ID_TYP_ELEMENTU AND WK1.ID_ELEMENT = WK.ID_ELEMENT AND WK1.ID_WEZEL = WK.ID_WEZEL_1'#13#10 +
             '                  LEFT JOIN WEZEL_KABEL WK2 ON WK2.ID_TYP_ELEMENTU = WK.ID_TYP_ELEMENTU AND WK2.ID_ELEMENT = WK.ID_ELEMENT AND WK2.ID_WEZEL = WK.ID_WEZEL_2'#13#10 +
             '                  WHERE    (WK.ZAPAS_1 IS NOT NULL OR WK.ZAPAS_2 IS NOT NULL)'#13#10 +
             '                  AND      (WK1.ID IS NOT NULL OR WK2.ID IS NOT NULL)'#13#10 +
             '                  AND      (WK1.LP > WK.LP OR WK2.LP < WK.LP)'#13#10 +
             ');' );

  { date: 2012-10-05, file: parsing condition 2.sql }
  TestQuery( 'SELECT'#13#10 +
             '  K.ID, K.NR, W1.ELEMENT AS W1, W2.ELEMENT AS W2,'#13#10 +
             '  W1.TYP_ELEMENTU AS T1,'#13#10 +
             '  W2.TYP_ELEMENTU AS T2,'#13#10 +
             '  NVL(K.B_TUNEL, 0) AS TUNEL'#13#10 +
             'FROM   KANALIZACJA K'#13#10 +
             '  LEFT JOIN WEZEL_V W1 ON K.ID_WEZEL_1 = W1.ID'#13#10 +
             '  LEFT JOIN WEZEL_V W2 ON K.ID_WEZEL_2 = W2.ID'#13#10 +
             'WHERE  1 /*:DBLookupTyp.Value*/ = 1 AND K.ID_WEZEL_1 IN (1/*:WEZEL1*/,2/*:WEZEL2*/) AND K.ID_WEZEL_2 IN (1/*:WEZEL1*/,2/*:WEZEL2*/)'#13#10 +
             'OR     1 /*:DBLookupTyp.Value*/ = 2 AND(K.ID_WEZEL_1 IN (1/*:WEZEL1*/,2/*:WEZEL2*/) OR  K.ID_WEZEL_2 IN (1/*:WEZEL1*/,2/*:WEZEL2*/))'#13#10 +
             'OR     1 /*:DBLookupTyp.Value*/ = 3'#13#10 +
             'ORDER BY'#13#10 +
             '  K.NR' );

  { date: 2012-10-11, file: constraint name parse.sql }
  TestQuery( 'CREATE TABLE  PASSPORT_VGC ('#13#10 +
             '              ID         INT NOT NULL CONSTRAINT PASSPORT_VGC$PK PRIMARY KEY,'#13#10 +
             '              REPORT_POS INT NOT NULL,'#13#10 +
             '              ALIAS      VARCHAR(50) NOT NULL,'#13#10 +
             '              V_EXT      INT);' );

  { date: 2012-10-13, file: parse parameters with dots.sql - comment inside string - TestQuery function problem }
  TestQuery( 'SELECT ''---'' FROM DUAL;' );

  { date: 2012-10-13, file: parse parameters with dots.sql }
  TestQuery( 'SELECT TAB.*,'#13#10 +

             '       CASE WHEN ID_PORT_2 = TO_NUMBER(:qEdit.ID_PORT_2)  OR ID_PORT_2B = TO_NUMBER(:qEdit.ID_PORT_2)'#13#10 +
             '              OR ID_PORT_2 = TO_NUMBER(:qEdit.ID_PORT_2B) OR ID_PORT_2B = TO_NUMBER(:qEdit.ID_PORT_2B)'#13#10 +
             '       THEN ''OSIAGNIÊTY'''#13#10 +
             '       ELSE ''-''||''-''||''-'' '#13#10 +
             '       END AS KONIEC_OSIAGNIETY'#13#10 +
             'FROM ('#13#10 +
             '    SELECT         TAB.*, U2A.B_SZKIELET, W2A.ID_ELEMENT AS ID_ELEMENT_2,'#13#10 +
             '                   W2A.ELEMENT || '' / '' || P2A.NR ||'#13#10 +
             '                   CASE WHEN P2B.NR != P2A.NR THEN '' , '' || P2B.NR END ||'#13#10 +
             '                   CASE WHEN kontener IS NOT NULL THEN '' / '' || kontener END AS koniec'#13#10 +
             '    FROM ('#13#10 +
             '        SELECT     TRS.LP, K2.NUMER_UPR AS kontener,'#13#10 +
             '                   CASE WHEN TRS.ID_PORT_1 = TRSP.ID_PORT_1 OR TRS.ID_PORT_1 = TRSP.ID_PORT_1B OR TRS.ID_PORT_1 = TRSP.ID_PORT_2 OR TRS.ID_PORT_1 = TRSP.ID_PORT_2B OR TRSP.ID IS NULL AND TRS.ID_PORT_1 = :qEdit.ID_PORT_1 ' + 'THEN TRS.ID_PORT_2     ELSE TRS.ID_PORT_1     END AS ID_PORT_2,'#13#10 +
             '                   CASE WHEN TRS.ID_PORT_1 = TRSP.ID_PORT_1 OR TRS.ID_PORT_1 = TRSP.ID_PORT_1B OR TRS.ID_PORT_1 = TRSP.ID_PORT_2 OR TRS.ID_PORT_1 = TRSP.ID_PORT_2B OR TRSP.ID IS NULL AND TRS.ID_PORT_1 = :qEdit.ID_PORT_1 ' + 'THEN TRS.ID_PORT_2B    ELSE TRS.ID_PORT_1B    END AS ID_PORT_2B,'#13#10 +
             '                   CASE WHEN TRS.ID_PORT_1 = TRSP.ID_PORT_1 OR TRS.ID_PORT_1 = TRSP.ID_PORT_1B OR TRS.ID_PORT_1 = TRSP.ID_PORT_2 OR TRS.ID_PORT_1 = TRSP.ID_PORT_2B OR TRSP.ID IS NULL AND TRS.ID_PORT_1 = :qEdit.ID_PORT_1 ' + 'THEN TRS.ID_GNIAZDO_2A ELSE TRS.ID_GNIAZDO_1A END AS ID_GNIAZDO_2A,'#13#10 +
             '                   CASE WHEN TRS.ID_PORT_1 = TRSP.ID_PORT_1 OR TRS.ID_PORT_1 = TRSP.ID_PORT_1B OR TRS.ID_PORT_1 = TRSP.ID_PORT_2 OR TRS.ID_PORT_1 = TRSP.ID_PORT_2B OR TRSP.ID IS NULL AND TRS.ID_PORT_1 = :qEdit.ID_PORT_1 ' + 'THEN TRS.ID_GNIAZDO_2B ELSE TRS.ID_GNIAZDO_1B END AS ID_GNIAZDO_2B'#13#10 +
             ''#13#10 +
             '        FROM       TRANSMISJA_SKL_PORTY TRS'#13#10 +
             '        LEFT JOIN  TRANSMISJA_SKL_PORTY TRSP ON TRSP.ID_TRANSMISJA = TRS.ID_TRANSMISJA AND TRSP.LP = TRS.LP - 1'#13#10 +
             '        LEFT JOIN  KONTENERY_SDH K2 ON K2.ID = TRS.ID_KONTENERY_SDH_2'#13#10 +
             '        WHERE      TRS.ID_TRANSMISJA = :qEdit.ID'#13#10 +
             '              AND  TRS.LP != 999'#13#10 +
             ''#13#10 +
             '        UNION ALL'#13#10 +
             ''#13#10 +
             '        SELECT     -1, NULL, TR.ID_PORT_1, TR.ID_PORT_1B, TR.ID_GNIAZDO_1A, TR.ID_GNIAZDO_1B'#13#10 +
             '        FROM       TRANSMISJA TR'#13#10 +
             '        WHERE      TR.ID = :qEdit.ID'#13#10 +
             ''#13#10 +
             '        ORDER BY   1 DESC'#13#10 +
             '    ) TAB'#13#10 +
             '    LEFT JOIN  PORT       P2A ON P2A.ID = TAB.ID_PORT_2'#13#10 +
             '    LEFT JOIN  PORT       P2B ON P2B.ID = TAB.ID_PORT_2B'#13#10 +
             '    LEFT JOIN  WEZEL_V    W2A ON W2A.ID = P2A.ID_WEZEL'#13#10 +
             '    LEFT JOIN  URZADZENIE U2A ON U2A.ID = W2A.ID_ELEMENT AND W2A.ID_TYP_WEZLA = 5'#13#10 +
             '    WHERE    ROWNUM = 1'#13#10 +
             ') TAB' );

  { date: 2012-10-13, file: connect by adds PRIOR to expression.sql }
  TestQuery( 'SELECT      TS.LP, TS.ID, TS.ID_TRANSMISJA, TS.ID_TRANSMISJA_SKL, NVL2(TRD.ID_TRANSMISJA,1,0),'#13#10 +
             '            rodzaj, opis, ID_TYP_SCHEMA, ID_ELEMENT,  ID_TYP_TRANSLACJI, TS.NR_VLAN,'#13#10 +
             '            NR_WEZEL_1, TS.ID_URZADZENIE_1, NR_PORT_1, TS.ID_PORT_1, NR_PORT_1B, TS.ID_PORT_1B, NR_KONTENERY_SDH_1,'#13#10 +
             '            NR_WEZEL_2, TS.ID_URZADZENIE_2, NR_PORT_2, TS.ID_PORT_2, NR_PORT_2B, TS.ID_PORT_2B, NR_KONTENERY_SDH_2,'#13#10 +
             '            TS.ID_PRZEPLYWNOSC'#13#10 +
             'FROM        TRANSMISJA_SKLADNIKI TSA'#13#10 +
             'JOIN        TRANSMISJA_SKL_PORTY TS ON TS.ID = TSA.ID'#13#10 +
             'LEFT JOIN   TRANSMISJA_DROZNA   TRD ON TRD.ID_TRANSMISJA = TS.ID_TRANSMISJA_SKL'#13#10 +
             'WHERE       TSA.LP != 999 /* tylko uporz¹dkowane sk³adniki */'#13#10 +
             'AND         TSA.ID_TRANSMISJA IN (  SELECT ID_TRANSMISJA_SKL'#13#10 +
             '                                    FROM   TRANSMISJA_SKLADNIKI'#13#10 +
             '                                    WHERE  ID_TRANSMISJA_SKL IS NOT NULL'#13#10 +
             '                                    CONNECT BY NOCYCLE ID_TRANSMISJA = PRIOR ID_TRANSMISJA_SKL'#13#10 +
             '                                    START WITH ID_TRANSMISJA = TransId );' );

  { date: 2012-10-14, file: condition parse.sql }
  TestQuery( 'SELECT 1 FROM dual WHERE a = :a1 AND b = &b2;' );

  { date: 2012-10-16, file: Test cases (columns from aliased tables).sql }
  TestQuery( '    SELECT    G.ID AS ID, G.TYP_ELEMENTU AS TYP, V.ID_TYP_WEZLA'#13#10 +
             '    FROM      KABEL_CU S'#13#10 +
             '    JOIN      WEZEL_V  V  ON  S.ID_WEZEL_2 = V.ID_WEZEL_PARENT AND V.ID_TYP_WEZLA BETWEEN 117 AND 119'#13#10 +
             '    JOIN      WEZEL_V  G  ON  G.ID_WEZEL_PARENT = V.ID AND G.ID_TYP_WEZLA = 116'#13#10 +
             '    WHERE     S.ID = :qEdit.SID.a'#13#10 +
             ''#13#10 +
             '    UNION ALL'#13#10 +
             ''#13#10 +
             '    SELECT    V.ID, V.TYP_ELEMENTU AS TYP, V.ID_TYP_WEZLA'#13#10 +
             '    FROM      KABEL_CU     S'#13#10 +
             '    JOIN      WEZEL_V      V  ON  S.ID_WEZEL_2 = V.ID_WEZEL_PARENT AND V.ID_TYP_WEZLA = 3'#13#10 +
             '    JOIN      PRZELACZNICA M  ON  V.ID_ELEMENT = M.ID'#13#10 +
             '    WHERE     S.ID = :qEdit.SID.b' );

  { date: 2012-10-16, file: Test cases (built in to older versions).sql }
  TestQuery( '/* Yet Another SQL Edit 0.59, build: 417 - demo SQL parsera */' );
  TestQuery( 'CREATE TABLE dept_table ( deptno INT CONSTRAINT pk_dept PRIMARY KEY, deptname VARCHAR(100) CONSTRAINT uq_dept UNIQUE);' );
  TestQuery( 'CREATE TABLE emp_table ( deptno INT CONSTRAINT fk_emp_to_dept REFERENCES dept_table, empname VARCHAR(100), CONSTRAINT uq_emp UNIQUE (deptno, empname) );' );
  TestQuery( 'ALTER TABLE tabelka1 ADD CONSTRAINT tabelka1_PK PRIMARY KEY (pole1);' );
  TestQuery( 'SELECT empname Employee, deptname Department, emp_table.deptno AS DepartamentNo, deptno AmbigousDeptNo FROM emp_table, dept_table WHERE emp_table.deptno (+) = dept_table.deptno + 1;' );
  TestQuery( 'SELECT empname Employee, deptname Department, deptno Ambigous FROM emp_table, dept_table T6 WHERE emp_table.deptno (+) = T6.deptno;' );
  TestQuery( 'SELECT empname Employee, deptname Department, deptno Ambigous FROM emp_table JOIN dept_table T6 ON emp_table.deptno = T6.deptno;' );
  TestQuery( 'INSERT INTO emp_table VALUES ( 1, ''Scott'' );' );
  TestQuery( 'INSERT INTO emp_table ( deptno, empname ) VALUES ( 1, ''Scott'' );' );
  TestQuery( 'INSERT INTO emp_table SELECT 1, ''Scott'' FROM DUAL;' );
  TestQuery( 'INSERT INTO emp_table ( deptno, empname ) SELECT 1, ''Scott'' FROM DUAL;' );
  TestQuery( 'UPDATE emp_table SET deptno = 2 WHERE empname = ''Scott'';' );
  TestQuery( 'DELETE FROM emp_table WHERE empname = ''Scott'';' );
  TestQuery( 'SELECT DISTINCT 1*(2+3)+4 FROM emp_table;' );
  TestQuery( 'SELECT DISTINCT 2*(2*(2*(2*(2*(2*(1+1)+1)+1)+1)+1)+1)+1 FROM emp_table;' );
  TestQuery( 'SELECT CASE a WHEN 1 THEN ''1'' WHEN 2 THEN ''2'' ELSE CASE WHEN b=2 THEN ''7'' ELSE ''0'' END END AS CaseExpr FROM emp_table;' );
  TestQuery( 'SELECT CASE WHEN a=1 THEN ''1'' WHEN a=2 THEN ''2'' ELSE ''0'' END AS CaseExpr FROM emp_table;' );
  TestQuery( 'SELECT CASE WHEN a=1 AND b=1 THEN ''1'' WHEN a=2 AND b=2 THEN ''2'' ELSE ''0'' END AS CaseExpr FROM emp_table;' );
  TestQuery( 'SELECT COUNT(*), MAX(ID), NVL(a) FROM emp_table;' );

  TestQuery( 'SAVEPOINT tgi_savepoint_101;' );
  TestQuery( 'COMMIT;' );
  TestQuery( 'COMMIT WORK;' );
  TestQuery( 'COMMIT TRAN tgi_tran_101;' );
  TestQuery( 'ROLLBACK;' );
  TestQuery( 'ROLLBACK TO SAVEPOINT tgi_savepoint_101;' );
  TestQuery( 'ROLLBACK TRAN tgi_tran_101;' );
  TestQuery( 'BEGIN TRAN tgi_tran_101;' );
  TestQuery( 'BEGIN TRANSACTION tgi_tran_101;' );
  TestQuery( 'START TRANSACTION tgi_tran_101;' );
  TestQuery( 'END TRAN tgi_tran_101;' );
  TestQuery( 'END TRANSACTION tgi_tran_101;' );
  TestQuery( 'STOP TRANSACTION tgi_tran_101;' );
  TestQuery( 'CREATE SEQUENCE tab_seq START WITH 1 INCREMENT BY 1;' );
  TestQuery( 'DROP SEQUENCE tab_seq;' );
  TestQuery( 'CREATE VIEW xyz_v AS SELECT 1 FROM DUAL;' );
  TestQuery( 'CREATE OR REPLACE VIEW xyz_v AS SELECT 1 FROM DUAL;' );
  TestQuery( 'DROP VIEW xyz_v;' );
  TestQuery( 'CREATE SYNONYM xyz_s FOR xyz_v;' );
  TestQuery( 'CREATE PUBLIC SYNONYM xyz_s FOR xyz_v;' );
  TestQuery( 'CREATE OR REPLACE SYNONYM xyz_s FOR xyz_v;' );
  TestQuery( 'CREATE OR REPLACE PUBLIC SYNONYM xyz_s FOR xyz_v;' );
  TestQuery( 'DROP SYNONYM xyz_s;' );
  TestQuery( 'USE AdventureWorks;' );
  TestQuery( 'TRUNCATE TABLE emp_table;' );
  TestQuery( 'GRANT SELECT ON emp TO tgi;' );
  TestQuery( 'DENY SELECT ON emp TO tgi;' );
//TestQuery( 'REVOKE SELECT ON emp FROM tgi;' ); { sprawdziæ dokumentacjê od Oracle i MSSQL !! }
  TestQuery( 'SELECT "ola monola" FROM "ala ma kota" "ala mia³a kota";' );
  TestQuery( 'SELECT 1 FROM dual WHERE a = :a1 AND b = &b2;' );
//TestQuery( 'SELECT 1 FROM dual WHERE a LIKE ''nic\%%'' ESCAPE ''\'' COLLATE dupa;' );

  { date: 2012-10-20, file: upper function name.sql }
  TestQuery( 'select fun (1,2) from dual;' );

  { date: 2012-10-20, file: subquery alias in from clause.sql }
  TestQuery( 'select * from (select * from dual) tab join (select * from dual) tab2 on tab1.a = tab2.a' );

  { date: 2012-10-20, file: old 3 union - nested.sql }
  TestQuery( '         SELECT  1'#13#10 +
             '           FROM  a'#13#10 +
             '          WHERE  a = 1'#13#10 +
             ''#13#10 +
             '      UNION ALL'#13#10 +
             ''#13#10 +
             '         SELECT  2'#13#10 +
             '           FROM  b'#13#10 +
             '          WHERE  b = 2'#13#10 +
             ''#13#10 +
             '      UNION ALL'#13#10 +
             ''#13#10 +
             '         SELECT  3'#13#10 +
             '           FROM  c'#13#10 +
             '          WHERE  c = 3 ;' );

  { date: 2012-10-22, file: old subquery split on union.sql }
  TestQuery( 'SELECT td.nazwa, trs.id_transmisja'#13#10 +
             'FROM   transmisja_skladniki trs'#13#10 +
             'JOIN   transmisja_przebieg trp ON trp.id_transmisja = trs.id_transmisja'#13#10 +
             'JOIN   PORT P ON P.ID = trp.id_port_1'#13#10 +
             'JOIN   transport_det td ON td.id = p.id_transport_det'#13#10 +
             'WHERE  trs.id_transmisja_skl = 12193'#13#10 +
             'AND    trp.id_port_1 IN ('#13#10 +
             '        SELECT      p11.id'#13#10 +
             '        FROM        transmisja tr'#13#10 +
             '        JOIN        port  p1 ON p1.id = tr.id_port_1'#13#10 +
             '        JOIN        port p11 ON p11.id_wezel = p1.id_wezel'#13#10 +
             '        WHERE       tr.id = 12193'#13#10 +
             '        AND         p11.id_transport IN (-4,-5)'#13#10 +
             '        AND         p11.id_transport_det IS NOT NULL'#13#10 +
             '        UNION ALL'#13#10 +
             '        SELECT      p22.id'#13#10 +
             '        FROM        transmisja tr'#13#10 +
             '        JOIN        port  p2 ON p2.id = tr.id_port_2'#13#10 +
             '        JOIN        port p22 ON p22.id_wezel = p2.id_wezel'#13#10 +
             '        WHERE       tr.id = 12193'#13#10 +
             '        AND         p22.id_transport IN (-4,-5)'#13#10 +
             '        AND         p22.id_transport_det IS NOT NULL'#13#10 +
             ')'#13#10 +
             'UNION ALL'#13#10 +
             'SELECT td.nazwa, trs.id_transmisja'#13#10 +
             'FROM   transmisja_skladniki trs'#13#10 +
             'JOIN   transmisja_przebieg trp ON trp.id_transmisja = trs.id_transmisja'#13#10 +
             'JOIN   PORT P ON P.ID = trp.id_port_2'#13#10 +
             'JOIN   transport_det td ON td.id = p.id_transport_det'#13#10 +
             'WHERE  trs.id_transmisja_skl = 12193'#13#10 +
             'AND    trp.id_port_1 IN ('#13#10 +
             '        SELECT      p11.id'#13#10 +
             '        FROM        transmisja tr'#13#10 +
             '        JOIN        port  p1 ON p1.id = tr.id_port_1'#13#10 +
             '        JOIN        port p11 ON p11.id_wezel = p1.id_wezel'#13#10 +
             '        WHERE       tr.id = 12193'#13#10 +
             '        AND         p11.id_transport IN (-4,-5)'#13#10 +
             '        AND         p11.id_transport_det IS NOT NULL'#13#10 +
             '        UNION ALL'#13#10 +
             '        SELECT      p22.id'#13#10 +
             '        FROM        transmisja tr'#13#10 +
             '        JOIN        port  p2 ON p2.id = tr.id_port_2'#13#10 +
             '        JOIN        port p22 ON p22.id_wezel = p2.id_wezel'#13#10 +
             '        WHERE       tr.id = 12193'#13#10 +
             '        AND         p22.id_transport IN (-4,-5)'#13#10 +
             '        AND         p22.id_transport_det IS NOT NULL'#13#10 +
             ')' );

  { date: 2012-10-26, file: old 4 union - nested.sql }
  TestQuery( 'SELECT DISTINCT ID FROM TRANSMISJA TR WHERE TR.ID = 12590'#13#10 +
             'UNION'#13#10 +
             'SELECT ID FROM TRANSMISJA TR WHERE TR.ID = 12590'#13#10 +
             'UNION'#13#10 +
             'SELECT ID FROM TRANSMISJA TR WHERE TR.ID = 12590'#13#10 +
             'UNION'#13#10 +
             'SELECT ID FROM TRANSMISJA TR WHERE TR.ID = 12590 ;' );

  { date: 2012-10-26, file: start with.sql }
  TestQuery( 'DELETE FROM TMP_PORTY;' );
  TestQuery( '/* tylko porty WDM Band lub porty DWDM Channel */'#13#10 +
             'INSERT INTO TMP_PORTY ( ID_PORT )'#13#10 +
             'SELECT DISTINCT P.ID FROM TRANSMISJA TR JOIN PORT P ON P.ID = TR.ID_PORT_1 WHERE TR.ID = 12590 AND P.ID_TRANSPORT IN (-2,-4)'#13#10 +
             'UNION'#13#10 +
             'SELECT P.ID FROM TRANSMISJA TR JOIN PORT P ON P.ID = TR.ID_PORT_2  WHERE TR.ID = 12590 AND P.ID_TRANSPORT IN (-2,-4)'#13#10 +
             'UNION'#13#10 +
             'SELECT P.ID FROM TRANSMISJA TR JOIN PORT P ON P.ID = TR.ID_PORT_1B WHERE TR.ID = 12590 AND P.ID_TRANSPORT IN (-2,-4)'#13#10 +
             'UNION'#13#10 +
             'SELECT P.ID FROM TRANSMISJA TR JOIN PORT P ON P.ID = TR.ID_PORT_2B WHERE TR.ID = 12590 AND P.ID_TRANSPORT IN (-2,-4);' );
  TestQuery( 'SELECT      LEVEL, SYS_CONNECT_BY_PATH(TAB.ID_PORT_1 ||''-''|| TAB.ID_PORT_2, ''/'')'#13#10 +
             'FROM      ('#13#10 +
             '            SELECT 1, TR.ID, TR.ID_PORT_1, TR.ID_PORT_2'#13#10 +
             '            FROM   TRANSLACJE_SDH TR'#13#10 +
             '            WHERE  TR.ID_URZADZENIE IN (SELECT W.ID_ELEMENT FROM TMP_PORTY TMP JOIN PORT ON PORT.ID = TMP.ID_PORT JOIN WEZEL W ON W.ID = PORT.ID_WEZEL AND W.ID_TYP_WEZLA = 5)'#13#10 +
             '            UNION ALL'#13#10 +
             '            SELECT 2, PT.ID, PT.ID_PORT_1, PT.ID_PORT_2'#13#10 +
             '            FROM   PATCHCORD PT'#13#10 +
             '            JOIN   PORT P1 ON P1.ID = PT.ID_PORT_1'#13#10 +
             '            JOIN   PORT P2 ON P2.ID = PT.ID_PORT_2'#13#10 +
             '            WHERE  P1.ID_WEZEL = P2.ID_WEZEL'#13#10 +
             '            AND    P1.ID_WEZEL IN (SELECT W.ID FROM TMP_PORTY TMP JOIN PORT ON PORT.ID = TMP.ID_PORT JOIN WEZEL W ON W.ID = PORT.ID_WEZEL)'#13#10 +
             '          ) TAB'#13#10 +
             'WHERE       CONNECT_BY_ISLEAF = 1'#13#10 +
             'CONNECT BY  NOCYCLE TAB.ID_PORT_1 = PRIOR TAB.ID_PORT_1 AND TAB.ID_PORT_2 != PRIOR TAB.ID_PORT_2 AND PRIOR TAB.ID_PORT_1 NOT IN (SELECT ID_PORT FROM TMP_PORTY)'#13#10 +
             '                OR  TAB.ID_PORT_1 = PRIOR TAB.ID_PORT_2 AND TAB.ID_PORT_2 != PRIOR TAB.ID_PORT_1 AND PRIOR TAB.ID_PORT_2 NOT IN (SELECT ID_PORT FROM TMP_PORTY)'#13#10 +
             '                OR  TAB.ID_PORT_2 = PRIOR TAB.ID_PORT_1 AND TAB.ID_PORT_1 != PRIOR TAB.ID_PORT_2 AND PRIOR TAB.ID_PORT_1 NOT IN (SELECT ID_PORT FROM TMP_PORTY)'#13#10 +
             '                OR  TAB.ID_PORT_2 = PRIOR TAB.ID_PORT_2 AND TAB.ID_PORT_1 != PRIOR TAB.ID_PORT_1 AND PRIOR TAB.ID_PORT_2 NOT IN (SELECT ID_PORT FROM TMP_PORTY)'#13#10 +
             'START WITH  TAB.ID_PORT_1 IN (SELECT ID_PORT FROM TMP_PORTY) OR TAB.ID_PORT_2 IN (SELECT ID_PORT FROM TMP_PORTY);' );

  { date: 2012-10-31, file: for update.sql }
//  TestQuery( 'SELECT  *'#13#10 +
//             'FROM    WLOKNO W'#13#10 +
//             'WHERE   W.ID = 128436'#13#10 +
//             'FOR UPDATE OF W.ID NOWAIT' );

  { date: 2012-10-31, file: function skip one expr on line.sql }
  TestQuery( 'CREATE OR REPLACE VIEW SYRION_MUFY_V AS'#13#10 +
             'SELECT    M.ID AS ID_MUFA,'#13#10 +
             '          M.NR AS NR_MUFA,'#13#10 +
             '          T.NAZWA ||'' ''|| COALESCE(S.Nr, L.Symbol) AS Obiekt,'#13#10 +
             '          ADR_ID ( NVL(S.ID_ADRES, B.ID_ADRES) ) AS Lokalizacja,'#13#10 +
             '          CASE WHEN NVL(S.Y,L.Y) IS NOT NULL AND NVL(S.X,L.X) IS NOT NULL THEN'#13#10 +
             '          ''http://maps.google.com/maps?q=''||REPLACE(NVL(S.Y,L.Y)/10000,'','',''.'')||'',''||REPLACE(NVL(S.X,L.X)/10000,'','',''.'')'#13#10 +
             '          END AS LinkGoogle'#13#10 +
             ''#13#10 +
             'FROM      MUFA        M'#13#10 +
             'JOIN      WEZEL       W ON W.ID  = M.ID_WEZEL'#13#10 +
             'JOIN      TYP_WEZLA   T ON T.ID  = W.ID_TYP_WEZLA'#13#10 +
             'LEFT JOIN STUDNIA     S ON S.ID  = W.ID_ELEMENT AND W.ID_TYP_WEZLA = 2'#13#10 +
             'LEFT JOIN LOKALIZACJA L ON L.ID  = W.ID_ELEMENT AND W.ID_TYP_WEZLA = 24'#13#10 +
             'LEFT JOIN WEZEL      WL ON WL.ID = L.ID_WEZEL'#13#10 +
             'LEFT JOIN BUDYNEK     B ON B.ID  = WL.ID_ELEMENT AND WL.ID_TYP_WEZLA = 1 ;' );

  { date: 2012-10-31, file: apostrophe colors.sql }
  TestQuery( 'SELECT ''http://maps.google.com/maps?q=''|| REPLACE(Y,'','', ''.'') FROM MUFA M;' );

  { date: 2012-11-04, file: function without params in brackets - AV.sql }
  TestQuery( 'UPDATE UKE_TAB_RAPORT'#13#10 +
             'SET    DATA_GEN_TAB = SYSDATE(), DATA_GEN_XML = NULL, RAPORT = NULL,'#13#10 +
             '       B_PUSTY = (SELECT NVL(MAX(1),0) FROM UKE_TAB_LINIA_KABLOWA WHERE ID_UKE_RAPORT = aRaport)'#13#10 +
             'WHERE  ID = aRaport;' );

  { date: 2012-11-04, file: alter table with check constraint.sql }
  TestQuery( 'ALTER TABLE UKE_TAB_RAPORT'#13#10 +
             '       ADD B_PUSTY NUMBER(1) DEFAULT 0 NOT NULL'#13#10 +
             'CONSTRAINT  UKE_TAB_RAPORT$CK_PUSTY CHECK (B_PUSTY IN (0,1));' );

  { date: 2012-11-05, file: format - bracket color when clause.sql }
  TestQuery( 'INSERT INTO  ZAJETOSC_RURY'#13#10 +
             '          (  ID_TYP_ELEMENTU'#13#10 +
             '          ,  ID_ELEMENT'#13#10 +
             '          ,  ID_RURA'#13#10 +
             '          ,  LP'#13#10 +
             '          ,  ID_KANALIZACJA )'#13#10 +
             '   VALUES (  :ELEMENT_TYP'#13#10 +
             '          ,  :ELEMENT_ID'#13#10 +
             '          ,  :qRury.ID'#13#10 +
             '          ,  1'#13#10 +
             '          ,  :qKanalizacja.ID );' );

  { date: 2012-11-06, file: old vector.sql }
  TestQuery( 'SELECT PTYP.ID_TYP_ELEMENTU, V.PASSPORT_TABLE_ID, PTYP.PASSPORT_TYPE_NAME, V.NR'#13#10 +
             'FROM   PASSPORT_V V'#13#10 +
             'JOIN   PASSPORT_TYPE PTYP ON PTYP.ID = V.ID_PASSPORT_TYPE'#13#10 +
             'WHERE (V.ID_PASSPORT_TYPE,V.PASSPORT_TABLE_ID) IN ('#13#10 +
             'SELECT PTYP.ID, WK1.ID_ELEMENT'#13#10 +
             'FROM   KANALIZACJA K'#13#10 +
             'JOIN   WEZEL_KABEL WK1 ON WK1.ID_WEZEL = K.ID_WEZEL_1'#13#10 +
             'JOIN   WEZEL_KABEL WK2 ON WK2.ID_WEZEL = K.ID_WEZEL_2 AND WK1.ID_TYP_ELEMENTU = WK2.ID_TYP_ELEMENTU AND WK1.ID_ELEMENT = WK2.ID_ELEMENT AND WK1.LP + 1 = WK2.LP'#13#10 +
             'JOIN   PASSPORT_TYPE PTYP ON PTYP.ID_TYP_ELEMENTU = WK1.ID_TYP_ELEMENTU'#13#10 +
             'WHERE  K.ID = 30755'#13#10 +
             'UNION'#13#10 +
             'SELECT PTYP.ID, WK1.ID_ELEMENT'#13#10 +
             'FROM   KANALIZACJA K'#13#10 +
             'JOIN   WEZEL_KABEL WK1 ON WK1.ID_WEZEL = K.ID_WEZEL_2'#13#10 +
             'JOIN   WEZEL_KABEL WK2 ON WK2.ID_WEZEL = K.ID_WEZEL_1 AND WK1.ID_TYP_ELEMENTU = WK2.ID_TYP_ELEMENTU AND WK1.ID_ELEMENT = WK2.ID_ELEMENT AND WK1.LP + 1 = WK2.LP'#13#10 +
             'JOIN   PASSPORT_TYPE PTYP ON PTYP.ID_TYP_ELEMENTU = WK1.ID_TYP_ELEMENTU'#13#10 +
             'WHERE  K.ID = 30755'#13#10 +
             ')'#13#10 +
             'ORDER BY PTYP.PASSPORT_TYPE_NAME, V.NR' );

  { date: 2012-11-07, file: old UNION quote.sql }
  TestQuery( 'SELECT ID_SWIATLOWOD, NR_WLOKNO, NR'#13#10 +
             'FROM ('#13#10 +
             'SELECT V.ID_SWIATLOWOD, V.NR_WLOKNO, NR.NR'#13#10 +
             'FROM   WLOKNO_WKV V'#13#10 +
             'JOIN   NR ON NR.NR BETWEEN V.LP1 AND V.LP2 OR NR.NR BETWEEN V.LP2 AND V.LP1'#13#10 +
             'MINUS'#13#10 +
             'SELECT ID_SWIATLOWOD, NR_WLOKNO, NR'#13#10 +
             'FROM  ('#13#10 +
             'SELECT V.ID_SWIATLOWOD, V.NR_WLOKNO, V.LP1 NR'#13#10 +
             'FROM   WLOKNO_WKV V'#13#10 +
             'UNION'#13#10 +
             'SELECT V.ID_SWIATLOWOD, V.NR_WLOKNO, V.LP2 NR'#13#10 +
             'FROM   WLOKNO_WKV V'#13#10 +
             ') TAB'#13#10 +
             'GROUP BY ID_SWIATLOWOD, NR_WLOKNO, NR'#13#10 +
             'HAVING COUNT(*) = 2'#13#10 +
             ') TAB'#13#10 +
             'GROUP BY ID_SWIATLOWOD, NR_WLOKNO, NR'#13#10 +
             'HAVING COUNT(*) != 1' );

  { date: 2012-11-07, file: format - additional bracket after subquery.sql }
  TestQuery( 'SELECT ID_SWIATLOWOD, NR_WLOKNO, NR'#13#10 +
             'FROM ('#13#10 +
             'SELECT V.ID_SWIATLOWOD, V.NR_WLOKNO, NR.NR'#13#10 +
             'FROM   WLOKNO_WKV V'#13#10 +
             'JOIN   NR ON NR.NR BETWEEN V.LP1 AND V.LP2 OR NR.NR BETWEEN V.LP2 AND V.LP1'#13#10 +
             'MINUS'#13#10 +
             'SELECT ID_SWIATLOWOD, NR_WLOKNO, NR'#13#10 +
             'FROM  ('#13#10 +
             'SELECT V.ID_SWIATLOWOD, V.NR_WLOKNO, V.LP1 NR'#13#10 +
             'FROM   WLOKNO_WKV V'#13#10 +
             'UNION'#13#10 +
             'SELECT V.ID_SWIATLOWOD, V.NR_WLOKNO, V.LP2 NR'#13#10 +
             'FROM   WLOKNO_WKV V'#13#10 +
             ') TAB'#13#10 +
             'GROUP BY ID_SWIATLOWOD, NR_WLOKNO, NR'#13#10 +
             'HAVING COUNT(*) = 2'#13#10 +
             ') TAB'#13#10 +
             'GROUP BY ID_SWIATLOWOD, NR_WLOKNO, NR'#13#10 +
             'HAVING COUNT(*) != 1' );

  { date: 2012-11-09, file: format - missing operator.sql }
  TestQuery( 'select tab.*, replace(''UPDATE STUDNIA SET X=''||x_prime||'', Y=''||y_prime||'' WHERE NR=''''||tab.nr||'''';'', '','',''.'')'#13#10 +
             'from ('#13#10 +
             'select nr, max(x) x, max(y) y, max(x_prime) x_prime, max(y_prime) y_prime'#13#10 +
             'from ('#13#10 +
             ''#13#10 +
             'select  st.nr, st.x, st.y, null as x_prime, null as y_prime from studnia st where nr like ''£ódS%'''#13#10 +
             'union all'#13#10 +
             'select  t.name, null, null,'#13#10 +
             '        CASE WHEN COORD_X_DETAILS BETWEEN    1 AND    9 THEN COORD_X_DETAILS / 10'#13#10 +
             '             WHEN COORD_X_DETAILS BETWEEN   10 AND   99 THEN COORD_X_DETAILS / 100'#13#10 +
             '             WHEN COORD_X_DETAILS BETWEEN  100 AND  999 THEN COORD_X_DETAILS / 1000'#13#10 +
             '             WHEN COORD_X_DETAILS BETWEEN 1000 AND 9999 THEN COORD_X_DETAILS / 10000'#13#10 +
             '             ELSE 0'#13#10 +
             '        END + t.coord_x x,'#13#10 +
             '        CASE WHEN COORD_y_DETAILS BETWEEN    1 AND    9 THEN COORD_y_DETAILS / 10'#13#10 +
             '             WHEN COORD_y_DETAILS BETWEEN   10 AND   99 THEN COORD_y_DETAILS / 100'#13#10 +
             '             WHEN COORD_y_DETAILS BETWEEN  100 AND  999 THEN COORD_y_DETAILS / 1000'#13#10 +
             '             WHEN COORD_y_DETAILS BETWEEN 1000 AND 9999 THEN COORD_y_DETAILS / 10000'#13#10 +
             '             ELSE 0'#13#10 +
             '        END + t.coord_y y'#13#10 +
             'from v_tdr_char_points t where t.name like ''£ódS%'''#13#10 +
             ''#13#10 +
             ')  tab'#13#10 +
             'group by nr'#13#10 +
             ') tab'#13#10 +
             'where ( abs(DDDMMSS_2_DDD(x) - DDDMMSS_2_DDD(x_prime)) > 1/3600 or abs(DDDMMSS_2_DDD(y) - DDDMMSS_2_DDD(y_prime)) > 1/3600 );' );

  { date: 2012-11-09, file: old UNION internal intend.sql }
  TestQuery( 'select tab.*, replace(''UPDATE STUDNIA SET X=''||x_prime||'', Y=''||y_prime||'' WHERE NR=''''||tab.nr||'''';'', '','',''.'')'#13#10 +
             'from ('#13#10 +
             'select nr, max(x) x, max(y) y, max(x_prime) x_prime, max(y_prime) y_prime'#13#10 +
             'from ('#13#10 +
             'select  st.nr, st.x, st.y, null as x_prime, null as y_prime from studnia st where nr like ''£ódS%'''#13#10 +
             'union all'#13#10 +
             'select  t.name, null, null,'#13#10 +
             '        CASE WHEN COORD_X_DETAILS BETWEEN    1 AND    9 THEN COORD_X_DETAILS / 10'#13#10 +
             '             WHEN COORD_X_DETAILS BETWEEN   10 AND   99 THEN COORD_X_DETAILS / 100'#13#10 +
             '             WHEN COORD_X_DETAILS BETWEEN  100 AND  999 THEN COORD_X_DETAILS / 1000'#13#10 +
             '             WHEN COORD_X_DETAILS BETWEEN 1000 AND 9999 THEN COORD_X_DETAILS / 10000'#13#10 +
             '             ELSE 0'#13#10 +
             '        END + t.coord_x x,'#13#10 +
             '        CASE WHEN COORD_y_DETAILS BETWEEN    1 AND    9 THEN COORD_y_DETAILS / 10'#13#10 +
             '             WHEN COORD_y_DETAILS BETWEEN   10 AND   99 THEN COORD_y_DETAILS / 100'#13#10 +
             '             WHEN COORD_y_DETAILS BETWEEN  100 AND  999 THEN COORD_y_DETAILS / 1000'#13#10 +
             '             WHEN COORD_y_DETAILS BETWEEN 1000 AND 9999 THEN COORD_y_DETAILS / 10000'#13#10 +
             '             ELSE 0'#13#10 +
             '        END + t.coord_y y'#13#10 +
             'from v_tdr_char_points t where t.name like ''£ódS%'''#13#10 +
             ')  tab'#13#10 +
             'group by nr'#13#10 +
             ') tab'#13#10 +
             'where ( abs(DDDMMSS_2_DDD(x) - DDDMMSS_2_DDD(x_prime)) > 1/3600 or abs(DDDMMSS_2_DDD(y) - DDDMMSS_2_DDD(y_prime)) > 1/3600 );' );

  { date: 2012-11-09, file: old subsequent apostrophes.sql }
  TestQuery( 'select ''a''''b'' from dual' );

  { date: 2012-11-10, file: sub query expression intendation.sql }
  TestQuery( '           SELECT  ('#13#10 +
             '                              SELECT  L.OZNACZENIE || ''; '' || L.ADRES || '', '' || L.MIASTO'#13#10 +
             '                                FROM  WEZEL_KABEL AS WK2'#13#10 +
             '                                JOIN  WEZEL_ADRES_V AS L ON L.ID = WK2.ID_WEZEL'#13#10 +
             '                               WHERE  WK2.ID_TYP_ELEMENTU = WK.ID_TYP_ELEMENTU'#13#10 +
             '                                 AND  WK2.ID_ELEMENT = WK.ID_ELEMENT'#13#10 +
             '                                 AND  WK2.LP = WK.LP'#13#10 +
             '   )  AS WEZEL_2'#13#10 +
             '             FROM  WEZEL_KABEL AS WK'#13#10 +
             '            WHERE  WK.ID = AKTID ;' );

  { date: 2012-11-10, file: format - exists subqery.sql }
  TestQuery( 'select 1 from dual where exists(select 1 from dual)' );

  { date: 2012-11-10, file: format - in subqery.sql }
  TestQuery( 'select 1 from dual where a in (select 1 from dual)' );

  { date: 2012-11-17, file: intends and colors.sql -- problem due to ASC keyword skipped by default }
  TestQuery( '     SELECT  p.id'#13#10 +
             '          ,  v.element || '' . '' || p.nr AS nr'#13#10 +
             '          ,  pp.nazwa AS przeplywnosc'#13#10 +
             '          ,  pt.nazwa AS transport'#13#10 +
             '          ,  ptd.nazwa AS transport_det'#13#10 +
             '          ,  ps.nazwa AS sygnal'#13#10 +
             '          ,  psd.nazwa AS sygnal_det'#13#10 +
             '       FROM  port p'#13#10 +
             '  LEFT JOIN  wezel_v v ON v.id = p.id_wezel'#13#10 +
             '  LEFT JOIN  transport pt ON pt.id = p.id_transport'#13#10 +
             '  LEFT JOIN  transport_det ptd ON ptd.id = p.id_transport_det'#13#10 +
             '  LEFT JOIN  sygnal ps ON ps.id = p.id_sygnal'#13#10 +
             '  LEFT JOIN  sygnal_det psd ON psd.id = p.id_sygnal_det'#13#10 +
             '  LEFT JOIN  przeplywnosc pp ON pp.id = p.id_przeplywnosc'#13#10 +
             '      WHERE  p.id_wezel = 73006'#13#10 +
             '   ORDER BY  to_number ( nrpart ( p.nr, 2 ) ) ASC'#13#10 +
             '          ,  to_number ( nrpart ( p.nr, 4 ) ) ASC'#13#10 +
             '          ,  to_number ( nrpart ( p.nr, 6 ) ) ASC;' );

  { date: 2012-11-17, file: CAST function parse.sql }
//  TestQuery( 'CREATE OR REPLACE VIEW HD_USLUGA_INTERFACE AS'#13#10 +
//             'SELECT /*DISTINCT*/'#13#10 +
//             '       CAST(TAB.ID                      AS NUMBER( 8,0) ) AS ID,'#13#10 +
//             '       CAST(TAB.HD_PORT_1               AS NUMBER( 8,0) ) AS ID_PORT_INTERFACE_1,'#13#10 +
//             '       CAST(TAB.HD_PORT_2               AS NUMBER( 8,0) ) AS ID_PORT_INTERFACE_2,'#13#10 +
//             '       CAST(TAB.NRVLAN_1                AS NUMBER( 5,0) ) AS SUBINTERFACE_1,'#13#10 +
//             '       CAST(TAB.NRVLAN_2                AS NUMBER( 5,0) ) AS SUBINTERFACE_2,'#13#10 +
//             '       CAST(NULL                        AS NUMBER(10,0) ) AS SHAPING_CISCO,'#13#10 +
//             '       CAST(TAB.NRVLAN_1                AS NUMBER( 5,0) ) AS VLAN,'#13#10 +
//             '       CAST(NULL                        AS NUMBER( 5,0) ) AS GRUPA_HSRP,'#13#10 +
//             '--     CAST(USL.ID_HD/*, USL.ID_W_HD*/  AS NUMBER( 8,0) ) AS ID_USLUGATELE,'#13#10 +
//             '            USL.ID_HD/*, USL.ID_W_HD*/                    AS ID_USLUGATELE,'#13#10 +
//             '       CAST(NULL                        AS NUMBER       ) AS KTO_UTW,'#13#10 +
//             '       CAST(NULL                        AS DATE         ) AS DATA_UTW,'#13#10 +
//             '       CAST(''Y''                         AS CHAR(1)      ) AS GLOWNY_FLAG,'#13#10 +
//             '       CAST(TAB.PRZEPLYWNOSC            AS FLOAT(126)   ) AS RATE_LIMIT, /* kbps */'#13#10 +
//             '       CAST(NULL                        AS VARCHAR2(200)) AS UWAGI,'#13#10 +
//             '       CAST(NULL                        AS VARCHAR2(100)) AS UWAGI_ZMIANA,'#13#10 +
//             '       CAST(''Y''                         AS CHAR(1)      ) AS ROUTER_FLAG,'#13#10 +
//             '       CAST(TAB.TR_DROZNA               AS CHAR(1)      ) AS TRANSMISJA_DROZNA'#13#10 +
//             'FROM   ('#13#10 +
//             '        SELECT    MIN(ID) AS ID, ID_USLUGA, MIN(WARTOSC) AS PRZEPLYWNOSC,'#13#10 +
//             '                  CASE WHEN MAX(TR_DROZNA) = ''Y'' AND MIN(TR_DROZNA) = ''Y'' THEN ''Y'''#13#10 +
//             '                    -- WHEN MAX(TR_DROZNA) = ''N'' AND MIN(TR_DROZNA) = ''N'' THEN ''N'''#13#10 +
//             '                       ELSE ''N'''#13#10 +
//             '                  END  TR_DROZNA,'#13#10 +
//             '                  MAX(CASE WHEN ROWNO = 1 THEN NRVLAN END) AS NRVLAN_1,'#13#10 +
//             '                  MAX(CASE WHEN ROWNO = 2 THEN NRVLAN END) AS NRVLAN_2,'#13#10 +
//             '                  MAX(CASE WHEN ROWNO = 1 THEN ID_HD  END) AS HD_PORT_1,'#13#10 +
//             '                  MAX(CASE WHEN ROWNO = 2 THEN ID_HD  END) AS HD_PORT_2'#13#10 +
//             '        FROM ('#13#10 +
//             '                SELECT    TRP.ID, TRP.NRVLAN, PT.ID_HD, P.WARTOSC, UTR.ID_USLUGA,'#13#10 +
//             '                          NVL2(TRD.ID_TRANSMISJA, ''Y'', ''N'') AS TR_DROZNA,'#13#10 +
//             '                         (SELECT NVL(SUM(1),0)'#13#10 +
//             '                          FROM   TRANSMISJA_PORTY_ABCDE TRP2'#13#10 +
//             '                          JOIN   USLUGA_TRANSMISJA UTR2 ON UTR2.ID_TRANSMISJA = TRP2.ID_TRANSMISJA'#13#10 +
//             '                          JOIN   PORT               PT2 ON PT2.ID = TRP2.ID_PORT'#13#10 +
//             '                          JOIN   WEZEL               W2 ON W2.ID = PT2.ID_WEZEL'#13#10 +
//             '                          JOIN   URZADZENIE        URZ2 ON URZ2.ID = W2.ID_ELEMENT AND W2.ID_TYP_WEZLA = 5'#13#10 +
//             '                          WHERE  UTR2.ID_USLUGA = UTR.ID_USLUGA'#13#10 +
//             '                          AND    URZ2.ID_TYP_URZADZENIA = -6 /* router */'#13#10 +
//             '                          AND    TRP2.TYP = TRP.TYP'#13#10 +
//             '                          AND    TRP2.ID <= TRP.ID) ROWNO /* dziki pivot */'#13#10 +
//             '                FROM      TRANSMISJA_PORTY_ABCDE TRP'#13#10 +
//             '                JOIN      PORT                    PT ON PT.ID = TRP.ID_PORT'#13#10 +
//             '                JOIN      WEZEL                    W ON W.ID = PT.ID_WEZEL'#13#10 +
//             '                JOIN      URZADZENIE             URZ ON URZ.ID = W.ID_ELEMENT AND W.ID_TYP_WEZLA = 5'#13#10 +
//             '                JOIN      USLUGA_TRANSMISJA      UTR ON UTR.ID_TRANSMISJA = TRP.ID_TRANSMISJA'#13#10 +
//             '                LEFT JOIN TRANSMISJA_DROZNA      TRD ON TRD.ID_TRANSMISJA = TRP.ID_TRANSMISJA -- 2012-11-15, AL, przypadek ATMAN.1'#13#10 +
//             '                LEFT JOIN PRZEPLYWNOSC             P ON P.ID = TRP.ID_PRZEPLYWNOSC'#13#10 +
//             '                WHERE     1=1 --TRP.TYP = ''E'' -- 2012-09-28, A.Lemieszek, przypdaek _Akamai.1'#13#10 +
//             '                  AND     URZ.ID_TYP_URZADZENIA = -6 /* router */'#13#10 +
//             '       ) TAB'#13#10 +
//             '       GROUP BY   ID_USLUGA'#13#10 +
//             ') TAB'#13#10 +
//             'JOIN      USLUGA                 USL ON USL.ID = TAB.ID_USLUGA'#13#10 +
//             'UNION ALL'#13#10 +
//             'SELECT'#13#10 +
//             '       CAST(MIN(TRP.ID)                 AS NUMBER( 8,0) ) AS ID,'#13#10 +
//             '       CAST(NULL /*PT.ID_HD*/           AS NUMBER( 8,0) ) AS ID_PORT_INTERFACE_1,'#13#10 +
//             '       CAST(NULL                        AS NUMBER( 8,0) ) AS ID_PORT_INTERFACE_2,'#13#10 +
//             '       CAST(TRP.NRVLAN                  AS NUMBER( 5,0) ) AS SUBINTERFACE_1,'#13#10 +
//             '       CAST(NULL                        AS NUMBER( 5,0) ) AS SUBINTERFACE_2,'#13#10 +
//             '       CAST(NULL                        AS NUMBER(10,0) ) AS SHAPING_CISCO,'#13#10 +
//             '       CAST(TRP.NRVLAN                  AS NUMBER( 5,0) ) AS VLAN,'#13#10 +
//             '       CAST(NULL                        AS NUMBER( 5,0) ) AS GRUPA_HSRP,'#13#10 +
//             '--     CAST(USL.ID_HD/*, USL.ID_W_HD*/  AS NUMBER( 8,0) ) AS ID_USLUGATELE,'#13#10 +
//             '            USL.ID_HD/*, USL.ID_W_HD*/                    AS ID_USLUGATELE,'#13#10 +
//             '       CAST(NULL                        AS NUMBER       ) AS KTO_UTW,'#13#10 +
//             '       CAST(NULL                        AS DATE         ) AS DATA_UTW,'#13#10 +
//             '       CAST(''Y''                         AS CHAR(1)      ) AS GLOWNY_FLAG,'#13#10 +
//             '       CAST(MIN(PRZ.WARTOSC)            AS FLOAT(126)   ) AS RATE_LIMIT, /* kbps */'#13#10 +
//             '       CAST(NULL                        AS VARCHAR2(200)) AS UWAGI,'#13#10 +
//             '       CAST(NULL                        AS VARCHAR2(100)) AS UWAGI_ZMIANA,'#13#10 +
//             '       CAST(''N''                         AS CHAR(1)      ) AS ROUTER_FLAG,'#13#10 +
//             '--     CAST(NVL2(TRD.ID_TRANSMISJA, ''Y'', ''N'') AS CHAR(1)) AS TRANSMISJA_DROZNA'#13#10 +
//             '       CAST(CASE WHEN MAX(NVL2(TRD.ID_TRANSMISJA, ''Y'', ''N'')) = ''Y'' AND MIN(NVL2(TRD.ID_TRANSMISJA, ''Y'', ''N'')) = ''Y'' THEN ''Y'''#13#10 +
//             '              -- WHEN MAX(NVL2(TRD.ID_TRANSMISJA, ''Y'', ''N'')) = ''N'' AND MIN(NVL2(TRD.ID_TRANSMISJA, ''Y'', ''N'')) = ''N'' THEN ''N'''#13#10 +
//             '                 ELSE ''N'''#13#10 +
//             '            END                         AS CHAR(1)      ) AS TRANSMISJA_DROZNA'#13#10 +
//             ''#13#10 +
//             ''#13#10 +
//             'FROM      TRANSMISJA_PORTY_ABCDE TRP'#13#10 +
//             'JOIN      USLUGA_TRANSMISJA      UTR ON UTR.ID_TRANSMISJA = TRP.ID_TRANSMISJA'#13#10 +
//             'JOIN      USLUGA                 USL ON USL.ID = UTR.ID_USLUGA'#13#10 +
//             'JOIN      PORT                    PT ON PT.ID = TRP.ID_PORT'#13#10 +
//             'JOIN      WEZEL                    W ON W.ID = PT.ID_WEZEL'#13#10 +
//             'JOIN      URZADZENIE             URZ ON URZ.ID = W.ID_ELEMENT AND W.ID_TYP_WEZLA = 5'#13#10 +
//             'LEFT JOIN PRZEPLYWNOSC           PRZ ON PRZ.ID = TRP.ID_PRZEPLYWNOSC'#13#10 +
//             'LEFT JOIN TRANSMISJA_DROZNA      TRD ON TRD.ID_TRANSMISJA = TRP.ID_TRANSMISJA -- 2012-11-15, AL, przypadek ATMAN.1'#13#10 +
//             'WHERE     TRP.TYP = ''E'''#13#10 +
//             'AND       URZ.ID_TYP_URZADZENIA != -6 /* router */'#13#10 +
//             'AND       TRP.NRVLAN NOT IN ('#13#10 +
//             '                SELECT    TRP2.NRVLAN'#13#10 +
//             '                FROM      TRANSMISJA_PORTY_ABCDE TRP2'#13#10 +
//             '                JOIN      PORT                    PT ON PT.ID = TRP2.ID_PORT'#13#10 +
//             '                JOIN      WEZEL                    W ON W.ID = PT.ID_WEZEL'#13#10 +
//             '                JOIN      URZADZENIE             URZ ON URZ.ID = W.ID_ELEMENT AND W.ID_TYP_WEZLA = 5'#13#10 +
//             '                WHERE     TRP2.TYP = ''E'''#13#10 +
//             '               AND  TRP2.ID_TRANSMISJA = TRP.ID_TRANSMISJA'#13#10 +
//             '                AND       URZ.ID_TYP_URZADZENIA = -6 /* router */'#13#10 +
//             ')'#13#10 +
//             'GROUP BY  UTR.ID_USLUGA, /*NVL(USL.ID_HD,*/ USL.ID_HD, TRP.NRVLAN'#13#10 +
//             'UNION /*ALL*/'#13#10 +
//             'SELECT'#13#10 +
//             '       CAST(MIN(TRP.ID)                 AS NUMBER( 8,0) ) AS ID,'#13#10 +
//             '       CAST(NULL /*PT.ID_HD*/           AS NUMBER( 8,0) ) AS ID_PORT_INTERFACE_1,'#13#10 +
//             '       CAST(NULL                        AS NUMBER( 8,0) ) AS ID_PORT_INTERFACE_2,'#13#10 +
//             '       CAST(NULL                        AS NUMBER( 5,0) ) AS SUBINTERFACE_1,'#13#10 +
//             '       CAST(NULL                        AS NUMBER( 5,0) ) AS SUBINTERFACE_2,'#13#10 +
//             '       CAST(NULL                        AS NUMBER(10,0) ) AS SHAPING_CISCO,'#13#10 +
//             '       CAST(NULL                        AS NUMBER( 5,0) ) AS VLAN,'#13#10 +
//             '       CAST(NULL                        AS NUMBER( 5,0) ) AS GRUPA_HSRP,'#13#10 +
//             '--     CAST(USL.ID_HD/*, USL.ID_W_HD*/  AS NUMBER( 8,0) ) AS ID_USLUGATELE,'#13#10 +
//             '            USL.ID_HD/*, USL.ID_W_HD*/                    AS ID_USLUGATELE,'#13#10 +
//             '       CAST(NULL                        AS NUMBER       ) AS KTO_UTW,'#13#10 +
//             '       CAST(NULL                        AS DATE         ) AS DATA_UTW,'#13#10 +
//             '       CAST(''Y''                         AS CHAR(1)      ) AS GLOWNY_FLAG,'#13#10 +
//             '       CAST(MIN(PRZ.WARTOSC)            AS FLOAT(126)   ) AS RATE_LIMIT, /* kbps */'#13#10 +
//             '       CAST(NULL                        AS VARCHAR2(200)) AS UWAGI,'#13#10 +
//             '       CAST(NULL                        AS VARCHAR2(100)) AS UWAGI_ZMIANA,'#13#10 +
//             '       CAST(''N''                         AS CHAR(1)      ) AS ROUTER_FLAG,'#13#10 +
//             '--     CAST(NVL2(TRD.ID_TRANSMISJA, ''Y'', ''N'') AS CHAR(1)) AS TRANSMISJA_DROZNA'#13#10 +
//             '       CAST(CASE WHEN MAX(NVL2(TRD.ID_TRANSMISJA, ''Y'', ''N'')) = ''Y'' AND MIN(NVL2(TRD.ID_TRANSMISJA, ''Y'', ''N'')) = ''Y'' THEN ''Y'''#13#10 +
//             '              -- WHEN MAX(NVL2(TRD.ID_TRANSMISJA, ''Y'', ''N'')) = ''N'' AND MIN(NVL2(TRD.ID_TRANSMISJA, ''Y'', ''N'')) = ''N'' THEN ''N'''#13#10 +
//             '                 ELSE ''N'''#13#10 +
//             '            END                         AS CHAR(1)      ) AS TRANSMISJA_DROZNA'#13#10 +
//             'FROM      TRANSMISJA_PORTY_ABCDE TRP'#13#10 +
//             'JOIN      USLUGA_TRANSMISJA      UTR ON UTR.ID_TRANSMISJA = TRP.ID_TRANSMISJA'#13#10 +
//             'JOIN      USLUGA                 USL ON USL.ID = UTR.ID_USLUGA'#13#10 +
//             'LEFT JOIN PRZEPLYWNOSC           PRZ ON PRZ.ID = TRP.ID_PRZEPLYWNOSC'#13#10 +
//             'LEFT JOIN TRANSMISJA_DROZNA      TRD ON TRD.ID_TRANSMISJA = TRP.ID_TRANSMISJA -- 2012-11-15, AL, przypadek ATMAN.1'#13#10 +
//             'WHERE     TRP.TYP = ''F'''#13#10 +
//             'GROUP BY  USL.ID_HD;' );

  { date: 2012-11-17, file: minus expression.sql }
  TestQuery( '       SELECT  W.ID'#13#10 +
             '            ,  W.ID_WEZEL'#13#10 +
             '            ,  NVL ( W.ZAPAS_1, 0 ) + NVL ( W.ZAPAS_2, 0 ) AS ZAPAS'#13#10 +
             '            ,  W.ID_TYP_ELEMENTU'#13#10 +
             '            ,  W.ID_ELEMENT'#13#10 +
             '            ,  DECODE ( W.ID_TYP_ELEMENTU, 101, S.NR, 112, I.NR, 111, C.NR ) AS NR'#13#10 +
             '            ,  DECODE ( W.ID_TYP_ELEMENTU, 101, S.UWAGI, 112, I.UWAGI, 111, C.UWAGI ) AS UWAGI'#13#10 +
             '            ,  DECODE ( W.ID_TYP_ELEMENTU, 101, ST.NAZWA, 112, IT.NAZWA, 111, CT.NAZWA ) AS TYP'#13#10 +
             '            ,  DECODE ( W.ID_TYP_ELEMENTU, 101, SW1.ELEMENT, 112, IW1.ELEMENT, 111, CW1.ELEMENT ) AS WEZEL1'#13#10 +
             '            ,  DECODE ( W.ID_TYP_ELEMENTU, 101, SW2.ELEMENT, 112, IW2.ELEMENT, 111, CW2.ELEMENT ) AS WEZEL2'#13#10 +
             '            ,  DECODE ( W.ID_TYP_ELEMENTU, 101, ST.LICZBA, 112, 1, 111, CT.LICZBAPAR ) AS LICZBA'#13#10 +
             '            ,  DECODE ( W.ID_TYP_ELEMENTU, 101, S.DLUGOSC_TRASOWA, 112, I.DLUGOSC_TRASOWA, C.DLUGOSC_TRASOWA ) AS DLUGOSC_TRASOWA'#13#10 +
             '            ,  NVL ( ST.LICZBA - ILEWLOKIENWOLNYCH ( S.ID ), 0 ) AS ZAJETYCH'#13#10 +
             '            ,  S.DLUGOSC_OPTYCZNA AS DLUGOSC_OPTYCZNA'#13#10 +
             '         FROM  WEZEL_KABEL W'#13#10 +
             '    LEFT JOIN  SWIATLOWOD_WKV S ON W.ID_TYP_ELEMENTU = 101 AND W.ID_ELEMENT = S.ID'#13#10 +
             '    LEFT JOIN  TYP_SWIATLOWODU ST ON S.ID_TYP_SWIATLOWODU = ST.ID'#13#10 +
             '    LEFT JOIN  WEZEL_V SW1 ON S.ID_WEZEL_1 = SW1.ID'#13#10 +
             '    LEFT JOIN  WEZEL_V SW2 ON S.ID_WEZEL_2 = SW2.ID'#13#10 +
             '    LEFT JOIN  WIAZKA_INF I ON W.ID_TYP_ELEMENTU = 112 AND W.ID_ELEMENT = I.ID'#13#10 +
             '    LEFT JOIN  TYP_KABLA_INF IT ON I.ID_TYP_KABLA_INF = IT.ID'#13#10 +
             '    LEFT JOIN  WEZEL_V IW1 ON I.ID_WEZEL_1 = IW1.ID'#13#10 +
             '    LEFT JOIN  WEZEL_V IW2 ON I.ID_WEZEL_2 = IW2.ID'#13#10 +
             '    LEFT JOIN  KABEL_CU C ON W.ID_TYP_ELEMENTU = 111 AND W.ID_ELEMENT = C.ID'#13#10 +
             '    LEFT JOIN  TYP_KABLA_CU CT ON C.ID_TYP_KABLA_CU = CT.ID'#13#10 +
             '    LEFT JOIN  WEZEL_V CW1 ON C.ID_WEZEL_1 = CW1.ID'#13#10 +
             '    LEFT JOIN  WEZEL_V CW2 ON C.ID_WEZEL_2 = CW2.ID'#13#10 +
             '        WHERE  W.ID_WEZEL = :qWezelDlaSzaf.ID'#13#10 +
             '     ORDER BY  4 ASC;' );

  { date: 2012-11-17, file: subquery intends.sql }
  TestQuery( '       UPDATE  KANALIZACJA K'#13#10 +
             '          SET  NR = NEXTNRKANALIZACJI ( ID_SM, ID_WEZEL_1, ID_WEZEL_2 )'#13#10 +
             '        WHERE  ID IN ('#13#10 +
             '       SELECT  ID_ELEMENT'#13#10 +
             '         FROM  IMP_KANALIZACJA'#13#10 +
             '        WHERE  ID_TYP_ELEMENTU = 102'#13#10 +
             ''#13#10 +
             '    UNION ALL'#13#10 +
             ''#13#10 +
             '       SELECT  ID_ELEMENT_KANAL'#13#10 +
             '         FROM  IMP_KANALIZACJA'#13#10 +
             '        WHERE  ID_ELEMENT_KANAL IS NOT NULL )'#13#10 +
             '          AND  NOT EXISTS ('#13#10 +
             '       SELECT  1'#13#10 +
             '         FROM  KANALIZACJA K2'#13#10 +
             '        WHERE  K2.NR = NEXTNRKANALIZACJI ( K.ID_SM, K.ID_WEZEL_1, K.ID_WEZEL_2 ) );' );

  { date: 2012-11-24, file: alter constraint.sql }
  TestQuery( '    ALTER TABLE TRANSPORT_DET'#13#10 +
             '            ADD ID_TRANSPORT_PARENT INT'#13#10 +
             '     CONSTRAINT TRANSPORT_DET$FK_TR'#13#10 +
             '     REFERENCES TRANSPORT;' );

  { date: 2012-11-24, file: lister long expression.sql }
  TestQuery( '       SELECT  SUBSTR ( DECODE ( S1.NR, NULL, '''', CHR ( 13 ) || CHR ( 10 ) || ''w³ókno: '' || DECODE ( S1.NR, NULL, NULL, S1.NR || ''#'' ) || DECODE ( W1.NR_ODCINKA, NULL, TO_CHAR ( W1.NR ), W1.NR_ODCINKA ) ) || ' +
                                     'DECODE ( S2.NR, NULL, '''', CHR ( 13 ) || CHR ( 10 ) || ''w³ókno: '' || DECODE ( S2.NR, NULL, NULL, S2.NR || ''#'' ) || DECODE ( W2.NR_ODCINKA, NULL, TO_CHAR ( W2.NR ), W2.NR_ODCINKA ) ) || ' +
                                     'DECODE ( K1.NR, NULL, '''', CHR ( 13 ) || CHR ( 10 ) || ''para: '' || DECODE ( K1.NR, NULL, NULL, K1.NR || ''#'' ) || DECODE ( PR1.NR_ODCINKA, NULL, TO_CHAR ( PR1.NR ), PR1.NR_ODCINKA ) ) || ' +
                                     'DECODE ( K2.NR, NULL, '''', CHR ( 13 ) || CHR ( 10 ) || ''para: '' || DECODE ( K2.NR, NULL, NULL, K2.NR || ''#'' ) || DECODE ( PR2.NR_ODCINKA, NULL, TO_CHAR ( PR2.NR ), PR2.NR_ODCINKA ) ) || ' +
                                     'DECODE ( P1.SYMBOL, NULL, '''', CHR ( 13 ) || CHR ( 10 ) || ''patchcord: '' || P1.SYMBOL ) || ' +
                                     'DECODE ( P2.SYMBOL, NULL, '''', CHR ( 13 ) || CHR ( 10 ) || ''patchcord: '' || P2.SYMBOL ) || ' +
                                     'DECODE ( T1.NR, NULL, '''', CHR ( 13 ) || CHR ( 10 ) || ''trasa obca: '' || T1.NR ) || ' +
                                     'DECODE ( T2.NR, NULL, '''', CHR ( 13 ) || CHR ( 10 ) || ''trasa obca: '' || T2.NR ) || ' +
                                     'DECODE ( KK1.NR, NULL, '''', CHR ( 13 ) || CHR ( 10 ) || ''koncentryk: '' || KK1.NR ) || ' +
                                     'DECODE ( KK2.NR, NULL, '''', CHR ( 13 ) || CHR ( 10 ) || ''koncentryk: '' || KK2.NR ) || ' +
                                     'DECODE ( KI1.NR, NULL, '''', CHR ( 13 ) || CHR ( 10 ) || ''kabel: '' || KI1.NR ) || ' +
                                     'DECODE ( KI2.NR, NULL, '''', CHR ( 13 ) || CHR ( 10 ) || ''kabel: '' || KI2.NR ) || ' +
                                     'DECODE ( UW1.ID, NULL, '''', CHR ( 13 ) || CHR ( 10 ) || ''w³ókno urz: '' || URZ1.NR ) || ' +
                                     'DECODE ( UW2.ID, NULL, '''', CHR ( 13 ) || CHR ( 10 ) || ''w³ókno urz: '' || URZ2.NR ), 3 )'#13#10 +
             '         FROM  GNIAZDO G'#13#10 +
             '    LEFT JOIN  WLOKNO W1 ON ( W1.ID_GNIAZDO_1 = G.ID OR W1.ID_GNIAZDO_2 = G.ID )'#13#10 +
             '    LEFT JOIN  WLOKNO W2 ON ( W2.ID_GNIAZDO_1 = G.ID OR W2.ID_GNIAZDO_2 = G.ID ) AND ( W1.ID <> W2.ID )'#13#10 +
             '    LEFT JOIN  SWIATLOWOD S1 ON W1.ID_SWIATLOWOD = S1.ID'#13#10 +
             '    LEFT JOIN  SWIATLOWOD S2 ON W2.ID_SWIATLOWOD = S2.ID'#13#10 +
             '    LEFT JOIN  TRASA_SW T1 ON ( T1.ID_GNIAZDO_1 = G.ID OR T1.ID_GNIAZDO_2 = G.ID ) AND NVL ( T1.B_ZESPOLONA, 0 ) = 0 AND T1.B_DZIERZAWA = 1'#13#10 +
             '    LEFT JOIN  TRASA_SW T2 ON ( T2.ID_GNIAZDO_1 = G.ID OR T2.ID_GNIAZDO_2 = G.ID ) AND ( T1.ID <> T2.ID ) AND NVL ( T2.B_ZESPOLONA, 0 ) = 0 AND T2.B_DZIERZAWA = 1'#13#10 +
             '    LEFT JOIN  TRASA_SW_POL P1 ON ( P1.ID_GNIAZDO_1 = G.ID OR P1.ID_GNIAZDO_2 = G.ID ) AND P1.ID_TYP_POLACZENIA = 1'#13#10 +
             '    LEFT JOIN  TRASA_SW_POL P2 ON ( P2.ID_GNIAZDO_1 = G.ID OR P2.ID_GNIAZDO_2 = G.ID ) AND ( P1.ID <> P2.ID ) AND P2.ID_TYP_POLACZENIA = 1'#13#10 +
             '    LEFT JOIN  PARA_CU PR1 ON ( PR1.ID_GNIAZDO_1 = G.ID OR PR1.ID_GNIAZDO_2 = G.ID )'#13#10 +
             '    LEFT JOIN  PARA_CU PR2 ON ( PR2.ID_GNIAZDO_1 = G.ID OR PR2.ID_GNIAZDO_2 = G.ID ) AND ( PR1.ID <> PR2.ID )'#13#10 +
             '    LEFT JOIN  KABEL_CU K1 ON PR1.ID_KABEL_CU = K1.ID'#13#10 +
             '    LEFT JOIN  KABEL_CU K2 ON PR2.ID_KABEL_CU = K2.ID'#13#10 +
             '    LEFT JOIN  PATCHCORD_CU PCU1 ON ( PCU1.ID_GNIAZDO_1 = G.ID OR PCU1.ID_GNIAZDO_2 = G.ID )'#13#10 +
             '    LEFT JOIN  PATCHCORD_CU PCU2 ON ( PCU2.ID_GNIAZDO_1 = G.ID OR PCU2.ID_GNIAZDO_2 = G.ID ) AND ( PCU1.ID <> PCU2.ID )'#13#10 +
             '    LEFT JOIN  KABEL_INF KI1 ON ( KI1.ID_GNIAZDO_1 = G.ID OR KI1.ID_GNIAZDO_2 = G.ID )'#13#10 +
             '    LEFT JOIN  KABEL_INF KI2 ON ( KI2.ID_GNIAZDO_1 = G.ID OR KI2.ID_GNIAZDO_2 = G.ID ) AND ( KI1.ID <> KI2.ID )'#13#10 +
             '    LEFT JOIN  KABEL_KONCENTRYCZNY KK1 ON ( KK1.ID_GNIAZDO_1 = G.ID OR KK1.ID_GNIAZDO_2 = G.ID )'#13#10 +
             '    LEFT JOIN  KABEL_KONCENTRYCZNY KK2 ON ( KK2.ID_GNIAZDO_1 = G.ID OR KK2.ID_GNIAZDO_2 = G.ID ) AND KK1.ID != KK2.ID'#13#10 +
             '    LEFT JOIN  URZADZENIE_WLOKNO UW1 ON ( UW1.ID_GNIAZDO_1 = G.ID OR UW1.ID_GNIAZDO_2 = G.ID )'#13#10 +
             '    LEFT JOIN  URZADZENIE_WLOKNO UW2 ON ( UW2.ID_GNIAZDO_1 = G.ID OR UW2.ID_GNIAZDO_2 = G.ID ) AND UW1.ID != UW2.ID'#13#10 +
             '    LEFT JOIN  URZADZENIE URZ1 ON URZ1.ID = UW1.ID_URZADZENIE'#13#10 +
             '    LEFT JOIN  URZADZENIE URZ2 ON URZ2.ID = UW2.ID_URZADZENIE'#13#10 +
             '        WHERE  G.ID = AGNIAZDOID'#13#10 +
             '          AND  ROWNUM = 1;' );

  { date: 2012-12-11, file: concat changed to div.sql }
  TestQuery( 'CREATE OR REPLACE VIEW KLASY_IP4_V AS'#13#10 +
             'SELECT K.ID,'#13#10 +
             'K.ID_PARENT,'#13#10 +
             'K.ID_W_HD,'#13#10 +
             'K.ID_TYP_KLASY_IP,'#13#10 +
             'TK.CIDR,'#13#10 +
             'TK.NO_IP_ADDRES,'#13#10 +
             'K.IP,'#13#10 +
             'K.BROADCAST,'#13#10 +
             'BITAND( K.IP, 4278190080  ) / 16777216          AS IP_A,'#13#10 +
             'BITAND( BITAND( K.IP,   16711680  ) /    65536  , 255 ) AS IP_B,'#13#10 +
             'BITAND( BITAND( K.IP,      65280  ) /      256  , 255 ) AS IP_C,'#13#10 +
             'BITAND( K.IP,        255  )                     AS IP_D,'#13#10 +
             'BITAND( K.BROADCAST, 4278190080  ) / 16777216          AS BROADCAST_A,'#13#10 +
             'BITAND( BITAND( K.BROADCAST,   16711680  ) /    65536  , 255 ) AS BROADCAST_B,'#13#10 +
             'BITAND( BITAND( K.BROADCAST,      65280  ) /      256  , 255 ) AS BROADCAST_C,'#13#10 +
             'BITAND( K.BROADCAST,        255  )                     AS BROADCAST_D,'#13#10 +
             'K.ID_PRZEZNACZENIE_IP,'#13#10 +
             'K.B_OBCA,'#13#10 +
             'UIP.ID_USLUGA,'#13#10 +
             'U.NR AS NR_USLUGA,'#13#10 +
             'CASE WHEN (SELECT MIN(ID) FROM KLASY_IP4 WHERE ID_PARENT = K.ID) IS NOT NULL THEN ''nadrzedna'' END AS HAS_CHILD,'#13#10 +
             'K.UWAGI,'#13#10 +
             'BITAND( K.IP, 4278190080  ) / 16777216          || ''.'' ||'#13#10 +
             'BITAND( BITAND( K.IP,   16711680  ) /    65536  , 255 ) || ''.'' ||'#13#10 +
             'BITAND( BITAND( K.IP,      65280  ) /      256  , 255 ) || ''.'' ||'#13#10 +
             'BITAND( K.IP,        255  )                     ||'#13#10 +
             'TK.CIDR AS IP_CIDR,'#13#10 +
             'BITAND( KP.IP, 4278190080  ) / 16777216          || ''.'' ||'#13#10 +
             'BITAND( BITAND( KP.IP,   16711680  ) /    65536  , 255 ) || ''.'' ||'#13#10 +
             'BITAND( BITAND( KP.IP,      65280  ) /      256  , 255 ) || ''.'' ||'#13#10 +
             'BITAND( KP.IP,        255  )                     ||'#13#10 +
             'TKP.CIDR AS IP_CIDR_PARENT'#13#10 +
             'FROM      KLASY_IP4 K'#13#10 +
             'LEFT JOIN TYP_KLASY_IP  TK ON TK.ID = K.ID_TYP_KLASY_IP'#13#10 +
             'LEFT JOIN USLUGA_IP    UIP ON UIP.ID_KLASY_IP4 = K.ID'#13#10 +
             'LEFT JOIN USLUGA         U ON U.ID = UIP.ID_USLUGA'#13#10 +
             'LEFT JOIN KLASY_IP4     KP ON KP.ID = K.ID_PARENT'#13#10 +
             'LEFT JOIN TYP_KLASY_IP TKP ON TKP.ID = KP.ID_TYP_KLASY_IP;' );

  { date: 2012-12-29, file: case in.sql }
  TestQuery( 'SELECT CASE WHEN PT.ID IN (401, 847, 720, 867, 868, 869, 403, 400, 885, 100, 120,'#13#10 +
             '                      420, 421, 422, 430 /*= 859 ??*/, 857, 110, 202, 200, 201, 868, 835,'#13#10 +
             '                      420, 402, 401, 404, 200, 201)'#13#10 +
             '       THEN ''USED'' END,'#13#10 +
             '       PT.*'#13#10 +
             'FROM   PASSPORT_TYPE PT'#13#10 +
             'ORDER BY ID' );

  { date: 2012-12-29, file: in condition.sql }
  TestQuery( 'SELECT * FROM swiatlowod WHERE 29830 IN (ID_WEZEL_1, ID_WEZEL_2);' );

  { date: 2012-12-29, file: order by and where and join conditions.sql }
  TestQuery( 'SELECT'#13#10 +
             'W.*'#13#10 +
             ','#13#10 +
             'StatusEksploatacji_WloknoInt(W.ID) AS B_ZAJETE,'#13#10 +
             'WL.NAZWA AS WLASCICIEL,'#13#10 +
             'G1.NR AS GNIAZDO1, SUBSTR( W1.ELEMENT, 1, 50 ) AS P1,'#13#10 +
             'G2.NR AS GNIAZDO2, SUBSTR( W2.ELEMENT, 1, 50 ) AS P2,'#13#10 +
             '          NVL(STA.NR, LOKA.SYMBOL) AS WEZEL_A, NVL(STB.NR, LOKB.SYMBOL) AS WEZEL_B,'#13#10 +
             '          CASE WHEN G1.ID_TYP_GNIAZDA = -1 THEN ''spaw'' ELSE SUBSTR( COALESCE(U1.NR, P1.NR), 1, 50 ) || '' . '' || G1.NR END AS PORT1,'#13#10 +
             '          CASE WHEN G2.ID_TYP_GNIAZDA = -1 THEN ''spaw'' ELSE SUBSTR( COALESCE(U2.NR, P2.NR), 1, 50 ) || '' . '' || G2.NR END AS PORT2,'#13#10 +
             'S.NAZWA AS STATUS,'#13#10 +
             'GETTRASADLAWLOKNA(W.ID) AS TRASA,'#13#10 +
             '(SELECT DECODE(COUNT(*), 0, 1, COUNT(*)) FROM WLOKNO WHERE ID_WLOKNO = W.ID) AS SEG,'#13#10 +
             'GETPROJEKTDLAELEMENTU(107, W.ID) AS PROJEKT,'#13#10 +
             'GETTRASAIDDLAWLOKNA(W.ID) AS ID_TRASA,'#13#10 +
             'WloknoSwUslugi (W.ID) AS USLUGI'#13#10 +
             ''#13#10 +
             'FROM WLOKNO W'#13#10 +
             'LEFT JOIN GNIAZDO G1 ON W.ID_GNIAZDO_1 = G1.ID'#13#10 +
             'LEFT JOIN GNIAZDO G2 ON W.ID_GNIAZDO_2 = G2.ID'#13#10 +
             '--LEFT JOIN WEZEL_V W1 ON G1.ID_WEZEL = W1.ID AND W1.ID_TYP_WEZLA IN (3, 5, 23)'#13#10 +
             '--LEFT JOIN WEZEL_V W2 ON G2.ID_WEZEL = W2.ID AND W2.ID_TYP_WEZLA IN (3, 5, 23)'#13#10 +
             'LEFT JOIN WLASCICIEL WL ON WL.ID = W.ID_WLASCICIEL'#13#10 +
             'LEFT JOIN STATUS S ON S.ID = W.ID_STATUS'#13#10 +
             ''#13#10 +
             'LEFT JOIN WEZEL         WA ON WA.ID = W.ID_WEZEL_1'#13#10 +
             'LEFT JOIN WEZEL         WB ON WB.ID = W.ID_WEZEL_2'#13#10 +
             'LEFT JOIN STUDNIA      STA ON STA.ID = WA.ID_ELEMENT AND WA.ID_TYP_WEZLA = 2'#13#10 +
             'LEFT JOIN STUDNIA      STB ON STB.ID = WB.ID_ELEMENT AND WB.ID_TYP_WEZLA = 2'#13#10 +
             'LEFT JOIN LOKALIZACJA LOKA ON LOKA.ID = WA.ID_ELEMENT AND WA.ID_TYP_WEZLA = 24'#13#10 +
             'LEFT JOIN LOKALIZACJA LOKB ON LOKB.ID = WB.ID_ELEMENT AND WB.ID_TYP_WEZLA = 24'#13#10 +
             'LEFT JOIN WEZEL_V       W1 ON W1.ID = G1.ID_WEZEL AND G1.ID_TYP_GNIAZDA != -1'#13#10 +
             'LEFT JOIN URZADZENIE    U1 ON U1.ID = W1.ID_ELEMENT AND W1.ID_TYP_WEZLA = 5'#13#10 +
             'LEFT JOIN PRZELACZNICA  P1 ON P1.ID = W1.ID_ELEMENT AND W1.ID_TYP_WEZLA = 3'#13#10 +
             'LEFT JOIN WEZEL_V       W2 ON W2.ID = G2.ID_WEZEL AND G2.ID_TYP_GNIAZDA != -1'#13#10 +
             'LEFT JOIN URZADZENIE    U2 ON U2.ID = W2.ID_ELEMENT AND W2.ID_TYP_WEZLA = 5'#13#10 +
             'LEFT JOIN PRZELACZNICA  P2 ON P2.ID = W2.ID_ELEMENT AND W2.ID_TYP_WEZLA = 3'#13#10 +
             ''#13#10 +
             'WHERE'#13#10 +
             'W.ID_SWIATLOWOD = 4564 --:qEdit.ID'#13#10 +
             'AND W.ID_WLOKNO IS NULL'#13#10 +
             ''#13#10 +
             'ORDER BY'#13#10 +
             'W.NR' );

  { date: 2012-12-29, file: YA stops working.sql }


  { date: 2012-12-29, file: old grants.sql }
  TestQuery( '-- PST:' );
  TestQuery( 'CREATE OR REPLACE PUBLIC SYNONYM /*PST.*/ FAST_ATTACHMENTS FOR PST_SYSTEM.FAST_ATTACHMENTS;' );
  TestQuery( '-- PST_SYSTEM:' );
  TestQuery( 'GRANT SELECT ON PST.FAST_ATTACHMENTS TO PST;' );
  TestQuery( 'GRANT INSERT ON PST.FAST_ATTACHMENTS TO PST;' );
  TestQuery( 'GRANT SELECT ON PST_SYSTEM.FAST_ATTACHMENTS TO PST;' );
  TestQuery( 'GRANT INSERT ON PST_SYSTEM.FAST_ATTACHMENTS TO PST;' );

  { date: 2013-01-23, file: kwerenda do HD_USLUGA_INTERFACE MDembka.sql }
  TestQuery( 'select distinct '#13#10 +
             '       ut.id_uslugatele as id_uslugatele'#13#10 +
             '      ,ut.id_rodzaju'#13#10 +
             '      ,transakcja.id_klienta'#13#10 +
             '      ,k_kht.prowadzi_atman as id_hpk'#13#10 +
             '      ,lejek_projekt.numer'#13#10 +
             '      ,hpp.id_pracownika'#13#10 +
             '      ,ut.nazwaid as nazwa'#13#10 +
             '      ,ut.nazwa as opis'#13#10 +
             '      ,ut.obca_flag as czy_obca'#13#10 +
             '      ,st.status'#13#10 +
             '      ,(select nvl(max(data), ut.data_utw) from drk.rek__log_01'#13#10 +
             '            where obiekt = ''Us³uga tele.'' and id_obiektu = ut.id_uslugatele and opis = ''Zmiana statusu'') as data_zmiany_statusu'#13#10 +
             '      ,ut.data_uruchomienia'#13#10 +
             '      ,ut.data_zakonczenia'#13#10 +
             '      ,ut.uwagi_kanal as uwagi_dla_klienta'#13#10 +
             '      ,drk.uslugatele_przeplywnosc(ut.id_uslugatele) as przeplywnosc_ip'#13#10 +
             '      ,pracownik.nazwisko || '' '' || pracownik.imie as opiekun'#13#10 +
             '      ,case pr_ok_flag'#13#10 +
             '            when ''Y'' then'#13#10 +
             '                 case ut.pr_nr'#13#10 +
             '                        when 0 then ''Abon. sta³y'''#13#10 +
             '                        when 1 then ''Limit ruchu GB'''#13#10 +
             '                        when 11 then ''Limit ruchu Mb'''#13#10 +
             '                        when 2 then ''N-ty percentyl'''#13#10 +
             '                        when 3 then ''Pakiet GB'''#13#10 +
             '                        else null'#13#10 +
             '                  end'#13#10 +
             '            else null'#13#10 +
             '      end as plan_ratingowy'#13#10 +
             '      ,case pr_ok_flag'#13#10 +
             '            when ''Y'' then'#13#10 +
             '                  case ut.pr_nr'#13#10 +
             '                        when 1 then to_char(ut.pr1_limit)'#13#10 +
             '                        when 11 then to_char(ut.pr1_limit)'#13#10 +
             '                        when 2 then trim('#13#10 +
             '                                    nvl(to_char(ut.pr_limit1),'' '') || '' '' || nvl(trim(to_char(ut.pr_stawka1, ''9990.00'')),'' '') || '' '' ||'#13#10 +
             '                                    nvl(to_char(ut.pr_limit2),'' '') || '' '' || nvl(trim(to_char(ut.pr_stawka2, ''9990.00'')),'' '') || '' '' ||'#13#10 +
             '                                    nvl(to_char(ut.pr_limit3),'' '') || '' '' || nvl(trim(to_char(ut.pr_stawka3, ''9990.00'')),'' '') || '' '''#13#10 +
             '                                    )'#13#10 +
             '                        when 3 then to_char(ut.pr_limit1)'#13#10 +
             '                        else '''''#13#10 +
             '                  end'#13#10 +
             '            else '''''#13#10 +
             '     end as parametry_planu'#13#10 +
             '     ,(select id_urzadzenie from drk.rek__ps_port_v, drk.rek__uslugatele_ps'#13#10 +
             '                 where rek__ps_port_v.id = rek__uslugatele_ps.id_port and rek__uslugatele_ps.styk = ''A'''#13#10 +
             '                        and rek__uslugatele_ps.id_uslugatele = ut.id_uslugatele'#13#10 +
             '                        and rownum = 1'#13#10 +
             '      ) as psa_urzadzenie'#13#10 +
             '      ,drk.rek__common.ps_porty_symbole(ut.id_uslugatele,  ''A'') as psa_port'#13#10 +
             '      ,(select id_urzadzenie from drk.rek__ps_port_v, drk.rek__uslugatele_ps'#13#10 +
             '                 where rek__ps_port_v.id = rek__uslugatele_ps.id_port and rek__uslugatele_ps.styk = ''B'''#13#10 +
             '                        and rek__uslugatele_ps.id_uslugatele = ut.id_uslugatele'#13#10 +
             '                        and rownum = 1'#13#10 +
             '      ) as psb_urzadzenie'#13#10 +
             '      ,drk.rek__common.ps_porty_symbole(ut.id_uslugatele,  ''B'') as psb_port'#13#10 +
             '      ,(select id_urzadzenie from drk.rek__ps_port_v, drk.rek__uslugatele_ps'#13#10 +
             '                       where rek__ps_port_v.id = rek__uslugatele_ps.id_port and rek__uslugatele_ps.styk = ''C'''#13#10 +
             '                        and rek__uslugatele_ps.id_uslugatele = ut.id_uslugatele'#13#10 +
             '                        and rownum = 1'#13#10 +
             '      ) as pszka_urzadzenie'#13#10 +
             '      ,drk.rek__common.ps_porty_symbole(ut.id_uslugatele,  ''C'') as pszka_port'#13#10 +
             '      ,(select id_urzadzenie from drk.rek__ps_port_v, drk.rek__uslugatele_ps'#13#10 +
             '                 where rek__ps_port_v.id = rek__uslugatele_ps.id_port and rek__uslugatele_ps.styk = ''D'''#13#10 +
             '                        and rek__uslugatele_ps.id_uslugatele = ut.id_uslugatele'#13#10 +
             '                        and rownum = 1'#13#10 +
             '      ) as pszkb_urzadzenie'#13#10 +
             '      ,drk.rek__common.ps_porty_symbole(ut.id_uslugatele,  ''D'') as pszkb_port'#13#10 +
             '      '#13#10 +
             '      ,to_char(ifc.vlan) as vlan'#13#10 +
             '      ,port1.id_urzadzenie as interfejs1_urzadzenie'#13#10 +
             '      ,port1.port_symbol as interfejs1_port'#13#10 +
             '      ,to_char(ifc.subinterface_1) as interfejs1_subinf'#13#10 +
             '      ,port2.id_urzadzenie as interfejs2_urzadzenie'#13#10 +
             '      ,port2.port_symbol as interfejs2_port'#13#10 +
             '      ,to_char(ifc.subinterface_2) as interfejs2_subinf'#13#10 +
             '      ,ifc.transmisja_drozna_flag as transmisja_drozna_flag'#13#10 +
             ''#13#10 +
             '      ,ut.id_uslugatele_podstawowa'#13#10 +
             '      ,ut.vrrp_flag'#13#10 +
             '      ,ut.id_uslugatele_handlowa'#13#10 +
             '      ,ut.awaria_powiadomienie_ang_flag'#13#10 +
             '      ,ut.peering_flag'#13#10 +
             '      ,ut.monitoring_ip'#13#10 +
             '      ,ut.id_kategoria_ruchu'#13#10 +
             'from drk.rek__uslugatele ut, drk.lejek_zlecenie_uslugatele, drk.transakcja, drk.lejek_projekt, drk.lejek_umowa, drk.pracownik hpp'#13#10 +
             '     ,drk.rek__usluga_status st, drk.pracownik, drk.k_kht'#13#10 +
             '     ,drk.rek__interface_pk ifc, drk.rek__ps_port_v port1, drk.rek__ps_port_v port2'#13#10 +
             'where'#13#10 +
             '          lejek_zlecenie_uslugatele.id = ut.id_lejek_zlecenie_uslugatele'#13#10 +
             '      and transakcja.id_transakcji = lejek_zlecenie_uslugatele.id_transakcji'#13#10 +
             '      and lejek_projekt.id_transakcji = transakcja.id_transakcji'#13#10 +
             '      and k_kht.nr_klienta = transakcja.id_klienta and k_kht.stan = ''A'''#13#10 +
             '      and lejek_umowa.id_transakcji = transakcja.id_transakcji and lejek_umowa.stan = ''A'' and hpp.id_pracownika = lejek_umowa.id_pracownika'#13#10 +
             '      and st.id = ut.id_statusu'#13#10 +
             '      and pracownik.id_pracownika(+) = ut.id_pracownika'#13#10 +
             ''#13#10 +
             '      and port1.id(+) = ifc.id_port_interface_1 and port2.id(+) = ifc.id_port_interface_2'#13#10 +
             '      and ifc.id(+) = drk.uslugatele_interface_get_pk(ut.id_uslugatele)'#13#10 +
             'order by nazwa' );

  { date: 2013-02-07, file: format vs list by tokens.sql }
  TestQuery( '         SELECT  DISTINCT'#13#10 +
             '                 1'#13#10 +
             '              ,  fun( 1 + ( 2 * b.b ) )'#13#10 +
             '              ,  ''abc'''#13#10 +
             '              ,  b.a AS a'#13#10 +
             '           FROM  dual AS b'#13#10 +
             '          WHERE  b.c = :qEdit.ID'#13#10 +
             ''#13#10 +
             '          UNION'#13#10 +
             ''#13#10 +
             '         SELECT  COUNT( * ) ;' );
  TestQuery( '   CREATE TABLE  nic'#13#10 +
             '              (  a INTEGER'#13#10 +
             '              )  ;' );
  TestQuery( ' TRUNCATE TABLE  nic ;' );
  TestQuery( '     DROP TABLE  nic ;' );

  { date: 2013-02-15, file: in.sql }
  TestQuery( 'select sum(nvl(tlumiennosc, 0)) AS l_TlumiennoscPT'#13#10 +
             'from trasa_sw where id in'#13#10 +
             '('#13#10 +
             '  select id_wlokno from trasa_sw_odc where id_typ_elementu = 103 and id_trasa_sw = aTrasaId'#13#10 +
             ');' );

  { date: 2013-02-16, file: unique format.sql }
  TestQuery( ' CREATE TABLE  UMOWA_PASSPORT'#13#10 +
             '            (  ID INTEGER NOT NULL CONSTRAINT UMOWA_PASSPORT$PK PRIMARY KEY'#13#10 +
             '            ,  ID_UMOWA INTEGER NOT NULL CONSTRAINT UMOWA_PASSPORT$FK_UMOWA REFERENCES UMOWA'#13#10 +
             '            ,  ID_PASSPORT_TYPE INTEGER NOT NULL CONSTRAINT UMOWA_PASSPORT$FK_PASS_TYPE REFERENCES PASSPORT_TYPE'#13#10 +
             '            ,  ID_PASSPORT INTEGER NOT NULL'#13#10 +
             '            ,  CONSTRAINT UMOWA_PASSPORT$UK UNIQUE ( ID_UMOWA, ID_PASSPORT_TYPE, ID_PASSPORT )'#13#10 +
             '            );' );

  { date: 2013-02-20, file: three space clause intend.sql }
  TestQuery( 'create table tabelka(a int, b int, c int);' );
  TestQuery( 'INSERT INTO tabelka(a, b, c) VALUES (1,2,3);' );
  TestQuery( 'select * from (select * from tabelka) tab;' );

  { date: 2013-02-20, file: create table clauses colors.sql }
  TestQuery( 'CREATE TABLE   ZLECENIE'#13#10 +
             '           (   ID INT NOT NULL'#13#10 +
             '               CONSTRAINT ZLECENIE$PK PRIMARY KEY'#13#10 +
             '           ,   NR VARCHAR(20) NOT NULL'#13#10 +
             '               CONSTRAINT ZLECENIE$UK UNIQUE'#13#10 +
             '           ,   DATA DATE NOT NULL'#13#10 +
             ''#13#10 +
             '           ,   ID_NUMER_TEL INT NOT NULL'#13#10 +
             '               CONSTRAINT ZLECENIE$FK_NUMER_TEL REFERENCES NUMER_TEL'#13#10 +
             '           ,   ID_ABONENT INT NOT NULL'#13#10 +
             '               CONSTRAINT ZLECENIE$FK_ABONENT REFERENCES WLASCICIEL'#13#10 +
             ''#13#10 +
             '           ,   ID_RODZAJ INT NOT NULL'#13#10 +
             '               CONSTRAINT ZLECENIE$FK_RODZAJ REFERENCES ZLECENIE_RODZAJ'#13#10 +
             '           ,   ID_STATUS INT NOT NULL'#13#10 +
             '               CONSTRAINT ZLECENIE$FK_STATUS REFERENCES ZLECENIE_STATUS'#13#10 +
             '           );' );

  { date: 2013-02-21, file: lister table and alias intend.sql }
  TestQuery( '   SELECT  *'#13#10 +
             '     FROM  GNIAZDO G'#13#10 +
             'LEFT JOIN  WLOKNO W1 ON G.ID IN (W1.ID_GNIAZDO_1, W1.ID_GNIAZDO_2)'#13#10 +
             'LEFT JOIN  WLOKNO W2 ON G.ID IN (W2.ID_GNIAZDO_1, W2.ID_GNIAZDO_2) AND ( W1.ID <> W2.ID )'#13#10 +
             'LEFT JOIN  SWIATLOWOD S1 ON W1.ID_SWIATLOWOD = S1.ID'#13#10 +
             'LEFT JOIN  SWIATLOWOD S2 ON W2.ID_SWIATLOWOD = S2.ID'#13#10 +
             'LEFT JOIN  TRASA_SW T1 ON G.ID IN ( T1.ID_GNIAZDO_1 , T1.ID_GNIAZDO_2  ) AND NVL ( T1.B_ZESPOLONA, 0 ) = 0 AND T1.B_DZIERZAWA = 1'#13#10 +
             'LEFT JOIN  TRASA_SW T2 ON G.ID IN ( T2.ID_GNIAZDO_1 , T2.ID_GNIAZDO_2  )  AND ( T1.ID <> T2.ID ) AND NVL ( T2.B_ZESPOLONA, 0 ) = 0 AND T2.B_DZIERZAWA = 1'#13#10 +
             'LEFT JOIN  TRASA_SW_POL P1 ON G.ID IN ( P1.ID_GNIAZDO_1 , P1.ID_GNIAZDO_2 ) AND P1.ID_TYP_POLACZENIA = 1'#13#10 +
             'LEFT JOIN  TRASA_SW_POL P2 ON G.ID IN ( P2.ID_GNIAZDO_1 , P2.ID_GNIAZDO_2 ) AND ( P1.ID <> P2.ID ) AND P2.ID_TYP_POLACZENIA = 1'#13#10 +
             'LEFT JOIN  PARA_CU PR1 ON G.ID IN ( PR1.ID_GNIAZDO_1, PR1.ID_GNIAZDO_2 )'#13#10 +
             'LEFT JOIN  PARA_CU PR2 ON G.ID IN ( PR2.ID_GNIAZDO_1, PR2.ID_GNIAZDO_2 ) AND ( PR1.ID <> PR2.ID )'#13#10 +
             'LEFT JOIN  KABEL_CU K1 ON PR1.ID_KABEL_CU = K1.ID'#13#10 +
             'LEFT JOIN  KABEL_CU K2 ON PR2.ID_KABEL_CU = K2.ID'#13#10 +
             'LEFT JOIN  PATCHCORD_CU PCU1 ON G.ID IN ( PCU1.ID_GNIAZDO_1 , PCU1.ID_GNIAZDO_2 )'#13#10 +
             'LEFT JOIN  PATCHCORD_CU PCU2 ON G.ID IN ( PCU2.ID_GNIAZDO_1 , PCU2.ID_GNIAZDO_2 ) AND ( PCU1.ID <> PCU2.ID )'#13#10 +
             'LEFT JOIN  KABEL_INF KI1 ON G.ID IN ( KI1.ID_GNIAZDO_1, KI1.ID_GNIAZDO_2 )'#13#10 +
             'LEFT JOIN  KABEL_INF KI2 ON G.ID IN ( KI2.ID_GNIAZDO_1, KI2.ID_GNIAZDO_2 ) AND ( KI1.ID <> KI2.ID )'#13#10 +
             'LEFT JOIN  KABEL_KONCENTRYCZNY KK1 ON G.ID IN ( KK1.ID_GNIAZDO_1, KK1.ID_GNIAZDO_2 )'#13#10 +
             'LEFT JOIN  KABEL_KONCENTRYCZNY KK2 ON G.ID IN ( KK2.ID_GNIAZDO_1, KK2.ID_GNIAZDO_2 ) AND KK1.ID != KK2.ID'#13#10 +
             'LEFT JOIN  URZADZENIE_WLOKNO UW1 ON  G.ID IN ( UW1.ID_GNIAZDO_1, UW1.ID_GNIAZDO_2 )'#13#10 +
             'LEFT JOIN  URZADZENIE_WLOKNO UW2 ON G.ID IN ( UW2.ID_GNIAZDO_1, UW2.ID_GNIAZDO_2 ) AND UW1.ID != UW2.ID'#13#10 +
             'LEFT JOIN  URZADZENIE URZ1 ON URZ1.ID = UW1.ID_URZADZENIE'#13#10 +
             'LEFT JOIN  URZADZENIE URZ2 ON URZ2.ID = UW2.ID_URZADZENIE'#13#10 +
             'left join sm on sm.id = g.id_sm'#13#10 +
             'left join VERY_LONG_TABLE_NAME_WITH_EXTRA_CHARS_ADDED VL ON VL.ID = SM.ID_VL'#13#10 +
             '          WHERE  G.ID = AGNIAZDOID'#13#10 +
             '            AND  ROWNUM = 1;' );

  { date: 2013-02-22, file: lister set expr intend.sql }
  TestQuery( ' UPDATE   tabelka'#13#10 +
             '    SET   a  = 1'#13#10 +
             '      ,   bb = 3'#13#10 +
             '      ,   value = 2'#13#10 +
             '  WHERE   x = 3 ;' );

  { date: 2013-02-22, file: CREATE MATERIALIZED VIEW.sql }
  TestQuery( 'CREATE MATERIALIZED VIEW HD_USLUGA_INTERFACE2_XXX'#13#10 +
//           'REFRESH FORCE ON DEMAND'#13#10 +
//           'START WITH TO_DATE(''22-02-2013 15:35:40'', ''DD-MM-YYYY HH24:MI:SS'') NEXT SYSDATE + 1'#13#10 +
             'AS'#13#10 +
             'SELECT    UTR.ID_USLUGA, UTR.ID_TRANSMISJA, SKL.ID, SKL.LP, TRL.ID_PORT_2, TRL.NR_VLAN'#13#10 +
             ''#13#10 +
             'FROM      pst.USLUGA               USL'#13#10 +
             'JOIN      pst.USLUGA_TRANSMISJA    UTR ON UTR.ID_USLUGA = USL.ID'#13#10 +
             'JOIN      pst.TRANSMISJA_SKLADNIKI SKL ON SKL.ID_TRANSMISJA = UTR.ID_TRANSMISJA AND SKL.ID_TRANSLACJE_SDH IS NOT NULL'#13#10 +
             'JOIN      pst.TRANSLACJE_SDH       TRL ON TRL.ID = SKL.ID_TRANSLACJE_SDH'#13#10 +
             'JOIN      pst.URZADZENIE           URZ ON URZ.ID = TRL.ID_URZADZENIE'#13#10 +
             ''#13#10 +
             'WHERE     USL.ID_RODZAJ_USLUGI  = -3'#13#10 +
             'AND       URZ.ID_TYP_URZADZENIA = -6'#13#10 +
             'AND     ( TRL.ID_TYP_TRANSLACJI = -2 AND TRL.NR_VLAN = pst.NRPART(TRL.NR_VLAN,2)'#13#10 +
             '       OR TRL.ID_TYP_TRANSLACJI = -5 /*AND TRL.NR_VLAN IS NULL*/ );' );

  { date: 2013-02-24, file: cast.sql }
//TestQuery( 'select cast(1+1 as varchar(5)) as value1, convert(int, ''13'', 123) as value2 from dual' );

  { date: 2013-02-25, file: HD_USL query.sql }
  TestQuery( '   SELECT  USL.NR'#13#10 +
             '        ,  W1.ELEMENT || ''.'' || P1.NR || ''.'' || HDV.SUBINTERFACE_1 AS INTF1'#13#10 +
             '        ,  W2.ELEMENT || ''.'' || P2.NR || ''.'' || HDV.SUBINTERFACE_2 AS INTF2'#13#10 +
             '        ,  HDV.*'#13#10 +
             '     FROM  HD_USLUGA_INTERFACE HDV'#13#10 +
             'LEFT JOIN  PORT P1 ON P1.ID_HD = HDV.ID_PORT_INTERFACE_1'#13#10 +
             'LEFT JOIN  WEZEL_V W1 ON W1.ID = P1.ID_WEZEL'#13#10 +
             'LEFT JOIN  PORT P2 ON P2.ID_HD = HDV.ID_PORT_INTERFACE_2'#13#10 +
             'LEFT JOIN  WEZEL_V W2 ON W2.ID = P2.ID_WEZEL'#13#10 +
             'LEFT JOIN  USLUGA USL ON USL.ID_HD = HDV.ID_USLUGATELE'#13#10 +
             '    WHERE  HDV.ID_USLUGATELE = 2164517'#13#10 +
             '      AND  HDV.ROUTER_FLAG = ''Y'';' );

  { date: 2013-02-26, file: old UNION ALL color.sql }
  TestQuery( 'select 1 from dual union all select 2 from dual' );

  { date: 2013-02-27, file: AV or no column.sql }
  TestQuery( 'ALTER TABLE LOKALIZACJA ADD'#13#10 +
             '  ID_WLASCICIEL     NUMBER'#13#10 +
             'CONSTRAINT LOKALIZACJA$FK_WLASCICIEL REFERENCES WLASCICIEL' );

  { date: 2013-02-27, file: close bracket style on format.sql }
  TestQuery( 'INSERT INTO   WLOKNO_POMIAR_TYP'#13#10 +
             '          (   ID'#13#10 +
             '          ,   NAZWA )'#13#10 +
             '   VALUES (   -1'#13#10 +
             '          ,   ''powykonawczy'' );' );
  TestQuery( 'INSERT INTO   WLOKNO_POMIAR_TYP'#13#10 +
             '          (   ID'#13#10 +
             '          ,   NAZWA )'#13#10 +
             '   VALUES (   -2'#13#10 +
             '          ,   ''okresowy'' );' );

  { date: 2013-02-27, file: todo join subquery intend on format.sql }
  TestQuery( 'CREATE OR REPLACE   VIEW WLOKNO_POM_WKV AS'#13#10 +
             ' SELECT   WL.ID'#13#10 +
             '      ,   WL.ID_SWIATLOWOD'#13#10 +
             '      ,   WL.NR_WLOKNO'#13#10 +
             '      ,   WL.NR_WLOKNO AS NR'#13#10 +
             '      ,   WL.UWAGI'#13#10 +
             '      ,   WL.ID_GNIAZDO_1'#13#10 +
             '      ,   WL.ID_GNIAZDO_2'#13#10 +
             '      ,   WL.ID_WLASCICIEL'#13#10 +
             '      ,   WL.KOLOR'#13#10 +
             '      ,   WL.KOLORTUBY'#13#10 +
             '      ,   WL.TUBA'#13#10 +
             '      ,   WL.B_SZKIELET'#13#10 +
             '      ,   WL.ID_STATUS'#13#10 +
             '      ,   WL.TMP_TRASA'#13#10 +
             '      ,   WL.B_DZIERZAWA'#13#10 +
             '      ,   WL.ID_WEZEL_KABEL_1'#13#10 +
             '      ,   WL.ID_WEZEL_KABEL_2'#13#10 +
             '      ,   WK1.ID_WEZEL AS ID_WEZEL_1'#13#10 +
             '      ,   WK2.ID_WEZEL AS ID_WEZEL_2'#13#10 +
             '      ,   WK1.LP AS LP1'#13#10 +
             '      ,   WK2.LP AS LP2'#13#10 +
             '      ,   WL.NR_ODCINKA'#13#10 +
             '      ,   WL.NR_UMOWY_NAJMU'#13#10 +
             '      ,   WL.IMP_TRASA_1'#13#10 +
             '      ,   WL.IMP_TRASA_2'#13#10 +
             '      ,   WL.IMP_TRASAOPIS_1'#13#10 +
             '      ,   WL.IMP_TRASAOPIS_2'#13#10 +
             '      ,   WL.B_USZKODZONE'#13#10 +
             '      ,   WL.B_REZERWACJA'#13#10 +
             '      ,   WL.REZERWACJA_OPIS'#13#10 +
             '      ,   WL.USLUGA_OPIS'#13#10 +
             '      ,   WP.DLUGOSC_OPT AS DLUGOSC_OPTYCZNA'#13#10 +
             'FROM      WLOKNO WL'#13#10 +
             'LEFT JOIN WEZEL_KABEL WK1 ON WK1.ID = WL.ID_WEZEL_KABEL_1'#13#10 +
             'LEFT JOIN WEZEL_KABEL WK2 ON WK2.ID = WL.ID_WEZEL_KABEL_2'#13#10 +
             '-- d³ugoœæ optyczna z ostatniego pomiaru'#13#10 +
             'LEFT JOIN ('#13#10 +
             '          SELECT *'#13#10 +
             '          FROM   WLOKNO_POMIAR WP2'#13#10 +
             '          WHERE  ID IN (SELECT MAX(ID)'#13#10 +
             '                        FROM   WLOKNO_POMIAR WP3'#13#10 +
             '                        WHERE  WP3.ID_WLOKNO = WP2.ID_WLOKNO'#13#10 +
             '                           AND WP3.DLUGOSC_OPT IS NOT NULL'#13#10 +
             '                           AND WP3.DATA IN(SELECT MAX(DATA)'#13#10 +
             '                                           FROM   WLOKNO_POMIAR WP4'#13#10 +
             '                                           WHERE  WP4.ID_WLOKNO = WP3.ID_WLOKNO'#13#10 +
             '                                           AND    WP4.DLUGOSC_OPT IS NOT NULL'#13#10 +
             '                                          )'#13#10 +
             '                       )'#13#10 +
             '          )WP ON WP.ID_WLOKNO = WL.ID' );

  { date: 2013-02-28, file: todo clause dynamic intend.sql }
  TestQuery( 'create view x as'#13#10 +
             'select 2 from dual union select 1 from a left join b on b.id = a.id' );

  { date: 2013-02-28, file: intend subquery connect by.sql }
  TestQuery( '   SELECT   NVL( UA.NR, UE.NR ) AS NR'#13#10 +
             '        ,   DECODE( UA.ID, NULL, 999, 200 ) AS ID_PASSPORT_TYPE'#13#10 +
             '        ,   NVL( UA.ID, UE.ID ) AS ID_PASSPORT'#13#10 +
             '     FROM (       SELECT   DISTINCT'#13#10 +
             '                           P.ID_EL_URZ'#13#10 +
             '                       ,   P.ID_AKT_URZ'#13#10 +
             '                    FROM (       SELECT   P.ID'#13#10 +
             '                                      ,   P.ID_PORT_POL'#13#10 +
             '                                      ,   P.ID_EL_URZ'#13#10 +
             '                                      ,   P.ID_AKT_URZ'#13#10 +
             '                                   FROM   EL_URZADZENIE_PORTY P'#13#10 +
             '                              UNION ALL'#13#10 +
             '                                 SELECT   PZ.ID'#13#10 +
             '                                      ,   PO.ID AS ID_PORT_POL'#13#10 +
             '                                      ,   EU.ID AS ID_EL_URZ'#13#10 +
             '                                      ,   NULL AS ID_AKT_URZ'#13#10 +
             '                                   FROM   EL_URZADZENIE       EU'#13#10 +
             '                                   JOIN   EL_URZADZENIE_PORTY PO ON PO.ID_EL_URZ = EU.ID AND PO.ID_TYP = 2'#13#10 +
             '                                   JOIN   EL_URZADZENIE_PORTY PZ ON PZ.ID_EL_URZ = EU.ID AND PZ.ID_TYP = 1'#13#10 +
             '                                  WHERE   EU.ID_RODZAJ = 4'#13#10 +
             '                         ) P'#13#10 +
             '               CONNECT BY   NOCYCLE P.ID = PRIOR P.ID_PORT_POL'#13#10 +
             '                      OR   P.ID_PORT_POL = PRIOR P.ID'#13#10 +
             '               START WITH   ID_EL_URZ = 84'#13#10 +
             '          ) TAB'#13#10 +
             'LEFT JOIN   URZADZENIE    UA ON UA.ID = TAB.ID_AKT_URZ'#13#10 +
             'LEFT JOIN   EL_URZADZENIE UE ON UE.ID = TAB.ID_EL_URZ' );

  { date: 2013-02-28, file: intend CREATE TABLE.sql }
  TestQuery( 'CREATE TABLE   ZDARZENIE_TYP'#13#10 +
             '          (   ID INTEGER NOT NULL'#13#10 +
             '              CONSTRAINT ZDARZENIE_TYP$PK PRIMARY KEY'#13#10 +
             '          ,   NAZWA VARCHAR( 50 ) NOT NULL'#13#10 +
             '              CONSTRAINT ZDARZENIE_TYP$UK UNIQUE'#13#10 +
             '          )' );

  { date: 2013-02-28, file: intend INSERT.sql }
  TestQuery( 'INSERT INTO ZDARZENIE'#13#10 +
             '     (   ID_TYP'#13#10 +
             '     ,   DATA'#13#10 +
             '     ,   UWAGI )'#13#10 +
             'VALUES (   :ID_TYP'#13#10 +
             '     ,   :DATA'#13#10 +
             '     ,   :UWAGI )' );

  { date: 2013-03-01, file: parse global temporary table.sql }
  TestQuery( 'create global temporary table a (b int );' );

  { date: 2013-03-04, file: ADD CONSTRAINT.sql }
  TestQuery( 'ALTER TABLE TYP_SWIATLOWODU_WLOKNO'#13#10 +
             'ADD ID_WLOKNO_TYP INT NULL REFERENCES WLOKNO_TYP;' );

  { date: 2013-03-05, file: select subquery intend on format.sql }
  TestQuery( 'CREATE OR REPLACE   VIEW WLOKNO_POM_WKV AS'#13#10 +
             ' SELECT   WL.ID'#13#10 +
             '      ,   WL.ID_SWIATLOWOD'#13#10 +
             ''#13#10 +
             ',('#13#10 +
             '          SELECT *'#13#10 +
             '          FROM   WLOKNO_POMIAR WP2'#13#10 +
             '          WHERE  ID IN (SELECT MAX(ID)'#13#10 +
             '                        FROM   WLOKNO_POMIAR WP3'#13#10 +
             '                        WHERE  WP3.ID_WLOKNO = WP2.ID_WLOKNO'#13#10 +
             '                           AND WP3.DLUGOSC_OPT IS NOT NULL'#13#10 +
             '                           AND WP3.DATA IN(SELECT MAX(DATA)'#13#10 +
             '                                           FROM   WLOKNO_POMIAR WP4'#13#10 +
             '                                           WHERE  WP4.ID_WLOKNO = WP3.ID_WLOKNO'#13#10 +
             '                                           AND    WP4.DLUGOSC_OPT IS NOT NULL'#13#10 +
             '                                          )'#13#10 +
             '                       )'#13#10 +
             '          )WP'#13#10 +
             ''#13#10 +
             'FROM      WLOKNO WL'#13#10 +
             'LEFT JOIN WEZEL_KABEL WK1 ON WK1.ID = WL.ID_WEZEL_KABEL_1'#13#10 +
             'LEFT JOIN WEZEL_KABEL WK2 ON WK2.ID = WL.ID_WEZEL_KABEL_2' );

  { date: 2013-03-09, file: alias and table color in column name with dots.sql }
  TestQuery( 'SELECT   *'#13#10 +
             '  FROM   Scott.Emp_tab Emp'#13#10 +
             '     ,   Scott.Dept_tab'#13#10 +
             ' WHERE   scott.dept_tab.deptno = emp.deptno;' );
  TestQuery( 'select (select count(*) from Emp_Table Emp where Emp.DeptNo = Dept.DeptNo)'#13#10 +
             'from   Dept_Table Dept' );

  { date: 2013-03-11, file: old commons.sql }
  TestQuery( '   USE   Comit ;' );
  TestQuery( 'TRUNCATE TABLE tab1 ;' );
  TestQuery( '    GO   ;' );
  TestQuery( 'SAVEPOINT svp_01 ;' );

  { date: 2013-03-13, file: ext query alias color.sql }
  TestQuery( '  SELECT   *'#13#10 +
             '    FROM ('#13#10 +
             '             SELECT   -110 AS id'#13#10 +
             '                  ,   ''patchcord'' AS nr'#13#10 +
             '                  ,   110 AS typ'#13#10 +
             '                  ,   0 AS sort'#13#10 +
             '               FROM   dual'#13#10 +
             '              UNION'#13#10 +
             '             SELECT   -103 AS id'#13#10 +
             '                  ,   ''trasa optyczna'' AS nr'#13#10 +
             '                  ,   103 AS typ'#13#10 +
             '                  ,   2 AS sort'#13#10 +
             '               FROM   dual'#13#10 +
             '              UNION'#13#10 +
             '             SELECT   S.id'#13#10 +
             '                  ,   S.nr'#13#10 +
             '                  ,   107 AS TYP'#13#10 +
             '                  ,   1 AS sort'#13#10 +
             '               FROM   swiatlowod S'#13#10 +
             '                  ,   szafa     SZ'#13#10 +
             '                  ,   wezel_v    V'#13#10 +
             '              WHERE   SZ.id = V.id_szafa'#13#10 +
             '                AND   V.id = :qEdit.id_element'#13#10 +
             '                AND   ( ( S.id_wezel_1 = SZ.id_wezel'#13#10 +
             '                 OR   S.id_wezel_2 = SZ.id_wezel )'#13#10 +
             '                 OR   S.id IN'#13#10 +
             '                    ('#13#10 +
             '                        SELECT   id_element'#13#10 +
             '                          FROM   wezel_kabel'#13#10 +
             '                         WHERE   id_typ_elementu = 101'#13#10 +
             '                           AND   id_wezel = SZ.id_wezel'#13#10 +
             '                    ) )'#13#10 +
             '              UNION'#13#10 +
             '             SELECT   S.id'#13#10 +
             '                  ,   S.nr'#13#10 +
             '                  ,   107 AS TYP'#13#10 +
             '                  ,   1 AS sort'#13#10 +
             '               FROM   swiatlowod S'#13#10 +
             '              WHERE   ( S.id_wezel_1 = :qEdit.PWID'#13#10 +
             '                 OR   S.id_wezel_2 = :qEdit.PWID )'#13#10 +
             '                 OR   S.id IN'#13#10 +
             '                    ('#13#10 +
             '                        SELECT   id_element'#13#10 +
             '                          FROM   wezel_kabel'#13#10 +
             '                         WHERE   id_typ_elementu = 101'#13#10 +
             '                           AND   id_wezel = :qEdit.PWID'#13#10 +
             '                    )'#13#10 +
             '         ) v'#13#10 +
             'ORDER BY   sort'#13#10 +
             '       ,   nr' );

  { date: 2013-03-13, file: ALTER TABLE MODIFY.sql }
//  TestQuery( 'ALTER TABLE EL_URZADZENIE_POMIARY MODIFY STAN_LICZNIKA NULL MODIFY POMIAR_NAT_PRADU NULL MODIFY'#13#10 +
//             '  POMIAR_NAPIECIA NULL MODIFY POMIAR_REZ_WEWN NULL' );

  { date: 2013-03-13, file: new line after column constraint in CREATE TABLE.sql }
  TestQuery( 'CREATE TABLE   aaaa'#13#10 +
             '           (   id INTEGER NOT NULL'#13#10 +
             '               CONSTRAINT aaaa$pk PRIMARY KEY'#13#10 +
             ''#13#10 +
             '           ,   id_sm INTEGER NOT NULL'#13#10 +
             '               CONSTRAINT aaaa$fk_sm REFERENCES sm'#13#10 +
             ''#13#10 +
             '           ,   nr VARCHAR( 100 )'#13#10 +
             '           )' );

  { date: 2013-03-13, file: ON cond left side order.sql }
  TestQuery( 'SELECT   *'#13#10 +
             '  FROM   emp  e'#13#10 +
             '  JOIN   dept d ON e.deptno = d.deptno ;' );
  TestQuery( 'SELECT   *'#13#10 +
             '  FROM   emp'#13#10 +
             '  JOIN   dept ON emp.deptno = dept.deptno ;' );
  TestQuery( 'SELECT   *'#13#10 +
             '  FROM   emp  e'#13#10 +
             '  JOIN   dept d ON e.deptno = d.deptno and e.subdeptno = d.subdeptno;' );

  { date: 2013-03-14, file: CREATE TABLE intend columns.sql }
  TestQuery( 'CREATE TABLE   ZDARZENIE_TYP'#13#10 +
             '           (   ID               INTEGER          NOT NULL'#13#10 +
             '               CONSTRAINT ZDARZENIE_TYP$PK PRIMARY KEY'#13#10 +
             ''#13#10 +
             '           ,   NAZWA            VARCHAR( 50 )    NOT NULL'#13#10 +
             '               CONSTRAINT ZDARZENIE_TYP$UK UNIQUE'#13#10 +
             ''#13#10 +
             '           ,   LiczbaPorzadkowa NUMBER( 10, 3 ) NULL'#13#10 +
             '           )' );

  { date: 2013-03-16, file: insert.sql }
  TestQuery( 'INSERT INTO tab( a) VALUES (1)' );

  { date: 2013-03-19, file: ALTER TABLE col + ref.sql }
  TestQuery( 'ALTER TABLE PARA_CU_POMIAR'#13#10 +
             'ADD ID_NUMER_TEL INT NULL'#13#10 +
             'CONSTRAINT PARA_CU_POMIAR$FK_NUMER_TEL'#13#10 +
             'REFERENCES NUMER_TEL;' );

  { date: 2013-03-19, file: JOIN alias after format.sql }
  TestQuery( '    SELECT   P.*'#13#10 +
             '         ,   WU.OPIS AS WYPOSAZENIE'#13#10 +
             '      FROM   WEZEL W'#13#10 +
             '      JOIN   PORT  P ON P.ID_WEZEL = W.ID'#13#10 +
             ' LEFT JOIN ('#13#10 +
             '                 SELECT   WU.ID'#13#10 +
             '                      ,   SYS_CONNECT_BY_PATH( WU.NAZWA, '' / '' ) AS OPIS'#13#10 +
             '                   FROM ('#13#10 +
             '                              SELECT   *'#13#10 +
             '                                FROM   WYPOSAZENIE_URZADZENIA'#13#10 +
             '                               WHERE   ID_URZADZENIE = :qEdit.ID'#13#10 +
             '                        ) WU'#13#10 +
             '             CONNECT BY   WU.ID_PARENT = PRIOR WU.ID'#13#10 +
             '             START WITH   WU.ID_PARENT IS NULL'#13#10 +
             '           ) WU ON WU.ID = P.ID_WYPOSAZENIE_URZADZENIA'#13#10 +
             '     WHERE   W.ID_TYP_WEZLA = 24'#13#10 +
             '       AND   W.ID_ELEMENT = :qEdit.ID' );

  { date: 2013-03-19, file: ALTER TRIGGER.sql }
  TestQuery( 'ALTER TRIGGER xxx DISABLE;' );
  TestQuery( 'ALTER TRIGGER xxx ENABLE;' );

  { date: 2013-03-21, file: empty line before clause in subquery.sql }
  TestQuery( '   SELECT   UE.ID AS ID_EL_URZ'#13#10 +
             '        ,   UE.NR'#13#10 +
             '        ,   UE.ID_WEZEL'#13#10 +
             '        ,   SUM(TAB.WARTOSC) AS SUMA_ODBIOROW'#13#10 +
             '     FROM ('#13#10 +
             '               SELECT   PO.ID'#13#10 +
             '                    ,   PZ.ID AS ID_PORT_POL'#13#10 +
             '                    ,   NULL'#13#10 +
             '                    , ('#13#10 +
             ''#13#10 +
             '                           SELECT   NVL( SUM(1), 1 )'#13#10 +
             '                             FROM   EL_URZADZENIE_PORTY PO'#13#10 +
             '                            WHERE   PO.ID_EL_URZ = EU.ID'#13#10 +
             '                              AND   PO.ID_TYP = 2'#13#10 +
             '                      ) / NVL( EU.SPRAWNOSC, 1 ) AS WSP'#13#10 +
             '                 FROM   EL_URZADZENIE       EU'#13#10 +
             '                 JOIN   EL_URZADZENIE_PORTY PO ON PO.ID_EL_URZ = EU.ID AND PO.ID_TYP = 2'#13#10 +
             '                 JOIN   EL_URZADZENIE_PORTY PZ ON PZ.ID_EL_URZ = EU.ID AND PZ.ID_TYP = 1'#13#10 +
             '                WHERE   EU.ID_RODZAJ = 4'#13#10 +
             '          ) TAB'#13#10 +
             'LEFT JOIN   EL_URZADZENIE_PORTY P ON P.ID = TAB.ROOT_ID'#13#10 +
             'LEFT JOIN   EL_URZADZENIE      UE ON UE.ID = P.ID_EL_URZ'#13#10 +
             '    WHERE   UE.ID_RODZAJ != 4'#13#10 +
             ' GROUP BY   UE.ID'#13#10 +
             '        ,   UE.NR'#13#10 +
             '        ,   UE.ID_WEZEL' );

  { date: 2013-03-29, file: ON clause intend.sql }
  TestQuery( 'SELECT   *'#13#10 +
             '  FROM   table_a   A'#13#10 +
             '  JOIN   table_b   B ON  B.id = A.id AND  B.x  = 1'#13#10 +
             '  JOIN   table_c  CC ON CC.id = A.id AND CC.x  = 1' );

  { date: 2013-04-06, file: ON NVL right bracket.sql }
  TestQuery( ''#13#10 +
             '   SELECT   TR.ID'#13#10 +
             '        ,   TR.NAZWA'#13#10 +
             '        ,   TR.NAZWA                             AS                CPT'#13#10 +
             '        ,   TR.UWAGI'#13#10 +
             '        ,   TR.ID_STATUS'#13#10 +
             '        ,   TR.ID_RODZAJ_TRANSMISJI'#13#10 +
             '        ,   TR.ID_PRZEPLYWNOSC'#13#10 +
             '        ,   TR.PRZEPLYWNOSC_WARTOSC'#13#10 +
             '        ,   TR.B_DZIERZAWA'#13#10 +
             '        ,   TR.B_SZKIELET'#13#10 +
             '        ,   TR.B_SPRAWDZONE'#13#10 +
             '        ,   TR.NR_UMOWY_NAJMU'#13#10 +
             '        ,   StatusEksploatacji_Transmisja(TR.ID) AS StatusEksploatacji'#13#10 +
             '        ,   TR.ID_PORT_1'#13#10 +
             '        ,   TR.ID_PORT_2'#13#10 +
             '        ,   TR.ID_PORT_1B'#13#10 +
             '        ,   TR.ID_PORT_2B'#13#10 +
             '        ,   TR.ID_PORT_1                         AS      ID_PORT_1_OLD'#13#10 +
             '        ,   TR.ID_PORT_2                         AS      ID_PORT_2_OLD'#13#10 +
             '        ,   TR.ID_PORT_1B                        AS     ID_PORT_1B_OLD'#13#10 +
             '        ,   TR.ID_PORT_2B                        AS     ID_PORT_2B_OLD'#13#10 +
             '        ,   SW1.ID                               AS       ID_WEZEL_SW1'#13#10 +
             '        ,   SW2.ID                               AS       ID_WEZEL_SW2'#13#10 +
             '        ,   SW1.ID_SZAFA                         AS         ID_SZAFA_1'#13#10 +
             '        ,   SW2.ID_SZAFA                         AS         ID_SZAFA_2'#13#10 +
             '        ,   W1.ID_TYP_WEZLA                      AS    WID_TYP_WEZLA_1'#13#10 +
             '        ,   W2.ID_TYP_WEZLA                      AS    WID_TYP_WEZLA_2'#13#10 +
             '        ,   W1.ID                                AS         ID_WEZEL_1'#13#10 +
             '        ,   W2.ID                                AS         ID_WEZEL_2'#13#10 +
             ''#13#10 +
             '     FROM   TRANSMISJA    TR'#13#10 +
             'LEFT JOIN   URZADZENIE    U1 ON  U1.ID =  TR.ID_URZADZENIE_1  '#13#10 +
             'LEFT JOIN   URZADZENIE    U2 ON  U2.ID =  TR.ID_URZADZENIE_2  '#13#10 +
             'LEFT JOIN   PRZELACZNICA  P1 ON  P1.ID =  TR.ID_PRZELACZNICA_1'#13#10 +
             'LEFT JOIN   PRZELACZNICA  P2 ON  P2.ID =  TR.ID_PRZELACZNICA_2'#13#10 +
             'LEFT JOIN   PORT         PT1 ON PT1.ID =  TR.ID_PORT_1        '#13#10 +
             'LEFT JOIN   PORT         PT2 ON PT2.ID =  TR.ID_PORT_2        '#13#10 +
             'LEFT JOIN   WEZEL_V      SW1 ON SW1.ID = PT1.ID_WEZEL         '#13#10 +
             'LEFT JOIN   WEZEL_V      SW2 ON SW2.ID = NVL( PT2.ID_WEZEL,  TR.ID_WEZEL_SW2     )'#13#10 +
             'LEFT JOIN   WEZEL_V       W1 ON  W1.ID = SW1.ID_WEZEL_PARENT  '#13#10 +
             'LEFT JOIN   WEZEL_V       W2 ON  W2.ID = NVL( SW2.ID_WEZEL_PARENT,  TR.ID_WEZEL_2       )'#13#10 +
             ''#13#10 +
             '    WHERE   TR.ID = :AKTID' );

  { date: 2013-04-08, file: select alias intend.sql }
  TestQuery( '   SELECT   TR.ID'#13#10 +
             '        ,   TR.NAZWA'#13#10 +
             '        ,   TR.UWAGI'#13#10 +
             '        ,   TR.B_DZIERZAWA'#13#10 +
             '        ,   TR.NR_UMOWY_NAJMU'#13#10 +
             '        ,   R.NAZWA                               AS       RODZAJ'#13#10 +
             '        ,   TR.B_SZKIELET'#13#10 +
             '        ,   TR.B_SPRAWDZONE'#13#10 +
             '        ,   WSW1.ELEMENT                          AS URZADZENIE_1'#13#10 +
             '        ,   WSW2.ELEMENT                          AS URZADZENIE_2'#13#10 +
             '        ,   NumerPortuLubSpawu(TR.ID_PORT_1) || CASE WHEN TR.ID_PORT_1B != TR.ID_PORT_1 THEN '' , '' || NumerPortuLubSpawu(TR.ID_PORT_1B) END AS       PORT_1'#13#10 +
             '        ,   NumerPortuLubSpawu(TR.ID_PORT_2) || CASE WHEN TR.ID_PORT_2B != TR.ID_PORT_2 THEN '' , '' || NumerPortuLubSpawu(TR.ID_PORT_2B) END AS       PORT_2'#13#10 +
             '        ,   W1.ELEMENT                            AS      WEZEL_1'#13#10 +
             '        ,   W2.ELEMENT                            AS      WEZEL_2'#13#10 +
             '        ,   ST.NAZWA                              AS       STATUS'#13#10 +
             '        ,   P.NAZWA                               AS PRZEPLYWNOSC'#13#10 +
             '        , ( '#13#10 +
             '               SELECT   NAZWA'#13#10 +
             '                 FROM   STATUS  ST'#13#10 +
             '                WHERE   ST.ID = TR.ID_STATUS'#13#10 +
             '          )                   AS       STATUS'#13#10 +
             '        , ( '#13#10 +
             '               SELECT   NAZWA'#13#10 +
             '                 FROM   PRZEPLYWNOSC  P'#13#10 +
             '                WHERE   P.ID = TR.ID_PRZEPLYWNOSC'#13#10 +
             '          )                   AS PRZEPLYWNOSC'#13#10 +
             ''#13#10 +
             '     FROM   TRANSMISJA           TR'#13#10 +
             'LEFT JOIN   RODZAJ_TRANSMISJI     R ON    R.ID = TR.ID_RODZAJ_TRANSMISJI'#13#10 +
             'LEFT JOIN   STATUS               ST ON   ST.ID = TR.ID_STATUS           '#13#10 +
             'LEFT JOIN   PRZEPLYWNOSC          P ON    P.ID = TR.ID_PRZEPLYWNOSC     '#13#10 +
             'LEFT JOIN   PORT                 P1 ON   P1.ID = TR.ID_PORT_1           '#13#10 +
             'LEFT JOIN   PORT                 P2 ON   P2.ID = TR.ID_PORT_2           '#13#10 +
             'LEFT JOIN   WEZEL_V            WSW1 ON WSW1.ID = P1.ID_WEZEL            '#13#10 +
             'LEFT JOIN   WEZEL_V            WSW2 ON WSW2.ID = P2.ID_WEZEL            '#13#10 +
             'LEFT JOIN   WEZEL_LOK_STUDNIA_V  W1 ON   W1.ID = TR.ID_WEZEL_1          '#13#10 +
             'LEFT JOIN   WEZEL_LOK_STUDNIA_V  W2 ON   W2.ID = TR.ID_WEZEL_2          ' );

  { date: 2013-04-08, file: ON second condition.sql }
  TestQuery( ''#13#10 +
             'CREATE OR REPLACE VIEW WEZEL_V AS'#13#10 +
             ''#13#10 +
             '     SELECT   W.ID'#13#10 +
             '       FROM   WEZEL               W'#13#10 +
             '  LEFT JOIN   TYP_WEZLA           T ON    T.ID           = W.ID_TYP_WEZLA'#13#10 +
             '  LEFT JOIN   TYP_ELEMENTU        S ON    S.ID           = T.ID          '#13#10 +
             '  LEFT JOIN   LOKALIZACJA       E24 ON    W.ID_TYP_WEZLA = 24 AND E24.ID = W.ID_ELEMENT'#13#10 +
             '  LEFT JOIN   BUDYNEK            E1 ON    W.ID_TYP_WEZLA = 1 AND E1.ID = W.ID_ELEMENT'#13#10 +
             '  LEFT JOIN   STUDNIA            E2 ON    W.ID_TYP_WEZLA = 2 AND E2.ID = W.ID_ELEMENT'#13#10 +
             '  LEFT JOIN   PRZELACZNICA       E3 ON    W.ID_TYP_WEZLA = 3 AND E3.ID = W.ID_ELEMENT'#13#10 +
             '  LEFT JOIN   URZADZENIE         E5 ON    W.ID_TYP_WEZLA = 5 AND E5.ID = W.ID_ELEMENT'#13#10 +
             '  LEFT JOIN   MUFA              E23 ON    W.ID_TYP_WEZLA = 23 AND E23.ID = W.ID_ELEMENT'#13#10 +
             '  LEFT JOIN   GLOWICA_CU       E116 ON    W.ID_TYP_WEZLA = 116 AND E116.ID = W.ID_ELEMENT'#13#10 +
             '  LEFT JOIN   MDF              E117 ON    W.ID_TYP_WEZLA = 117 AND E117.ID = W.ID_ELEMENT'#13#10 +
             '  LEFT JOIN   SZAFA_CU         E118 ON    W.ID_TYP_WEZLA = 118 AND E118.ID = W.ID_ELEMENT'#13#10 +
             '  LEFT JOIN   GRUPA_GLOWIC_CU  E119 ON    W.ID_TYP_WEZLA = 119 AND E119.ID = W.ID_ELEMENT'#13#10 +
             '  LEFT JOIN   TUNEL            E138 ON    W.ID_TYP_WEZLA = 138 AND E138.ID = W.ID_ELEMENT'#13#10 +
             '      WHERE   ID_TYP_WEZLA IN ( -1, 1, 2, 3, 5, 23, 24, 116, 117, 118, 119, 138 )' );

  { date: 2013-04-09, file: subquery empty line before clause.sql }
  TestQuery( ''#13#10 +
             'INSERT INTO   TRANSMISJA_PORTY_ABCDE'#13#10 +
             '            ( ID_TRANSMISJA, TYP, ID_PRZEPLYWNOSC )'#13#10 +
             '     SELECT   TransID, ''F'', MIN(ID)'#13#10 +
             '       FROM   PRZEPLYWNOSC'#13#10 +
             '      WHERE   WARTOSC IN '#13#10 +
             '            ( '#13#10 +
             '                   SELECT   MIN(WARTOSC)'#13#10 +
             '              '#13#10 +
             '                     FROM ( '#13#10 +
             '                                 SELECT   PRZ.WARTOSC'#13#10 +
             '                                   FROM   TMP_TRANSMISJA_SCHEMAT  TMP'#13#10 +
             '                                   JOIN   PRZEPLYWNOSC            PRZ ON PRZ.ID = TMP.ID_PRZEPLYWNOSC'#13#10 +
             '                            '#13#10 +
             '                              UNION ALL   '#13#10 +
             '                            '#13#10 +
             '                                 SELECT   PRZ.WARTOSC'#13#10 +
             '                                   FROM   TRANSMISJA     TR'#13#10 +
             '                                   JOIN   PORT            P ON   P.ID = TR.ID_PORT_1      '#13#10 +
             '                                   JOIN   PRZEPLYWNOSC  PRZ ON PRZ.ID =  P.ID_PRZEPLYWNOSC'#13#10 +
             '                            '#13#10 +
             '                              UNION ALL   '#13#10 +
             '                            '#13#10 +
             '                                 SELECT   PRZ.WARTOSC'#13#10 +
             '                                   FROM   TRANSMISJA     TR'#13#10 +
             '                                   JOIN   PORT            P ON   P.ID = TR.ID_PORT_2      '#13#10 +
             '                                   JOIN   PRZEPLYWNOSC  PRZ ON PRZ.ID =  P.ID_PRZEPLYWNOSC'#13#10 +
             '                          ) '#13#10 +
             '            ) ' );

  { date: 2013-04-09, file: ON intend.sql }
  TestQuery( 'INSERT INTO   TRANSMISJA_PORTY_ABCDE'#13#10 +
             '            ( ID_TRANSMISJA, TYP, ID_PRZEPLYWNOSC )'#13#10 +
             '     SELECT   TransID, ''F'', MIN(ID)'#13#10 +
             '       FROM   PRZEPLYWNOSC'#13#10 +
             '      WHERE   WARTOSC IN '#13#10 +
             '            ( '#13#10 +
             '                   SELECT   MIN(WARTOSC)'#13#10 +
             '              '#13#10 +
             '                     FROM ( '#13#10 +
             '                                 SELECT   PRZ.WARTOSC'#13#10 +
             '                                   FROM   TMP_TRANSMISJA_SCHEMAT  TMP'#13#10 +
             '                                   JOIN   PRZEPLYWNOSC            PRZ ON PRZ.ID = TMP.ID_PRZEPLYWNOSC'#13#10 +
             '                            '#13#10 +
             '                              UNION ALL   '#13#10 +
             '                            '#13#10 +
             '                                 SELECT   PRZ.WARTOSC'#13#10 +
             '                                   FROM   TRANSMISJA     TR'#13#10 +
             '                                   JOIN   PORT            P ON   P.ID = TR.ID_PORT_1      '#13#10 +
             '                                   JOIN   PRZEPLYWNOSC  PRZ ON PRZ.ID =  P.ID_PRZEPLYWNOSC'#13#10 +
             '                                  WHERE   TR.ID = TransID'#13#10 +
             '                            '#13#10 +
             '                              UNION ALL   '#13#10 +
             '                            '#13#10 +
             '                                 SELECT   PRZ.WARTOSC'#13#10 +
             '                                   FROM   TRANSMISJA     TR'#13#10 +
             '                                   JOIN   PORT            P ON   P.ID = TR.ID_PORT_2      '#13#10 +
             '                                   JOIN   PRZEPLYWNOSC  PRZ ON PRZ.ID =  P.ID_PRZEPLYWNOSC'#13#10 +
             '                                  WHERE   TR.ID = TransID'#13#10 +
             '                          ) '#13#10 +
             '            ) ;'#13#10 +
             ''#13#10 +
             '     SELECT   TMP.ID_PRZEPLYWNOSC'#13#10 +
             '       FROM   TMP_TRANSMISJA_SCHEMAT  TMP'#13#10 +
             ''#13#10 +
             '  UNION ALL   '#13#10 +
             ''#13#10 +
             '     SELECT   PRZ.WARTOSC'#13#10 +
             '       FROM   TRANSMISJA     TR'#13#10 +
             '  LEFT JOIN   PORT            P ON   P.ID             IN ( TR.ID_PORT_1, TR.ID_PORT_2 )'#13#10 +
             '  LEFT JOIN   PRZEPLYWNOSC  PRZ ON PRZ.ID = P.ID_PRZEPLYWNOSC'#13#10 +
             '      WHERE   TR.ID = TransID ;' );

  { date: 2013-04-13, file: AV after Ctrl X.sql. }
  TestQuery( 'SELECT   PRZ.WARTOSC'#13#10 +
             '  FROM   TMP_TRANSMISJA_SCHEMAT  TMP'#13#10 +
             '  JOIN   PRZEPLYWNOSC            PRZ ON PRZ.ID = TMP.ID_PRZEPLYWNOSC /* cut this expression to get AV */' );

  { date: 2013-04-15, file: ON cond second expr intend.sql }
  TestQuery( ''#13#10 +
             '     SELECT   TMP.ID'#13#10 +
             '          ,   DECODE( TMP.REF_PASSPORT_TYPE, 727, 868, TMP.REF_PASSPORT_TYPE )                   AS     REF_PASSPORT_TYPE'#13#10 +
             '          ,   DECODE( TMP.REF_PASSPORT_TYPE, 727, TRS.ID_TRANSMISJA, TMP.REF_PASSPORT_TABLE_ID ) AS REF_PASSPORT_TABLE_ID'#13#10 +
             '          ,   COALESCE( V.NR, V.NAZWA, TR.NAZWA )                                                AS                    NR'#13#10 +
             '          ,   T.PASSPORT_TYPE_NAME'#13#10 +
             '          ,   T.PASSPORT_FORM_NAME'#13#10 +
             '       FROM   TMP_PASSPORT_REFERENCES  TMP'#13#10 +
             '       JOIN   PASSPORT_TYPE              T ON   T.ID                = DECODE( TMP.REF_PASSPORT_TYPE, 727, 868, TMP.REF_PASSPORT_TYPE )'#13#10 +
             '  LEFT JOIN   PASSPORT_V                 V ON   V.ID_PASSPORT_TYPE  = TMP.REF_PASSPORT_TYPE    AND V.PASSPORT_TABLE_ID = TMP.REF_PASSPORT_TABLE_ID'#13#10 +
             '  LEFT JOIN   TRANSMISJA_SKLADNIKI     TRS ON TMP.REF_PASSPORT_TYPE = 727                      AND TRS.ID = TMP.REF_PASSPORT_TABLE_ID'#13#10 +
             '  LEFT JOIN   USLUGA_TRANSMISJA        UTR ON TMP.REF_PASSPORT_TYPE = 882                      AND UTR.ID = TMP.REF_PASSPORT_TABLE_ID'#13#10 +
             '  LEFT JOIN   USLUGA                   USL ON USL.ID                = UTR.ID_USLUGA            '#13#10 +
             '  LEFT JOIN   TRANSMISJA                TR ON  TR.ID                = UTR.ID_TRANSMISJA        '#13#10 +
             '   ORDER BY   T.PASSPORT_TYPE_NAME'#13#10 +
             '          ,   NVL( V.NR, V.NAZWA ) ;' );
  TestQuery( ''#13#10 +
             'CREATE OR REPLACE VIEW WEZEL_V AS'#13#10 +
             ''#13#10 +
             '     SELECT   W.ID'#13#10 +
             '       FROM   WEZEL               W'#13#10 +
             '  LEFT JOIN   TYP_WEZLA           T ON    T.ID           = W.ID_TYP_WEZLA'#13#10 +
             '  LEFT JOIN   TYP_ELEMENTU        S ON    S.ID           = T.ID          '#13#10 +
             '  LEFT JOIN   LOKALIZACJA       E24 ON    W.ID_TYP_WEZLA = 24            AND E24.ID = W.ID_ELEMENT'#13#10 +
             '  LEFT JOIN   BUDYNEK            E1 ON    W.ID_TYP_WEZLA = 1             AND E1.ID = W.ID_ELEMENT'#13#10 +
             '  LEFT JOIN   STUDNIA            E2 ON    W.ID_TYP_WEZLA = 2             AND E2.ID = W.ID_ELEMENT'#13#10 +
             '  LEFT JOIN   PRZELACZNICA       E3 ON    W.ID_TYP_WEZLA = 3             AND E3.ID = W.ID_ELEMENT'#13#10 +
             '  LEFT JOIN   URZADZENIE         E5 ON    W.ID_TYP_WEZLA = 5             AND E5.ID = W.ID_ELEMENT'#13#10 +
             '  LEFT JOIN   MUFA              E23 ON    W.ID_TYP_WEZLA = 23            AND E23.ID = W.ID_ELEMENT'#13#10 +
             '  LEFT JOIN   GLOWICA_CU       E116 ON    W.ID_TYP_WEZLA = 116           AND E116.ID = W.ID_ELEMENT'#13#10 +
             '  LEFT JOIN   MDF              E117 ON    W.ID_TYP_WEZLA = 117           AND E117.ID = W.ID_ELEMENT'#13#10 +
             '  LEFT JOIN   SZAFA_CU         E118 ON    W.ID_TYP_WEZLA = 118           AND E118.ID = W.ID_ELEMENT'#13#10 +
             '  LEFT JOIN   GRUPA_GLOWIC_CU  E119 ON    W.ID_TYP_WEZLA = 119           AND E119.ID = W.ID_ELEMENT'#13#10 +
             '  LEFT JOIN   TUNEL            E138 ON    W.ID_TYP_WEZLA = 138           AND E138.ID = W.ID_ELEMENT'#13#10 +
             '      WHERE   ID_TYP_WEZLA IN ( -1, 1, 2, 3, 5, 23, 24, 116, 117, 118, 119, 138 ) ;' );

  { date: 2013-04-16, file: old ORDER BY nulls first.sql }
  TestQuery( 'SELECT *'#13#10 +
             'FROM ('#13#10 +
             '        SELECT R2.WIAZKA_1 ||''-''|| R2.NR_1 AS WNR1, R2.WIAZKA_2 ||''-''|| R2.NR_2 AS WNR2,'#13#10 +
             '               R1.ID_KOLOR AS KOLOR1, R1.WIAZKA_1 AS W11, R1.NR_1 AS NR11, R1.WIAZKA_2 AS W12, R1.NR_2 AS NR12,'#13#10 +
             '               R2.ID_KOLOR AS KOLOR2, R2.WIAZKA_1 AS W21, R2.NR_1 AS NR21, R2.WIAZKA_2 AS W22, R2.NR_2 AS NR22,'#13#10 +
             '               R3.ID_KOLOR AS KOLOR3, R3.WIAZKA_1 AS W31, R3.NR_1 AS NR31, R3.WIAZKA_2 AS W32, R3.NR_2 AS NR32'#13#10 +
             '        FROM   RURA R1'#13#10 +
             '        JOIN   RURA R2 ON R2.ID_RURA = R1.ID'#13#10 +
             '        JOIN   RURA R3 ON R3.ID_RURA = R2.ID'#13#10 +
             '        WHERE  R1.ID_KANALIZACJA = 123'#13#10 +
             '          AND  R1.ID_RURA IS NULL'#13#10 +
             ''#13#10 +
             '        UNION ALL'#13#10 +
             ''#13#10 +
             '        SELECT R2.WIAZKA_1 ||''-''|| R2.NR_1, R2.WIAZKA_2 ||''-''|| R2.NR_2,'#13#10 +
             '               R1.ID_KOLOR, R1.WIAZKA_1 AS W11, R1.NR_1 AS NR11, R1.WIAZKA_2 AS W12, R1.NR_2 AS NR12,'#13#10 +
             '               R2.ID_KOLOR, R2.WIAZKA_1 AS W21, R2.NR_1 AS NR21, R2.WIAZKA_2 AS R22, R2.NR_2 AS NR22,'#13#10 +
             '               NULL, NULL, NULL, NULL, NULL'#13#10 +
             '        FROM   RURA R1'#13#10 +
             '        JOIN   RURA R2 ON R2.ID_RURA = R1.ID'#13#10 +
             '        WHERE  R1.ID_KANALIZACJA = 123'#13#10 +
             '          AND  R1.ID_RURA IS NULL'#13#10 +
             ''#13#10 +
             '        UNION ALL'#13#10 +
             ''#13#10 +
             '        SELECT R1.WIAZKA_1 ||''-''|| R1.NR_1, R1.WIAZKA_2 ||''-''|| R1.NR_2,'#13#10 +
             '               R1.ID_KOLOR, R1.WIAZKA_1, R1.NR_1, R1.WIAZKA_2, R1.NR_2,'#13#10 +
             '               NULL, NULL, NULL, NULL, NULL,'#13#10 +
             '               NULL, NULL, NULL, NULL, NULL'#13#10 +
             '        FROM   RURA R1'#13#10 +
             '        WHERE  R1.ID_KANALIZACJA = 123'#13#10 +
             '          AND  R1.ID_RURA IS NULL'#13#10 +
             ')'#13#10 +
             'ORDER BY W11, NR11, W12, NR12,'#13#10 +
             '         W21 NULLS FIRST, NR21, W22, NR22,'#13#10 +
             '         W31 NULLS FIRST, NR31, W32, NR32');

  { date: 2013-04-16, file: alias intend for subquery.sql }
  TestQuery( '   SELECT   MAX( CASE WHEN POS = 1 THEN NR END) AS  NR_1'#13#10 +
             '        ,   MAX( CASE WHEN POS = 1 THEN ID END) AS  ID_1'#13#10 +
             '        ,   MAX( CASE WHEN POS = 2 THEN NR END) AS  NR_2'#13#10 +
             '        ,   MAX( CASE WHEN POS = 2 THEN ID END) AS  ID_2'#13#10 +
             '        ,   MAX( CASE WHEN POS = 3 THEN NR END) AS  NR_3'#13#10 +
             '        ,   MAX( CASE WHEN POS = 3 THEN ID END) AS  ID_3'#13#10 +
             '        ,   MAX( CASE WHEN POS = 4 THEN NR END) AS  NR_4'#13#10 +
             '        ,   MAX( CASE WHEN POS = 4 THEN ID END) AS  ID_4'#13#10 +
             '        ,   MAX( CASE WHEN POS = 5 THEN NR END) AS  NR_5'#13#10 +
             '        ,   MAX( CASE WHEN POS = 5 THEN ID END) AS  ID_5'#13#10 +
             '        ,   MAX( CASE WHEN POS = 6 THEN NR END) AS  NR_6'#13#10 +
             '        ,   MAX( CASE WHEN POS = 6 THEN ID END) AS  ID_6'#13#10 +
             '        ,   MAX( CASE WHEN POS = 7 THEN NR END) AS  NR_7'#13#10 +
             '        ,   MAX( CASE WHEN POS = 7 THEN ID END) AS  ID_7'#13#10 +
             '        ,   MAX( CASE WHEN POS = 8 THEN NR END) AS  NR_8'#13#10 +
             '        ,   MAX( CASE WHEN POS = 8 THEN ID END) AS  ID_8'#13#10 +
             '        ,   0                                   AS NEW_1'#13#10 +
             '        ,   0                                   AS NEW_2'#13#10 +
             '        ,   0                                   AS NEW_3'#13#10 +
             '        ,   0                                   AS NEW_4'#13#10 +
             '        ,   0                                   AS NEW_5'#13#10 +
             '        ,   0                                   AS NEW_6'#13#10 +
             '        ,   0                                   AS NEW_7'#13#10 +
             '        ,   0                                   AS NEW_8'#13#10 +
             '     FROM ( '#13#10 +
             '               SELECT   ROWNUM                  AS POS'#13#10 +
             '                    ,   NR'#13#10 +
             '                    ,   ID'#13#10 +
             '                 FROM ( '#13#10 +
             '                           SELECT   R3.ID'#13#10 +
             '                                ,   R1.NR || '' / '' || R2.NR || '' / '' || R3.NR AS NR'#13#10 +
             '                             FROM   RURA  R1'#13#10 +
             '                             JOIN   RURA  R2 ON R2.ID_RURA = R1.ID'#13#10 +
             '                             JOIN   RURA  R3 ON R3.ID_RURA = R2.ID'#13#10 +
             '                            WHERE   R1.ID_KANALIZACJA = 123'#13#10 +
             '                              AND   R1.ID_RURA IS NULL'#13#10 +
             '                        '#13#10 +
             '                        UNION ALL   '#13#10 +
             '                        '#13#10 +
             '                           SELECT   R2.ID'#13#10 +
             '                                ,   R1.NR || '' / '' || R2.NR                   AS NR'#13#10 +
             '                             FROM   RURA  R1'#13#10 +
             '                             JOIN   RURA  R2 ON R2.ID_RURA = R1.ID'#13#10 +
             '                            WHERE   R1.ID_KANALIZACJA = 123'#13#10 +
             '                              AND   R1.ID_RURA IS NULL'#13#10 +
             '                        '#13#10 +
             '                        UNION ALL   '#13#10 +
             '                        '#13#10 +
             '                           SELECT   R1.ID'#13#10 +
             '                                ,   TO_CHAR(R1.NR)                            AS NR'#13#10 +
             '                             FROM   RURA  R1'#13#10 +
             '                            WHERE   R1.ID_KANALIZACJA = 123'#13#10 +
             '                              AND   R1.ID_RURA IS NULL'#13#10 +
             '                         ORDER BY   NR'#13#10 +
             '                      ) '#13#10 +
             '          ) ' );

  { date: 2013-04-17, file: CREATE TABLE - ON COMMIT.sql }
  TestQuery( 'CREATE GLOBAL TEMPORARY TABLE TMP_RURY_PROFIL'#13#10 +
             '           (   ID_KANALIZACJA  INTEGER'#13#10 +
             '           ,   ID_RURA_PROFIL  INTEGER'#13#10 +
             '           ,   ID_TYP_ELEMENTU INTEGER'#13#10 +
             '           ,   ID_ELEMENT      INTEGER'#13#10 +
             '           )   ON COMMIT PRESERVE ROWS;' );

  { date: 2013-04-17, file: INSERT SELECT empty line.sql }
  TestQuery( 'INSERT INTO   a '#13#10 +
             '          (   a )'#13#10 +
             '     SELECT   1'#13#10 +
             ''#13#10 +
             '       FROM   dual' );

  { date: 2013-04-18, file: CREATE TABLE CHECK.sql }
  TestQuery( 'CREATE TABLE   tab1'#13#10 +
             '           (   col1 INTEGER'#13#10 +
             '               CONSTRAINT tab1$ck_col1 CHECK( col1 > 0 )'#13#10 +
             '           )   ' );

  { date: 2013-04-18, file: column with their constraints.sql }
//  TestQuery( 'CREATE TABLE   aaa'#13#10 +
//             '           (   id_bbb                '#13#10 +
//             '               CONSTRAINT aaa$fk_bbb REFERENCES bbb'#13#10 +
//             '           )   ;' );
//  TestQuery( ' ALTER TABLE   aaa'#13#10 +
//             '         ADD   id_bbb'#13#10 +
//             '  CONSTRAINT   aaa$fk_bbb'#13#10 +
//             '  REFERENCES   bbb ;' );

  { date: 2013-04-19, file: DELETE FROM intend.sql }
  TestQuery( '  SELECT   ID_GNIAZDO'#13#10 +
             '    INTO   G'#13#10 +
             '    FROM   SPAW_TMP'#13#10 +
             '   WHERE   ROWNUM = 1'#13#10 +
             'ORDER BY   ID ;' );
  TestQuery( '  UPDATE   WLOKNO_WKV'#13#10 +
             '     SET   ID_GNIAZDO_1 = G'#13#10 +
             '   WHERE   ID = :RECORDID'#13#10 +
             '     AND   ID_GNIAZDO_1 IS NULL'#13#10 +
             '     AND   NVL( ID_GNIAZDO_2, -1 ) != G'#13#10 +
             '     AND   ID_WEZEL_1 = :cmbLokalizacja.Value ;' );
  TestQuery( '  UPDATE   WLOKNO_WKV'#13#10 +
             '     SET   ID_GNIAZDO_2 = G'#13#10 +
             '   WHERE   ID = :RECORDID'#13#10 +
             '     AND   ID_GNIAZDO_2 IS NULL'#13#10 +
             '     AND   NVL( ID_GNIAZDO_1, -1 ) != G'#13#10 +
             '     AND   ID_WEZEL_2 = :cmbLokalizacja.Value ;' );
  TestQuery( '  DELETE '#13#10 +
             '   FROM    SPAW_TMP'#13#10 +
             '   WHERE   ID_GNIAZDO = G ;' );
  TestQuery( '  COMMIT   ;' );

  { date: 2013-04-19, file: IN PRIOR.sql }
  TestQuery( 'SELECT     SUM('#13#10 +
             '           CASE WHEN'#13#10 +
             '           CASE WHEN CONNECT_BY_ROOT(ID_WEZEL_1) = (SELECT ID_WEZEL_1 FROM SWIATLOWOD_WKV WHERE ID = :qEdit.ID)'#13#10 +
             '           THEN ID_WEZEL_2 ELSE ID_WEZEL_1 END   = (SELECT ID_WEZEL_2 FROM SWIATLOWOD_WKV WHERE ID = :qEdit.ID)'#13#10 +
             '           THEN 1 END) AS PRZEBIEG_ZGODNY,'#13#10 +
             ''#13#10 +
             '           NVL(SUM(K.DLUGOSC),0) AS DLUGOSC_TRASOWA,'#13#10 +
             ''#13#10 +
             '           NVL(MAX((SELECT SUM(NVL(WK.ZAPAS_1,0) + NVL(WK.ZAPAS_2,0) + NVL(WK.DLUGOSC_KABLA_W_BUDYNKU,0)) FROM WEZEL_KABEL WK WHERE WK.ID_TYP_ELEMENTU = 101 AND WK.ID_ELEMENT = :qEdit.ID)),0)'#13#10 +
             '           + NVL(SUM(K.DLUGOSC),0) AS DLUGOSC_KABLOWA'#13#10 +
             ''#13#10 +
             'FROM    (  SELECT *'#13#10 +
             '           FROM   KANALIZACJA K'#13#10 +
             '           WHERE  K.ID IN (SELECT ID_KANALIZACJA FROM ZAJETOSC_RURY WHERE ID_TYP_ELEMENTU = 101 AND ID_ELEMENT = :qEdit.ID)'#13#10 +
             '        )  K '#13#10 +
             'CONNECT BY NOCYCLE '#13#10 +
             '           K.ID_WEZEL_1 IN (PRIOR K.ID_WEZEL_1, PRIOR K.ID_WEZEL_2)'#13#10 +
             '        OR K.ID_WEZEL_2 IN (PRIOR K.ID_WEZEL_1, PRIOR K.ID_WEZEL_2)'#13#10 +
             'START WITH (SELECT ID_WEZEL_1 FROM SWIATLOWOD_WKV WHERE ID = :qEdit.ID) IN (K.ID_WEZEL_1, K.ID_WEZEL_2)' );

  { date: 2013-04-22, file: old UNION intend.sql }
  TestQuery( 'SELECT   WK.*'#13#10 +
             '     ,   L.OZNACZENIE || ''; '' || L.ADRES || '', '' || L.MIASTO               AS   WEZEL'#13#10 +
             '     , ('#13#10 +
             '         SELECT   MAX(SW.NR)                                               AS NR'#13#10 +
             '           FROM   SWIATLOWOD  SW'#13#10 +
             '          WHERE   SW.ID = WK.ID_ELEMENT'#13#10 +
             '            AND   WK.ID_TYP_ELEMENTU = 101'#13#10 +
             ''#13#10 +
             '         UNION ALL'#13#10 +
             ''#13#10 +
             '         SELECT   MAX(CU.NR)                                               AS NR'#13#10 +
             '           FROM   KABEL_CU  CU'#13#10 +
             '          WHERE   CU.ID = WK.ID_ELEMENT'#13#10 +
             '            AND   WK.ID_TYP_ELEMENTU = 111'#13#10 +
             ''#13#10 +
             '         UNION ALL'#13#10 +
             ''#13#10 +
             '         SELECT   MAX(INF.NR)                                              AS NR'#13#10 +
             '           FROM   KABEL_CU  INF'#13#10 +
             '          WHERE   INF.ID = WK.ID_ELEMENT'#13#10 +
             '            AND   WK.ID_TYP_ELEMENTU = 135'#13#10 +
             ''#13#10 +
             '         UNION ALL'#13#10 +
             ''#13#10 +
             '         SELECT   MAX(KON.NR)                                              AS NR'#13#10 +
             '           FROM   KABEL_KONCENTRYCZNY  KON'#13#10 +
             '          WHERE   KON.ID = WK.ID_ELEMENT'#13#10 +
             '            AND   WK.ID_TYP_ELEMENTU = 141'#13#10 +
             '       )                                                                   AS      NR'#13#10 +
             '     , ('#13#10 +
             '         SELECT   MAX(L.OZNACZENIE || ''; '' || L.ADRES || '', '' || L.MIASTO)'#13#10 +
             '           FROM   WEZEL_KABEL  WK1'#13#10 +
             '           JOIN   WEZEL_ADRES_V  L ON L.ID = WK1.ID_WEZEL'#13#10 +
             '          WHERE   WK1.ID_TYP_ELEMENTU = WK.ID_TYP_ELEMENTU'#13#10 +
             '            AND   WK1.ID_ELEMENT = WK.ID_ELEMENT'#13#10 +
             '            AND   WK1.LP = WK.LP - 1'#13#10 +
             '       )                                                                   AS WEZEL_1'#13#10 +
             '     , ('#13#10 +
             '         SELECT   MAX(L.OZNACZENIE || ''; '' || L.ADRES || '', '' || L.MIASTO)'#13#10 +
             '           FROM   WEZEL_KABEL  WK2'#13#10 +
             '           JOIN   WEZEL_ADRES_V  L ON L.ID = WK2.ID_WEZEL'#13#10 +
             '          WHERE   WK2.ID_TYP_ELEMENTU = WK.ID_TYP_ELEMENTU'#13#10 +
             '            AND   WK2.ID_ELEMENT = WK.ID_ELEMENT'#13#10 +
             '            AND   WK2.LP = WK.LP + 1'#13#10 +
             '       )                                                                   AS WEZEL_2'#13#10 +
             '  FROM   WEZEL_KABEL   WK'#13#10 +
             '     ,   WEZEL_ADRES_V  L'#13#10 +
             ' WHERE   WK.ID = 93'#13#10 +
             '   AND   WK.ID_WEZEL = L.ID ;' );

  { date: 2013-04-29, file: table alias B2 vs AS keyword hash.sql }
  TestQuery( '   SELECT   SW.ID'#13#10 +
             '        ,   SW.NR'#13#10 +
             '        ,   SW.DLUGOSC_OPTYCZNA'#13#10 +
             '        ,   SW.DLUGOSC_TRASOWA'#13#10 +
             ''#13#10 +
             '     FROM   SWIATLOWOD         SW'#13#10 +
             ''#13#10 +
             'JOIN   BUDYNEK            B2 ON   B2.ID              =  SW.ID_ELEMENT        AND SW.ID_TYP_WEZLA = 1' );

  { date: 2013-04-29, file: UPDATE aliased column.sql }
  TestQuery( 'UPDATE   TRANSMISJA_SKLADNIKI  TRS1'#13#10 +
             '   SET   TRS1.LP = 1'#13#10 +
             ' WHERE   TRS1.ID_TRANSMISJA = 1' );

  { date: 2013-04-29, file: ON current query alias color.sql }
  TestQuery( '   SELECT   SW.ID'#13#10 +
             '        ,   SW.NR'#13#10 +
             '     FROM   SWIATLOWOD  SK'#13#10 +
             'LEFT JOIN ( '#13#10 +
             '               SELECT   S.ID'#13#10 +
             '                    ,   S.NR'#13#10 +
             '                 FROM   SWIATLOWOD_WKV  S'#13#10 +
             '                WHERE   S.ID = SK.ID'#13#10 +
             '          ) SW ON SW.ID = SK.ID'#13#10 +
             ' ORDER BY   SW.NR ;' );
  TestQuery( '   SELECT   WK.*'#13#10 +
             '        ,   L.OZNACZENIE || ''; '' || L.ADRES || '', '' || L.MIASTO                  AS   WEZEL'#13#10 +
             '        , ( '#13#10 +
             '               SELECT   MAX(L.OZNACZENIE || ''; '' || L.ADRES || '', '' || L.MIASTO)'#13#10 +
             '                 FROM   WEZEL_KABEL  WK1'#13#10 +
             '                 JOIN   WEZEL_ADRES_V  L ON L.ID = WK1.ID_WEZEL'#13#10 +
             '                WHERE   WK1.ID_TYP_ELEMENTU = WK.ID_TYP_ELEMENTU'#13#10 +
             '                  AND   WK1.ID_ELEMENT = WK.ID_ELEMENT'#13#10 +
             '                  AND   WK1.LP = WK.LP - 1'#13#10 +
             '          )                                                                      AS WEZEL_1'#13#10 +
             '        , ( '#13#10 +
             '               SELECT   MAX(L.OZNACZENIE || ''; '' || L.ADRES || '', '' || L.MIASTO)'#13#10 +
             '                 FROM   WEZEL_KABEL  WK2'#13#10 +
             '                 JOIN   WEZEL_ADRES_V  L ON L.ID = WK2.ID_WEZEL'#13#10 +
             '                WHERE   WK2.ID_TYP_ELEMENTU = WK.ID_TYP_ELEMENTU'#13#10 +
             '                  AND   WK2.ID_ELEMENT = WK.ID_ELEMENT'#13#10 +
             '                  AND   WK2.LP = WK.LP + 1'#13#10 +
             '          )                                                                      AS WEZEL_2'#13#10 +
             '     FROM   WEZEL_KABEL   WK'#13#10 +
             '        ,   WEZEL_ADRES_V  L'#13#10 +
             '    WHERE   WK.ID = 93'#13#10 +
             '      AND   WK.ID_WEZEL = L.ID ;' );

  { date: 2013-05-05, file: ON Cond sort.sql }
  TestQuery( 'SELECT   *'#13#10 +
             '  FROM   a  A'#13#10 +
             '  JOIN   b  B ON A.typ = 1 AND B.id = A.id;' );
  TestQuery( 'SELECT   *'#13#10 +
             '  FROM   a  '#13#10 +
             '  JOIN   b   ON A.typ = 1 AND B.id = A.id;' );

  { date: 2013-05-07, file: CREATE TEMP TABLE intend.sql }
  TestQuery( 'CREATE GLOBAL TEMPORARY TABLE TMP_TRACE'#13#10 +
             '           (   LP              NUMBER'#13#10 +
             '           ,   ID_TYP_ELEMENTU NUMBER'#13#10 +
             '           ,   ID_ELEMENT      NUMBER'#13#10 +
             '           ,   ID_GNIAZDO_1    NUMBER'#13#10 +
             '           ,   ID_GNIAZDO_2    NUMBER'#13#10 +
             '           ,   ID_WEZEL_1      NUMBER'#13#10 +
             '           ,   ID_WEZEL_2      NUMBER'#13#10 +
             '           )   ' );

  { date: 2013-05-07, file: PRIOR in SELECT and WHERE.sql }
  TestQuery( 'SELECT     ROWNUM, ID_TYP, ID_ELEMENT, ID_GNIAZDO_1, ID_GNIAZDO_2'#13#10 +
             'FROM       ('#13#10 +
             ''#13#10 +
             'SELECT     TAB.ID_TYP, TAB.ID_ELEMENT, '#13#10 +
             '           CASE WHEN TAB.DEEP != 1 THEN MIN(TT.ID_GNIAZDO) END AS ID_GNIAZDO_1, '#13#10 +
             '           CASE WHEN TAB.DEEP =  1 OR MIN(TT.ID_GNIAZDO) != MAX(TT.ID_GNIAZDO) THEN MAX(TT.ID_GNIAZDO) END AS ID_GNIAZDO_2'#13#10 +
             'FROM       ('#13#10 +
             ''#13#10 +
             'SELECT     LEVEL AS DEEP, TT.ID_TYP, TT.ID_ELEMENT, PRIOR TT.ID_TYP, PRIOR TT.ID_ELEMENT'#13#10 +
             'FROM       TGI_TRACE TT'#13#10 +
             'WHERE      NVL(ID_TYP,0) != NVL(PRIOR ID_TYP,0) OR NVL(ID_ELEMENT,0) != NVL(PRIOR ID_ELEMENT,0)'#13#10 +
             'CONNECT BY NOCYCLE '#13#10 +
             '           ID_GNIAZDO = PRIOR ID_GNIAZDO OR ID_TYP = PRIOR ID_TYP AND ID_ELEMENT = PRIOR ID_ELEMENT'#13#10 +
             'START WITH ID_TYP = 1 AND ID_ELEMENT = 10'#13#10 +
             ''#13#10 +
             ') TAB'#13#10 +
             'LEFT JOIN  TGI_TRACE TT ON TT.ID_TYP = TAB.ID_TYP AND TT.ID_ELEMENT = TAB.ID_ELEMENT'#13#10 +
             'GROUP BY   TAB.DEEP, TAB.ID_TYP, TAB.ID_ELEMENT'#13#10 +
             'ORDER BY   TAB.DEEP'#13#10 +
             ')'#13#10 +
             ';' );

  { date: 2013-05-08, file: PRIOR in IN.sql }
  TestQuery( '    SELECT   ID_TYP'#13#10 +
             '         ,   ID_ELEMENT'#13#10 +
             '         ,   CASE WHEN ID_GNIAZDO_1 IN (PRIOR ID_GNIAZDO_1, PRIOR ID_GNIAZDO_2) THEN ID_GNIAZDO_1'#13#10 +
             '                  WHEN ID_GNIAZDO_2 IN (PRIOR ID_GNIAZDO_1, PRIOR ID_GNIAZDO_2) THEN ID_GNIAZDO_2'#13#10 +
             '             END AS ID_GNIAZDO_1     '#13#10 +
             '         ,   CASE WHEN ID_GNIAZDO_1 IN (PRIOR ID_GNIAZDO_1, PRIOR ID_GNIAZDO_2) THEN ID_GNIAZDO_2'#13#10 +
             '                  WHEN ID_GNIAZDO_2 IN (PRIOR ID_GNIAZDO_1, PRIOR ID_GNIAZDO_2) THEN ID_GNIAZDO_1'#13#10 +
             '                  WHEN DEEP = 1                                                 THEN NVL(ID_GNIAZDO_1, ID_GNIAZDO_2)     '#13#10 +
             '             END AS ID_GNIAZDO_2'#13#10 +
             '      FROM ( '#13#10 +
             '                 SELECT   MIN(DEEP)                                                                      AS         DEEP'#13#10 +
             '                      ,   TAB.ID_TYP'#13#10 +
             '                      ,   TAB.ID_ELEMENT'#13#10 +
             '                      ,   MIN(TT.ID_GNIAZDO)                                                             AS ID_GNIAZDO_1'#13#10 +
             '                      ,   CASE WHEN MIN(TT.ID_GNIAZDO) != MAX(TT.ID_GNIAZDO) THEN MAX(TT.ID_GNIAZDO) END AS ID_GNIAZDO_2'#13#10 +
             '                   FROM ( '#13#10 +
             '                              SELECT   LEVEL               AS             DEEP'#13#10 +
             '                                   ,   TT.ID_TYP'#13#10 +
             '                                   ,   TT.ID_ELEMENT'#13#10 +
             '                                   ,   PRIOR TT.ID_TYP     AS     PRIOR_ID_TYP'#13#10 +
             '                                   ,   PRIOR TT.ID_ELEMENT AS PRIOR_ID_ELEMENT'#13#10 +
             '                                FROM   TGI_TRACE  TT'#13#10 +
             '                               WHERE   NVL( ID_TYP, 0 ) != NVL( PRIOR ID_TYP, 0 )'#13#10 +
             '                                  OR   NVL( ID_ELEMENT, 0 ) != NVL( PRIOR ID_ELEMENT, 0 )'#13#10 +
             '                          CONNECT BY   NOCYCLE ID_GNIAZDO = PRIOR ID_GNIAZDO'#13#10 +
             '                                  OR   ID_TYP = PRIOR ID_TYP'#13#10 +
             '                                 AND   ID_ELEMENT = PRIOR ID_ELEMENT'#13#10 +
             '                          START WITH   ID_TYP = 1'#13#10 +
             '                                 AND   ID_ELEMENT = 10'#13#10 +
             '                        ) TAB'#13#10 +
             '              LEFT JOIN   TGI_TRACE  TT ON TT.ID_TYP = TAB.ID_TYP AND TT.ID_ELEMENT = TAB.ID_ELEMENT'#13#10 +
             '               GROUP BY   TAB.ID_TYP'#13#10 +
             '                      ,   TAB.ID_ELEMENT'#13#10 +
             '           ) TAB'#13#10 +
             'CONNECT BY   NOCYCLE ID_GNIAZDO_1 = PRIOR ID_GNIAZDO_1'#13#10 +
             '        OR   ID_GNIAZDO_1 = PRIOR ID_GNIAZDO_2'#13#10 +
             '        OR   ID_GNIAZDO_2 = PRIOR ID_GNIAZDO_1'#13#10 +
             '        OR   ID_GNIAZDO_2 = PRIOR ID_GNIAZDO_2'#13#10 +
             'START WITH   DEEP = 1' );

  { date: 2013-05-08, file: two CONSTRAINTS on a column.sql }
  TestQuery( 'CREATE TABLE   GNIAZDO_POLACZENIE '#13#10 +
             '           (   ID_GNIAZDO        NUMBER(10) NOT NULL CONSTRAINT GNIAZDO_POLACZENIE$PK       PRIMARY KEY  '#13#10 +
             '                                                     CONSTRAINT GNIAZDO_POLACZENIE$FK_GN REFERENCES GNIAZDO'#13#10 +
             '           ,   ID_TYP_ELEMENTU   NUMBER( 4) NOT NULL CONSTRAINT GNIAZDO_POLACZENIE$FK_TYP_1 REFERENCES TYP_ELEMENTU'#13#10 +
             '           ,   ID_ELEMENT        NUMBER(10) NOT NULL '#13#10 +
             '           );');
  TestQuery( 'CREATE INDEX GNIAZDO_POL$IX_TYP_ELEMENT ON GNIAZDO_POLACZENIE ( ID_TYP_ELEMENTU, ID_ELEMENT );' );

  { date: 2013-05-08, file: format NOCYCLE.sql }
  TestQuery( '    SELECT   CBY.ID_PASSPORT_TYPE'#13#10 +
             '         ,   CBY.ID_PASSPORT'#13#10 +
             '      INTO   lTyp'#13#10 +
             '         ,   lPass'#13#10 +
             '      FROM ( '#13#10 +
             '                 SELECT   LEVEL            AS DEEP'#13#10 +
             '                      ,   ID_PASSPORT_TYPE'#13#10 +
             '                      ,   ID_PASSPORT'#13#10 +
             '                   FROM   GNIAZDO_POLACZENIE_EKSPL'#13#10 +
             '                  WHERE   CONNECT_BY_ISLEAF = 1'#13#10 +
             '             CONNECT BY   NOCYCLE ID_GNIAZDO = PRIOR ID_GNIAZDO'#13#10 +
             '                     OR   ID_PASSPORT_TYPE = PRIOR ID_PASSPORT_TYPE'#13#10 +
             '                    AND   ID_PASSPORT = PRIOR ID_PASSPORT'#13#10 +
             '             START WITH   ID_PASSPORT_TYPE = aTyp'#13#10 +
             '                    AND   ID_PASSPORT = aPassport'#13#10 +
             '           ) CBY'#13#10 +
             ' LEFT JOIN   GNIAZDO_POLACZENIE_EKSPL  GPE ON GPE.ID_PASSPORT_TYPE = CBY.ID_PASSPORT_TYPE AND GPE.ID_PASSPORT = CBY.ID_PASSPORT'#13#10 +
             '  GROUP BY   CBY.ID_PASSPORT_TYPE'#13#10 +
             '         ,   CBY.ID_PASSPORT' );

  { date: 2013-05-08, file: CONSTRAINTS.sql }
  TestQuery( 'CREATE TABLE   ZDARZENIE_TYP'#13#10 +
             '           (   ID               INTEGER          NOT NULL'#13#10 +
             '               CONSTRAINT ZDARZENIE_TYP$PK PRIMARY KEY'#13#10 +
             ''#13#10 +
             '        , ID_TYP INT NULL'#13#10 +
             'CONSTRAINT ZDARZENIE_TYP$FK_TYP REFERENCES TYP'#13#10 +
             ''#13#10 +
             '           ,   NAZWA            VARCHAR( 50 )    NOT NULL'#13#10 +
             '               CONSTRAINT ZDARZENIE_TYP$UK UNIQUE'#13#10 +
             ''#13#10 +
             '           ,   LiczbaPorzadkowa NUMBER( 10, 3 ) NULL'#13#10 +
             '           )   ' );

  { date: 2013-05-17, file: params with no colon after format.sql }
  TestQuery( '               SELECT   :SKANALIZACJADLAKABLA'#13#10 +
             '                 FROM   DUAL' );

  { date: 2013-05-17, file: FOR UPDATE column name.sql }
//  TestQuery( 'SELECT'#13#10 +
//             'WLASCICIEL.*'#13#10 +
//             'FROM'#13#10 +
//             'WLASCICIEL'#13#10 +
//             'WHERE'#13#10 +
//             'WLASCICIEL.ID = :AKTID'#13#10 +
//             'AND WLASCICIEL.ID_ADRES_SZCZ = ADRES_SZCZ.ID(+)'#13#10 +
//             'FOR UPDATE OF'#13#10 +
//             'WLASCICIEL.ID NOWAIT' );

  { date: 2013-05-25, file: TRUNCATE TABLE.sql }
  TestQuery( 'TRUNCATE TABLE AUDITING;' );

  { date: 2013-05-25, file: CREATE TABLE AS.sql }
  TestQuery( 'CREATE TABLE AUDITING_20120525 AS'#13#10 +
             'SELECT   * '#13#10 +
             'FROM     AUDITING '#13#10 +
             'WHERE   (OPERACJA NOT IN (''INSERT'',''DELETE'') OR TABLE_NAME NOT IN (''GNIAZDO'',''PORT''))'#13#10 +
             'AND     (TABLE_NAME != ''PORT'' OR OPERACJA NOT IN (''LINK'',''UNLINK'') OR CHILD_TABLE_NAME != ''GNIAZDO'')'#13#10 +
             'AND      TABLE_NAME IN (''TYP_SWIATLOWODU'', ''TYP_SWIATLOWODU_WLOKNO'','#13#10 +
             '                        ''CECHA'', ''CECHA_ELEMENT'', ''MUFA'', ''WEZEL_KABEL'', '#13#10 +
             '                        ''KANALIZACJA'', ''PRZEKROJ_KANALIZACJI'', ''KANALIZACJA_WYPROWADZENIE'', ''RURA'','#13#10 +
             '                        ''STUDNIA'', ''GNIAZDO'', ''PORT'','#13#10 +
             '                        ''SWIATLOWOD'', ''WLOKNO'', ''TRASA_SW_POL'')'#13#10 +
             'AND      OPERACJA NOT IN (''LINK'',''UNLINK'');' );

  { date: 2013-05-25, file: COND intend.sql }
  TestQuery( 'SELECT   *'#13#10 +
             '  FROM   URZADZENIE       URZ'#13#10 +
             '  JOIN   TYP_URZADZENIA  TURZ ON TURZ.ID           = URZ.ID_TYP_URZADZENIA'#13#10 +
             '  JOIN   WEZEL_V           W2 ON   W2.ID           = URZ.ID_WEZEL         AND W2.ID_TYP_WEZLA = 23'#13#10 +
             '  JOIN   WEZEL              W ON    W.ID_TYP_WEZLA = 5                     AND W.ID_ELEMENT = URZ.ID' );

  { date: 2013-05-25, file: subquery intend.sql }
  TestQuery( '  UPDATE   PASSPORT_TYPE'#13#10 +
             '     SET   NULL = NULL'#13#10 +
             '   WHERE   PASSPORT_TABLE_NAME NOT IN '#13#10 +
             '         ( '#13#10 +
             '             SELECT   PASSPORT_TABLE_NAME'#13#10 +
             '               FROM   PASSPORT_TYPE'#13#10 +
             '           GROUP BY   PASSPORT_TABLE_NAME'#13#10 +
             '             HAVING   COUNT(*) > 1'#13#10 +
             '         ) ' );

  { date: 2013-05-29, file: old GRANT DENY REVOKE with multi names.sql }
  TestQuery( 'grant select, insert, delete ON tab1, tab2 TO tomek, marek;' );
  TestQuery( 'deny select, insert, delete ON tab1, tab2 TO tomek, marek;' );
  TestQuery( 'revoke select, insert, delete ON tab1, tab2 TO tomek, marek;' );

  { date: 2013-05-29, file: old CREATE USER and LOGIN.sql }
  TestQuery( 'create user tomek;' );
  TestQuery( 'create login tomek;' );

  { date: 2013-05-31, file: INSERT OR REPLACE.sql }
  TestQuery( 'insert or replace into a (b) values (1);' );

  { date: 2013-06-01, file: old ORDER BY intend after UNION.sql }
  TestQuery( '   SELECT   MAX(INF.NR) AS NR'#13#10 +
             '     FROM   KABEL_CU  INF'#13#10 +
             '    WHERE   INF.ID = WK.ID_ELEMENT'#13#10 +
             '      AND   WK.ID_TYP_ELEMENTU = 135'#13#10 +
             'UNION ALL'#13#10 +
             '   SELECT   MAX(KON.NR) AS NR'#13#10 +
             '     FROM   KABEL_KONCENTRYCZNY  KON'#13#10 +
             '    WHERE   KON.ID = WK.ID_ELEMENT'#13#10 +
             '      AND   WK.ID_TYP_ELEMENTU = 141'#13#10 +
             ' ORDER BY   1' );

  { date: 2013-06-04, file: INSERT INTO after format.sql }
  TestQuery( 'insert into AUDITING_TEST( id, '#13#10 +
             'table_name, '#13#10 +
             'table_id, '#13#10 +
             'operacja, '#13#10 +
             'child_table_name, '#13#10 +
             'child_table_id, '#13#10 +
             'column_name, '#13#10 +
             'data, '#13#10 +
             'uzytkownik, '#13#10 +
             'przyczyna, '#13#10 +
             'new_value, '#13#10 +
             'old_value, '#13#10 +
             'called_from) select  id, '#13#10 +
             'table_name, '#13#10 +
             'table_id, '#13#10 +
             'operacja, '#13#10 +
             'child_table_name, '#13#10 +
             'child_table_id, '#13#10 +
             'column_name, '#13#10 +
             'data, '#13#10 +
             'uzytkownik, '#13#10 +
             'przyczyna, '#13#10 +
             'new_value, '#13#10 +
             'old_value, '#13#10 +
             'called_from from auditing;' );

  { date: 2013-06-06, file: test DROP TABLE PURGE.sql }
//TestQuery( 'DROP TABLE   xxx PURGE ;' );
  TestQuery( 'PURGE RECYCLEBIN ;' );
  TestQuery( 'ALTER TABLE   xxx ENABLE ROW MOVEMENT ;' );
  TestQuery( 'ALTER TABLE   xxx deallocate unused;' );
  TestQuery( 'ALTER TABLE   xxx ENABLE ROW MOVEMENT ;' );
  TestQuery( 'ALTER TABLE   xxx SHRINK SPACE ;' );
  TestQuery( 'alter index  xxx coalesce;' );
  TestQuery( 'analyze index xxx validate structure' );

  { date: 2013-06-13, file: DELETE exprlist.sql }
  TestQuery( 'DELETE FROM tab1;' );
  TestQuery( 'DELETE m.* FROM materials m JOIN product p;' );

  { date: 2013-07-03, file: BETWEEN expr.sql }
  TestQuery( 'select * from KLASY_IP4_V WHERE 256*(256*(256*IP_A+IP_B)+IP_C)+IP_D BETWEEN IP AND BROADCAST' );

  { date: 2013-07-04, file: NEW params.sql }
  TestQuery( '  INSERT INTO  KLASY_IP4'#13#10 +
             '  (  ID, ID_PARENT, ID_W_HD,'#13#10 +
             '  ID_TYP_KLASY_IP, IP,'#13#10 +
             '  ID_PRZEZNACZENIE_IP, B_OBCA, UWAGI )'#13#10 +
             '  VALUES (  :NEW.ID, :NEW.ID_PARENT, :NEW.ID_W_HD,'#13#10 +
             '  :NEW.ID_TYP_KLASY_IP, l_IP,'#13#10 +
             '  :NEW.ID_PRZEZNACZENIE_IP, NVL(:NEW.B_OBCA,0), :NEW.UWAGI );' );

  { date: 2013-07-11, file: ALIAS intend.sql }
  TestQuery( '    SELECT   ''W''                                                                   AS       TYP'#13#10 +
             '         ,   WU.ID'#13#10 +
             '         ,   WU.NAZWA'#13#10 +
             '         ,   TWU.NAZWA || CASE WHEN WU2.ID IS NOT NULL THEN '' - '' END || WU2.NAZWA AS TWU_NAZWA'#13#10 +
             '         ,   WU.ILOSC_GNIAZD'#13#10 +
             '         ,   WU.OPIS'#13#10 +
             '         ,   WU.NR_SERYJNY'#13#10 +
             ''#13#10 +
             '      FROM   WYPOSAZENIE_URZADZENIA       WU'#13#10 +
             '      JOIN   TYP_WYPOSAZENIE_URZADZENIA  TWU ON TWU.ID = WU.ID_TYP_WYPOSAZENIE_URZADZENIA'#13#10 +
             ' LEFT JOIN   WYPOSAZENIE_URZADZENIA      WU2 ON WU2.ID = WU.ID_PARENT                     '#13#10 +
             ''#13#10 +
             '     WHERE   WU.ID_URZADZENIE = 678'#13#10 +
             ''#13#10 +
             ' UNION ALL   '#13#10 +
             ''#13#10 +
             '    SELECT   ''U''                                                                   AS          TYP'#13#10 +
             '         ,   W.ID_ELEMENT                                                          AS           ID'#13#10 +
             '         ,   W.ELEMENT                                                             AS        NAZWA'#13#10 +
             '         ,   NULL                                                                  AS    TWU_NAZWA'#13#10 +
             '         ,   0                                                                     AS ILOSC_GNIAZD'#13#10 +
             '         ,   NULL                                                                  AS         OPIS'#13#10 +
             '         ,   NULL                                                                  AS   NR_SERYJNY'#13#10 +
             ''#13#10 +
             '      FROM   WEZEL_V  W'#13#10 +
             ''#13#10 +
             'CONNECT BY   W.ID_WEZEL_PARENT = PRIOR W.ID'#13#10 +
             ''#13#10 +
             'START WITH   W.ID_WEZEL_PARENT IN '#13#10 +
             '           ( '#13#10 +
             '                 SELECT   ID'#13#10 +
             '                   FROM   WEZEL  W'#13#10 +
             '                  WHERE   W.ID_TYP_WEZLA = 5'#13#10 +
             '                    AND   W.ID_ELEMENT = 678'#13#10 +
             '           ) '#13#10 +
             ''#13#10 +
             ''#13#10 +
             '  ORDER BY   3' );

  { date: 2013-07-12, file: old IN  UNION empty lines.sql }
  TestQuery( ''#13#10 +
             'CREATE OR REPLACE VIEW USLUGI_V AS'#13#10 +
             ''#13#10 +
             '     SELECT   U.ID'#13#10 +
             '          ,   U.ID_DOSTAWCA'#13#10 +
             '          ,   U.ID_HANDLOWIEC'#13#10 +
             '          ,   U.ID_HD'#13#10 +
             '          ,   U.ID_KIEROWNIK'#13#10 +
             '          ,   U.ID_ODPOWIEDZIALNY'#13#10 +
             '          ,   U.ID_OPIEKUN'#13#10 +
             '          ,   U.ID_PARENT'#13#10 +
             '          ,   U.ID_PRODUKT'#13#10 +
             '          ,   U.ID_RODZAJ_USLUGI'#13#10 +
             '          ,   U.ID_STATUS_USLUGI'#13#10 +
             '          ,   U.ID_TYP_USLUGI'#13#10 +
             '          ,   U.ID_URZADZENIE_1'#13#10 +
             '          ,   U.ID_URZADZENIE_2'#13#10 +
             '          ,   U.ID_WLASCICIEL'#13#10 +
             '          ,   U.ID_W_HD'#13#10 +
             '          ,   U.B_BGP'#13#10 +
             '          ,   U.B_KONCOWA'#13#10 +
             '          ,   U.B_OBCA'#13#10 +
             '          ,   U.B_POMIAR_RUCHU'#13#10 +
             '          ,   U.B_ZGODNY'#13#10 +
             '          ,   U.DATA_URUCHOMIENIA'#13#10 +
             '          ,   U.DATA_WPISU'#13#10 +
             '          ,   U.DATA_ZAKONCZENIA'#13#10 +
             '          ,   U.BGP'#13#10 +
             '          ,   U.NAZWA'#13#10 +
             '          ,   U.NAZWA_PLIKU_STREF'#13#10 +
             '          ,   U.NR'#13#10 +
             '          ,   U.NR_TRANSAKCJI'#13#10 +
             '          ,   U.NR_UMOWY'#13#10 +
             '          ,   U.NR_USLUGI_HANDLOWEJ'#13#10 +
             '          ,   U.NR_USLUGI_OBCY'#13#10 +
             '          ,   U.NR_ZLECENIA'#13#10 +
             '          ,   U.OKRES_WYPOW'#13#10 +
             '          ,   U.UWAGI'#13#10 +
             '          ,   U.WARTOSC_AMORTYZACJI'#13#10 +
             '          ,   U.WARTOSC_KSIEGOWA'#13#10 +
             '          ,   U.WARTOSC_ODTWORZENIOWA'#13#10 +
             '          ,   U.TYP_USLUGI'#13#10 +
             '          ,   U.STATUS_USLUGI'#13#10 +
             '          ,   U.WLASCICIEL'#13#10 +
             '          ,   U.HANDLOWIEC'#13#10 +
             '          ,   U.KIEROWNIK'#13#10 +
             '          ,   U.OPIEKUN'#13#10 +
             '          ,   U.PRODUKT'#13#10 +
             '          ,   U.ODPOWIEDZIALNY'#13#10 +
             '          ,   U.PARENT'#13#10 +
             '       FROM ( '#13#10 +
             '                   SELECT   DISTINCT UTR.ID_USLUGA AS ID'#13#10 +
             '                     FROM ( '#13#10 +
             '                                 SELECT   ID'#13#10 +
             '                                   FROM   TRANSMISJA  T'#13#10 +
             '                                  WHERE   T.ID_PORT_1 IN '#13#10 +
             '                                        ( '#13#10 +
             '                                               SELECT   ID_PORT'#13#10 +
             '                                                 FROM   TMP_PORTY'#13#10 +
             '                                        ) '#13#10 +
             '                            '#13#10 +
             '                              UNION ALL   '#13#10 +
             '                            '#13#10 +
             '                                 SELECT   ID'#13#10 +
             '                                   FROM   TRANSMISJA  T'#13#10 +
             '                                  WHERE   T.ID_PORT_2 IN '#13#10 +
             '                                        ( '#13#10 +
             '                                               SELECT   ID_PORT'#13#10 +
             '                                                 FROM   TMP_PORTY'#13#10 +
             '                                        ) '#13#10 +
             '                            '#13#10 +
             '                              UNION ALL   '#13#10 +
             '                            '#13#10 +
             '                                 SELECT   TRS.ID_TRANSMISJA AS ID'#13#10 +
             '                                   FROM   TRANSMISJA_SKLADNIKI  TRS'#13#10 +
             '                             CONNECT BY   NOCYCLE TRS.ID_TRANSMISJA_SKL = PRIOR TRS.ID_TRANSMISJA'#13#10 +
             '                             START WITH   TRS.ID IN '#13#10 +
             '                                        ( '#13#10 +
             '                                               SELECT   TRS.ID'#13#10 +
             '                                                 FROM   TMP_PORTY             TMP'#13#10 +
             '                                                 JOIN   TRANSMISJA              T ON   T.ID_PORT_1         = TMP.ID_PORT'#13#10 +
             '                                                 JOIN   TRANSMISJA_SKLADNIKI  TRS ON TRS.ID_TRANSMISJA_SKL =   T.ID      '#13#10 +
             '                                          '#13#10 +
             '                                            UNION ALL   '#13#10 +
             '                                          '#13#10 +
             '                                               SELECT   TRS.ID'#13#10 +
             '                                                 FROM   TMP_PORTY             TMP'#13#10 +
             '                                                 JOIN   TRANSMISJA              T ON   T.ID_PORT_2         = TMP.ID_PORT'#13#10 +
             '                                                 JOIN   TRANSMISJA_SKLADNIKI  TRS ON TRS.ID_TRANSMISJA_SKL =   T.ID      '#13#10 +
             '                                          '#13#10 +
             '                                            UNION ALL   '#13#10 +
             '                                          '#13#10 +
             '                                               SELECT   TRS.ID'#13#10 +
             '                                                 FROM   TMP_PORTY             TMP'#13#10 +
             '                                                 JOIN   TRANSLACJE_SDH          T ON   T.ID_PORT_1         = TMP.ID_PORT'#13#10 +
             '                                                 JOIN   TRANSMISJA_SKLADNIKI  TRS ON TRS.ID_TRANSLACJE_SDH =   T.ID      '#13#10 +
             '                                          '#13#10 +
             '                                            UNION ALL   '#13#10 +
             '                                          '#13#10 +
             '                                               SELECT   TRS.ID'#13#10 +
             '                                                 FROM   TMP_PORTY             TMP'#13#10 +
             '                                                 JOIN   TRANSLACJE_SDH          T ON   T.ID_PORT_2         = TMP.ID_PORT'#13#10 +
             '                                                 JOIN   TRANSMISJA_SKLADNIKI  TRS ON TRS.ID_TRANSLACJE_SDH =   T.ID      '#13#10 +
             '                                          '#13#10 +
             '                                            UNION ALL   '#13#10 +
             '                                          '#13#10 +
             '                                               SELECT   TRS.ID'#13#10 +
             '                                                 FROM   TMP_PORTY             TMP'#13#10 +
             '                                                 JOIN   KABEL_INF               T ON   T.ID_PORT_1    = TMP.ID_PORT'#13#10 +
             '                                                 JOIN   TRANSMISJA_SKLADNIKI  TRS ON TRS.ID_KABEL_INF =   T.ID      '#13#10 +
             '                                          '#13#10 +
             '                                            UNION ALL   '#13#10 +
             '                                          '#13#10 +
             '                                               SELECT   TRS.ID'#13#10 +
             '                                                 FROM   TMP_PORTY             TMP'#13#10 +
             '                                                 JOIN   KABEL_INF               T ON   T.ID_PORT_2    = TMP.ID_PORT'#13#10 +
             '                                                 JOIN   TRANSMISJA_SKLADNIKI  TRS ON TRS.ID_KABEL_INF =   T.ID      '#13#10 +
             '                                          '#13#10 +
             '                                            UNION ALL   '#13#10 +
             '                                          '#13#10 +
             '                                               SELECT   TRS.ID'#13#10 +
             '                                                 FROM   TMP_GNIAZDA           TMP'#13#10 +
             '                                                 JOIN   TRASA_SW_POL            T ON   T.ID_GNIAZDO_1      = TMP.ID_GNIAZDO'#13#10 +
             '                                                 JOIN   TRANSMISJA_SKLADNIKI  TRS ON TRS.ID_TRASA_SW_POL_1 =   T.ID         '#13#10 +
             '                                          '#13#10 +
             '                                            UNION ALL   '#13#10 +
             '                                          '#13#10 +
             '                                               SELECT   TRS.ID'#13#10 +
             '                                                 FROM   TMP_GNIAZDA           TMP'#13#10 +
             '                                                 JOIN   TRASA_SW_POL            T ON   T.ID_GNIAZDO_2      = TMP.ID_GNIAZDO'#13#10 +
             '                                                 JOIN   TRANSMISJA_SKLADNIKI  TRS ON TRS.ID_TRASA_SW_POL_1 =   T.ID         '#13#10 +
             '                                          '#13#10 +
             '                                            UNION ALL   '#13#10 +
             '                                          '#13#10 +
             '                                               SELECT   TRS.ID'#13#10 +
             '                                                 FROM   TMP_GNIAZDA           TMP'#13#10 +
             '                                                 JOIN   TRASA_SW_POL            T ON   T.ID_GNIAZDO_1      = TMP.ID_GNIAZDO'#13#10 +
             '                                                 JOIN   TRANSMISJA_SKLADNIKI  TRS ON TRS.ID_TRASA_SW_POL_2 =   T.ID         '#13#10 +
             '                                          '#13#10 +
             '                                            UNION ALL   '#13#10 +
             '                                          '#13#10 +
             '                                               SELECT   TRS.ID'#13#10 +
             '                                                 FROM   TMP_GNIAZDA           TMP'#13#10 +
             '                                                 JOIN   TRASA_SW_POL            T ON   T.ID_GNIAZDO_2      = TMP.ID_GNIAZDO'#13#10 +
             '                                                 JOIN   TRANSMISJA_SKLADNIKI  TRS ON TRS.ID_TRASA_SW_POL_2 =   T.ID         '#13#10 +
             '                                          '#13#10 +
             '                                            UNION ALL   '#13#10 +
             '                                          '#13#10 +
             '                                               SELECT   TRS.ID'#13#10 +
             '                                                 FROM   TMP_GNIAZDA           TMP'#13#10 +
             '                                                 JOIN   TRASA_SW                T ON   T.ID_GNIAZDO_1  = TMP.ID_GNIAZDO'#13#10 +
             '                                                 JOIN   TRANSMISJA_SKLADNIKI  TRS ON TRS.ID_TRASA_SW_1 =   T.ID         '#13#10 +
             '                                          '#13#10 +
             '                                            UNION ALL   '#13#10 +
             '                                          '#13#10 +
             '                                               SELECT   TRS.ID'#13#10 +
             '                                                 FROM   TMP_GNIAZDA           TMP'#13#10 +
             '                                                 JOIN   TRASA_SW                T ON   T.ID_GNIAZDO_2  = TMP.ID_GNIAZDO'#13#10 +
             '                                                 JOIN   TRANSMISJA_SKLADNIKI  TRS ON TRS.ID_TRASA_SW_1 =   T.ID         '#13#10 +
             '                                          '#13#10 +
             '                                            UNION ALL   '#13#10 +
             '                                          '#13#10 +
             '                                               SELECT   TRS.ID'#13#10 +
             '                                                 FROM   TMP_GNIAZDA           TMP'#13#10 +
             '                                                 JOIN   TRASA_SW_ODC            T ON   T.ID_GNIAZDO_1  = TMP.ID_GNIAZDO  '#13#10 +
             '                                                 JOIN   TRANSMISJA_SKLADNIKI  TRS ON TRS.ID_TRASA_SW_1 =   T.ID_TRASA_SW'#13#10 +
             '                                          '#13#10 +
             '                                            UNION ALL   '#13#10 +
             '                                          '#13#10 +
             '                                               SELECT   TRS.ID'#13#10 +
             '                                                 FROM   TMP_GNIAZDA           TMP'#13#10 +
             '                                                 JOIN   TRASA_SW_ODC            T ON   T.ID_GNIAZDO_2  = TMP.ID_GNIAZDO  '#13#10 +
             '                                                 JOIN   TRANSMISJA_SKLADNIKI  TRS ON TRS.ID_TRASA_SW_1 =   T.ID_TRASA_SW'#13#10 +
             '                                          '#13#10 +
             '                                            UNION ALL   '#13#10 +
             '                                          '#13#10 +
             '                                               SELECT   TRS.ID'#13#10 +
             '                                                 FROM   TMP_GNIAZDA           TMP'#13#10 +
             '                                                 JOIN   TRASA_SW                T ON   T.ID_GNIAZDO_1  = TMP.ID_GNIAZDO'#13#10 +
             '                                                 JOIN   TRANSMISJA_SKLADNIKI  TRS ON TRS.ID_TRASA_SW_2 =   T.ID         '#13#10 +
             '                                          '#13#10 +
             '                                            UNION ALL   '#13#10 +
             '                                          '#13#10 +
             '                                               SELECT   TRS.ID'#13#10 +
             '                                                 FROM   TMP_GNIAZDA           TMP'#13#10 +
             '                                                 JOIN   TRASA_SW                T ON   T.ID_GNIAZDO_2  = TMP.ID_GNIAZDO'#13#10 +
             '                                                 JOIN   TRANSMISJA_SKLADNIKI  TRS ON TRS.ID_TRASA_SW_2 =   T.ID         '#13#10 +
             '                                          '#13#10 +
             '                                            UNION ALL   '#13#10 +
             '                                          '#13#10 +
             '                                               SELECT   TRS.ID'#13#10 +
             '                                                 FROM   TMP_GNIAZDA           TMP'#13#10 +
             '                                                 JOIN   TRASA_SW_ODC            T ON   T.ID_GNIAZDO_1  = TMP.ID_GNIAZDO  '#13#10 +
             '                                                 JOIN   TRANSMISJA_SKLADNIKI  TRS ON TRS.ID_TRASA_SW_2 =   T.ID_TRASA_SW'#13#10 +
             '                                          '#13#10 +
             '                                            UNION ALL   '#13#10 +
             '                                          '#13#10 +
             '                                               SELECT   TRS.ID'#13#10 +
             '                                                 FROM   TMP_GNIAZDA           TMP'#13#10 +
             '                                                 JOIN   TRASA_SW_ODC            T ON   T.ID_GNIAZDO_2  = TMP.ID_GNIAZDO  '#13#10 +
             '                                                 JOIN   TRANSMISJA_SKLADNIKI  TRS ON TRS.ID_TRASA_SW_2 =   T.ID_TRASA_SW'#13#10 +
             '                                          '#13#10 +
             '                                            UNION ALL   '#13#10 +
             '                                          '#13#10 +
             '                                               SELECT   TRS.ID'#13#10 +
             '                                                 FROM   TMP_GNIAZDA           TMP'#13#10 +
             '                                                 JOIN   WLOKNO                  T ON   T.ID_GNIAZDO_1 = TMP.ID_GNIAZDO'#13#10 +
             '                                                 JOIN   TRANSMISJA_SKLADNIKI  TRS ON TRS.ID_WLOKNO_1  =   T.ID         '#13#10 +
             '                                          '#13#10 +
             '                                            UNION ALL   '#13#10 +
             '                                          '#13#10 +
             '                                               SELECT   TRS.ID'#13#10 +
             '                                                 FROM   TMP_GNIAZDA           TMP'#13#10 +
             '                                                 JOIN   WLOKNO                  T ON   T.ID_GNIAZDO_2 = TMP.ID_GNIAZDO'#13#10 +
             '                                                 JOIN   TRANSMISJA_SKLADNIKI  TRS ON TRS.ID_WLOKNO_1  =   T.ID         '#13#10 +
             '                                          '#13#10 +
             '                                            UNION ALL   '#13#10 +
             '                                          '#13#10 +
             '                                               SELECT   TRS.ID'#13#10 +
             '                                                 FROM   TMP_TRANSLACJE        TMP'#13#10 +
             '                                                 JOIN   TRANSMISJA_SKLADNIKI  TRS ON TRS.ID_TRANSLACJE_SDH = TMP.ID_TRANSLACJA'#13#10 +
             '                                          '#13#10 +
             '                                          '#13#10 +
             '                                          '#13#10 +
             '                                          '#13#10 +
             '                                          '#13#10 +
             '                                          '#13#10 +
             '                                          '#13#10 +
             '                                          '#13#10 +
             '                                          '#13#10 +
             '                                          '#13#10 +
             '                                          '#13#10 +
             '                                          '#13#10 +
             '                                          '#13#10 +
             '                                          '#13#10 +
             '                                          '#13#10 +
             '                                          '#13#10 +
             '                                          '#13#10 +
             '                                          '#13#10 +
             '                                          '#13#10 +
             '                                          '#13#10 +
             '                                        ) '#13#10 +
             '                            '#13#10 +
             '                            '#13#10 +
             '                          ) UU'#13#10 +
             '                     JOIN   USLUGA_TRANSMISJA  UTR ON UTR.ID_TRANSMISJA = UU.ID'#13#10 +
             '              '#13#10 +
             '                    UNION   '#13#10 +
             '              '#13#10 +
             '                   SELECT   UTR.ID_USLUGA                      AS ID'#13#10 +
             '                     FROM   USLUGA_TRASA  UTR'#13#10 +
             '                    WHERE   UTR.ID_TRASA_SW IN '#13#10 +
             '                          ( '#13#10 +
             '                                 SELECT   T.ID'#13#10 +
             '                                   FROM   TMP_GNIAZDA  TMP'#13#10 +
             '                                   JOIN   TRASA_SW       T ON T.ID_GNIAZDO_1 = TMP.ID_GNIAZDO'#13#10 +
             '                            '#13#10 +
             '                              UNION ALL   '#13#10 +
             '                            '#13#10 +
             '                                 SELECT   T.ID'#13#10 +
             '                                   FROM   TMP_GNIAZDA  TMP'#13#10 +
             '                                   JOIN   TRASA_SW       T ON T.ID_GNIAZDO_2 = TMP.ID_GNIAZDO'#13#10 +
             '                            '#13#10 +
             '                              UNION ALL   '#13#10 +
             '                            '#13#10 +
             '                                 SELECT   T.ID_TRASA_SW'#13#10 +
             '                                   FROM   TMP_GNIAZDA  TMP'#13#10 +
             '                                   JOIN   TRASA_SW_ODC   T ON T.ID_GNIAZDO_1 = TMP.ID_GNIAZDO'#13#10 +
             '                            '#13#10 +
             '                              UNION ALL   '#13#10 +
             '                            '#13#10 +
             '                                 SELECT   T.ID_TRASA_SW'#13#10 +
             '                                   FROM   TMP_GNIAZDA  TMP'#13#10 +
             '                                   JOIN   TRASA_SW_ODC   T ON T.ID_GNIAZDO_2 = TMP.ID_GNIAZDO'#13#10 +
             '                            '#13#10 +
             '                            '#13#10 +
             '                            '#13#10 +
             '                          ) '#13#10 +
             '              '#13#10 +
             '                    UNION   '#13#10 +
             '              '#13#10 +
             '                   SELECT   UKI.ID_USLUGA                      AS ID'#13#10 +
             '                     FROM   KABEL_INF        KINF'#13#10 +
             '                     JOIN   USLUGA_KABEL_INF  UKI ON UKI.ID_KABEL_INF = KINF.ID'#13#10 +
             '                    WHERE   KINF.ID_GNIAZDO_1 IN '#13#10 +
             '                          ( '#13#10 +
             '                                 SELECT   ID_GNIAZDO'#13#10 +
             '                                   FROM   TMP_GNIAZDA'#13#10 +
             '                          ) '#13#10 +
             '                       OR   KINF.ID_GNIAZDO_2 IN '#13#10 +
             '                          ( '#13#10 +
             '                                 SELECT   ID_GNIAZDO'#13#10 +
             '                                   FROM   TMP_GNIAZDA'#13#10 +
             '                          ) '#13#10 +
             '                       OR   KINF.ID_PORT_1 IN '#13#10 +
             '                          ( '#13#10 +
             '                                 SELECT   ID_PORT'#13#10 +
             '                                   FROM   TMP_PORTY'#13#10 +
             '                          ) '#13#10 +
             '                       OR   KINF.ID_PORT_1 IN '#13#10 +
             '                          ( '#13#10 +
             '                                 SELECT   ID_PORT'#13#10 +
             '                                   FROM   TMP_PORTY'#13#10 +
             '                          ) '#13#10 +
             '              '#13#10 +
             '                    UNION   '#13#10 +
             '              '#13#10 +
             '                   SELECT   UURZ.ID_USLUGA                     AS ID'#13#10 +
             '                     FROM   USLUGA_URZADZENIE  UURZ'#13#10 +
             '                    WHERE   UURZ.ID_URZADZENIE IN '#13#10 +
             '                          ( '#13#10 +
             '                                 SELECT   ID_URZADZENIE'#13#10 +
             '                                   FROM   TMP_URZADZENIA'#13#10 +
             '                          ) '#13#10 +
             '              '#13#10 +
             '              '#13#10 +
             '              '#13#10 +
             '            ) UU'#13#10 +
             '       JOIN   USLUGA_EXT  U ON U.ID = UU.ID'#13#10 +
             ''#13#10 +
             '   ORDER BY   U.NR' );

  { date: 2013-07-24, file: DELETE FROM intend 2.sql }
  TestQuery( 'DELETE        '#13#10 +
             'FROM          TMP_PASSPORT_REFERENCES  TMP'#13#10 +
             'WHERE         f( TMP.ID_PASSPORT_TYPE, TMP.PASSPORT_TABLE_ID ) IN '#13#10 +
             '            ( '#13#10 +
             '              SELECT        TMP.ID_PASSPORT_TYPE'#13#10 +
             '              ,             TMP.PASSPORT_TABLE_ID'#13#10 +
             '              FROM          TMP_PASSPORT_REFERENCES  TMP'#13#10 +
             '              JOIN          PASSPORT_V                PV ON PV.ID_PASSPORT_TYPE = TMP.ID_PASSPORT_TYPE AND PV.PASSPORT_TABLE_ID = TMP.PASSPORT_TABLE_ID'#13#10 +
             '              WHERE         f( PV.ID_PASSPORT_TYPE_PARENT, PV.ID_PASSPORT_PARENT ) IN '#13#10 +
             '                          ( '#13#10 +
             '                            SELECT        ID_PASSPORT_TYPE'#13#10 +
             '                            ,             PASSPORT_TABLE_ID'#13#10 +
             '                            FROM          TMP_PASSPORT_REFERENCES'#13#10 +
             '                          ) '#13#10 +
             '            ) ' );

  { date: 2013-08-14, file: nested table ALIAS.sql }
  TestQuery( 'SELECT   *'#13#10 +
             '  FROM ( '#13#10 +
             '         SELECT   *'#13#10 +
             '           FROM   tab'#13#10 +
             '          WHERE   tab.id = ext.id'#13#10 +
             '       ) ext ;' );
  TestQuery( 'SELECT   *'#13#10 +
             '  FROM ( '#13#10 +
             '         SELECT   *'#13#10 +
             '           FROM ( '#13#10 +
             '                  SELECT   *'#13#10 +
             '                    FROM   tab'#13#10 +
             '                   WHERE   tab.id = upper.id'#13#10 +
             '                ) ext'#13#10 +
             '       ) upper ;' );

  { date: 2013-08-22, file: no empty lines before clauses (short query set to 6 lines).sql }
  TestQuery( '    SELECT   NR.NR  AS      NR_TACKA'#13#10 +
             '         ,   SW.NR  AS NR_SWIATLOWOD'#13#10 +
             '         ,   SWT.NR AS       NR_TUBA'#13#10 +
             '      FROM   MUFA               M'#13#10 +
             'CROSS JOIN   NR                NR'#13#10 +
             ' LEFT JOIN   MUFA_TACKA_TUBA  MTT ON MTT.ID_MUFA =   M.ID                 AND MTT.NR_TACKA = NR.NR'#13#10 +
             ' LEFT JOIN   SWIATLOWOD_TUBA  SWT ON SWT.ID      = MTT.ID_SWIATLOWOD_TUBA'#13#10 +
             ' LEFT JOIN   SWIATLOWOD        SW ON  SW.ID      = SWT.ID_SWIATLOWOD      '#13#10 +
             '     WHERE   M.ID = :qEdit.ID'#13#10 +
             '       AND   NR.NR BETWEEN 1 AND M.POJEMNOSC' );

  { date: 2013-08-23, file: WHERE AV no conditions.sql }
  TestQuery( 'SELECT'#13#10 +
             '*'#13#10 +
             'FROM'#13#10 +
             'PRZEGLAD P'#13#10 +
             'ORDER BY'#13#10 +
             'P.DATA' );

  { date: 2013-08-30, file: EXT QUERY alias.sql }
  TestQuery( 'SELECT   ''U''          AS          TYP'#13#10 +
             '     ,   W.ID_ELEMENT AS           ID'#13#10 +
             '     ,   W.ELEMENT    AS        NAZWA'#13#10 +
             '     ,   NULL         AS    TWU_NAZWA'#13#10 +
             '     ,   0            AS ILOSC_GNIAZD'#13#10 +
             '     ,   NULL         AS         OPIS'#13#10 +
             '     ,   NULL         AS   NR_SERYJNY'#13#10 +
             ''#13#10 +
             '  FROM   URZADZENIE  U'#13#10 +
             ''#13#10 +
             ' WHERE   U.ID_URZADZENIE_LINK = :QEDIT.ID ;' );

  { date: 2013-09-11, file: old ORDER BY too much empty lines.sql }
  TestQuery( '    SELECT   ''W''                                                                   AS       TYP'#13#10 +
             '         ,   ''wyposa¿enie''                                                         AS  TYP_OPIS'#13#10 +
             '         ,   WU.ID'#13#10 +
             '         ,   WU.NAZWA'#13#10 +
             '         ,   TWU.NAZWA || CASE WHEN WU2.ID IS NOT NULL THEN '' - '' END || WU2.NAZWA AS TWU_NAZWA'#13#10 +
             '         ,   WU.ILOSC_GNIAZD'#13#10 +
             '         ,   WU.OPIS'#13#10 +
             '         ,   WU.NR_SERYJNY'#13#10 +
             ''#13#10 +
             '      FROM   WYPOSAZENIE_URZADZENIA       WU'#13#10 +
             '      JOIN   TYP_WYPOSAZENIE_URZADZENIA  TWU ON TWU.ID = WU.ID_TYP_WYPOSAZENIE_URZADZENIA'#13#10 +
             ' LEFT JOIN   WYPOSAZENIE_URZADZENIA      WU2 ON WU2.ID = WU.ID_PARENT'#13#10 +
             ''#13#10 +
             '     WHERE   WU.ID_URZADZENIE = :qList.ID'#13#10 +
             ''#13#10 +
             ' UNION ALL'#13#10 +
             ''#13#10 +
             '    SELECT   ''U''                                                                   AS          TYP'#13#10 +
             '         ,   ''wewnêtrzne''                                                          AS     TYP_OPIS'#13#10 +
             '         ,   W.ID_ELEMENT                                                          AS           ID'#13#10 +
             '         ,   W.ELEMENT                                                             AS        NAZWA'#13#10 +
             '         ,   NULL                                                                  AS    TWU_NAZWA'#13#10 +
             '         ,   0                                                                     AS ILOSC_GNIAZD'#13#10 +
             '         ,   NULL                                                                  AS         OPIS'#13#10 +
             '         ,   NULL                                                                  AS   NR_SERYJNY'#13#10 +
             ''#13#10 +
             '      FROM   WEZEL_V  W'#13#10 +
             ''#13#10 +
             'CONNECT BY   W.ID_WEZEL_PARENT = PRIOR W.ID'#13#10 +
             ''#13#10 +
             'START WITH   W.ID_WEZEL_PARENT IN'#13#10 +
             '           ('#13#10 +
             '                 SELECT   ID'#13#10 +
             '                   FROM   WEZEL'#13#10 +
             '                  WHERE   ID_TYP_WEZLA = 5'#13#10 +
             '                    AND   ID_ELEMENT = :qList.ID'#13#10 +
             '           )'#13#10 +
             ''#13#10 +
             ' UNION ALL'#13#10 +
             ''#13#10 +
             '    SELECT   ''L''                                                                   AS          TYP'#13#10 +
             '         ,   ''po³¹czone''                                                           AS     TYP_OPIS'#13#10 +
             '         ,   U.ID                                                                  AS           ID'#13#10 +
             '         ,   U.NR                                                                  AS        NAZWA'#13#10 +
             '         ,   NULL                                                                  AS    TWU_NAZWA'#13#10 +
             '         ,   0                                                                     AS ILOSC_GNIAZD'#13#10 +
             '         ,   NULL                                                                  AS         OPIS'#13#10 +
             '         ,   NULL                                                                  AS   NR_SERYJNY'#13#10 +
             ''#13#10 +
             '      FROM   URZADZENIE  U'#13#10 +
             ''#13#10 +
             '     WHERE   U.ID_URZADZENIE_LINK = :qList.ID'#13#10 +
             ''#13#10 +
             ''#13#10 +
             ''#13#10 +
             '  ORDER BY   4' );

  { date: 2013-09-17, file: second CONSTRAINT.sql }
  TestQuery( 'ALTER TABLE   TRANSLACJE_SDH'#13#10 +
             '        ADD   ID_Y_CC INTEGER NULL'#13#10 +
             ' CONSTRAINT   TRANSLACJE_SDH$FK_Y_CC'#13#10 +
             ' REFERENCES   TRANSLACJE_SDH'#13#10 +
             'CONSTRAINT TRANSLACJE_SDH$UK_Y_CC UNIQUE'#13#10 +
             ';' );

  { date: 2013-09-25, file: empty line before CLAUSE on short query to no linespaces.sql }
  TestQuery( '  SELECT   *'#13#10 +
             ''#13#10 +
             '    FROM   KONTENERY_SDH  T'#13#10 +
             ''#13#10 +
             '   WHERE   ID_SYGNAL_DET_GLOWNY = -105'#13#10 +
             '     AND   ( ID - 1000000 * TRUNC(ID / 1000000) < 23999 )'#13#10 +
             ''#13#10 +
             'ORDER BY   ID' );

  { date: 2013-09-30, file: AV ON condition broken.sql }
  TestQuery( 'SELECT NVL(SZP.POZYCJA, W.POZYCJA) AS POZYCJA, /*DECODE(W.ID_TYP_WEZLA, 3, ''panel'',*/ W.TYP AS TYP, W.ELEMENT,'#13#10 +
             '       W.ID_TYP_WEZLA, W.ID_ELEMENT, W.WYMIAR, NVL2(NVL(SZP.POZYCJA, W.POZYCJA), 1, 0) * NVL2(W.WYMIAR, 1, 0) AS B_ZAJMUJEU,'#13#10 +
             '       W.ID_KARTOTEKA_URZADZEN'#13#10 +
             'FROM   WEZEL_X_SZAFA_V W'#13#10 +
             'LEFT JOIN SZAFA_POLKA SZP ON SZP.ID = W.ID_SZAFA_POLKA'#13#10 +
             'LEFT JOIN kartoteka_urzadzen KU ON KU.ID '#13#10 +
             'WHERE  W.ID_SZAFA = :qEdit.ID'#13#10 +
             ''#13#10 +
             'UNION ALL'#13#10 +
             ''#13#10 +
             'SELECT SK.POZYCJA, ''skrzynka'' AS TYP, SK.OPIS, 9999, SK.ID, SK.WYMIAR, NVL2(SK.POZYCJA, 1, 0) * NVL2(SK.WYMIAR, 1, 0) AS B_ZAJMUJEU,'#13#10 +
             '       NULL'#13#10 +
             'FROM   SKRZYNKA SK'#13#10 +
             'WHERE  SK.ID_SZAFA = :qEdit.ID'#13#10 +
             ''#13#10 +
             'UNION ALL'#13#10 +
             ''#13#10 +
             'SELECT SZP.POZYCJA, COALESCE(TSZP.NAZWA, ''pó³ka'') AS TYP, SZP.UWAGI, 9998, SZP.ID, SZP.WYMIAR, SZP.B_ZAJMUJEU,'#13#10 +
             '       NULL'#13#10 +
             'FROM   SZAFA_POLKA SZP'#13#10 +
             '       LEFT JOIN TYP_SZAFA_POLKA TSZP ON SZP.ID_TYP_SZAFA_POLKA = TSZP.ID'#13#10 +
             'WHERE  SZP.ID_SZAFA = :qEdit.ID'#13#10 +
             ''#13#10 +
             'ORDER BY 1' );

  { date: 2013-10-03, file: CLAUSE FROM no empty line.sql }
  TestQuery( 'SELECT   0       AS   TYP'#13#10 +
             '     ,   ''szafa'' AS WEZEL'#13#10 +
             '  FROM ( '#13#10 +
             '         SELECT   ID  FROM   WEZEL'#13#10 +
             '          WHERE   ID_TYP_WEZLA = 2'#13#10 +
             '            AND   ID_ELEMENT = :QEDIT.ID'#13#10 +
             '       ) ' );

  { date: 2013-10-03, file: EXPR intend with no AS keyword.sql }
  TestQuery( 'SELECT   0          TYP'#13#10 +
             '     ,   ''szafa'' WEZEL'#13#10 +
             '     ,   S.ID'#13#10 +
             '     ,   S.NR'#13#10 +
             '  FROM   SZAFA  S' );

  { date: 2013-10-09, file: old parse EXPR multiple apostrophes.sql }
  TestQuery( 'SELECT ''SELECT '''' || table_name || '''', '' || ''COUNT(*) AS ILE FROM '' || table_name || '' WHERE ID IS NULL'' || CHR(13) || CHR(10) ||'#13#10 +
             '       ''UNION ALL'' /*|| CHR(13) || CHR(10)*/'#13#10 +
             'FROM user_tab_cols'#13#10 +
             'WHERE column_name = ''ID'''#13#10 +
             'ORDER BY TABLE_NAME;' );

  { date: 2013-10-10, file: old ORDER BY empty lines.sql }
  TestQuery( '   SELECT   UW.ID'#13#10 +
             '        ,   GW.NR                      AS  GN_WEWN'#13#10 +
             '        ,   WZ.ELEMENT || ''.'' || GZ.NR AS  GN_ZEWN'#13#10 +
             '        ,   PWZ.TYP                    AS TYP_ZEWN'#13#10 +
             ''#13#10 +
             '     FROM   WEZEL               V'#13#10 +
             '     JOIN   GNIAZDO            GW ON  GW.ID_WEZEL         =  V.ID'#13#10 +
             '     JOIN   URZADZENIE_WLOKNO  UW ON  UW.ID_GNIAZDO_1     = GW.ID'#13#10 +
             '     JOIN   GNIAZDO            GZ ON  GZ.ID               = UW.ID_GNIAZDO_2'#13#10 +
             '     JOIN   WEZEL              WZ ON  WZ.ID               = GZ.ID_WEZEL'#13#10 +
             '     JOIN   PASSPORT_TYPE      PT ON  PT.ID_TYP_WEZLA     = WZ.ID_TYP_WEZLA'#13#10 +
             '     JOIN   PASSPORT_WEZEL_V  PWZ ON PWZ.ID_PASSPORT_TYPE = PT.ID           AND PWZ.PASSPORT_TABLE_ID = WZ.ID_ELEMENT'#13#10 +
             ''#13#10 +
             '    WHERE   V.ID_TYP_WEZLA = 5'#13#10 +
             '      AND   V.ID_ELEMENT = :qEdit.ID'#13#10 +
             ''#13#10 +
             '    UNION   ALL'#13#10 +
             ''#13#10 +
             '   SELECT   UW.ID'#13#10 +
             '        ,   GW.NR                      AS  GN_WEWN'#13#10 +
             '        ,   WZ.ELEMENT || ''.'' || GZ.NR AS  GN_ZEWN'#13#10 +
             '        ,   PWZ.TYP                    AS TYP_ZEWN'#13#10 +
             ''#13#10 +
             '     FROM   WEZEL               V'#13#10 +
             '     JOIN   GNIAZDO            GW ON  GW.ID_WEZEL         =  V.ID'#13#10 +
             '     JOIN   URZADZENIE_WLOKNO  UW ON  UW.ID_GNIAZDO_2     = GW.ID'#13#10 +
             '     JOIN   GNIAZDO            GZ ON  GZ.ID               = UW.ID_GNIAZDO_1'#13#10 +
             '     JOIN   WEZEL              WZ ON  WZ.ID               = GZ.ID_WEZEL'#13#10 +
             '     JOIN   PASSPORT_TYPE      PT ON  PT.ID_TYP_WEZLA     = WZ.ID_TYP_WEZLA'#13#10 +
             '     JOIN   PASSPORT_WEZEL_V  PWZ ON PWZ.ID_PASSPORT_TYPE = PT.ID           AND PWZ.PASSPORT_TABLE_ID = WZ.ID_ELEMENT'#13#10 +
             ''#13#10 +
             '    WHERE   V.ID_TYP_WEZLA = 5'#13#10 +
             '      AND   V.ID_ELEMENT = :qEdit.ID'#13#10 +
             '      AND   V.ID != WZ.ID'#13#10 +
             ''#13#10 +
             ''#13#10 +
             ' ORDER BY   2' );

  { date: 2013-10-11, file: old empty lines after ORDER BY.sql }
  TestQuery( '   SELECT   *'#13#10 +
             '     FROM   WEZEL_ADRES_V'#13#10 +
             '    WHERE   ID_TYP_WEZLA IN (1, 2, 24, 200)'#13#10 +
             '      AND   ID IN'#13#10 +
             '          ('#13#10 +
             '               SELECT   COALESCE(EU.ID_WEZEL, AU.ID_WEZEL) AS ID_WEZEL'#13#10 +
             '                 FROM   EL_URZADZENIE_PORTY  P'#13#10 +
             '            LEFT JOIN   EL_URZADZENIE       EU ON EU.ID = P.ID_EL_URZ'#13#10 +
             '            LEFT JOIN   URZADZENIE          AU ON AU.ID = P.ID_AKT_URZ'#13#10 +
             '                WHERE   P.ID_PORT_POL IS NULL'#13#10 +
             '          )'#13#10 +
             ' ORDER BY   ELEMENT' );

  { date: 2013-10-11, file: empty lines after WHERE.sql }
  TestQuery( '   SELECT   COALESCE(EU.ID_WEZEL, AU.ID_WEZEL) AS ID_WEZEL'#13#10 +
             '     FROM   EL_URZADZENIE_PORTY  P'#13#10 +
             'LEFT JOIN   EL_URZADZENIE       EU ON EU.ID = P.ID_EL_URZ  '#13#10 +
             'LEFT JOIN   URZADZENIE          AU ON AU.ID = P.ID_AKT_URZ'#13#10 +
             '    WHERE   P.ID_PORT_POL IS NULL' );

  { date: 2013-10-14, file: EXT QUERY alias 2.sql }
  TestQuery( '  SELECT   X.*'#13#10 +
             '       , ( '#13#10 +
             '             SELECT   COUNT(*)'#13#10 +
             '               FROM   XXX  X2'#13#10 +
             '              WHERE   X2.ID <= x.ID'#13#10 +
             '         )                                     AS LP'#13#10 +
             '    FROM   XXX  X'#13#10 +
             'ORDER BY   X.A' );

  { date: 2013-10-17, file: JOIN QUERY cond not sorted.sql }
  TestQuery( '   SELECT   W.*'#13#10 +
             '        ,   DECODE(NVL(TWW.ID, 0), 0, 0, 1)                                    AS B_ZAJETE'#13#10 +
             '        ,   DECODE(G1.NR, NULL, '''', SUBSTR(W1.ELEMENT, 1, 50) || ''/'' || G1.NR) AS       P1'#13#10 +
             '        ,   DECODE(G2.NR, NULL, '''', SUBSTR(W2.ELEMENT, 1, 50) || ''/'' || G2.NR) AS       P2'#13#10 +
             '        ,   S.NAZWA                                                            AS   STATUS'#13#10 +
             '        ,   NVL(TW.NR, W.NUMER_TEL)                                            AS    TRASA'#13#10 +
             ''#13#10 +
             '     FROM   PARA_CU         W'#13#10 +
             'LEFT JOIN   GNIAZDO        G1 ON  G1.ID         =   W.ID_GNIAZDO_1'#13#10 +
             'LEFT JOIN   GNIAZDO        G2 ON  G2.ID         =   W.ID_GNIAZDO_2'#13#10 +
             'LEFT JOIN ( '#13#10 +
             '               SELECT   *'#13#10 +
             '                 FROM   WEZEL_V'#13#10 +
             '                WHERE   ID_TYP_WEZLA IN (3, 116)'#13#10 +
             '          ) W1 ON  G1.ID_WEZEL   =  W1.ID           '#13#10 +
             'LEFT JOIN ( '#13#10 +
             '               SELECT   *'#13#10 +
             '                 FROM   WEZEL_V'#13#10 +
             '                WHERE   ID_TYP_WEZLA IN (3, 116)'#13#10 +
             '          ) W2 ON  G2.ID_WEZEL   =  W2.ID           '#13#10 +
             'LEFT JOIN   TRASA_CU_ODC  TWW ON TWW.ID_PARA_CU =   W.ID           '#13#10 +
             'LEFT JOIN   TRASA_CU       TW ON  TW.ID         = TWW.ID_TRASA_CU  '#13#10 +
             '        ,   STATUS          S'#13#10 +
             ''#13#10 +
             '    WHERE   W.ID_KABEL_CU = 209'#13#10 +
             '      AND   W.ID_STATUS = S.ID'#13#10 +
             '      AND   W.ID_PARA_CU IS NULL'#13#10 +
             ''#13#10 +
             ' ORDER BY   W.NR ;' );

  { date: 2013-11-28, file: FUNCTION params.sql }
  TestQuery( ' UPDATE   IMP_LOKALIZACJA  IMP'#13#10 +
             '    SET   ID_TYP_ELEMENTU = ELEMENT_TYPES.LOKALIZACJA'#13#10 +
             '      ,   ID_ELEMENT      = LOKALIZACJA_FIND_OR_CREATE ( p_SYMBOL             => IMP.SYMBOL'#13#10 +
             '                                                       , p_ID_TYP_LOKALIZACJI => -1 '#13#10 +
             '                                                       , p_ID_WEZEL           => WEZEL_FIND_OR_CREATE ( p_ID_TYP_WEZLA => WEZEL_TYPES.BUDYNEK'#13#10 +
             '                                                                                                      , p_ID_ELEMENT   => BUDYNEK_FIND_OR_CREATE ( p_NR             => IMP.SYMBOL'#13#10 +
             '                                                                                                                                                 , p_ID_ADRES       => UTWORZADRES2( IMP.MIASTO, IMP.ULICA, IMP.POSESJA, NULL )'#13#10 +
             '                                                                                                                                                 , p_ID_TYP_BUDYNKU => -1 '#13#10 +
             '                                                                                                                                                 )'#13#10 +
             '                                                                                                      )                                           '#13#10 +
             '                                                       )'#13#10 +
             '  WHERE   IMP.ID_ELEMENT  IS NULL'#13#10 +
             '    AND   IMP.KOD         = ''L'';' );

  { date: 2013-12-06, file: old UNION alias intend.sql }
  TestQuery( ' SELECT   1 AS         A1'#13#10 +
             '      ,   2 AS SECOND_ONE'#13#10 +
             '   FROM   dual'#13#10 +
             ''#13#10 +
             '  UNION'#13#10 +
             ''#13#10 +
             ' SELECT   200 AS     ANOTHER_ALIAS'#13#10 +
             '      ,   201 AS YET_ANOTHER_ALIAS'#13#10 +
             '   FROM   dual'#13#10 +
             ';' );
  TestQuery( ' SELECT   2 AS XX'#13#10 +
             '      ,   3 AS YY'#13#10 +
             '   FROM   dual ;' );
  TestQuery( 'select 1 as x, 2 as y from dual;' );

  { date: 2013-12-16, file: no ALIAS after AS - terminate keyword.sql }
//  TestQuery( ' SELECT   CAST(SUM(PRZ.WARTOSC) AS NUMBER(10,2)) || '' Mbps'' AS '#13#10 +
//             '   FROM   USLUGA_TRANSMISJA  UTR'#13#10 +
//             '   JOIN   TRANSMISJA          TR ON  TR.ID = UTR.ID_TRANSMISJA   '#13#10 +
//             '   JOIN   PRZEPLYWNOSC       PRZ ON PRZ.ID =  TR.ID_PRZEPLYWNOSC' );

  { date: 2013-12-18, file: old BEGIN END.sql }
  TestQuery( 'BEGIN'#13#10 +
             ''#13#10 +
             ' SELECT   1'#13#10 +
             '   INTO   A'#13#10 +
             '   FROM   dual ;'#13#10 +
             ''#13#10 +
             'END;' );

  { date: 2013-12-25, file: old IN condition wo close bracket.sql }
  TestQuery( ' SELECT   USL.*'#13#10 +
             '   FROM   USLUGA_EXT  USL'#13#10 +
             '  WHERE   USL.ID IN'#13#10 +
             '        ('#13#10 +
             '           SELECT   UTR.ID_USLUGA'#13#10 +
             '             FROM   USLUGA_TRANSMISJA  UTR'#13#10 +
             '        )' );

  { date: 2014-01-14, file: INSERT RETURNING.sql }
  TestQuery( 'insert into tab(a) values(b) returning id into lid' );

  { date: 2014-01-15, file: old RETURN.sql }
  TestQuery( 'return a+1' );

  { date: 2014-01-17, file: old vector - simplified.sql }
  TestQuery( 'SELECT *'#13#10 +
             'FROM   PASSPORT_V V'#13#10 +
             'WHERE (V.ID_PASSPORT_TYPE,V.PASSPORT_TABLE_ID) IN ('#13#10 +
             'SELECT 1,2'#13#10 +
             ')'#13#10 +
             'ORDER BY PTYP.PASSPORT_TYPE_NAME, V.NR' );


  { date: 2014-02-24, file: parse WHERE subquery.sql }
  TestQuery( '  INSERT INTO   URZADZENIE_IP'#13#10 +
             '            (   ID_URZADZENIE'#13#10 +
             '            ,   ID_KLASY_IP4   )'#13#10 +
             '       SELECT   lURz'#13#10 +
             '            ,   IP.ID'#13#10 +
             '         FROM   KLASY_IP4  IP'#13#10 +
             '        WHERE   IP.IP = l_IP'#13#10 +
             '           OR   IP.IP = l_IP + (SELECT NO_IP_ADDRES FROM TYP_KLASY_IP WHERE ID = :NEW.ID_TYP_KLASY_IP) - 1;' );

  { date: 2014-03-19, file: EXPR logic.sql }
  TestQuery( 'SELECT TR.ID_WEZEL_1, TR.ID_PORT_1 '#13#10 +
             'FROM transmisja tr '#13#10 +
             'JOIN wezel w ON w.id = tr.id_wezel_1'#13#10 +
             'JOIN passport_type pt ON pt.id_typ_wezla = w.id_typ_wezla'#13#10 +
             'JOIN passport_v pv ON pv.ID_PASSPORT_TYPE = pt.id AND pv.PASSPORT_TABLE_ID = w.id_element'#13#10 +
             'LEFT JOIN wlasciciel wl ON wl.id = pv.ID_WLASCICIEL'#13#10 +
             'WHERE tr.b_punkt_styku_a != 1'#13#10 +
             'AND ( pv.b_dzierzawa = 1 OR wl.b_operator_wlasny != 1 )' );

  TestQuery( 'SELECT ''SELECT '''''' || table_name || '''''', '' || ''COUNT(*) AS ILE FROM '' || table_name || '' WHERE ID IS NULL'' || CHR(13) || CHR(10) ||'#13#10 +
             '       ''UNION ALL'' /*|| CHR(13) || CHR(10)*/'#13#10 +
             'FROM user_tab_cols'#13#10 +
             'WHERE column_name = ''ID'''#13#10 +
             'ORDER BY TABLE_NAME;' );

  { date: 2014-05-11, file: parse EXPR vector.sql }
  TestQuery( 'SELECT *'#13#10 +
             'FROM   PASSPORT_V V '#13#10 +
             'WHERE (V.ID_PASSPORT_TYPE,V.PASSPORT_TABLE_ID) IN ('#13#10 +
             'SELECT 1,2'#13#10 +
             ')'#13#10 +
             'ORDER BY PTYP.PASSPORT_TYPE_NAME, V.NR;' );
  TestQuery( 'SELECT PTYP.ID_TYP_ELEMENTU, V.PASSPORT_TABLE_ID, PTYP.PASSPORT_TYPE_NAME, V.NR'#13#10 +
             'FROM   PASSPORT_V V '#13#10 +
             'JOIN   PASSPORT_TYPE PTYP ON PTYP.ID = V.ID_PASSPORT_TYPE'#13#10 +
             'WHERE (V.ID_PASSPORT_TYPE,V.PASSPORT_TABLE_ID) IN ('#13#10 +
             ''#13#10 +
             'SELECT PTYP.ID, WK1.ID_ELEMENT'#13#10 +
             'FROM   KANALIZACJA K'#13#10 +
             'JOIN   WEZEL_KABEL WK1 ON WK1.ID_WEZEL = K.ID_WEZEL_1'#13#10 +
             'JOIN   WEZEL_KABEL WK2 ON WK2.ID_WEZEL = K.ID_WEZEL_2 AND WK1.ID_TYP_ELEMENTU = WK2.ID_TYP_ELEMENTU AND WK1.ID_ELEMENT = WK2.ID_ELEMENT AND WK1.LP + 1 = WK2.LP'#13#10 +
             'JOIN   PASSPORT_TYPE PTYP ON PTYP.ID_TYP_ELEMENTU = WK1.ID_TYP_ELEMENTU'#13#10 +
             'WHERE  K.ID = 30755'#13#10 +
             'UNION'#13#10 +
             'SELECT PTYP.ID, WK1.ID_ELEMENT'#13#10 +
             'FROM   KANALIZACJA K'#13#10 +
             'JOIN   WEZEL_KABEL WK1 ON WK1.ID_WEZEL = K.ID_WEZEL_2'#13#10 +
             'JOIN   WEZEL_KABEL WK2 ON WK2.ID_WEZEL = K.ID_WEZEL_1 AND WK1.ID_TYP_ELEMENTU = WK2.ID_TYP_ELEMENTU AND WK1.ID_ELEMENT = WK2.ID_ELEMENT AND WK1.LP + 1 = WK2.LP'#13#10 +
             'JOIN   PASSPORT_TYPE PTYP ON PTYP.ID_TYP_ELEMENTU = WK1.ID_TYP_ELEMENTU'#13#10 +
             'WHERE  K.ID = 30755'#13#10 +
             ')'#13#10 +
             'ORDER BY PTYP.PASSPORT_TYPE_NAME, V.NR' );

  { date: 2014-05-11, file: format ORDER BY misc.sql }
  TestQuery( '   SELECT   MAX(INF.NR) AS NR'#13#10 +
             '     FROM   KABEL_CU  INF'#13#10 +
             '    WHERE   INF.ID = WK.ID_ELEMENT'#13#10 +
             '      AND   WK.ID_TYP_ELEMENTU = 135'#13#10 +
             'UNION ALL   '#13#10 +
             '   SELECT   MAX(KON.NR) AS NR'#13#10 +
             '     FROM   KABEL_KONCENTRYCZNY  KON'#13#10 +
             '    WHERE   KON.ID = WK.ID_ELEMENT'#13#10 +
             '      AND   WK.ID_TYP_ELEMENTU = 141'#13#10 +
             ' ORDER BY   1'#13#10 +
             ';' );
  TestQuery( '   SELECT   *'#13#10 +
             '     FROM   WEZEL_ADRES_V'#13#10 +
             '    WHERE   ID_TYP_WEZLA IN (1, 2, 24, 200)'#13#10 +
             '      AND   ID IN '#13#10 +
             '          ( '#13#10 +
             '               SELECT   COALESCE(EU.ID_WEZEL, AU.ID_WEZEL) AS ID_WEZEL'#13#10 +
             '                 FROM   EL_URZADZENIE_PORTY  P'#13#10 +
             '            LEFT JOIN   EL_URZADZENIE       EU ON EU.ID = P.ID_EL_URZ  '#13#10 +
             '            LEFT JOIN   URZADZENIE          AU ON AU.ID = P.ID_AKT_URZ'#13#10 +
             '                WHERE   P.ID_PORT_POL IS NULL'#13#10 +
             '          ) '#13#10 +
             ' ORDER BY   ELEMENT'#13#10 +
             ';' );
  TestQuery( 'SELECT * '#13#10 +
             'FROM ('#13#10 +
             '        SELECT R2.WIAZKA_1 ||''-''|| R2.NR_1 AS WNR1, R2.WIAZKA_2 ||''-''|| R2.NR_2 AS WNR2, '#13#10 +
             '               R1.ID_KOLOR AS KOLOR1, R1.WIAZKA_1 AS W11, R1.NR_1 AS NR11, R1.WIAZKA_2 AS W12, R1.NR_2 AS NR12,'#13#10 +
             '               R2.ID_KOLOR AS KOLOR2, R2.WIAZKA_1 AS W21, R2.NR_1 AS NR21, R2.WIAZKA_2 AS W22, R2.NR_2 AS NR22,'#13#10 +
             '               R3.ID_KOLOR AS KOLOR3, R3.WIAZKA_1 AS W31, R3.NR_1 AS NR31, R3.WIAZKA_2 AS W32, R3.NR_2 AS NR32'#13#10 +
             '        FROM   RURA R1'#13#10 +
             '        JOIN   RURA R2 ON R2.ID_RURA = R1.ID'#13#10 +
             '        JOIN   RURA R3 ON R3.ID_RURA = R2.ID'#13#10 +
             '        WHERE  R1.ID_KANALIZACJA = 123'#13#10 +
             '          AND  R1.ID_RURA IS NULL'#13#10 +
             ''#13#10 +
             '        UNION ALL'#13#10 +
             ''#13#10 +
             '        SELECT R2.WIAZKA_1 ||''-''|| R2.NR_1, R2.WIAZKA_2 ||''-''|| R2.NR_2, '#13#10 +
             '               R1.ID_KOLOR, R1.WIAZKA_1 AS W11, R1.NR_1 AS NR11, R1.WIAZKA_2 AS W12, R1.NR_2 AS NR12,'#13#10 +
             '               R2.ID_KOLOR, R2.WIAZKA_1 AS W21, R2.NR_1 AS NR21, R2.WIAZKA_2 AS R22, R2.NR_2 AS NR22,'#13#10 +
             '               NULL, NULL, NULL, NULL, NULL'#13#10 +
             '        FROM   RURA R1'#13#10 +
             '        JOIN   RURA R2 ON R2.ID_RURA = R1.ID'#13#10 +
             '        WHERE  R1.ID_KANALIZACJA = 123'#13#10 +
             '          AND  R1.ID_RURA IS NULL'#13#10 +
             ''#13#10 +
             '        UNION ALL'#13#10 +
             ''#13#10 +
             '        SELECT R1.WIAZKA_1 ||''-''|| R1.NR_1, R1.WIAZKA_2 ||''-''|| R1.NR_2, '#13#10 +
             '               R1.ID_KOLOR, R1.WIAZKA_1, R1.NR_1, R1.WIAZKA_2, R1.NR_2,'#13#10 +
             '               NULL, NULL, NULL, NULL, NULL,'#13#10 +
             '               NULL, NULL, NULL, NULL, NULL'#13#10 +
             '        FROM   RURA R1'#13#10 +
             '        WHERE  R1.ID_KANALIZACJA = 123'#13#10 +
             '          AND  R1.ID_RURA IS NULL'#13#10 +
             ')'#13#10 +
             'ORDER BY W11, NR11, W12, NR12,'#13#10 +
             '         W21 NULLS FIRST, NR21, W22, NR22,'#13#10 +
             '         W31 NULLS FIRST, NR31, W32, NR32'#13#10 +
             ';' );
  TestQuery( '    SELECT   ''W''                                                                   AS       TYP'#13#10 +
             '         ,   ''wyposa¿enie''                                                         AS  TYP_OPIS'#13#10 +
             '         ,   WU.ID'#13#10 +
             '         ,   WU.NAZWA'#13#10 +
             '         ,   TWU.NAZWA || CASE WHEN WU2.ID IS NOT NULL THEN '' - '' END || WU2.NAZWA AS TWU_NAZWA'#13#10 +
             '         ,   WU.ILOSC_GNIAZD'#13#10 +
             '         ,   WU.OPIS'#13#10 +
             '         ,   WU.NR_SERYJNY'#13#10 +
             ''#13#10 +
             '      FROM   WYPOSAZENIE_URZADZENIA       WU'#13#10 +
             '      JOIN   TYP_WYPOSAZENIE_URZADZENIA  TWU ON TWU.ID = WU.ID_TYP_WYPOSAZENIE_URZADZENIA'#13#10 +
             ' LEFT JOIN   WYPOSAZENIE_URZADZENIA      WU2 ON WU2.ID = WU.ID_PARENT                     '#13#10 +
             ''#13#10 +
             '     WHERE   WU.ID_URZADZENIE = :qList.ID'#13#10 +
             ''#13#10 +
             ' UNION ALL   '#13#10 +
             ''#13#10 +
             '    SELECT   ''U''                                                                   AS          TYP'#13#10 +
             '         ,   ''wewnêtrzne''                                                          AS     TYP_OPIS'#13#10 +
             '         ,   W.ID_ELEMENT                                                          AS           ID'#13#10 +
             '         ,   W.ELEMENT                                                             AS        NAZWA'#13#10 +
             '         ,   NULL                                                                  AS    TWU_NAZWA'#13#10 +
             '         ,   0                                                                     AS ILOSC_GNIAZD'#13#10 +
             '         ,   NULL                                                                  AS         OPIS'#13#10 +
             '         ,   NULL                                                                  AS   NR_SERYJNY'#13#10 +
             ''#13#10 +
             '      FROM   WEZEL_V  W'#13#10 +
             ''#13#10 +
             'CONNECT BY   W.ID_WEZEL_PARENT = PRIOR W.ID'#13#10 +
             ''#13#10 +
             'START WITH   W.ID_WEZEL_PARENT IN '#13#10 +
             '           ( '#13#10 +
             '                 SELECT   ID'#13#10 +
             '                   FROM   WEZEL'#13#10 +
             '                  WHERE   ID_TYP_WEZLA = 5'#13#10 +
             '                    AND   ID_ELEMENT = :qList.ID'#13#10 +
             '           ) '#13#10 +
             ''#13#10 +
             ' UNION ALL   '#13#10 +
             ''#13#10 +
             '    SELECT   ''L''                                                                   AS          TYP'#13#10 +
             '         ,   ''po³¹czone''                                                           AS     TYP_OPIS'#13#10 +
             '         ,   U.ID                                                                  AS           ID'#13#10 +
             '         ,   U.NR                                                                  AS        NAZWA'#13#10 +
             '         ,   NULL                                                                  AS    TWU_NAZWA'#13#10 +
             '         ,   0                                                                     AS ILOSC_GNIAZD'#13#10 +
             '         ,   NULL                                                                  AS         OPIS'#13#10 +
             '         ,   NULL                                                                  AS   NR_SERYJNY'#13#10 +
             ''#13#10 +
             '      FROM   URZADZENIE  U'#13#10 +
             ''#13#10 +
             '     WHERE   U.ID_URZADZENIE_LINK = :qList.ID'#13#10 +
             ''#13#10 +
             ''#13#10 +
             ''#13#10 +
             '  ORDER BY   4'#13#10 +
             ';' );
  TestQuery( '   SELECT   UW.ID'#13#10 +
             '        ,   GW.NR                      AS  GN_WEWN'#13#10 +
             '        ,   WZ.ELEMENT || ''.'' || GZ.NR AS  GN_ZEWN'#13#10 +
             '        ,   PWZ.TYP                    AS TYP_ZEWN'#13#10 +
             ''#13#10 +
             '     FROM   WEZEL               V'#13#10 +
             '     JOIN   GNIAZDO            GW ON  GW.ID_WEZEL         =  V.ID           '#13#10 +
             '     JOIN   URZADZENIE_WLOKNO  UW ON  UW.ID_GNIAZDO_1     = GW.ID           '#13#10 +
             '     JOIN   GNIAZDO            GZ ON  GZ.ID               = UW.ID_GNIAZDO_2'#13#10 +
             '     JOIN   WEZEL              WZ ON  WZ.ID               = GZ.ID_WEZEL     '#13#10 +
             '     JOIN   PASSPORT_TYPE      PT ON  PT.ID_TYP_WEZLA     = WZ.ID_TYP_WEZLA'#13#10 +
             '     JOIN   PASSPORT_WEZEL_V  PWZ ON PWZ.ID_PASSPORT_TYPE = PT.ID           AND PWZ.PASSPORT_TABLE_ID = WZ.ID_ELEMENT'#13#10 +
             ''#13#10 +
             '    WHERE   V.ID_TYP_WEZLA = 5'#13#10 +
             '      AND   V.ID_ELEMENT = :qEdit.ID'#13#10 +
             ''#13#10 +
             '    UNION   ALL'#13#10 +
             ''#13#10 +
             '   SELECT   UW.ID'#13#10 +
             '        ,   GW.NR                      AS  GN_WEWN'#13#10 +
             '        ,   WZ.ELEMENT || ''.'' || GZ.NR AS  GN_ZEWN'#13#10 +
             '        ,   PWZ.TYP                    AS TYP_ZEWN'#13#10 +
             ''#13#10 +
             '     FROM   WEZEL               V'#13#10 +
             '     JOIN   GNIAZDO            GW ON  GW.ID_WEZEL         =  V.ID           '#13#10 +
             '     JOIN   URZADZENIE_WLOKNO  UW ON  UW.ID_GNIAZDO_2     = GW.ID           '#13#10 +
             '     JOIN   GNIAZDO            GZ ON  GZ.ID               = UW.ID_GNIAZDO_1'#13#10 +
             '     JOIN   WEZEL              WZ ON  WZ.ID               = GZ.ID_WEZEL     '#13#10 +
             '     JOIN   PASSPORT_TYPE      PT ON  PT.ID_TYP_WEZLA     = WZ.ID_TYP_WEZLA'#13#10 +
             '     JOIN   PASSPORT_WEZEL_V  PWZ ON PWZ.ID_PASSPORT_TYPE = PT.ID           AND PWZ.PASSPORT_TABLE_ID = WZ.ID_ELEMENT'#13#10 +
             ''#13#10 +
             '    WHERE   V.ID_TYP_WEZLA = 5'#13#10 +
             '      AND   V.ID_ELEMENT = :qEdit.ID'#13#10 +
             '      AND   V.ID != WZ.ID'#13#10 +
             ''#13#10 +
             ' ORDER BY   2' );

  { date: 2014-05-11, file: parse CODE elements.sql }
  TestQuery( '   USE   Comit ;' );
  TestQuery( 'TRUNCATE TABLE tab1 ;' );
  TestQuery( '    GO   ;' );
  TestQuery( 'SAVEPOINT svp_01 ;' );
  TestQuery( 'create user tomek;' );
  TestQuery( 'create login tomek;' );
  TestQuery( 'grant select, insert, delete ON tab1, tab2 TO tomek, marek;' );
  TestQuery( 'deny select, insert, delete ON tab1, tab2 TO tomek, marek;' );
  TestQuery( 'revoke select, insert, delete ON tab1, tab2 TO tomek, marek;' );
  TestQuery( 'BEGIN'#13#10 +
             ''#13#10 +
             ' SELECT   1'#13#10 +
             '   INTO   A'#13#10 +
             '   FROM   dual ;'#13#10 +
             ''#13#10 +
             'return a+1;'#13#10 +
             ''#13#10 +
             'END;' );
  TestQuery( 'CREATE OR REPLACE PUBLIC SYNONYM /*PST.*/ FAST_ATTACHMENTS FOR PST_SYSTEM.FAST_ATTACHMENTS;' );
  TestQuery( 'GRANT SELECT ON PST.FAST_ATTACHMENTS TO PST;' );
  TestQuery( 'GRANT INSERT ON PST.FAST_ATTACHMENTS TO PST;' );
  TestQuery( 'GRANT SELECT ON PST_SYSTEM.FAST_ATTACHMENTS TO PST;' );
  TestQuery( 'GRANT INSERT ON PST_SYSTEM.FAST_ATTACHMENTS TO PST;' );

  { date: 2014-05-11, file: format hang UNION misc - LONG.sql, format UNION misc - LOOP.sql }
  TestQuery( 'select 1 from dual union all select 2 from dual;' );
  TestQuery( 'select 1 from dual'#13#10 +
             'union '#13#10 +
             'select 2 from dual'#13#10 +
             'order by 1'#13#10 +
             ';' );
  TestQuery( ' SELECT   1 AS         A1'#13#10 +
             '      ,   2 AS SECOND_ONE'#13#10 +
             '   FROM   dual'#13#10 +
             ''#13#10 +
             '  UNION   '#13#10 +
             ''#13#10 +
             ' SELECT   200 AS     ANOTHER_ALIAS'#13#10 +
             '      ,   201 AS YET_ANOTHER_ALIAS'#13#10 +
             '   FROM   dual'#13#10 +
             ';' );
  TestQuery( ' SELECT   2 AS XX'#13#10 +
             '      ,   3 AS YY'#13#10 +
             '   FROM   dual ;'#13#10 +
             'select 1 as x, 2 as y from dual;' );
  TestQuery( '         SELECT  1'#13#10 +
             '           FROM  a'#13#10 +
             '          WHERE  a = 1'#13#10 +
             ''#13#10 +
             '      UNION ALL  '#13#10 +
             ''#13#10 +
             '         SELECT  2'#13#10 +
             '           FROM  b'#13#10 +
             '          WHERE  b = 2'#13#10 +
             ''#13#10 +
             '      UNION ALL  '#13#10 +
             ''#13#10 +
             '         SELECT  3'#13#10 +
             '           FROM  c'#13#10 +
             '          WHERE  c = 3 ;' );
  TestQuery( 'SELECT DISTINCT ID FROM TRANSMISJA TR WHERE TR.ID = 12590 '#13#10 +
             'UNION'#13#10 +
             'SELECT ID FROM TRANSMISJA TR WHERE TR.ID = 12590 '#13#10 +
             'UNION'#13#10 +
             'SELECT ID FROM TRANSMISJA TR WHERE TR.ID = 12590 '#13#10 +
             'UNION'#13#10 +
             'SELECT ID FROM TRANSMISJA TR WHERE TR.ID = 12590 ;' );
  TestQuery( 'SELECT td.nazwa, trs.id_transmisja'#13#10 +
             'FROM   transmisja_skladniki trs '#13#10 +
             'JOIN   transmisja_przebieg trp ON trp.id_transmisja = trs.id_transmisja'#13#10 +
             'JOIN   PORT P ON P.ID = trp.id_port_1'#13#10 +
             'JOIN   transport_det td ON td.id = p.id_transport_det'#13#10 +
             'WHERE  trs.id_transmisja_skl = 12193'#13#10 +
             'AND    trp.id_port_1 IN ('#13#10 +
             '        SELECT      p11.id'#13#10 +
             '        FROM        transmisja tr'#13#10 +
             '        JOIN        port  p1 ON p1.id = tr.id_port_1'#13#10 +
             '        JOIN        port p11 ON p11.id_wezel = p1.id_wezel'#13#10 +
             '        WHERE       tr.id = 12193'#13#10 +
             '        AND         p11.id_transport IN (-4,-5)'#13#10 +
             '        AND         p11.id_transport_det IS NOT NULL'#13#10 +
             '        UNION ALL'#13#10 +
             '        SELECT      p22.id'#13#10 +
             '        FROM        transmisja tr'#13#10 +
             '        JOIN        port  p2 ON p2.id = tr.id_port_2'#13#10 +
             '        JOIN        port p22 ON p22.id_wezel = p2.id_wezel'#13#10 +
             '        WHERE       tr.id = 12193'#13#10 +
             '        AND         p22.id_transport IN (-4,-5)'#13#10 +
             '        AND         p22.id_transport_det IS NOT NULL'#13#10 +
             ') '#13#10 +
             'UNION ALL'#13#10 +
             'SELECT td.nazwa, trs.id_transmisja'#13#10 +
             'FROM   transmisja_skladniki trs '#13#10 +
             'JOIN   transmisja_przebieg trp ON trp.id_transmisja = trs.id_transmisja'#13#10 +
             'JOIN   PORT P ON P.ID = trp.id_port_2'#13#10 +
             'JOIN   transport_det td ON td.id = p.id_transport_det'#13#10 +
             'WHERE  trs.id_transmisja_skl = 12193'#13#10 +
             'AND    trp.id_port_1 IN ('#13#10 +
             '        SELECT      p11.id'#13#10 +
             '        FROM        transmisja tr'#13#10 +
             '        JOIN        port  p1 ON p1.id = tr.id_port_1'#13#10 +
             '        JOIN        port p11 ON p11.id_wezel = p1.id_wezel'#13#10 +
             '        WHERE       tr.id = 12193'#13#10 +
             '        AND         p11.id_transport IN (-4,-5)'#13#10 +
             '        AND         p11.id_transport_det IS NOT NULL'#13#10 +
             '        UNION ALL'#13#10 +
             '        SELECT      p22.id'#13#10 +
             '        FROM        transmisja tr'#13#10 +
             '        JOIN        port  p2 ON p2.id = tr.id_port_2'#13#10 +
             '        JOIN        port p22 ON p22.id_wezel = p2.id_wezel'#13#10 +
             '        WHERE       tr.id = 12193'#13#10 +
             '        AND         p22.id_transport IN (-4,-5)'#13#10 +
             '        AND         p22.id_transport_det IS NOT NULL'#13#10 +
             ') '#13#10 +
             ';' );
  TestQuery( 'SELECT ID_SWIATLOWOD, NR_WLOKNO, NR'#13#10 +
             'FROM ('#13#10 +
             'SELECT V.ID_SWIATLOWOD, V.NR_WLOKNO, NR.NR'#13#10 +
             'FROM   WLOKNO_WKV V '#13#10 +
             'JOIN   NR ON NR.NR BETWEEN V.LP1 AND V.LP2 OR NR.NR BETWEEN V.LP2 AND V.LP1'#13#10 +
             'MINUS'#13#10 +
             'SELECT ID_SWIATLOWOD, NR_WLOKNO, NR'#13#10 +
             'FROM  ('#13#10 +
             'SELECT V.ID_SWIATLOWOD, V.NR_WLOKNO, V.LP1 NR'#13#10 +
             'FROM   WLOKNO_WKV V '#13#10 +
             'UNION'#13#10 +
             'SELECT V.ID_SWIATLOWOD, V.NR_WLOKNO, V.LP2 NR'#13#10 +
             'FROM   WLOKNO_WKV V '#13#10 +
             ') TAB'#13#10 +
             'GROUP BY ID_SWIATLOWOD, NR_WLOKNO, NR'#13#10 +
             'HAVING COUNT(*) = 2'#13#10 +
             ') TAB'#13#10 +
             'GROUP BY ID_SWIATLOWOD, NR_WLOKNO, NR'#13#10 +
             'HAVING COUNT(*) != 1'#13#10 +
             ';' );
  TestQuery( 'select tab.*, replace(''UPDATE STUDNIA SET X=''||x_prime||'', Y=''||y_prime||'' WHERE NR=''''''||tab.nr||'''''';'', '','',''.'')'#13#10 +
             'from ('#13#10 +
             'select nr, max(x) x, max(y) y, max(x_prime) x_prime, max(y_prime) y_prime'#13#10 +
             'from ('#13#10 +
             ''#13#10 +
             'select  st.nr, st.x, st.y, null as x_prime, null as y_prime from studnia st where nr like ''£ódS%'''#13#10 +
             'union all'#13#10 +
             'select  t.name, null, null,'#13#10 +
             '        CASE WHEN COORD_X_DETAILS BETWEEN    1 AND    9 THEN COORD_X_DETAILS / 10'#13#10 +
             '             WHEN COORD_X_DETAILS BETWEEN   10 AND   99 THEN COORD_X_DETAILS / 100'#13#10 +
             '             WHEN COORD_X_DETAILS BETWEEN  100 AND  999 THEN COORD_X_DETAILS / 1000'#13#10 +
             '             WHEN COORD_X_DETAILS BETWEEN 1000 AND 9999 THEN COORD_X_DETAILS / 10000'#13#10 +
             '             ELSE 0'#13#10 +
             '        END + t.coord_x x,'#13#10 +
             '        CASE WHEN COORD_y_DETAILS BETWEEN    1 AND    9 THEN COORD_y_DETAILS / 10'#13#10 +
             '             WHEN COORD_y_DETAILS BETWEEN   10 AND   99 THEN COORD_y_DETAILS / 100'#13#10 +
             '             WHEN COORD_y_DETAILS BETWEEN  100 AND  999 THEN COORD_y_DETAILS / 1000'#13#10 +
             '             WHEN COORD_y_DETAILS BETWEEN 1000 AND 9999 THEN COORD_y_DETAILS / 10000'#13#10 +
             '             ELSE 0'#13#10 +
             '        END + t.coord_y y'#13#10 +
             'from v_tdr_char_points t where t.name like ''£ódS%'''#13#10 +
             ''#13#10 +
             ')  tab'#13#10 +
             'group by nr'#13#10 +
             ') tab'#13#10 +
             'where ( abs(DDDMMSS_2_DDD(x) - DDDMMSS_2_DDD(x_prime)) > 1/3600 or abs(DDDMMSS_2_DDD(y) - DDDMMSS_2_DDD(y_prime)) > 1/3600 );'#13#10 +
             ';' );
  TestQuery( 'SELECT   WK.*'#13#10 +
             '     ,   L.OZNACZENIE || ''; '' || L.ADRES || '', '' || L.MIASTO               AS   WEZEL'#13#10 +
             '     , ( '#13#10 +
             '         SELECT   MAX(SW.NR)                                               AS NR'#13#10 +
             '           FROM   SWIATLOWOD  SW'#13#10 +
             '          WHERE   SW.ID = WK.ID_ELEMENT'#13#10 +
             '            AND   WK.ID_TYP_ELEMENTU = 101'#13#10 +
             '         '#13#10 +
             '         UNION ALL'#13#10 +
             '         '#13#10 +
             '         SELECT   MAX(CU.NR)                                               AS NR'#13#10 +
             '           FROM   KABEL_CU  CU'#13#10 +
             '          WHERE   CU.ID = WK.ID_ELEMENT'#13#10 +
             '            AND   WK.ID_TYP_ELEMENTU = 111'#13#10 +
             '         '#13#10 +
             '         UNION ALL'#13#10 +
             '         '#13#10 +
             '         SELECT   MAX(INF.NR)                                              AS NR'#13#10 +
             '           FROM   KABEL_CU  INF'#13#10 +
             '          WHERE   INF.ID = WK.ID_ELEMENT'#13#10 +
             '            AND   WK.ID_TYP_ELEMENTU = 135'#13#10 +
             '         '#13#10 +
             '         UNION ALL'#13#10 +
             '         '#13#10 +
             '         SELECT   MAX(KON.NR)                                              AS NR'#13#10 +
             '           FROM   KABEL_KONCENTRYCZNY  KON'#13#10 +
             '          WHERE   KON.ID = WK.ID_ELEMENT'#13#10 +
             '            AND   WK.ID_TYP_ELEMENTU = 141'#13#10 +
             '       )                                                                   AS      NR'#13#10 +
             '     , ( '#13#10 +
             '         SELECT   MAX(L.OZNACZENIE || ''; '' || L.ADRES || '', '' || L.MIASTO)'#13#10 +
             '           FROM   WEZEL_KABEL  WK1'#13#10 +
             '           JOIN   WEZEL_ADRES_V  L ON L.ID = WK1.ID_WEZEL'#13#10 +
             '          WHERE   WK1.ID_TYP_ELEMENTU = WK.ID_TYP_ELEMENTU'#13#10 +
             '            AND   WK1.ID_ELEMENT = WK.ID_ELEMENT'#13#10 +
             '            AND   WK1.LP = WK.LP - 1'#13#10 +
             '       )                                                                   AS WEZEL_1'#13#10 +
             '     , ( '#13#10 +
             '         SELECT   MAX(L.OZNACZENIE || ''; '' || L.ADRES || '', '' || L.MIASTO)'#13#10 +
             '           FROM   WEZEL_KABEL  WK2'#13#10 +
             '           JOIN   WEZEL_ADRES_V  L ON L.ID = WK2.ID_WEZEL'#13#10 +
             '          WHERE   WK2.ID_TYP_ELEMENTU = WK.ID_TYP_ELEMENTU'#13#10 +
             '            AND   WK2.ID_ELEMENT = WK.ID_ELEMENT'#13#10 +
             '            AND   WK2.LP = WK.LP + 1'#13#10 +
             '       )                                                                   AS WEZEL_2'#13#10 +
             '  FROM   WEZEL_KABEL   WK'#13#10 +
             '     ,   WEZEL_ADRES_V  L'#13#10 +
             ' WHERE   WK.ID = 93'#13#10 +
             '   AND   WK.ID_WEZEL = L.ID ;' );
  TestQuery( 'CREATE OR REPLACE VIEW USLUGI_V AS'#13#10 +
             ''#13#10 +
             '     SELECT   U.ID'#13#10 +
             '          ,   U.ID_DOSTAWCA'#13#10 +
             '          ,   U.ID_HANDLOWIEC'#13#10 +
             '          ,   U.ID_HD'#13#10 +
             '          ,   U.ID_KIEROWNIK'#13#10 +
             '          ,   U.ID_ODPOWIEDZIALNY'#13#10 +
             '          ,   U.ID_OPIEKUN'#13#10 +
             '          ,   U.ID_PARENT'#13#10 +
             '          ,   U.ID_PRODUKT'#13#10 +
             '          ,   U.ID_RODZAJ_USLUGI'#13#10 +
             '          ,   U.ID_STATUS_USLUGI'#13#10 +
             '          ,   U.ID_TYP_USLUGI'#13#10 +
             '          ,   U.ID_URZADZENIE_1'#13#10 +
             '          ,   U.ID_URZADZENIE_2'#13#10 +
             '          ,   U.ID_WLASCICIEL'#13#10 +
             '          ,   U.ID_W_HD'#13#10 +
             '          ,   U.B_BGP'#13#10 +
             '          ,   U.B_KONCOWA'#13#10 +
             '          ,   U.B_OBCA'#13#10 +
             '          ,   U.B_POMIAR_RUCHU'#13#10 +
             '          ,   U.B_ZGODNY'#13#10 +
             '          ,   U.DATA_URUCHOMIENIA'#13#10 +
             '          ,   U.DATA_WPISU'#13#10 +
             '          ,   U.DATA_ZAKONCZENIA'#13#10 +
             '          ,   U.BGP'#13#10 +
             '          ,   U.NAZWA'#13#10 +
             '          ,   U.NAZWA_PLIKU_STREF'#13#10 +
             '          ,   U.NR'#13#10 +
             '          ,   U.NR_TRANSAKCJI'#13#10 +
             '          ,   U.NR_UMOWY'#13#10 +
             '          ,   U.NR_USLUGI_HANDLOWEJ'#13#10 +
             '          ,   U.NR_USLUGI_OBCY'#13#10 +
             '          ,   U.NR_ZLECENIA'#13#10 +
             '          ,   U.OKRES_WYPOW'#13#10 +
             '          ,   U.UWAGI'#13#10 +
             '          ,   U.WARTOSC_AMORTYZACJI'#13#10 +
             '          ,   U.WARTOSC_KSIEGOWA'#13#10 +
             '          ,   U.WARTOSC_ODTWORZENIOWA'#13#10 +
             '          ,   U.TYP_USLUGI'#13#10 +
             '          ,   U.STATUS_USLUGI'#13#10 +
             '          ,   U.WLASCICIEL'#13#10 +
             '          ,   U.HANDLOWIEC'#13#10 +
             '          ,   U.KIEROWNIK'#13#10 +
             '          ,   U.OPIEKUN'#13#10 +
             '          ,   U.PRODUKT'#13#10 +
             '          ,   U.ODPOWIEDZIALNY'#13#10 +
             '          ,   U.PARENT'#13#10 +
             '       FROM ( '#13#10 +
             '                   SELECT   DISTINCT UTR.ID_USLUGA AS ID'#13#10 +
             '                     FROM ( '#13#10 +
             '                                 SELECT   ID'#13#10 +
             '                                   FROM   TRANSMISJA  T'#13#10 +
             '                                  WHERE   T.ID_PORT_1 IN '#13#10 +
             '                                        ( '#13#10 +
             '                                               SELECT   ID_PORT'#13#10 +
             '                                                 FROM   TMP_PORTY'#13#10 +
             '                                        ) '#13#10 +
             '                            '#13#10 +
             '                              UNION ALL   '#13#10 +
             '                            '#13#10 +
             '                                 SELECT   ID'#13#10 +
             '                                   FROM   TRANSMISJA  T'#13#10 +
             '                                  WHERE   T.ID_PORT_2 IN '#13#10 +
             '                                        ( '#13#10 +
             '                                               SELECT   ID_PORT'#13#10 +
             '                                                 FROM   TMP_PORTY'#13#10 +
             '                                        ) '#13#10 +
             '                            '#13#10 +
             '                              UNION ALL   '#13#10 +
             '                            '#13#10 +
             '                                 SELECT   TRS.ID_TRANSMISJA AS ID'#13#10 +
             '                                   FROM   TRANSMISJA_SKLADNIKI  TRS'#13#10 +
             '                             CONNECT BY   NOCYCLE TRS.ID_TRANSMISJA_SKL = PRIOR TRS.ID_TRANSMISJA'#13#10 +
             '                             START WITH   TRS.ID IN '#13#10 +
             '                                        ( '#13#10 +
             '                                               SELECT   TRS.ID'#13#10 +
             '                                                 FROM   TMP_PORTY             TMP'#13#10 +
             '                                                 JOIN   TRANSMISJA              T ON   T.ID_PORT_1         = TMP.ID_PORT'#13#10 +
             '                                                 JOIN   TRANSMISJA_SKLADNIKI  TRS ON TRS.ID_TRANSMISJA_SKL =   T.ID      '#13#10 +
             '                                          '#13#10 +
             '                                            UNION ALL   '#13#10 +
             '                                          '#13#10 +
             '                                               SELECT   TRS.ID'#13#10 +
             '                                                 FROM   TMP_PORTY             TMP'#13#10 +
             '                                                 JOIN   TRANSMISJA              T ON   T.ID_PORT_2         = TMP.ID_PORT'#13#10 +
             '                                                 JOIN   TRANSMISJA_SKLADNIKI  TRS ON TRS.ID_TRANSMISJA_SKL =   T.ID      '#13#10 +
             '                                          '#13#10 +
             '                                            UNION ALL   '#13#10 +
             '                                          '#13#10 +
             '                                               SELECT   TRS.ID'#13#10 +
             '                                                 FROM   TMP_PORTY             TMP'#13#10 +
             '                                                 JOIN   TRANSLACJE_SDH          T ON   T.ID_PORT_1         = TMP.ID_PORT'#13#10 +
             '                                                 JOIN   TRANSMISJA_SKLADNIKI  TRS ON TRS.ID_TRANSLACJE_SDH =   T.ID      '#13#10 +
             '                                          '#13#10 +
             '                                            UNION ALL   '#13#10 +
             '                                          '#13#10 +
             '                                               SELECT   TRS.ID'#13#10 +
             '                                                 FROM   TMP_PORTY             TMP'#13#10 +
             '                                                 JOIN   TRANSLACJE_SDH          T ON   T.ID_PORT_2         = TMP.ID_PORT'#13#10 +
             '                                                 JOIN   TRANSMISJA_SKLADNIKI  TRS ON TRS.ID_TRANSLACJE_SDH =   T.ID      '#13#10 +
             '                                          '#13#10 +
             '                                            UNION ALL   '#13#10 +
             '                                          '#13#10 +
             '                                               SELECT   TRS.ID'#13#10 +
             '                                                 FROM   TMP_PORTY             TMP'#13#10 +
             '                                                 JOIN   KABEL_INF               T ON   T.ID_PORT_1    = TMP.ID_PORT'#13#10 +
             '                                                 JOIN   TRANSMISJA_SKLADNIKI  TRS ON TRS.ID_KABEL_INF =   T.ID      '#13#10 +
             '                                          '#13#10 +
             '                                            UNION ALL   '#13#10 +
             '                                          '#13#10 +
             '                                               SELECT   TRS.ID'#13#10 +
             '                                                 FROM   TMP_PORTY             TMP'#13#10 +
             '                                                 JOIN   KABEL_INF               T ON   T.ID_PORT_2    = TMP.ID_PORT'#13#10 +
             '                                                 JOIN   TRANSMISJA_SKLADNIKI  TRS ON TRS.ID_KABEL_INF =   T.ID      '#13#10 +
             '                                          '#13#10 +
             '                                            UNION ALL   '#13#10 +
             '                                          '#13#10 +
             '                                               SELECT   TRS.ID'#13#10 +
             '                                                 FROM   TMP_GNIAZDA           TMP'#13#10 +
             '                                                 JOIN   TRASA_SW_POL            T ON   T.ID_GNIAZDO_1      = TMP.ID_GNIAZDO'#13#10 +
             '                                                 JOIN   TRANSMISJA_SKLADNIKI  TRS ON TRS.ID_TRASA_SW_POL_1 =   T.ID         '#13#10 +
             '                                          '#13#10 +
             '                                            UNION ALL   '#13#10 +
             '                                          '#13#10 +
             '                                               SELECT   TRS.ID'#13#10 +
             '                                                 FROM   TMP_GNIAZDA           TMP'#13#10 +
             '                                                 JOIN   TRASA_SW_POL            T ON   T.ID_GNIAZDO_2      = TMP.ID_GNIAZDO'#13#10 +
             '                                                 JOIN   TRANSMISJA_SKLADNIKI  TRS ON TRS.ID_TRASA_SW_POL_1 =   T.ID         '#13#10 +
             '                                          '#13#10 +
             '                                            UNION ALL   '#13#10 +
             '                                          '#13#10 +
             '                                               SELECT   TRS.ID'#13#10 +
             '                                                 FROM   TMP_GNIAZDA           TMP'#13#10 +
             '                                                 JOIN   TRASA_SW_POL            T ON   T.ID_GNIAZDO_1      = TMP.ID_GNIAZDO'#13#10 +
             '                                                 JOIN   TRANSMISJA_SKLADNIKI  TRS ON TRS.ID_TRASA_SW_POL_2 =   T.ID         '#13#10 +
             '                                          '#13#10 +
             '                                            UNION ALL   '#13#10 +
             '                                          '#13#10 +
             '                                               SELECT   TRS.ID'#13#10 +
             '                                                 FROM   TMP_GNIAZDA           TMP'#13#10 +
             '                                                 JOIN   TRASA_SW_POL            T ON   T.ID_GNIAZDO_2      = TMP.ID_GNIAZDO'#13#10 +
             '                                                 JOIN   TRANSMISJA_SKLADNIKI  TRS ON TRS.ID_TRASA_SW_POL_2 =   T.ID         '#13#10 +
             '                                          '#13#10 +
             '                                            UNION ALL   '#13#10 +
             '                                          '#13#10 +
             '                                               SELECT   TRS.ID'#13#10 +
             '                                                 FROM   TMP_GNIAZDA           TMP'#13#10 +
             '                                                 JOIN   TRASA_SW                T ON   T.ID_GNIAZDO_1  = TMP.ID_GNIAZDO'#13#10 +
             '                                                 JOIN   TRANSMISJA_SKLADNIKI  TRS ON TRS.ID_TRASA_SW_1 =   T.ID         '#13#10 +
             '                                          '#13#10 +
             '                                            UNION ALL   '#13#10 +
             '                                          '#13#10 +
             '                                               SELECT   TRS.ID'#13#10 +
             '                                                 FROM   TMP_GNIAZDA           TMP'#13#10 +
             '                                                 JOIN   TRASA_SW                T ON   T.ID_GNIAZDO_2  = TMP.ID_GNIAZDO'#13#10 +
             '                                                 JOIN   TRANSMISJA_SKLADNIKI  TRS ON TRS.ID_TRASA_SW_1 =   T.ID         '#13#10 +
             '                                          '#13#10 +
             '                                            UNION ALL   '#13#10 +
             '                                          '#13#10 +
             '                                               SELECT   TRS.ID'#13#10 +
             '                                                 FROM   TMP_GNIAZDA           TMP'#13#10 +
             '                                                 JOIN   TRASA_SW_ODC            T ON   T.ID_GNIAZDO_1  = TMP.ID_GNIAZDO  '#13#10 +
             '                                                 JOIN   TRANSMISJA_SKLADNIKI  TRS ON TRS.ID_TRASA_SW_1 =   T.ID_TRASA_SW'#13#10 +
             '                                          '#13#10 +
             '                                            UNION ALL   '#13#10 +
             '                                          '#13#10 +
             '                                               SELECT   TRS.ID'#13#10 +
             '                                                 FROM   TMP_GNIAZDA           TMP'#13#10 +
             '                                                 JOIN   TRASA_SW_ODC            T ON   T.ID_GNIAZDO_2  = TMP.ID_GNIAZDO  '#13#10 +
             '                                                 JOIN   TRANSMISJA_SKLADNIKI  TRS ON TRS.ID_TRASA_SW_1 =   T.ID_TRASA_SW'#13#10 +
             '                                          '#13#10 +
             '                                            UNION ALL   '#13#10 +
             '                                          '#13#10 +
             '                                               SELECT   TRS.ID'#13#10 +
             '                                                 FROM   TMP_GNIAZDA           TMP'#13#10 +
             '                                                 JOIN   TRASA_SW                T ON   T.ID_GNIAZDO_1  = TMP.ID_GNIAZDO'#13#10 +
             '                                                 JOIN   TRANSMISJA_SKLADNIKI  TRS ON TRS.ID_TRASA_SW_2 =   T.ID         '#13#10 +
             '                                          '#13#10 +
             '                                            UNION ALL   '#13#10 +
             '                                          '#13#10 +
             '                                               SELECT   TRS.ID'#13#10 +
             '                                                 FROM   TMP_GNIAZDA           TMP'#13#10 +
             '                                                 JOIN   TRASA_SW                T ON   T.ID_GNIAZDO_2  = TMP.ID_GNIAZDO'#13#10 +
             '                                                 JOIN   TRANSMISJA_SKLADNIKI  TRS ON TRS.ID_TRASA_SW_2 =   T.ID         '#13#10 +
             '                                          '#13#10 +
             '                                            UNION ALL   '#13#10 +
             '                                          '#13#10 +
             '                                               SELECT   TRS.ID'#13#10 +
             '                                                 FROM   TMP_GNIAZDA           TMP'#13#10 +
             '                                                 JOIN   TRASA_SW_ODC            T ON   T.ID_GNIAZDO_1  = TMP.ID_GNIAZDO  '#13#10 +
             '                                                 JOIN   TRANSMISJA_SKLADNIKI  TRS ON TRS.ID_TRASA_SW_2 =   T.ID_TRASA_SW'#13#10 +
             '                                          '#13#10 +
             '                                            UNION ALL   '#13#10 +
             '                                          '#13#10 +
             '                                               SELECT   TRS.ID'#13#10 +
             '                                                 FROM   TMP_GNIAZDA           TMP'#13#10 +
             '                                                 JOIN   TRASA_SW_ODC            T ON   T.ID_GNIAZDO_2  = TMP.ID_GNIAZDO  '#13#10 +
             '                                                 JOIN   TRANSMISJA_SKLADNIKI  TRS ON TRS.ID_TRASA_SW_2 =   T.ID_TRASA_SW'#13#10 +
             '                                          '#13#10 +
             '                                            UNION ALL   '#13#10 +
             '                                          '#13#10 +
             '                                               SELECT   TRS.ID'#13#10 +
             '                                                 FROM   TMP_GNIAZDA           TMP'#13#10 +
             '                                                 JOIN   WLOKNO                  T ON   T.ID_GNIAZDO_1 = TMP.ID_GNIAZDO'#13#10 +
             '                                                 JOIN   TRANSMISJA_SKLADNIKI  TRS ON TRS.ID_WLOKNO_1  =   T.ID         '#13#10 +
             '                                          '#13#10 +
             '                                            UNION ALL   '#13#10 +
             '                                          '#13#10 +
             '                                               SELECT   TRS.ID'#13#10 +
             '                                                 FROM   TMP_GNIAZDA           TMP'#13#10 +
             '                                                 JOIN   WLOKNO                  T ON   T.ID_GNIAZDO_2 = TMP.ID_GNIAZDO'#13#10 +
             '                                                 JOIN   TRANSMISJA_SKLADNIKI  TRS ON TRS.ID_WLOKNO_1  =   T.ID         '#13#10 +
             '                                          '#13#10 +
             '                                            UNION ALL   '#13#10 +
             '                                          '#13#10 +
             '                                               SELECT   TRS.ID'#13#10 +
             '                                                 FROM   TMP_TRANSLACJE        TMP'#13#10 +
             '                                                 JOIN   TRANSMISJA_SKLADNIKI  TRS ON TRS.ID_TRANSLACJE_SDH = TMP.ID_TRANSLACJA'#13#10 +
             '                                          '#13#10 +
             '                                          '#13#10 +
             '                                          '#13#10 +
             '                                          '#13#10 +
             '                                          '#13#10 +
             '                                          '#13#10 +
             '                                          '#13#10 +
             '                                          '#13#10 +
             '                                          '#13#10 +
             '                                          '#13#10 +
             '                                          '#13#10 +
             '                                          '#13#10 +
             '                                          '#13#10 +
             '                                          '#13#10 +
             '                                          '#13#10 +
             '                                          '#13#10 +
             '                                          '#13#10 +
             '                                          '#13#10 +
             '                                          '#13#10 +
             '                                          '#13#10 +
             '                                        ) '#13#10 +
             '                            '#13#10 +
             '                            '#13#10 +
             '                          ) UU'#13#10 +
             '                     JOIN   USLUGA_TRANSMISJA  UTR ON UTR.ID_TRANSMISJA = UU.ID'#13#10 +
             '              '#13#10 +
             '                    UNION   '#13#10 +
             '              '#13#10 +
             '                   SELECT   UTR.ID_USLUGA                      AS ID'#13#10 +
             '                     FROM   USLUGA_TRASA  UTR'#13#10 +
             '                    WHERE   UTR.ID_TRASA_SW IN '#13#10 +
             '                          ( '#13#10 +
             '                                 SELECT   T.ID'#13#10 +
             '                                   FROM   TMP_GNIAZDA  TMP'#13#10 +
             '                                   JOIN   TRASA_SW       T ON T.ID_GNIAZDO_1 = TMP.ID_GNIAZDO'#13#10 +
             '                            '#13#10 +
             '                              UNION ALL   '#13#10 +
             '                            '#13#10 +
             '                                 SELECT   T.ID'#13#10 +
             '                                   FROM   TMP_GNIAZDA  TMP'#13#10 +
             '                                   JOIN   TRASA_SW       T ON T.ID_GNIAZDO_2 = TMP.ID_GNIAZDO'#13#10 +
             '                            '#13#10 +
             '                              UNION ALL   '#13#10 +
             '                            '#13#10 +
             '                                 SELECT   T.ID_TRASA_SW'#13#10 +
             '                                   FROM   TMP_GNIAZDA  TMP'#13#10 +
             '                                   JOIN   TRASA_SW_ODC   T ON T.ID_GNIAZDO_1 = TMP.ID_GNIAZDO'#13#10 +
             '                            '#13#10 +
             '                              UNION ALL   '#13#10 +
             '                            '#13#10 +
             '                                 SELECT   T.ID_TRASA_SW'#13#10 +
             '                                   FROM   TMP_GNIAZDA  TMP'#13#10 +
             '                                   JOIN   TRASA_SW_ODC   T ON T.ID_GNIAZDO_2 = TMP.ID_GNIAZDO'#13#10 +
             '                            '#13#10 +
             '                            '#13#10 +
             '                            '#13#10 +
             '                          ) '#13#10 +
             '              '#13#10 +
             '                    UNION   '#13#10 +
             '              '#13#10 +
             '                   SELECT   UKI.ID_USLUGA                      AS ID'#13#10 +
             '                     FROM   KABEL_INF        KINF'#13#10 +
             '                     JOIN   USLUGA_KABEL_INF  UKI ON UKI.ID_KABEL_INF = KINF.ID'#13#10 +
             '                    WHERE   KINF.ID_GNIAZDO_1 IN '#13#10 +
             '                          ( '#13#10 +
             '                                 SELECT   ID_GNIAZDO'#13#10 +
             '                                   FROM   TMP_GNIAZDA'#13#10 +
             '                          ) '#13#10 +
             '                       OR   KINF.ID_GNIAZDO_2 IN '#13#10 +
             '                          ( '#13#10 +
             '                                 SELECT   ID_GNIAZDO'#13#10 +
             '                                   FROM   TMP_GNIAZDA'#13#10 +
             '                          ) '#13#10 +
             '                       OR   KINF.ID_PORT_1 IN '#13#10 +
             '                          ( '#13#10 +
             '                                 SELECT   ID_PORT'#13#10 +
             '                                   FROM   TMP_PORTY'#13#10 +
             '                          ) '#13#10 +
             '                       OR   KINF.ID_PORT_1 IN '#13#10 +
             '                          ( '#13#10 +
             '                                 SELECT   ID_PORT'#13#10 +
             '                                   FROM   TMP_PORTY'#13#10 +
             '                          ) '#13#10 +
             '              '#13#10 +
             '                    UNION   '#13#10 +
             '              '#13#10 +
             '                   SELECT   UURZ.ID_USLUGA                     AS ID'#13#10 +
             '                     FROM   USLUGA_URZADZENIE  UURZ'#13#10 +
             '                    WHERE   UURZ.ID_URZADZENIE IN '#13#10 +
             '                          ( '#13#10 +
             '                                 SELECT   ID_URZADZENIE'#13#10 +
             '                                   FROM   TMP_URZADZENIA'#13#10 +
             '                          ) '#13#10 +
             '              '#13#10 +
             '              '#13#10 +
             '              '#13#10 +
             '            ) UU'#13#10 +
             '       JOIN   USLUGA_EXT  U ON U.ID = UU.ID'#13#10 +
             ''#13#10 +
             '   ORDER BY   U.NR' );

  { date: 2014-05-13, file: parse EXPR subquery.sql -- too much brackets removed }
  TestQuery( '         SELECT   ('#13#10 +
             '         SELECT  1'#13#10 +
             '           FROM  SKRZYNKA SK'#13#10 +
             '           )'#13#10 +
             '           FROM  SZAFA SZ;' );

  { date: 2014-05-14, file: parse COND escalation.sql }
  TestQuery( '   SELECT   *'#13#10 +
             '     FROM   tab'#13#10 +
             '    WHERE ( a=a OR b=b )'#13#10 +
             '      AND   c=c' );

  { date: 2014-05-27, file: parse EXPR escalation.sql }
  TestQuery( 'SELECT     (1*2)/(3*4) alias1,'#13#10 +
             '    ((a - b)*c)/ (d*e) alias2'#13#10 +
             '  from index_stats;' );
  TestQuery( 'select name  NAME,'#13#10 +
             '    (br_rows_len*100)/(br_blk_len*br_blks) BRANCH_UTILIZATION,'#13#10 +
             '    ((lf_rows_len - del_lf_rows_len)*100)/ (lf_blk_len*lf_blks) LEAF_UTILIZATI,'#13#10 +
             '    decode (sign(ceil(log(br_blk_len/(br_rows_len/br_rows),'#13#10 +
             '            lf_blk_len/(lf_rows_len - del_lf_rows_len)/(lf_rows - del_lf_rows))'#13#10 +
             '           +1 - height)), -1,''YES'',''NO'') CAN_REDUCE_LEVEL'#13#10 +
             '  from index_stats;' );

  TestQuery( ' SELECT   USL.*'#13#10 +
             '   FROM   USLUGA_EXT  USL'#13#10 +
             '  WHERE   USL.ID IN '#13#10 +
             '        ( '#13#10 +
             '           SELECT   UTR.ID_USLUGA'#13#10 +
             '             FROM   USLUGA_TRANSMISJA  UTR'#13#10 +
             '        ) ;' );
//  TestQuery( '   SELECT   *'#13#10 +
//             '     FROM   AUDITING  '#13#10 +
//             '    WHERE   '#13#10 +
//             '          ( COLUMN_NAME_1 NOT LIKE ''ID\_%'' ESCAPE ''\''   AND   COLUMN_NAME_2 != ''ID''     OR  COLUMN_NAME_3 IS NULL )'#13#10 +
//             '       OR   :CheckBoxWidokUpr.Value = 0'#13#10 +
//             ' ORDER BY   ID DESC'#13#10 +
//             '        ,   COLUMN_NAME;' );

  { date: 2014-06-06, file: parse no WHERE in deepest query after format.sql }
  TestQuery( '-- Cost = 72   IO = 69   CPU = 39.877.731'#13#10 +
             '--                       CPU = 25.550.000 -- bez SMU1 i SMU2'#13#10 +
             '-- Cost = 33   IO = 33   CPU =    403.013 -- SMU top            -- 0.156 sek'#13#10 +
             '-- Cost = 10   IO =  9   CPU = 14.440.358 -- bez WKV (LOW)      -- 0.062 sek'#13#10 +
             '-- Cost = 45   IO = 41   CPU = 52.211.246 '#13#10 +
             '-- Cost =  3   IO =  3   CPU =    120.807 -- FK 1:1 JOIN -> subq cols           REAL IO = 22, CPU:    273.574 ???'#13#10 +
             '-- Cost =  3   IO =  3   CPU =    120.807 -- WEZEL_SW_12 AS FIRST               REAL IO = 32, CPU: 28.809.286 ???'#13#10 +
             '-- Cost =  3   IO =  3   CPU =    120.807 -- WEZEL_SW_12 AS MIN                 REAL IO = 32, CPU:    395.633 ???'#13#10 +
             'SELECT *'#13#10 +
             'FROM ('#13#10 +
             'SELECT'#13#10 +
             '  S.*, '#13#10 +
             '  ( SELECT WA1.OZNACZENIE || NVL2(WA1.ADRES, '', ''||WA1.ADRES, '''') FROM WEZEL_ADRES_V WA1 WHERE WA1.ID = S.ID_WEZEL_1) AS ADRES1,'#13#10 +
             '  ( SELECT WA2.OZNACZENIE || NVL2(WA2.ADRES, '', ''||WA2.ADRES, '''') FROM WEZEL_ADRES_V WA2 WHERE WA2.ID = S.ID_WEZEL_2) AS ADRES2,'#13#10 +
             '  ( SELECT WW1.ELEMENT FROM WEZEL_V WW1 WHERE WW1.ID = S.ID_WEZEL_SW_1) AS ELEMENT1,'#13#10 +
             '  ( SELECT WW2.ELEMENT FROM WEZEL_V WW2 WHERE WW2.ID = S.ID_WEZEL_SW_2) AS ELEMENT2,'#13#10 +
             '  ( SELECT NAZWA  FROM TYP_SWIATLOWODU T WHERE T.ID = S.ID_TYP_SWIATLOWODU ) AS TYP,'#13#10 +
             '  ( SELECT LICZBA FROM TYP_SWIATLOWODU T WHERE T.ID = S.ID_TYP_SWIATLOWODU ) AS LICZBA,'#13#10 +
             '  ( SELECT NAZWA  FROM STATUS ST WHERE ST.ID = S.ID_STATUS ) AS STATUS'#13#10 +
             ''#13#10 +
             'FROM ('#13#10 +
             '          SELECT S.*,'#13#10 +
             '               ( SELECT   MIN(G.ID_WEZEL) --KEEP (DENSE_RANK FIRST ORDER BY COUNT(*) DESC NULLS LAST)'#13#10 +
             '                 FROM     WLOKNO WL'#13#10 +
             '                 JOIN     GNIAZDO G ON G.ID = WL.ID_GNIAZDO_1'#13#10 +
             '                 WHERE    WL.ID_SWIATLOWOD = S.ID'#13#10 +
             '                    AND   WL.ID_WEZEL_KABEL_1 = S.ID_WEZEL_KABEL_1'#13#10 +
             '                 /*GROUP BY G.ID_WEZEL*/) AS ID_WEZEL_SW_1,'#13#10 +
             '               ( SELECT   MIN(G.ID_WEZEL) --KEEP (DENSE_RANK FIRST ORDER BY COUNT(*) DESC NULLS LAST)'#13#10 +
             '                 FROM     WLOKNO WL'#13#10 +
             '                 JOIN     GNIAZDO G ON G.ID = WL.ID_GNIAZDO_2'#13#10 +
             '                 WHERE    WL.ID_SWIATLOWOD = S.ID'#13#10 +
             '                    AND   WL.ID_WEZEL_KABEL_2 = S.ID_WEZEL_KABEL_2'#13#10 +
             '                 /*GROUP BY G.ID_WEZEL*/) AS ID_WEZEL_SW_2'#13#10 +
             ''#13#10 +
             '          FROM ('#13#10 +
             '                    SELECT S.*,'#13#10 +
             '                         ( SELECT ID_WEZEL FROM WEZEL_KABEL WK1 WHERE WK1.ID_TYP_ELEMENTU = 101 AND WK1.ID_ELEMENT = S.ID AND WK1.LP = S.MIN_LP ) AS ID_WEZEL_1,'#13#10 +
             '                         ( SELECT ID_WEZEL FROM WEZEL_KABEL WK2 WHERE WK2.ID_TYP_ELEMENTU = 101 AND WK2.ID_ELEMENT = S.ID AND WK2.LP = S.MIN_LP ) AS ID_WEZEL_2,'#13#10 +
             '                         ( SELECT ID       FROM WEZEL_KABEL WK1 WHERE WK1.ID_TYP_ELEMENTU = 101 AND WK1.ID_ELEMENT = S.ID AND WK1.LP = S.MIN_LP ) AS ID_WEZEL_KABEL_1,'#13#10 +
             '                         ( SELECT ID       FROM WEZEL_KABEL WK2 WHERE WK2.ID_TYP_ELEMENTU = 101 AND WK2.ID_ELEMENT = S.ID AND WK2.LP = S.MIN_LP ) AS ID_WEZEL_KABEL_2'#13#10 +
             '                    FROM ( '#13#10 +
             '                            /* subquery na SW, bo nie mo¿na u¿yæ subquery w JOIN na WEZEL_KABEL */'#13#10 +
             '                            SELECT SW.ID, SW.NR, SW.DLUGOSC_OPTYCZNA, SW.DLUGOSC_TRASOWA, SW.ID_TYP_SWIATLOWODU, SW.ID_STATUS,'#13#10 +
             '                                   ( SELECT MIN(LP) FROM WEZEL_KABEL WKM WHERE WKM.ID_TYP_ELEMENTU = 101 AND WKM.ID_ELEMENT = SW.ID ) AS MIN_LP,'#13#10 +
             '                                   ( SELECT MAX(LP) FROM WEZEL_KABEL WKM WHERE WKM.ID_TYP_ELEMENTU = 101 AND WKM.ID_ELEMENT = SW.ID ) AS MAX_LP'#13#10 +
             '                                   '#13#10 +
             '                            FROM   SWIATLOWOD      SW'#13#10 +
             '                            JOIN   SM_UZYTKOWNIK  SMU  ON SMU.ID_SM  = SW.ID_SM AND SMU.ID_UZYTKOWNIK = -1 /*:USERID*/'#13#10 +
             '                            WHERE (SW.ID_SM = 309 /*:LOOKUPSM.VALUE*/ OR 309 /*:LOOKUPSM.VALUE*/ = 0)'#13#10 +
             '                              AND (UPPER(SW.NR) LIKE UPPER(''%'' || /*:edtFilterNr.Value ||*/ ''%''))'#13#10 +
             '                         ) S'#13#10 +
             '               ) S'#13#10 +
             '     ) S'#13#10 +
             '     ) S'#13#10 +
             'WHERE ((UPPER(NVL(S.ELEMENT1, '' '')) LIKE UPPER(''%'' || /*:edtFilterLok.Value ||*/ ''%''))'#13#10 +
             '   OR  (UPPER(NVL(S.ELEMENT2, '' '')) LIKE UPPER(''%'' || /*:edtFilterLok.Value ||*/ ''%'')))'#13#10 +
             '                                                                                            '#13#10 +
             'ORDER BY nr2integerleft(S.NR), nr2integer(S.NR), S.NR' );

  { date: 2014-06-14, file: parse no WHERE after format.sql }
  TestQuery( '   SELECT   P.*'#13#10 +
             ''#13#10 +
             '     FROM   PRZELACZNICA     P'#13#10 +
             '     JOIN   WEZEL_ADRES_V  SMA ON SMA.ID    =   P.ID_WEZEL'#13#10 +
             ''#13#10 +
             'WHERE (SMA.ID_SM = :lookupSM.Value OR :lookupSM.Value = 0)'#13#10 +
             'AND (UPPER(P.NR) LIKE UPPER(''%'' || :edtFilterNr.Value || ''%''))'#13#10 +
             'AND (UPPER(NVL(SMA.ELEMENT, '' '')) LIKE UPPER(''%'' || :edtFilterLok.Value || ''%''))' );

  { date: 2014-06-21, file: hang FUNCTION nested calls.sql }
  TestQuery( '  SELECT SZAFA_FIND ( p_NR       => IMP.SYMBOL'#13#10 +
             '                    , p_ID_WEZEL => 1'#13#10 +
             '                    )'#13#10 +
             '  FROM   IMP_MUFA_PRZEL_SZAFA IMP'#13#10 +
             ';' );
  TestQuery( '  UPDATE IMP_MUFA_PRZEL_SZAFA IMP'#13#10 +
             '  SET    ID_TYP_ELEMENTU = ELEMENT_TYPES.SZAFA'#13#10 +
             '       , ID_ELEMENT      = SZAFA_FIND ( p_NR       => IMP.SYMBOL'#13#10 +
             '                                      , p_ID_WEZEL => 1'#13#10 +
             '                                      )'#13#10 +
             '  WHERE  IMP.ID_ELEMENT IS NULL'#13#10 +
             '  AND    IMP.KOD = ''S'';' );
  TestQuery( '  UPDATE IMP_MUFA_PRZEL_SZAFA IMP'#13#10 +
             '  SET    ID_TYP_ELEMENTU = ELEMENT_TYPES.SZAFA'#13#10 +
             '       , ID_ELEMENT      = SZAFA_FIND ( p_NR       => IMP.SYMBOL'#13#10 +
             '                                      , p_ID_WEZEL => WEZEL_FIND ( p_ID_TYP_WEZLA => WEZEL_TYPES.LOKALIZACJA'#13#10 +
             '                                                                 , p_ID_ELEMENT   => LOKALIZACJA_FIND ( p_SYMBOL => IMP.LOKALIZACJA )'#13#10 +
             '                                                                 )'#13#10 +
             '                                      )'#13#10 +
             '  WHERE  IMP.ID_ELEMENT IS NULL'#13#10 +
             '  AND    IMP.KOD = ''S'';' );
  TestQuery( '  UPDATE IMP_MUFA_PRZEL_SZAFA IMP'#13#10 +
             '  SET    ID_TYP_ELEMENTU = ELEMENT_TYPES.SZAFA'#13#10 +
             '       , ID_ELEMENT      = SZAFA_FIND ( p_NR       => IMP.SYMBOL'#13#10 +
             '                                      , p_ID_WEZEL => COALESCE( WEZEL_FIND ( p_ID_TYP_WEZLA => WEZEL_TYPES.LOKALIZACJA'#13#10 +
             '                                                                           , p_ID_ELEMENT   => LOKALIZACJA_FIND ( p_SYMBOL => IMP.LOKALIZACJA )'#13#10 +
             '                                                                           )'#13#10 +
             '                                                              , WEZEL_FIND ( p_ID_TYP_WEZLA => WEZEL_TYPES.STUDNIA'#13#10 +
             '                                                                           , p_ID_ELEMENT   => STUDNIA_FIND     ( p_NR     => IMP.LOKALIZACJA )'#13#10 +
             '                                                                           )'#13#10 +
             '                                                              , WEZEL_FIND ( p_ID_TYP_WEZLA => WEZEL_TYPES.SLUP'#13#10 +
             '                                                                           , p_ID_ELEMENT   => SLUP_FIND        ( p_NR     => IMP.LOKALIZACJA )'#13#10 +
             '                                                                           )'#13#10 +
             '                                                              )             '#13#10 +
             '                                      )'#13#10 +
             '  WHERE  IMP.ID_ELEMENT IS NULL'#13#10 +
             '  AND    IMP.KOD = ''S'';' );

  { date: 2014-07-17, file: parse too much brackets.sql }
  TestQuery( 'SELECT     SUM('#13#10 +
             '           CASE WHEN'#13#10 +
             '           CASE WHEN CONNECT_BY_ROOT(ID_WEZEL_1) = (SELECT ID_WEZEL_1 FROM SWIATLOWOD_WKV WHERE ID = :qEdit.ID)'#13#10 +
             '           THEN ID_WEZEL_2 ELSE ID_WEZEL_1 END   = (SELECT ID_WEZEL_2 FROM SWIATLOWOD_WKV WHERE ID = :qEdit.ID)'#13#10 +
             '           THEN 1 END) AS PRZEBIEG_ZGODNY,'#13#10 +
             ''#13#10 +
             '           NVL(SUM(K.DLUGOSC),0) AS DLUGOSC_TRASOWA,'#13#10 +
             ''#13#10 +
             '           NVL(MAX((SELECT SUM(NVL(WK.ZAPAS_1,0) + NVL(WK.ZAPAS_2,0) /*+ NVL(WK.DLUGOSC_KABLA_W_BUDYNKU,0)*/) FROM WEZEL_KABEL WK WHERE WK.ID_TYP_ELEMENTU = 101 AND WK.ID_ELEMENT = :qEdit.ID)),0)'#13#10 +
             '           + NVL(SUM(K.DLUGOSC),0) AS DLUGOSC_KABLOWA'#13#10 +
             ''#13#10 +
             'FROM    (  SELECT *'#13#10 +
             '           FROM   KANALIZACJA K'#13#10 +
             '           WHERE  K.ID IN (SELECT ID_KANALIZACJA FROM ZAJETOSC_RURY WHERE ID_TYP_ELEMENTU = 101 AND ID_ELEMENT = :qEdit.ID)'#13#10 +
             '        )  K '#13#10 +
             'CONNECT BY NOCYCLE '#13#10 +
             '           K.ID_WEZEL_1 IN (PRIOR K.ID_WEZEL_1, PRIOR K.ID_WEZEL_2)'#13#10 +
             '        OR K.ID_WEZEL_2 IN (PRIOR K.ID_WEZEL_1, PRIOR K.ID_WEZEL_2)'#13#10 +
             'START WITH (SELECT ID_WEZEL_1 FROM SWIATLOWOD_WKV WHERE ID = :qEdit.ID) IN (K.ID_WEZEL_1, K.ID_WEZEL_2)' );

  { date: 2014-08-18, file: START WITH too much brackets.sql }
  TestQuery( 'CREATE OR REPLACE VIEW USLUGI_V_ATTR_TGI AS'#13#10 +
             'SELECT  U.ID, U.ID_DOSTAWCA, U.ID_HANDLOWIEC, U.ID_HD, U.ID_KIEROWNIK, U.ID_ODPOWIEDZIALNY, U.ID_OPIEKUN, U.ID_PARENT, U.ID_PRODUKT,'#13#10 +
             '        U.ID_RODZAJ_USLUGI, U.ID_STATUS_USLUGI, U.ID_TYP_USLUGI, U.ID_URZADZENIE_1, U.ID_URZADZENIE_2, U.ID_WLASCICIEL, U.ID_W_HD,'#13#10 +
             '        U.B_BGP, U.B_KONCOWA, U.B_OBCA, U.B_POMIAR_RUCHU, U.B_ZGODNY,'#13#10 +
             '        U.DATA_URUCHOMIENIA, U.DATA_WPISU, U.DATA_ZAKONCZENIA,'#13#10 +
             '        U.BGP, U.NAZWA, U.NAZWA_PLIKU_STREF,'#13#10 +
             '        U.NR, U.NR_TRANSAKCJI, U.NR_UMOWY, U.NR_USLUGI_HANDLOWEJ, U.NR_USLUGI_OBCY, U.NR_ZLECENIA,'#13#10 +
             '        U.OKRES_WYPOW, U.UWAGI, U.WARTOSC_AMORTYZACJI, U.WARTOSC_KSIEGOWA, U.WARTOSC_ODTWORZENIOWA,'#13#10 +
             '        TU.NAZWA AS TYP_USLUGI, SU.NAZWA AS STATUS_USLUGI, WL.NAZWA AS WLASCICIEL, HL.NAZWISKO_IMIE AS HANDLOWIEC,'#13#10 +
             '        KR.NAZWISKO_IMIE AS KIEROWNIK, OP.NAZWISKO_IMIE AS OPIEKUN, PR.NAZWA AS PRODUKT, ODP.NAZWISKO_IMIE AS ODPOWIEDZIALNY,'#13#10 +
             '        UP.NR AS PARENT,'#13#10 +
             '        UU.ID_PORT, UU.ID_GNIAZDO'#13#10 +
             'FROM ('#13#10 +
             '        SELECT  UTR.ID_USLUGA AS ID, ID_PORT, ID_GNIAZDO'#13#10 +
             '        FROM ('#13#10 +
             '                SELECT  TMP.ID_PASSPORT AS ID_TRANSMISJA, GN.ID_PORT, TMP.ID_GNIAZDO'#13#10 +
             '                FROM    USLUGI_V_ATTR_TGI_INT TMP'#13#10 +
             '                JOIN    GNIAZDO                GN ON GN.ID = TMP.ID_GNIAZDO'#13#10 +
             '                WHERE   TMP.ID_PASSPORT_TYPE = PASSPORT_TYPES.TRANSMISJA'#13#10 +
             '                UNION ALL'#13#10 +
             '                SELECT  TAB.ID_TRANSMISJA, GN.ID_PORT, GN.ID AS ID_GNIAZDO'#13#10 +
             '                FROM  ( SELECT       TRS.ID_TRANSMISJA, CONNECT_BY_ROOT ( TRS.ID ) AS ID_TRANSMISJA_SKLADNIK'#13#10 +
             '                        FROM         TRANSMISJA_SKLADNIKI TRS'#13#10 +
             '                        CONNECT BY   NOCYCLE TRS.ID_TRANSMISJA_SKL = PRIOR TRS.ID_TRANSMISJA'#13#10 +
             '                        START WITH   TRS.ID IN ('#13#10 +
             '                                     SELECT  TRS.ID AS ID_TRANSMISJA_SKLADNIK'#13#10 +
             '                                     FROM    USLUGI_V_ATTR_TGI_INT TMP'#13#10 +
             '                                     JOIN    TRANSMISJA_SKLADNIKI  TRS ON TRS.ID_PASSPORT_TYPE_1 = TMP.ID_PASSPORT_TYPE'#13#10 +
             '                                                                      AND TRS.ID_PASSPORT_1 = TMP.ID_PASSPORT'#13#10 +
             '                                     UNION ALL'#13#10 +
             '                                     SELECT  TRS.ID AS ID_TRANSMISJA_SKLADNIK'#13#10 +
             '                                     FROM    USLUGI_V_ATTR_TGI_INT TMP'#13#10 +
             '                                     JOIN    TRANSMISJA_SKLADNIKI  TRS ON TRS.ID_PASSPORT_TYPE_2 = TMP.ID_PASSPORT_TYPE'#13#10 +
             '                                                                      AND TRS.ID_PASSPORT_2 = TMP.ID_PASSPORT'#13#10 +
             '                                     )'#13#10 +
             '                      ) TAB'#13#10 +
             '                JOIN  ( SELECT  TRS.ID AS ID_TRANSMISJA_SKLADNIK, TMP.ID_GNIAZDO'#13#10 +
             '                        FROM    USLUGI_V_ATTR_TGI_INT TMP'#13#10 +
             '                        JOIN    TRANSMISJA_SKLADNIKI  TRS ON TRS.ID_PASSPORT_TYPE_1 = TMP.ID_PASSPORT_TYPE'#13#10 +
             '                                                         AND TRS.ID_PASSPORT_1 = TMP.ID_PASSPORT'#13#10 +
             '                        UNION ALL'#13#10 +
             '                        SELECT  TRS.ID AS ID_TRANSMISJA_SKLADNIK, TMP.ID_GNIAZDO'#13#10 +
             '                        FROM    USLUGI_V_ATTR_TGI_INT TMP'#13#10 +
             '                        JOIN    TRANSMISJA_SKLADNIKI  TRS ON TRS.ID_PASSPORT_TYPE_2 = TMP.ID_PASSPORT_TYPE'#13#10 +
             '                                                         AND TRS.ID_PASSPORT_2 = TMP.ID_PASSPORT'#13#10 +
             '                      ) TMP ON TMP.ID_TRANSMISJA_SKLADNIK = TAB.ID_TRANSMISJA_SKLADNIK'#13#10 +
             '                JOIN    GNIAZDO GN ON GN.ID = TMP.ID_GNIAZDO'#13#10 +
             '          ) UU'#13#10 +
             '      JOIN USLUGA_TRANSMISJA UTR ON UTR.ID_TRANSMISJA = UU.ID_TRANSMISJA'#13#10 +
             '      UNION ALL'#13#10 +
             '      SELECT UTR.ID_USLUGA AS ID, G.ID_PORT, TMP.ID_GNIAZDO'#13#10 +
             '      FROM   USLUGI_V_ATTR_TGI_INT TMP'#13#10 +
             '      JOIN   USLUGA_TRASA          UTR ON UTR.ID_TRASA_SW = TMP.ID_PASSPORT AND UTR.STOP_DATA IS NULL'#13#10 +
             '      JOIN   GNIAZDO                 G ON G.ID = TMP.ID_GNIAZDO'#13#10 +
             '      WHERE  TMP.ID_PASSPORT_TYPE = PASSPORT_TYPES.TRASA_SW'#13#10 +
             '      UNION ALL'#13#10 +
             '      SELECT UKI.ID_USLUGA AS ID, G.ID_PORT, TMP.ID_GNIAZDO'#13#10 +
             '      FROM   USLUGI_V_ATTR_TGI_INT TMP'#13#10 +
             '      JOIN   USLUGA_KABEL_INF      UKI ON UKI.ID_KABEL_INF = TMP.ID_PASSPORT AND UKI.STOP_DATA IS NULL'#13#10 +
             '      JOIN   GNIAZDO                 G ON G.ID = TMP.ID_GNIAZDO'#13#10 +
             '      WHERE  TMP.ID_PASSPORT_TYPE = PASSPORT_TYPES.KABEL_INF'#13#10 +
             ') UU'#13#10 +
             'JOIN      USLUGA        U   ON U.ID = UU.ID'#13#10 +
             'LEFT JOIN RODZAJ_USLUGI RU  ON RU.ID = U.ID_RODZAJ_USLUGI'#13#10 +
             'LEFT JOIN TYP_USLUGI    TU  ON TU.ID  = U.ID_TYP_USLUGI'#13#10 +
             'LEFT JOIN STATUS_USLUGI SU  ON SU.ID  = U.ID_STATUS_USLUGI'#13#10 +
             'LEFT JOIN WLASCICIEL    WL  ON WL.ID  = U.ID_WLASCICIEL'#13#10 +
             'LEFT JOIN PRACOWNIK     HL  ON HL.ID  = U.ID_HANDLOWIEC'#13#10 +
             'LEFT JOIN PRACOWNIK     KR  ON KR.ID  = U.ID_KIEROWNIK'#13#10 +
             'LEFT JOIN PRACOWNIK     OP  ON OP.ID  = U.ID_OPIEKUN'#13#10 +
             'LEFT JOIN PRODUKT       PR  ON PR.ID  = U.ID_PRODUKT'#13#10 +
             'LEFT JOIN PRACOWNIK     ODP ON ODP.ID = U.ID_ODPOWIEDZIALNY'#13#10 +
             'LEFT JOIN USLUGA        UP  ON UP.ID  = U.ID_PARENT'#13#10 +
             'ORDER BY  U.NR;' );

  { date: 2014-10-21, file: parse column datatype.sql }
//TestQuery( 'create table nic ( nic nic.nic%type );' );








//TestFinish;
  ShowMessage('Test finished on ' + IntToStr(TestQueryCount) + ' queries with ' + IntToStr(TestQueryErrors) + ' errors !!!' );
end;

end.

