program Sql2XmlTree;

{$APPTYPE CONSOLE}

{$R *.res}

uses Classes, SysUtils, SqlVersion, SqlParser, SqlXmlTree;

  procedure PrintHelp;
  begin
    WriteLn;
    WriteLn('Sql2XmlTree ' + VER_VERSION );
    WriteLn('parses SQL file and save parse tree to an xml file.');
    WriteLn;
    WriteLn('usage: Sql2XmlTree queries.sql [queries.xml]');
  end;

var sInFileName, sOutFileName: String;

    function CheckParam(aParam: String): Boolean;
    begin
      Result := True;
      if aParam = '' then Exit;

      if aParam[1] = '-' then Result := False else
      if sInFileName = '' then sInFileName := aParam else
      if sOutFileName = '' then sOutFileName := aParam else
         Result := False;
    end;

var lSL: TStringList;
    Parser: TGtSqlParser;
    i: Integer;
begin
  sInFileName := '';
  sOutFileName := '';

  for i := 1 to ParamCount do
    if not CheckParam(ParamStr(i)) then begin
      PrintHelp;
      Exit;
    end;

  if sInFileName = '' then begin
    PrintHelp;
    Exit;
  end;

  if sOutFileName = '' then sOutFileName := ChangeFileExt(sInFileName, '.xml' );

  lSL := TStringList.Create;
  lSL.LoadFromFile(sInFileName); // wo. try...finally?

  Parser := TGtSqlParser.Create( nil, lSL );
  try
    Parser.Tokenize_Script;
    Parser.Parse_Statements;

    ParseTreeToXml (Parser.QueryList, sOutFileName);
  finally
    Parser.Free;
  end;
end.

