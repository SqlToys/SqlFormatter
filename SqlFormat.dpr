program SqlFormat;

{$APPTYPE CONSOLE}

{$R *.res}

uses Classes, SqlVersion, SqlCommon, SqlParser, SqlLister;

    procedure PrintHelp;
    begin
      WriteLn;
      WriteLn('SqlFormat ' + VER_VERSION );
      WriteLn('Formats (or compacts) SQL queries.');
      WriteLn;
      WriteLn('usage: SqlFormat [-c] queries.sql [formatted.sql]');
    end;

var bFormatCompact: Boolean;
    sInFileName, sOutFileName: String;

    function CheckParam(aParam: String): Boolean;
    begin
      Result := True;
      if aParam = '' then Exit;

      if aParam = '-c' then bFormatCompact := False else
      if aParam = '-f' then bFormatCompact := True else
      if aParam[1] = '-' then Result := False else
      if sInFileName = '' then sInFileName := aParam else
      if sOutFileName = '' then sOutFileName := aParam else
         Result := False;
    end;

var lSL: TStringList;
    Parser: TGtSqlParser;
    ScriptLister: TGtSqlFormatLister;
    i: Integer;
begin
  bFormatCompact := True;
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

  if sOutFileName = '' then sOutFileName := sInFileName;

  lSL := TStringList.Create;
  lSL.LoadFromFile(sInFileName);

  Parser := TGtSqlParser.Create( nil, lSL );
  try
    Parser.Tokenize_Script;
    Parser.Parse_Statements;

    ScriptLister := TGtSqlFormatLister.Create;
    try
      ScriptLister.FormattingMode  := gtfoText;

      SetScriptListerOptions(ScriptLister);
      SetScriptFormatOptions(ScriptLister, bFormatCompact);

      ScriptLister.SL :=  TStringList.Create;
      ScriptLister.List_SqlParser(Parser);

      ScriptLister.SL.SaveToFile(sOutFileName);
    finally
      ScriptLister.Free;
    end;
  finally
    Parser.Free;
  end;
end.

