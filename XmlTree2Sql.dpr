program XmlTree2Sql;

{$APPTYPE CONSOLE}

{$R *.res}

uses Classes, SysUtils, ComObj,
     SqlVersion, SqlCommon, SqlParser, SqlLister, SqlXmlTree;

  procedure PrintHelp;
  begin
    WriteLn;
    WriteLn('XmlTree2Sql ' + VER_VERSION );
    WriteLn('reads parse tree from XML file and save queries to SQL file.');
    WriteLn;
    WriteLn('usage: XmlTree2Sql queries.xml [queries.sql]');
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

var ScriptLister: TGtSqlFormatLister;
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

  if sOutFileName = '' then sOutFileName := ChangeFileExt(sInFileName, '.sql' );

  { this is needed, due to ComObj are not initialized for console app }
  { https://stackoverflow.com/questions/27154319/use-ixml-in-a-delphi-console-application }
  CoInitializeEx(nil, 2 {= COINIT_APARTMENTTHREADED});

  Parser := TGtSqlParser.Create( nil );
  XmlToParseTree( sInFileName, Parser.QueryList );

  ScriptLister := TGtSqlFormatLister.Create;
  try
    ScriptLister.FormattingMode  := gtfoText;
    SetScriptListerOptions(ScriptLister);
    SetScriptFormatOptions(ScriptLister, True);
    ScriptLister.List_SqlParser(Parser);
    ScriptLister.SL.SaveToFile( sOutFileName );
  finally
    ScriptLister.Free;
  end;
end.

