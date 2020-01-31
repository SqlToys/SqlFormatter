program SqlTool;

{$APPTYPE CONSOLE}

{$R *.res}

uses Classes;

var SL: TStringList;
    i: Integer;
begin
  if ParamCount <> 1 then Exit;

  SL := TStringList.Create;
  try
    SL.LoadFromFile( ParamStr(1) );
    i := 0;
    while i < SL.Count - 1 do
      if Copy(SL [i],1,2) = '//' then SL.Delete(i) else Inc(i);
    SL.SaveToFile( ParamStr(1) );
  finally
    SL.Free;
  end;
end.
