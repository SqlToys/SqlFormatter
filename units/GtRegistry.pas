(* $Header: /SQL Toys/SqlFormat/GtRegistry.pas 40    17-12-16 12:37 Tomek $
   (c) Tomasz Gierka, github.com/SqlToys, 2003.11.12                          *)
{--------------------------------------  --------------------------------------}
{$IFDEF RELEASE}
  {$DEBUGINFO OFF}
  {$LOCALSYMBOLS OFF}
{$ENDIF}
unit GtRegistry;

{ Klasa jest przeznaczona dla obiektów, które regularnie zapisuj¹ i odczytuj¹ dane z rejestru, }
{ szczególnie informacje diagnostyczne. Do jednorazowych i/lub pojedynczych operacji zapiu i }
{ odczytu z rejestru warto u¿ywaæ metody uproszczone. Gdy  }

interface

uses Windows, Registry, Classes,
     GtStandard;

{--------------------------------------  --------------------------------------}
{-------------------------- Klasa dostêpu do rejestru -------------------------}

type
  TGtRegistry = class (TRegistry)

    constructor Create(aRootKey: HKEY; aKeyName: String='');
    destructor  Destroy; override;
  protected
    FSubRegs: TStringList;

  public

    function GetDbl(sKeyName: String; aValue: Double=0): Double;
    function GetInt(sKeyName: String; aValue: Integer=0): Integer;
    function GetStr(sKeyName: String; aValue: String=''): String;
    function GetBool(sKeyName: String; aValue: Boolean=False): Boolean;
    function GetDate(sKeyName: String; aValue: TDate=0): TDate;
    function GetTime(sKeyName: String; aValue: TTime=0): TTime;
    function GetDateTime(sKeyName: String; aValue: TDateTime=0): TDateTime;
    function GetVar(sKeyName: String): Variant;
    function GetVarArray(sKeyName: String): VariantArray;

    function PutDbl(sKeyName: String; aValue: Double): Boolean;
    function PutInt(sKeyName: String; aValue: Integer): Boolean;
    function PutStr(sKeyName: String; aValue: String): Boolean;
    function PutBool(sKeyName: String; aValue: Boolean): Boolean;
    function PutDate(sKeyName: String; aValue: TDate): Boolean;
    function PutTime(sKeyName: String; aValue: TTime): Boolean;
    function PutDateTime(sKeyName: String; aValue: TDateTime): Boolean;
    function PutVar(sKeyName: String; aValue: Variant): Boolean;
    function PutVarArray(sKeyName: String; aValue: VariantArray): Boolean;

    function IncInt(sKeyName: String; aValue: Integer=1): Integer;

    procedure DeleteAll;
    procedure ReCreateKey;

    function GetRegSub(sKeyName: String): TGtRegistry; virtual;
  end;

{--------------------------------------  --------------------------------------}
{------------------------------- Funkcje ogólne -------------------------------}

{ UWAGA - Ostatnim parametrem funkcji odczytu jest wartoœæ domyœlna, zwracana w przypadku braku podanego klucza }

//function regGetDbl(RootKey: HKEY; sKeyName: String): Double;
//function regGetInt(RootKey: HKEY; sKeyName: String; aValue: Integer=0): Integer;
//function regGetStr(RootKey: HKEY; sKeyName: String): String;
//function regGetBool(RootKey: HKEY; sKeyName: String; aValue: Boolean=False): Boolean;
//function regGetDate(RootKey: HKEY; sKeyName: String): TDate;
//function regGetTime(RootKey: HKEY; sKeyName: String): TTime;
//function regGetDateTime(RootKey: HKEY; sKeyName: String): TDateTime;
//function regGetVarArray(RootKey: HKEY; sKeyName: String): VariantArray;
//
//function regPutDbl(RootKey: HKEY; sKeyName: String; vDbl: Double): Boolean;
//function regPutInt(RootKey: HKEY; sKeyName: String; vInt: Integer): Boolean;
//function regPutStr(RootKey: HKEY; sKeyName: String; vStr: String): Boolean;
//function regPutBool(RootKey: HKEY; sKeyName: String; vBool: Boolean): Boolean;
//function regPutDate(RootKey: HKEY; sKeyName: String; vDate: TDate): Boolean;
//function regPutTime(RootKey: HKEY; sKeyName: String; vTime: TTime): Boolean;
//function regPutDateTime(RootKey: HKEY; sKeyName: String; vDateTime: TDateTime): Boolean;
//function regPutVarArray(RootKey: HKEY; sKeyName: String; vVarArray: VariantArray): Boolean;

function regCreateKey(RootKey: HKEY; sKeyName: String): Boolean;
function regDeleteKey(RootKey: HKEY; sKeyName: String): Boolean;
function regDeleteVal(RootKey: HKEY; sKeyName: String): Boolean;

{------------------------------ Funkcje skrócone ------------------------------}
{--------------------------- dla kluczy u¿ytkownika ---------------------------}

function rguGetKeyNames(sKeyName: String): TStrings;
function rguGetValNames(sKeyName: String): TStrings;
//function rguGetDbl(sKeyName: String): Double;
function rguGetInt(sKeyName: String; aValue: Integer=0): Integer;
function rguGetStr(sKeyName: String; aValue: String=''): String;
function rguGetBool(sKeyName: String; aValue: Boolean=False): Boolean;
function rguGetDate(sKeyName: String): TDate;
//function rguGetTime(sKeyName: String): TTime;
//function rguGetDateTime(sKeyName: String): TDateTime;
function rguGetVarArray(sKeyName: String): VariantArray;

//function rguPutDbl(sKeyName: String; vDbl: Double): Boolean;
function rguPutInt(sKeyName: String; vInt: Integer): Boolean;
function rguPutStr(sKeyName: String; vStr: String): Boolean;
function rguPutBool(sKeyName: String; vBool: Boolean): Boolean;
//function rguPutDate(sKeyName: String; vDate: TDate): Boolean;
//function rguPutTime(sKeyName: String; vTime: TTime): Boolean;
//function rguPutDateTime(sKeyName: String; vDateTime: TDateTime): Boolean;
function rguPutVarArray(sKeyName: String; vVarArray: VariantArray): Boolean;

function rguCreateKey(sKeyName: String): Boolean;
function rguDeleteKey(sKeyName: String): Boolean;
function rguDeleteVal(sKeyName: String): Boolean;

{------------------------------ Funkcje skrócone ------------------------------}
{----------------------------- dla kluczy maszyny -----------------------------}

function rgmGetKeyNames(sKeyName: String): TStrings;
function rgmGetValNames(sKeyName: String): TStrings;
//function rgmGetDbl(sKeyName: String): Double;
//function rgmGetInt(sKeyName: String): Integer;
function rgmGetStr(sKeyName: String): String;
//function rgmGetBool(sKeyName: String; aValue: Boolean=False): Boolean;
function rgmGetDate(sKeyName: String): TDate;
//function rgmGetTime(sKeyName: String): TTime;
//function rgmGetDateTime(sKeyName: String): TDateTime;
//
//function rgmPutDbl(sKeyName: String; vDbl: Double): Boolean;
//function rgmPutInt(sKeyName: String; vInt: Integer): Boolean;
function rgmPutStr(sKeyName: String; vStr: String): Boolean;
//function rgmPutBool(sKeyName: String; vBool: Boolean): Boolean;
//function rgmPutDate(sKeyName: String; vDate: TDate): Boolean;
//function rgmPutTime(sKeyName: String; vTime: TTime): Boolean;
//function rgmPutDateTime(sKeyName: String; vDateTime: TDateTime): Boolean;

implementation

uses SysUtils, Variants;

{--------------------------------------  --------------------------------------}
{------------------------------- Funkcje ogólne -------------------------------}

function regGetKeyNames(RootKey: HKEY; sKeyName: String): TStrings;
var Reg: TRegistry;
    sKey, sName: String;
begin
  Result:=nil;
  Reg:=TRegistry.Create;
  strBreakOnLast('\', sKeyName, sKey, sName);
  try
    Reg.RootKey:=RootKey;
    if Reg.OpenKey(sKey, True) then begin
      Result:=TStringList.Create;
      Reg.GetKeyNames(Result);
    end;
  finally
    Reg.CloseKey;
    Reg.Free;
  end;
end;

function regGetValNames(RootKey: HKEY; sKeyName: String): TStrings;
var Reg: TRegistry;
    sKey, sName: String;
begin
  Result:=nil;
  Reg:=TRegistry.Create;
  strBreakOnLast('\', sKeyName, sKey, sName);
  try
    Reg.RootKey:=RootKey;
    if Reg.OpenKey(sKey, True) then begin
      Result:=TStringList.Create;
      Reg.GetValueNames(Result);
    end;
  finally
    Reg.CloseKey;
    Reg.Free;
  end;
end;

function regGetDbl(RootKey: HKEY; sKeyName: String; aValue: Double=0): Double;
var Reg: TRegistry;
    sKey, sName: String;
begin
  Result:=aValue;
  Reg:=TRegistry.Create;
  strBreakOnLast('\', sKeyName, sKey, sName);
  try
    try
      Reg.RootKey:=RootKey;
      if Reg.OpenKey(sKey, True) and Reg.ValueExists(sName)
        then Result:=Reg.ReadFloat(sName);
    except
      Result:=aValue;
    end;
  finally
    Reg.CloseKey;
    Reg.Free;
  end;
end;

function regGetInt(RootKey: HKEY; sKeyName: String; aValue: Integer=0): Integer;
var Reg: TRegistry;
    sKey, sName: String;
begin
  Result:=aValue;
  Reg:=TRegistry.Create;
  strBreakOnLast('\', sKeyName, sKey, sName);
  try
    try
      Reg.RootKey:=RootKey;
      if Reg.OpenKey(sKey, True) and Reg.ValueExists(sName)
        then Result:=Reg.ReadInteger(sName);
    except
      Result:=aValue;
    end;
  finally
    Reg.CloseKey;
    Reg.Free;
  end;
end;

function regGetStr(RootKey: HKEY; sKeyName: String; aValue: String=''): String;
var Reg: TRegistry;
    sKey, sName: String;
begin
  Result:=aValue;
  Reg:=TRegistry.Create;
  strBreakOnLast('\', sKeyName, sKey, sName);
  try
    try
      Reg.RootKey:=RootKey;
      if Reg.OpenKey(sKey, True) and Reg.ValueExists(sName)
        then Result:=Reg.ReadString(sName);
    except
      Result:=aValue;
    end;
  finally
    Reg.CloseKey;
    Reg.Free;
  end;
end;

function regGetBool(RootKey: HKEY; sKeyName: String; aValue: Boolean=False): Boolean;
var Reg: TRegistry;
    sKey, sName: String;
begin
  Result:=aValue;
  Reg:=TRegistry.Create;
  strBreakOnLast('\', sKeyName, sKey, sName);
  try
    try
      Reg.RootKey:=RootKey;
      if Reg.OpenKey(sKey, True) and Reg.ValueExists(sName)
        then Result:=Reg.ReadBool(sName);
    except
      Result:=aValue;
    end;
  finally
    Reg.CloseKey;
    Reg.Free;
  end;
end;

function regGetDate(RootKey: HKEY; sKeyName: String; aValue: TDate=-657434.0 {MinDateTime}): TDate;
var Reg: TRegistry;
    sKey, sName: String;
begin
  Result:=aValue;
  Reg:=TRegistry.Create;
  strBreakOnLast('\', sKeyName, sKey, sName);
  try
    try
      Reg.RootKey:=RootKey;
      if Reg.OpenKey(sKey, True) and Reg.ValueExists(sName)
        then Result:=Reg.ReadDate(sName);
    except
      Result:=aValue;
    end;
  finally
    Reg.CloseKey;
    Reg.Free;
  end;
end;

function regGetTime(RootKey: HKEY; sKeyName: String; aValue: TTime=-657434.0 {MinDateTime}): TTime;
var Reg: TRegistry;
    sKey, sName: String;
begin
  Result:=aValue;
  Reg:=TRegistry.Create;
  strBreakOnLast('\', sKeyName, sKey, sName);
  try
    try
      Reg.RootKey:=RootKey;
      if Reg.OpenKey(sKey, True) and Reg.ValueExists(sName)
        then Result:=Reg.ReadTime(sName);
    except
      Result:=aValue;
    end;
  finally
    Reg.CloseKey;
    Reg.Free;
  end;
end;

function regGetDateTime(RootKey: HKEY; sKeyName: String; aValue: TDateTime=-657434.0 {MinDateTime}): TDateTime;
var Reg: TRegistry;
    sKey, sName: String;
begin
  Result:=aValue;
  Reg:=TRegistry.Create;
  strBreakOnLast('\', sKeyName, sKey, sName);
  try
    try
      Reg.RootKey:=RootKey;
      if Reg.OpenKey(sKey, True) and Reg.ValueExists(sName)
        then Result:=Reg.ReadDateTime(sName);
    except
      Result:=aValue;
    end;
  finally
    Reg.CloseKey;
    Reg.Free;
  end;
end;

function regGetVarArray(RootKey: HKEY; sKeyName: String): VariantArray;
var i,Hi,Low: Integer;
begin
  Hi :=regGetInt(RootKey, sKeyName + '\Hi');
  Low:=regGetInt(RootKey, sKeyName + '\Low');

  Result:=VarArrayCreate([Low, Hi], varVariant);
  for i:=Low to Hi do
    Result[i]:=regGetStr(RootKey, sKeyName + '\' + IntToStr(i));
end;

function regPutDbl(RootKey: HKEY; sKeyName: String; vDbl: Double): Boolean;
var Reg: TRegistry;
    sKey, sName: String;
begin
  Result:=False;
  Reg:=TRegistry.Create;
  strBreakOnLast('\', sKeyName, sKey, sName);
  try
    Reg.RootKey:=RootKey;
    if Reg.OpenKey(sKey, True) then begin
       Reg.WriteFloat(sName, vDbl);
       Result:=True;
    end;
  finally
    Reg.CloseKey;
    Reg.Free;
  end;
end;

function regPutInt(RootKey: HKEY; sKeyName: String; vInt: Integer): Boolean;
var Reg: TRegistry;
    sKey, sName: String;
begin
  Result:=False;
  Reg:=TRegistry.Create;
  strBreakOnLast('\', sKeyName, sKey, sName);
  try
    Reg.RootKey:=RootKey;
    if Reg.OpenKey(sKey, True) then begin
       Reg.WriteInteger(sName, vInt);
       Result:=True;
    end;
  finally
    Reg.CloseKey;
    Reg.Free;
  end;
end;

function regPutStr(RootKey: HKEY; sKeyName: String; vStr: String): Boolean;
var Reg: TRegistry;
    sKey, sName: String;
begin
  Result:=False;
  Reg:=TRegistry.Create;
  strBreakOnLast('\', sKeyName, sKey, sName);
  try
    Reg.RootKey:=RootKey;
    if Reg.OpenKey(sKey, True) then begin
       Reg.WriteString(sName, vStr);
       Result:=True;
    end;
  finally
    Reg.CloseKey;
    Reg.Free;
  end;
end;

function regPutBool(RootKey: HKEY; sKeyName: String; vBool: Boolean): Boolean;
var Reg: TRegistry;
    sKey, sName: String;
begin
  Result:=False;
  Reg:=TRegistry.Create;
  strBreakOnLast('\', sKeyName, sKey, sName);
  try
    Reg.RootKey:=RootKey;
    if Reg.OpenKey(sKey, True) then begin
       Reg.WriteBool(sName, vBool);
       Result:=True;
    end;
  finally
    Reg.CloseKey;
    Reg.Free;
  end;
end;

function regPutDate(RootKey: HKEY; sKeyName: String; vDate: TDate): Boolean;
var Reg: TRegistry;
    sKey, sName: String;
begin
  Result:=False;
  Reg:=TRegistry.Create;
  strBreakOnLast('\', sKeyName, sKey, sName);
  try
    Reg.RootKey:=RootKey;
    if Reg.OpenKey(sKey, True) then begin
       Reg.WriteDate(sName, vDate);
       Result:=True;
    end;
  finally
    Reg.CloseKey;
    Reg.Free;
  end;
end;

function regPutTime(RootKey: HKEY; sKeyName: String; vTime: TTime): Boolean;
var Reg: TRegistry;
    sKey, sName: String;
begin
  Result:=False;
  Reg:=TRegistry.Create;
  strBreakOnLast('\', sKeyName, sKey, sName);
  try
    Reg.RootKey:=RootKey;
    if Reg.OpenKey(sKey, True) then begin
       Reg.WriteTime(sName, vTime);
       Result:=True;
    end;
  finally
    Reg.CloseKey;
    Reg.Free;
  end;
end;

function regPutDateTime(RootKey: HKEY; sKeyName: String; vDateTime: TDateTime): Boolean;
var Reg: TRegistry;
    sKey, sName: String;
begin
  Result:=False;
  Reg:=TRegistry.Create;
  strBreakOnLast('\', sKeyName, sKey, sName);
  try
    Reg.RootKey:=RootKey;
    if Reg.OpenKey(sKey, True) then begin
       Reg.WriteDateTime(sName, vDateTime);
       Result:=True;
    end;
  finally
    Reg.CloseKey;
    Reg.Free;
  end;
end;

function regPutVarArray(RootKey: HKEY; sKeyName: String; vVarArray: VariantArray): Boolean;
var i,j: Integer;
begin
  j:=0;
  Result:=True;
  regDeleteKey(RootKey, sKeyName);

  for i:=VarArrayLowBound(vVarArray,1) to VarArrayHighBound(vVarArray,1) do
    if VarIsNull(vVarArray[i]) or VarIsEmpty(vVarArray[i])
      then else begin
        Result:=Result and regPutStr(RootKey, sKeyName + '\' + IntToStr(i), vVarArray[i]);
        Inc(j);
      end;

  if j>0 then begin
    regPutInt(RootKey, sKeyName + '\Low', VarArrayLowBound(vVarArray,1));
    regPutInt(RootKey, sKeyName + '\Hi',  VarArrayHighBound(vVarArray,1));
  end;
end;

function regCreateKey(RootKey: HKEY; sKeyName: String): Boolean;
var Reg: TRegistry;
begin
  Reg:=TRegistry.Create;
  try
    Reg.RootKey:=RootKey;
    Result:=Reg.CreateKey(sKeyName)
  finally
    Reg.CloseKey;
    Reg.Free;
  end;
end;

function regDeleteKey(RootKey: HKEY; sKeyName: String): Boolean;
var Reg: TRegistry;
    sKey, sName: String;
begin
  Result:=False;
  Reg:=TRegistry.Create;
  strBreakOnLast('\', sKeyName, sKey, sName);
  try
    Reg.RootKey:=RootKey;
    if Reg.OpenKey(sKey, True) then begin
       Reg.DeleteKey(sName);
       Result:=True;
    end;
  finally
    Reg.CloseKey;
    Reg.Free;
  end;
end;

function regDeleteVal(RootKey: HKEY; sKeyName: String): Boolean;
var Reg: TRegistry;
    sKey, sName: String;
begin
  Result:=False;
  Reg:=TRegistry.Create;
  strBreakOnLast('\', sKeyName, sKey, sName);
  try
    Reg.RootKey:=RootKey;
    if Reg.OpenKey(sKey, True) then begin
       Reg.DeleteValue(sName);
       Result:=True;
    end;
  finally
    Reg.CloseKey;
    Reg.Free;
  end;
end;

{------------------------------ Funkcje skrócone ------------------------------}
{--------------------------- dla kluczy u¿ytkownika ---------------------------}

function rguGetDbl(sKeyName: String): Double;
begin
  Result:=regGetDbl(HKEY_CURRENT_USER, sKeyName);
end;

function rguGetKeyNames(sKeyName: String): TStrings;
begin
  Result:=regGetKeyNames(HKEY_CURRENT_USER, sKeyName);
end;

function rguGetValNames(sKeyName: String): TStrings;
begin
  Result:=regGetValNames(HKEY_CURRENT_USER, sKeyName);
end;

function rguGetInt(sKeyName: String; aValue: Integer=0): Integer;
begin
  Result:=regGetInt(HKEY_CURRENT_USER, sKeyName, aValue);
end;

function rguGetStr(sKeyName: String; aValue: String=''): String;
begin
  Result:=regGetStr(HKEY_CURRENT_USER, sKeyName, aValue);
end;

function rguGetBool(sKeyName: String; aValue: Boolean=False): Boolean;
begin
  Result:=regGetBool(HKEY_CURRENT_USER, sKeyName, aValue);
end;

function rguGetDate(sKeyName: String): TDate;
begin
  Result:=regGetDate(HKEY_CURRENT_USER, sKeyName);
end;

function rguGetTime(sKeyName: String): TTime;
begin
  Result:=regGetTime(HKEY_CURRENT_USER, sKeyName);
end;

function rguGetDateTime(sKeyName: String): TDateTime;
begin
  Result:=regGetDateTime(HKEY_CURRENT_USER, sKeyName);
end;

function  rguGetVarArray(sKeyName: String): VariantArray;
begin
  Result:=regGetVarArray(HKEY_CURRENT_USER, sKeyName);
end;

function rguPutDbl(sKeyName: String; vDbl: Double): Boolean;
begin
  Result:=regPutDbl(HKEY_CURRENT_USER, sKeyName, vDbl);
end;

function rguPutInt(sKeyName: String; vInt: Integer): Boolean;
begin
  Result:=regPutInt(HKEY_CURRENT_USER, sKeyName, vInt);
end;

function rguPutStr(sKeyName: String; vStr: String): Boolean;
begin
  Result:=regPutStr(HKEY_CURRENT_USER, sKeyName, vStr);
end;

function rguPutBool(sKeyName: String; vBool: Boolean): Boolean;
begin
  Result:=regPutBool(HKEY_CURRENT_USER, sKeyName, vBool);
end;

function rguPutDate(sKeyName: String; vDate: TDate): Boolean;
begin
  Result:=regPutDate(HKEY_CURRENT_USER, sKeyName, vDate);
end;

function rguPutTime(sKeyName: String; vTime: TTime): Boolean;
begin
  Result:=regPutTime(HKEY_CURRENT_USER, sKeyName, vTime);
end;

function rguPutDateTime(sKeyName: String; vDateTime: TDateTime): Boolean;
begin
  Result:=regPutDateTime(HKEY_CURRENT_USER, sKeyName, vDateTime);
end;

function rguPutVarArray(sKeyName: String; vVarArray: VariantArray): Boolean;
begin
  Result:=regPutVarArray(HKEY_CURRENT_USER, sKeyName, vVarArray);
end;

function rguCreateKey(sKeyName: String): Boolean;
begin
  Result:=regCreateKey(HKEY_CURRENT_USER, sKeyName);
end;

function rguDeleteKey(sKeyName: String): Boolean;
begin
  Result:=regDeleteKey(HKEY_CURRENT_USER, sKeyName);
end;

function rguDeleteVal(sKeyName: String): Boolean;
begin
  Result:=regDeleteVal(HKEY_CURRENT_USER, sKeyName);
end;

{------------------------------ Funkcje skrócone ------------------------------}
{----------------------------- dla kluczy maszyny -----------------------------}

function rgmGetKeyNames(sKeyName: String): TStrings;
begin
  Result:=regGetKeyNames(HKEY_CURRENT_USER, sKeyName);
end;

function rgmGetValNames(sKeyName: String): TStrings;
begin
  Result:=regGetValNames(HKEY_CURRENT_USER, sKeyName);
end;

function rgmGetDbl(sKeyName: String): Double;
begin
  Result:=regGetDbl(HKEY_LOCAL_MACHINE, sKeyName);
end;

function rgmGetInt(sKeyName: String): Integer;
begin
  Result:=regGetInt(HKEY_LOCAL_MACHINE, sKeyName);
end;

function rgmGetStr(sKeyName: String): String;
begin
  Result:=regGetStr(HKEY_LOCAL_MACHINE, sKeyName);
end;

function rgmGetBool(sKeyName: String; aValue: Boolean=False): Boolean;
begin
  Result:=regGetBool(HKEY_LOCAL_MACHINE, sKeyName, aValue);
end;

function rgmGetDate(sKeyName: String): TDate;
begin
  Result:=regGetDate(HKEY_LOCAL_MACHINE, sKeyName);
end;

function rgmGetTime(sKeyName: String): TTime;
begin
  Result:=regGetTime(HKEY_LOCAL_MACHINE, sKeyName);
end;

function rgmGetDateTime(sKeyName: String): TDateTime;
begin
  Result:=regGetDateTime(HKEY_LOCAL_MACHINE, sKeyName);
end;

function rgmPutDbl(sKeyName: String; vDbl: Double): Boolean;
begin
  Result:=regPutDbl(HKEY_LOCAL_MACHINE, sKeyName, vDbl);
end;

function rgmPutInt(sKeyName: String; vInt: Integer): Boolean;
begin
  Result:=regPutInt(HKEY_LOCAL_MACHINE, sKeyName, vInt);
end;

function rgmPutStr(sKeyName: String; vStr: String): Boolean;
begin
  Result:=regPutStr(HKEY_LOCAL_MACHINE, sKeyName, vStr);
end;

function rgmPutBool(sKeyName: String; vBool: Boolean): Boolean;
begin
  Result:=regPutBool(HKEY_LOCAL_MACHINE, sKeyName, vBool);
end;

function rgmPutDate(sKeyName: String; vDate: TDate): Boolean;
begin
  Result:=regPutDate(HKEY_LOCAL_MACHINE, sKeyName, vDate);
end;

function rgmPutTime(sKeyName: String; vTime: TTime): Boolean;
begin
  Result:=regPutTime(HKEY_LOCAL_MACHINE, sKeyName, vTime);
end;

function rgmPutDateTime(sKeyName: String; vDateTime: TDateTime): Boolean;
begin
  Result:=regPutDateTime(HKEY_LOCAL_MACHINE, sKeyName, vDateTime);
end;

{--------------------------------------  --------------------------------------}
{-------------------------- Klasa dostêpu do rejestru -------------------------}

{ konstruktor klasy }
constructor TGtRegistry.Create(aRootKey: HKEY; aKeyName: String='');
begin
  inherited Create;

  FSubRegs:=TStringList.Create;
  FSubRegs.Sorted:=True;
  FSubRegs.Duplicates:=dupError;

  RootKey:=aRootKey;
  if aKeyName<>'' then OpenKey(aKeyName, True);
end;

{ destruktor klasy }
destructor  TGtRegistry.Destroy;
begin
  FSubRegs.Free;
  inherited Destroy;
end;

{ pobiera obiekt obs³ugi podleg³ej ga³êzi rejestru }
function    TGtRegistry.GetRegSub(sKeyName: String): TGtRegistry;
var i: Integer;
    sKey, sName: String;
begin
  if strBreakOnFirst('\', sKeyName, sKey, sName) then sKeyName:=sKey;
  i:=FSubRegs.IndexOf(sKeyName);
  if i>-1 then Result:=TGtRegistry(FSubRegs.Objects[i]) else Result:=nil;
  if not Assigned(Result) then begin
    Result:=TGtRegistry.Create(RootKey{=HKEY_CURRENT_USER}, strEndBkSlash(CurrentPath)+sKeyName);
    if i>-1 then FSubRegs.Objects[i]:=Result else FSubRegs.AddObject(sKeyName, Result);
  end;
  if sName<>'' then Result:=Result.GetRegSub(sName);
end;

{ funkcja czyta liczbê rzeczywist¹, zwraca podan¹ wartoœæ gdy brak klucza }
function    TGtRegistry.GetDbl(sKeyName: String; aValue: Double=0): Double;
var sKey, sName: String;
begin
  if strBreakOnLast('\', sKeyName, sKey, sName)
    then Result:=GetRegSub(sKey).GetDbl(sName, aValue)
    else if ValueExists(sKeyName)
           then Result:=ReadFloat(sKeyName)
           else Result:=aValue;
end;

{ funkcja czyta liczbê ca³kowit¹, zwraca podan¹ wartoœæ gdy brak klucza }
function    TGtRegistry.GetInt(sKeyName: String; aValue: Integer=0): Integer;
var sKey, sName: String;
begin
  if strBreakOnLast('\', sKeyName, sKey, sName)
    then Result:=GetRegSub(sKey).GetInt(sName, aValue)
    else if ValueExists(sKeyName)
           then Result:=ReadInteger(sKeyName)
           else Result:=aValue;
end;

{ funkcja czyta string, zwraca podan¹ wartoœæ gdy brak klucza }
function    TGtRegistry.GetStr(sKeyName: String; aValue: String=''): String;
var sKey, sName: String;
begin
  if strBreakOnLast('\', sKeyName, sKey, sName)
    then Result:=GetRegSub(sKey).GetStr(sName, aValue)
    else if ValueExists(sKeyName)
           then Result:=ReadString(sKeyName)
           else Result:=aValue;
end;

{ funkcja czyta wartoœæ logiczn¹, zwraca podan¹ wartoœæ gdy brak klucza }
function    TGtRegistry.GetBool(sKeyName: String; aValue: Boolean=False): Boolean;
var sKey, sName: String;
begin
  if strBreakOnLast('\', sKeyName, sKey, sName)
    then Result:=GetRegSub(sKey).GetBool(sName, aValue)
    else if ValueExists(sKeyName)
           then Result:=ReadBool(sKeyName)
           else Result:=aValue;
end;

{ funkcja czyta datê, zwraca podan¹ wartoœæ gdy brak klucza }
function    TGtRegistry.GetDate(sKeyName: String; aValue: TDate=0): TDate;
var sKey, sName: String;
begin
  if strBreakOnLast('\', sKeyName, sKey, sName)
    then Result:=GetRegSub(sKey).GetDate(sName, aValue)
    else if ValueExists(sKeyName)
           then Result:=ReadDate(sKeyName)
           else Result:=aValue;
end;

{ funkcja czyta czas, zwraca podan¹ wartoœæ gdy brak klucza }
function    TGtRegistry.GetTime(sKeyName: String; aValue: TTime=0): TTime;
var sKey, sName: String;
begin
  if strBreakOnLast('\', sKeyName, sKey, sName)
    then Result:=GetRegSub(sKey).GetTime(sName, aValue)
    else if ValueExists(sKeyName)
           then Result:=ReadTime(sKeyName)
           else Result:=aValue;
end;

{ funkcja czyta datê i czas, zwraca podan¹ wartoœæ gdy brak klucza }
function    TGtRegistry.GetDateTime(sKeyName: String; aValue: TDateTime=0): TDateTime;
var sKey, sName: String;
begin
  if strBreakOnLast('\', sKeyName, sKey, sName)
    then Result:=GetRegSub(sKey).GetDateTime(sName, aValue)
    else if ValueExists(sKeyName)
           then Result:=ReadDateTime(sKeyName)
           else Result:=aValue;
end;

{ funkcja czyta wartoœæ wariantow¹ }
function    TGtRegistry.GetVar(sKeyName: String): Variant;
var sKey, sName: String;
begin
  Result:=UnAssigned;
  if strBreakOnLast('\', sKeyName, sKey, sName)
    then Result:=GetRegSub(sKey).GetVar(sName)
    else
  if ValueExists(sKeyName) then
    case GetDataType(sKeyName) of
      // rdBinary
      rdInteger:	    Result:=GetInt(sKeyName);
      rdString,
      rdExpandString: Result:=GetStr(sKeyName);
    else              Result:=UnAssigned;
    end;
end;

{ funkcja czyta wartoœæ tabeli wariantowej }
function    TGtRegistry.GetVarArray(sKeyName: String): VariantArray;
var i,Hi,Low: Integer;
begin
  Hi :=GetInt(sKeyName + '\Hi');
  Low:=GetInt(sKeyName + '\Low');

  Result:=VarArrayCreate([Low, Hi], varVariant);
  for i:=Low to Hi do
    Result[i]:=GetStr(sKeyName + '\' + IntToStr(i));
end;

{ zapisuje wartoœæ rzeczywist¹ }
function    TGtRegistry.PutDbl(sKeyName: String; aValue: Double): Boolean;
var sKey, sName: String;
begin
  if strBreakOnLast('\', sKeyName, sKey, sName)
    then Result:=GetRegSub(sKey).PutDbl(sName, aValue)
    else begin
      ReCreateKey;
      WriteFloat(sKeyName, aValue);
      Result:=True;
    end;
end;

{ zapisuje wartoœæ ca³kowit¹ }
function    TGtRegistry.PutInt(sKeyName: String; aValue: Integer): Boolean;
var sKey, sName: String;
begin
  if strBreakOnLast('\', sKeyName, sKey, sName)
    then Result:=GetRegSub(sKey).PutInt(sName, aValue)
    else begin
      ReCreateKey;
      WriteInteger(sKeyName, aValue);
      Result:=True;
    end;
end;

{ zapisuje string }
function    TGtRegistry.PutStr(sKeyName: String; aValue: String): Boolean;
var sKey, sName: String;
begin
  if strBreakOnLast('\', sKeyName, sKey, sName)
    then Result:=GetRegSub(sKey).PutStr(sName, aValue)
    else begin
      ReCreateKey;
      WriteString(sKeyName, aValue);
      Result:=True;
    end;
end;

{ zapisuje wartoœæ logiczn¹ }
function    TGtRegistry.PutBool(sKeyName: String; aValue: Boolean): Boolean;
var sKey, sName: String;
begin
  if strBreakOnLast('\', sKeyName, sKey, sName)
    then Result:=GetRegSub(sKey).PutBool(sName, aValue)
    else begin
      ReCreateKey;
      WriteBool(sKeyName, aValue);
      Result:=True;
    end;
end;

{ zapisuje datê }
function    TGtRegistry.PutDate(sKeyName: String; aValue: TDate): Boolean;
var sKey, sName: String;
begin
  if strBreakOnLast('\', sKeyName, sKey, sName)
    then Result:=GetRegSub(sKey).PutDate(sName, aValue)
    else begin
      ReCreateKey;
      WriteDate(sKeyName, aValue);
      Result:=True;
    end;
end;

{ zapisuje czas }
function    TGtRegistry.PutTime(sKeyName: String; aValue: TTime): Boolean;
var sKey, sName: String;
begin
  if strBreakOnLast('\', sKeyName, sKey, sName)
    then Result:=GetRegSub(sKey).PutTime(sName, aValue)
    else begin
      ReCreateKey;
      WriteTime(sKeyName, aValue);
      Result:=True;
    end;
end;

{ zapisuje datê i czas }
function    TGtRegistry.PutDateTime(sKeyName: String; aValue: TDateTime): Boolean;
var sKey, sName: String;
begin
  if strBreakOnLast('\', sKeyName, sKey, sName)
    then Result:=GetRegSub(sKey).PutDateTime(sName, aValue)
    else begin
      ReCreateKey;
      WriteDateTime(sKeyName, aValue);
      Result:=True;
    end;
end;

{ zapisuje wartoœæ wariantow¹ }
function    TGtRegistry.PutVar(sKeyName: String; aValue: Variant): Boolean;
begin
  case VarType(aValue) of
    varShortInt,
    varByte,
    varWord,
//    varLongWord,
//    varInt64,
    varSmallint,
    varInteger:     Result:=PutInt(sKeyName, aValue);
    varSingle,
    varDouble,
    varCurrency:    Result:=PutDbl(sKeyName, aValue);
    varDate:        Result:=PutDateTime(sKeyName, aValue);
    varString,
    varStrArg,
    varOleStr:      Result:=PutStr(sKeyName, aValue);
    varBoolean:     Result:=PutBool(sKeyName, aValue);
  else              Result:=False;
  end;
end;

{ zapisuje wartoœæ tablicy wariantowej }
function    TGtRegistry.PutVarArray(sKeyName: String; aValue: VariantArray): Boolean;
var i,j: Integer;
    sKey, sName: String;
begin
  j:=0;
  Result:=True;

  if not strBreakOnLast('\', sKeyName, sKey, sName) then sKey:=sKeyName;
  GetRegSub(sKey).DeleteAll;

  try
    for i:=VarArrayLowBound(aValue,1) to VarArrayHighBound(aValue,1) do
      if VarIsNull(aValue[i]) or VarIsEmpty(aValue[i])
        then else begin
          Result:=Result and PutStr(sKeyName + '\' + IntToStr(i), aValue[i]);
          Inc(j);
        end;

    if j>0 then begin
      Result:=Result and PutInt(sKeyName + '\Low', VarArrayLowBound(aValue,1));
      Result:=Result and PutInt(sKeyName + '\Hi',  VarArrayHighBound(aValue,1));
    end;

    if not Result then GetRegSub(sKey).DeleteAll;
  except
    { coœ niewysz³o, kasuje klucz, lepiej niezapisaæ warianta ni¿ zapisaæ kawa³ek }
    GetRegSub(sKey).DeleteAll;
    raise;
  end;
end;

{ inkrementuje wartoœæ ca³kowit¹ }
function    TGtRegistry.IncInt(sKeyName: String; aValue: Integer=1): Integer;
begin
  Result:=aValue + GetInt(sKeyName);
  PutInt(sKeyName, Result);
end;

{ usuwa wszystkie klucze i wszystkie wartoœci z tej ga³êzi rejestru }
{ UWAGA: nie wolno robiæ w tym miejscu DeleteKey('') bo obiekt jest sk³adowany }
{        przy kolejnym dostêpie zwróci niepotrzebne wyj¹tki }
procedure   TGtRegistry.DeleteAll;
var i: Integer;
    SL: TStringList;
begin
  SL:=TStringList.Create;
  try
    GetKeyNames(SL);
    for i:=0 to SL.Count-1 do DeleteKey(SL[i]);
    GetValueNames(SL);
    for i:=0 to SL.Count-1 do DeleteValue(SL[i]);
  finally
    SL.Free;
  end;
end;

{ metoda odtwarza ga³¹Ÿ rejestru, jeœli zosta³aby usuniêta w czasie pracy programu }
{ nie ma sensu tworzyæ ga³êzi przy odczycie - nic siê wtedy nieodczyta } 
procedure   TGtRegistry.ReCreateKey;
var FPath: String;
begin
  if not KeyExists('') then begin
    FPath := CurrentPath;
    rguCreateKey(FPath);
    CloseKey;
    OpenKey(FPath, True);
  end;
end;

end.

