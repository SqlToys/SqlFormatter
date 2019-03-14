(* $Header: /SQL Toys/SqlFormat/SqlVersion.pas 365   19-01-21 15:18 Tomek $
   (c) Tomasz Gierka, github.com/SqlToys, 2012.09.16                          *)
{--------------------------------------  --------------------------------------}
unit SqlVersion;

interface

var VER_NAME: string;

  function VER_VERSION: string;

  function VER_CAPTION: string;

  function VER_NUMBER: string;
  function VER_DATE: string;
  function VER_BUILD: string;

function GetFileVersion_Major  (aFileName: String): Integer;
function GetFileVersion_Minor  (aFileName: String): Integer;
function GetFileVersion_Release(aFileName: String): Integer;
function GetFileVersion_ReleaseAsDateStr(aFileName: String): String;
function GetFileVersion_Build  (aFileName: String): Integer;

implementation

uses SysUtils, Windows;

{ gets file version major number }
function GetFileVersion_Major(aFileName: String): Integer;
begin
  Result := GetFileVersion(aFileName) shr 16;
end;

{ gets file version minor number }
function GetFileVersion_Minor(aFileName: String): Integer;
begin
  Result := GetFileVersion(aFileName) and $FFFF;
end;

{ gets file version release number }
function GetFileVersion_Release(aFileName: string): Integer;
var
  VerInfoSize, VerValueSize, Dummy: DWord;
  VerInfo: Pointer;
  VerValue: PVSFixedFileInfo;
begin
  Result := 0;
  VerInfoSize := GetFileVersionInfoSize(PChar(aFileName), Dummy);
  GetMem(VerInfo, VerInfoSize);
  GetFileVersionInfo(PChar(aFileName), 0, VerInfoSize, VerInfo);
  if Assigned(VerInfo) then begin
    VerQueryValue(VerInfo, '\', Pointer(VerValue), VerValueSize);
    // Major := dwFileVersionMS shr 16;
    // Minor := dwFileVersionMS and $FFFF;
    Result := VerValue^.dwFileVersionLS shr 16;
  end;
  FreeMem(VerInfo, VerInfoSize);
end;

{ gets file version release number as date in string }
function GetFileVersion_ReleaseAsDateStr(aFileName: String): String;
begin
  Result := DateToStr( GetFileVersion_Release(aFileName) + EncodeDate(2000,1,1) );
end;

{ gets file version build number }
function GetFileVersion_Build(aFileName: string): Integer;
var
  VerInfoSize, VerValueSize, Dummy: DWord;
  VerInfo: Pointer;
  VerValue: PVSFixedFileInfo;
begin
  Result := 0;
  VerInfoSize := GetFileVersionInfoSize(PChar(aFileName), Dummy);
  GetMem(VerInfo, VerInfoSize);
  GetFileVersionInfo(PChar(aFileName), 0, VerInfoSize, VerInfo);
  if Assigned(VerInfo) then begin
    VerQueryValue(VerInfo, '\', Pointer(VerValue), VerValueSize);
    Result := VerValue^.dwFileVersionLS and $FFFF;
  end;
  FreeMem(VerInfo, VerInfoSize);
end;

function VER_VERSION: string;
begin
  Result := 'ver: ' + VER_NUMBER + ', build: ' + VER_BUILD + ', date: ' + VER_DATE;
end;

function VER_CAPTION: string;
begin
  Result := VER_NAME + ' ' + VER_NUMBER;
end;

function VER_NUMBER: string;
begin
  Result := IntToStr(GetFileVersion_Major(ParamStr(0))) + '.' + IntToStr(GetFileVersion_Minor(ParamStr(0)));
end;

function VER_DATE: string;
begin
  Result := GetFileVersion_ReleaseAsDateStr(ParamStr(0));
end;

function VER_BUILD: string;
begin
  Result := '775'; // current build
end;

end.
