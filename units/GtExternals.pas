(* $Header: /SQL Toys/SqlFormat/GtExternals.pas 38    18-01-28 12:47 Tomek $
   (c) Tomasz Gierka, github.com/SqlToys, 2010.10.08                          *)
{--------------------------------------  --------------------------------------}
{$IFDEF RELEASE}
  {$DEBUGINFO OFF}
  {$LOCALSYMBOLS OFF}
{$ENDIF}
unit GtExternals;

interface

uses Classes;

{------------------ General Hash Functions (c) Arash Partow -------------------}
//function Hash_RS_S32 (Str: String): LongInt;
//function Hash_RS_U32 (Str: String): Cardinal;
//
//function Hash_DJB_S32 (Str: String): LongInt;
//function Hash_DJB_U32 (Str: String): Cardinal;

function Hash_DJB_TGI_U32 (Str: String): Cardinal;

var CntHash: Integer;

function miscel_crc8(Buffer: String; Polynom: Cardinal = $07; Initial: Cardinal = 0): Cardinal;
function miscel_crc8reverse(Buffer: String; Polynom: Cardinal = $E0; Initial: Cardinal = 0): Cardinal;
function miscel_crc16(Buffer: String; Polynom: Cardinal = $8005; Initial: Cardinal = 0): Cardinal;
function miscel_crc16reverse(Buffer: String; Polynom: Cardinal = $A001; Initial: Cardinal = 0): Cardinal;
function miscel_crc32(Buffer: String; Polynom: Cardinal = $04C11DB7; Initial: Cardinal = 0): Cardinal;
function miscel_crc32reverse(Buffer: String; Polynom: Cardinal = $EDB88320; Initial: Cardinal = 0): Cardinal;

function AllocMemSize: Cardinal;

implementation

{ funkcja pobrana z internetu w 2005 roku, modu³ GeneralHashFunctions }
function Hash_RS_S32 (Str: String): LongInt;
const b = 378551;
var a : LongInt;
    i : Integer;
Begin
  a      := 63689;
  Result := 0;
  for i:= 1 to Length(Str) do begin
    Result := Result * a + Ord(Str[i]);
    a      := a * b;
  end;
  Result := Result and $7FFFFFFF;
end;

{ funkcja pobrana z internetu w 2011 roku, modu³ GeneralHashFunctions }
function Hash_RS_U32 (Str: String): Cardinal;
const b = 378551;
var
  a : Cardinal;
  i : Integer;
begin
  a      := 63689;
  Result := 0;
  for i := 1 to Length(Str) do begin
    Result := Result * a + Ord(Str[i]);
    a      := a * b;
  end;
end;

{ funkcja przepisana w pcciagu z C++ }
function Hash_DJB_S32 (Str: String): LongInt;
var i: Integer;
begin
  Result := 5381;

  for i := 1 to Length(Str) do
    Result := (Result shl 5) + Result + Ord(Str[i]);

  Result := Result and $7FFFFFFF;
end;

{ funkcja przepisana w pcciagu z C++, z przeróbka na Cardinal }
function Hash_DJB_U32 (Str: String): Cardinal;
var i: Integer;
begin
  Result := 5381;

  for i := 1 to Length(Str) do
    Result := (Result shl 5) + Result + Ord(Str[i]);
end;

{ funkcja DJB, z przeróbkami }
{ TGI: zmieniona wartoœæ pocz¹tkowa na 0, hash pustego stringa bêdzie zerem }
{ TGI: hash pojedynczego znaku bêdzie jego kodem ASCII }
function Hash_DJB_TGI_U32 (Str: String): Cardinal;
var i: Integer;
begin
  Inc(CntHash);

  Result := 0;

  for i := 1 to Length(Str) do
    Result := (Result shl 5) + Result + Ord(Str[i]);
end;

{ CRC-8 function from: http://www.miscel.dk/MiscEl/CRCcalculations.html }
function miscel_crc8;
var i, j: Integer;
begin
  Result := Initial;
  for i := 1 to Length(Buffer) do begin
    Result := Result xor Ord(Buffer[i]);
    for j := 0 to 7 do begin
      if (Result and $80) <> 0
        then Result := (Result shl 1) xor Polynom
        else Result :=  Result shl 1;
    end;
  end;
  Result:=Result and $FF;
end;

{ CRC-8 function from: http://www.miscel.dk/MiscEl/CRCcalculations.html }
function miscel_crc8reverse;
var i, j: Integer;
begin
  Result:=Initial;
  for i := 1 to Length(Buffer) do begin
    Result := Result xor ord(buffer[i]);
    for j := 0 to 7 do begin
      if (Result and $01) <> 0
        then Result := (Result shr 1) xor Polynom
        else Result :=  Result shr 1;
    end;
  end;
end;

{ CRC-16 function from: http://www.miscel.dk/MiscEl/CRCcalculations.html }
function miscel_crc16(Buffer: String; Polynom: Cardinal = $8005; Initial: Cardinal = 0): Cardinal;
var i, j: Integer;
begin
  Result := Initial;
  for i := 1 to Length(Buffer) do begin
    Result := Result xor (ord(buffer[i]) shl 8);
    for j := 0 to 7 do begin
      if (Result and $8000) <> 0
        then Result := (Result shl 1) xor Polynom
        else Result :=  Result shl 1;
    end;
  end;
  Result:=Result and $ffff;
end;

{ CRC-16 function from: http://www.miscel.dk/MiscEl/CRCcalculations.html }
function miscel_crc16reverse;
var i, j: Integer;
begin
  Result := Initial;
  for i := 1 to Length(Buffer) do begin
    Result := Result xor ord(buffer[i]);
    for j := 0 to 7 do begin
      if (Result and $0001)<>0
        then Result := (Result shr 1) xor Polynom
        else Result :=  Result shr 1;
    end;
  end;
end;

{ CRC-32 function from: http://www.miscel.dk/MiscEl/CRCcalculations.html }
function miscel_crc32;
var i, j: Integer;
begin
  Result := Initial;
  for i := 1 to Length(Buffer) do begin
    Result := Result xor (ord(buffer[i]) shl 24);
    for j := 0 to 7 do begin
      if (Result and $80000000) <> 0
        then Result := (Result shl 1) xor Polynom
        else Result :=  Result shl 1;
    end;
  end;
end;

{ CRC-32 function from: http://www.miscel.dk/MiscEl/CRCcalculations.html }
function miscel_crc32reverse;
var i, j: Integer;
begin
  Result := Initial;
  for i := 1 to Length(Buffer) do begin
    Result := Result xor ord(buffer[i]);
    for j := 0 to 7 do begin
      if (Result and $00000001) <> 0
        then Result := (Result shr 1) xor Polynom
        else Result :=  Result shr 1;
    end;
  end;
end;

{ funkcja pobrana z internetu w 2014 roku, przy zmianie kompilatora na XE7 }
{ http://pastebin.com/w3sWfi94 }
function AllocMemSize: Cardinal;
{$IF CompilerVersion >= 18}
var
  I: Integer;
  MemMgrState: TMemoryManagerState;
{$IFEND}
begin
{$IF CompilerVersion < 18}
  Result := AllocMemSize;
{$ELSE}
  GetMemoryManagerState(MemMgrState);
  Result := MemMgrState.TotalAllocatedMediumBlockSize +
    MemMgrState.TotalAllocatedLargeBlockSize;
  for I := 0 to High(MemMgrState.SmallBlockTypeStates) do
    Result := Result + MemMgrState.SmallBlockTypeStates[I].InternalBlockSize +
      MemMgrState.SmallBlockTypeStates[I].UseableBlockSize;
{$IFEND}
end;

{--------------------------------------  --------------------------------------}

begin
  CntHash := 0;
end.

