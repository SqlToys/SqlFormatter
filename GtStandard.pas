(* $Header: /SQL Toys/SqlParser/GtStandard.pas 2     17-12-16 19:58 Tomek $
   (c) Tomasz Gierka, github.com/SqlToys, 2003.03.30                          *)
{--------------------------------------  --------------------------------------}
unit GtStandard;

interface

uses Classes;

type VariantArray = array of Variant;

{ UWAGA: older GetAndSetBool renamed to GetAndPutBool }
function  GetAndPutBool(var B: Boolean; N: Boolean): Boolean;
function  GetAndSetBool(var B: Boolean): Boolean;
function  GetAndClrBool(var B: Boolean): Boolean;
function  BOR(var B1: Boolean; B2: Boolean): Boolean;

function  boolIf(Which: Boolean; BoolTrue: Boolean; BoolFalse: Boolean=False): Boolean;
function  strIf(Which: Boolean; StrTrue: String; StrFalse: String=''): String;
function  intIf(Which: Boolean; IntTrue: Integer; IntFalse: Integer=0): Integer;
//function  strCase(Which: Integer; Str1, Str2, Str3: String): String;

{ ³amanie stringów }
function  strBreakOnFirst(Ch: Char; Str: String; var Str1, Str2: String; aRemoveFirstChar: Boolean = True): Boolean;
function  strBreakOnFirst1(Ch: Char; Str: String; aRemoveFirstChar: Boolean = True): String;
function  strBreakOnFirst2(Ch: Char; Str: String; aRemoveFirstChar: Boolean = True): String;
function  strBreakOnLast(Ch: Char; Str: String; var Str1, Str2: String): Boolean;
function  strBreakOnLast1(Ch: Char; Str: String): String;
function  strBreakOnLast2(Ch: Char; Str: String): String;
function  strBreakOnLastNew(Ch: Char; Str: String; var Str1, Str2: String; aRemoveLastChar: Boolean = True): Boolean;
function  strBreakOnLastNew1(Ch: Char; Str: String; aRemoveLastChar: Boolean = True): String;
function  strBreakOnLastNew2(Ch: Char; Str: String; aRemoveLastChar: Boolean = True): String;
function  strBreakOn(Ch: Char; No: Integer; Str: String): String;
function  strReplace(ChFrom, ChTo: Char; Str: String): String;
function  strNoEndDot(Str: String): String;
procedure strAddSep(var Str: String; Sep,Str2: String);
function  strCountChars(Ch: Char; Str: String): Integer;
function  strCountSubStr(Ch: Char; Str: String): Integer;
function  strLeadChar(Ch: Char; Str: String; Len: Integer=2): String;

{ uzupe³nianie znaków na koñcu stringa }
function  strEndChar(Str: String; Ch: Char; EmptyIfEmpty: Boolean=True): String;
function  strEndBkSlash(Str: String; EmptyIfEmpty: Boolean=True): String;
function  strEndSlash(Str: String; EmptyIfEmpty: Boolean=True): String;
function  strEndDot(Str: String; EmptyIfEmpty: Boolean=True): String;

{ uzupe³nianie znaków na pocz¹tku stringa }
function  strBegChar(Str: String; Ch: Char; EmptyIfEmpty: Boolean=True): String;
function  strBegBkSlash(Str: String; EmptyIfEmpty: Boolean=True): String;
function  strBegSlash(Str: String; EmptyIfEmpty: Boolean=True): String;
function  strBegDot(Str: String; EmptyIfEmpty: Boolean=True): String;

{ uzupe³nianie znaków na pocz¹tku i koñcu stringa }
function  strBegEndChar(Str: String; Ch: Char; EmptyIfEmpty: Boolean=True): String;
function  strBegEndDot(Str: String; EmptyIfEmpty: Boolean=True): String;

{ funkcje typu substring }
function  strSubString(Str: String; aPos: Integer; aLen: Integer): String;
function  strLeft(Str: String; aLen: Integer): String;
function  strRight(Str: String; aLen: Integer): String;

{ cytowanie stringów }
function QuoteStr(Str: String): String;
function QuoteVar(var V: Variant): Boolean;
function QuoteVarStr(V: Variant): Variant;
function DblQuoteVarStr(V: Variant): Variant;
function UnQuoteStr(Str: String): String;

function QuoteAphStr(Str: String): String;

function Var2Str(V: Variant): String;

function DblQuoteStr(Str: String): String;
function UnDblQuoteStr(Str: String): String;

function GtGetStrDelimiter(Str: String): Char;

{ tagowane stringi }
function strRemoveTvTags(Str: String): String;

{ funkcje Min, Max }
function IntMin(a, b: Integer): Integer;
function IntMax(a, b: Integer): Integer;

{ funkcje if }
function IfZero(aInt: Integer; aIfZero: Integer=1): Integer; overload;
function IfZero(aDbl: Double;  aIfZero: Double=1): Double; overload;
function IfNull(aVar: Variant; aIfNull: Variant): Variant;

function RGB_SwapRandB(aRGB: Integer): Integer;

function HexToInt(Hex: String): Integer;
function HexToInt64(Hex: String): Int64;

{--------------------------------------  --------------------------------------}
{--------------------------- Funkcje liczenia czasu ---------------------------}

const
  GtOneHour        = 1/24;
  GtOneMinute      = 1/24/60;
  GtOneSecond      = 1/24/60/60;
  GtOneMilliSecond = 0.001/24/60/60;

const
  GtOneHalfOfSecond   = GtOneSecond/2;
  GtOneThirdOfSecond  = GtOneSecond/3;
  GtOneFourthOfSecond = GtOneSecond/4;
  GtOneFifthOfSecond  = GtOneSecond/5;

function GtNowToStr: String;
function GtDateToStr(Date: TDateTime): String;
function GtTimeToStr(Time: TDateTime): String;
function GtSuperToStr(Super: TDateTime): String;
function GtDateTimeToStr(DateTime: TDateTime): String;
function GtDaysTimeToStr(DateTime: TDateTime): String;
function GtSecsTimeToStr(DateTime: TDateTime): String;

function GtGetSuperTimeBase: Boolean; // OSTRO¯NIE SZK£O
function GtNowSuper: TDateTime;
function GtNowSuperUTC: TDateTime;
function GtNowSuperRef1000: TDateTime;
function GtNowToStrSuper: String;
function GtDateTimeToStrSuper(DateTime: TDateTime; Super: Boolean=True): String;

var CntGtNowSuper: Integer;

function GtStrToDate(Str: String): TDateTime;
function GtStrToTime(Str: String): TDateTime;
function GtStrToDateTime(Str: String): TDateTime;
function GtStrToDaysTime(Str: String): TDateTime;

function GtDateIntWeekNo(IntDateTime: Integer): Integer;

{ funkcje do realizacji blokad - oszczêdzaj¹ parê linijek kodu }
function DecToZero(var LockCnt: Integer): Boolean;
function IncFromZero(var LockCnt: Integer): Boolean;

{ funkcje do operacji na set-ach }
type
  ByteSet = Set Of Byte;

function GtSetSub(var aSet: ByteSet; aSub: ByteSet): Boolean;

function CRound(C: Currency; Dec: Integer=2): Currency;
function FRound(F: Double; Dec: Integer=2): Double;

implementation

uses Windows, SysUtils, Variants, Math;

{ zmienia wartoœæ zmiennej i zwraca jej wartoœæ sprzed zmiany }
function  GetAndPutBool(var B: Boolean; N: Boolean): Boolean;
begin
  Result:=B;
  B:=N;
end;

{ ustawia zmienna i zwraca jej wartoœæ sprzed zmiany }
function  GetAndSetBool(var B: Boolean): Boolean;
begin
  Result := GetAndPutBool(B, True);
end;

{ kasuje zmienna i zwraca jej wartoœæ sprzed zmiany }
function  GetAndClrBool(var B: Boolean): Boolean;
begin
  Result := GetAndPutBool(B, False);
end;

{ wykonuje operacjê OR na dwu wartoœciach Boolean }
{ funkcja przydatna w dwu przypadkach }
{ 1. Aby operacja zajmowa³a mniej miejsca np BOR(Result, A=B) zamiast Result:=Result or (A=B) }
{ 2. W przypadku jak wy¿ej wartoœæ drugiego wyra¿enie zostanie zawsze okreœlona, }
{    przydatne jeœli drugie wyra¿enie jest funkcj¹ }
function  BOR(var B1: Boolean; B2: Boolean): Boolean;
begin
  B1:=B1 or B2;
  Result:=B1;
end;

{ funkcja zwraca Str1 gdy Which=True lub Str2 w przeciwnym wypadku }
function  boolIf(Which: Boolean; BoolTrue: Boolean; BoolFalse: Boolean=False): Boolean;
begin
  if Which then Result := BoolTrue else Result := BoolFalse;
end;

{ funkcja zwraca Str1 gdy Which=True lub Str2 w przeciwnym wypadku }
{ funkcja przydatna przy sklejaniu stringów do zapytañ - mo¿na u¿yæ warunku w }
{ jednej operacji przypisania tekstu kwerendy bez sklejania elementów kwerendy }
function  strIf(Which: Boolean; StrTrue: String; StrFalse: String=''): String;
begin
  if Which then Result:=StrTrue else Result:=StrFalse;
end;

{ funkcja zwraca IntTrue gdy Which=True lub IntFalse w przeciwnym wypadku }
function  intIf(Which: Boolean; IntTrue: Integer; IntFalse: Integer=0): Integer;
begin
  if Which then Result:=IntTrue else Result:=IntFalse;
end;

{ dzieli string na dwa - przed i po pierwszym wyst¹pieniu znaku Ch }
{ True - jeœli podzia³ wykonany }
{ UWAGA: funkcja wycina znak oddzielaj¹cy z pocz¹tku Str2 (lub niewycina) }
function strBreakOnFirst(Ch: Char; Str: String; var Str1, Str2: String; aRemoveFirstChar: Boolean = True): Boolean;
var i: Integer;
begin
  Str1:=Str;
  Str2:='';
  Result:=False;
  i:=Pos(Ch, Str);
  if i>0 then begin
    if aRemoveFirstChar then begin
      Str1:=Copy(Str, 1, i-1);
      Str2:=Copy(Str, i+1, Length(Str)-i);
    end else begin
      Str1:=Copy(Str, 1, i-1);
      Str2:=Copy(Str, i, Length(Str)-i+1);
    end;
    Result:=True;
  end;
end;

{ zwraca pierwszy string zwracany przez strBreakOnFirst }
function  strBreakOnFirst1(Ch: Char; Str: String; aRemoveFirstChar: Boolean = True): String;
var Str2: String;
begin
  strBreakOnFirst(Ch, Str, Result, Str2, aRemoveFirstChar);
end;

{ zwraca drugi string zwracany przez strBreakOnFirst }
function  strBreakOnFirst2(Ch: Char; Str: String; aRemoveFirstChar: Boolean = True): String;
var Str1: String;
begin
  strBreakOnFirst(Ch, Str, Str1, Result, aRemoveFirstChar);
end;

{ dzieli string na dwa - przed i po ostatnim wyst¹pieniu znaku Ch }
{ True - jeœli podzia³ wykonany }
{ UWAGA: funkcja wycina znak oddzielaj¹cy z koñca Str1 (lub niewycina) }
function strBreakOnLastNew(Ch: Char; Str: String; var Str1, Str2: String; aRemoveLastChar: Boolean = True): Boolean;
var i: Integer;
begin
  Str1:='';   //Str; // was: '';      // changed on 2015-06-15 for Sql Formatter
  Str2:=Str;  //'';  // war: Str;     // changed on 2015-06-15 for Sql Formatter
  Result:=False;
  while Pos(Ch, Str2)>0 do begin
    i:=Pos(Ch, Str2);
    Str1:=Str1 + Copy(Str2, 1, i);
    Delete(Str2, 1, i);
    Result:=True;
  end;

  if Str1='' then begin // changed on 2015-06-15 for Sql Formatter
    { nie by³o zamiany }
    Str1 := Str2;
    Str2 := '';
  end else begin
    if aRemoveLastChar then Str1:=Copy(Str1,1,Length(Str1)-1);
  end;
end;

{ for backward compatibility }
function strBreakOnLast(Ch: Char; Str: String; var Str1, Str2: String): Boolean;
var i: Integer;
begin
  Str1:='';
  Str2:=Str;
  Result:=False;
  while Pos(Ch, Str2)>0 do begin
    i:=Pos(Ch, Str2);
    Str1:=Str1 + Copy(Str2, 1, i);
    Delete(Str2, 1, i);
    Result:=True;
  end;
  if (Str1<>'') then Str1:=Copy(Str1,1,Length(Str1)-1);
end;

{ zwraca pierwszy string zwracany przez strBreakOnLast }
function strBreakOnLast1(Ch: Char; Str: String{; aRemoveLastChar: Boolean = True}): String;
var Str1, Str2: String;
begin
  Result:='';
  strBreakOnLast(Ch, Str, Str1, Str2);
  Result:=Str1;
end;

{ zwraca drugi string zwracany przez strBreakOnLast }
function strBreakOnLast2(Ch: Char; Str: String{; aRemoveLastChar: Boolean = True}): String;
var Str1, Str2: String;
begin
  Result:='';
  strBreakOnLast(Ch, Str, Str1, Str2);
  Result:=Str1;
end;

{ zwraca pierwszy string zwracany przez strBreakOnLast }
function strBreakOnLastNew1(Ch: Char; Str: String; aRemoveLastChar: Boolean = True): String;
var Str1, Str2: String;
begin
  Result:='';
  strBreakOnLastNew(Ch, Str, Str1, Str2, aRemoveLastChar);
  Result:=Str1;
end;

{ zwraca drugi string zwracany przez strBreakOnLast }
function strBreakOnLastNew2(Ch: Char; Str: String; aRemoveLastChar: Boolean = True): String;
var Str1, Str2: String;
begin
  Result:='';
  strBreakOnLastNew(Ch, Str, Str1, Str2, aRemoveLastChar);
  Result:=Str1;
end;

{ zwraca N-ty ci¹g ze stringa zawieraj¹cego ci¹gi rozdzielone znakiem Ch }
{ UWAGA: kolejne ci¹gi musz¹ byæ cytowane cudzys³owem }
function strBreakOn(Ch: Char; No: Integer; Str: String): String;
var SL: TStringList;
begin
  Result:='';
  SL:=TStringList.Create;
  try
    SL.Delimiter:=Ch;
    SL.QuoteChar:='"';
    SL.DelimitedText:=Str;

    if No <= SL.Count then Result:=SL[No-1];
  finally
    SL.Free;
  end;
end;

{ zamienia w stringu wszystkie znaki ChFrom na znaki ChTo }
function strReplace(ChFrom, ChTo: Char; Str: String): String;
begin
  Result:=Str;
  while Pos(ChFrom, Result)>0 do Result[Pos(ChFrom, Result)]:=ChTo;
end;

{ wycina kropkê koñcz¹c¹ tekst w stringu }
function strNoEndDot(Str: String): String;
begin
  Result:=Trim(Str);
  if Length(Result)=0 then Exit;
  if Pos('.', Result)=0 then Exit;
  if Result[Length(Result)]='.' then Delete(Result, Length(Result), 1);
end;

{ dodaje ³añcuch Str2 do Str rozdzielaj¹c je separatorem jeœli potrzeba }
procedure strAddSep(var Str: String; Sep,Str2: String);
begin
  if Str='' then Str:=Str2 else Str:=Str + Sep + Str2;
end;

{ zlicza iloœæ wyst¹pieñ znaku Ch w ³añcuchu Str }
function  strCountChars(Ch: Char; Str: String): Integer;
var i: Integer;
begin
  Result:=0;
  repeat
    i:=Pos(Ch, Str);
    if i>0 then begin
      Inc(Result);
      if Ch<>#32 then Str[i]:=#32 else Str[i]:=#33;
    end;
  until i=0;
end;

{ zlicza iloœæ ci¹gów znaków rozdzielonych znakiem Ch }
function  strCountSubStr(Ch: Char; Str: String): Integer;
begin
  if Str=''
    then Result:=0
    else Result:=strCountChars(Ch, Str) + 1;
end;

function  strLeadChar(Ch: Char; Str: String; Len: Integer=2): String;
begin
  Result:=Str;
  while Length(Result)<Len do Result:=Ch+Result;
end;

{ uzupe³nia brakuj¹cy znak na koñcu stringa }
function  strEndChar(Str: String; Ch: Char; EmptyIfEmpty: Boolean=True): String;
begin
  if Length(Str)=0 then
    if EmptyIfEmpty then Result:='' else Result:=Ch
  else
  if Str[Length(Str)]<>Ch then Result:=Str+Ch else Result:=Str;
end;

{ uzupe³nia brakuj¹cy backslash na koñcu stringa }
function  strEndBkSlash(Str: String; EmptyIfEmpty: Boolean=True): String;
begin
  Result:=strEndChar(Str, '\', EmptyIfEmpty);
end;

{ uzupe³nia brakuj¹cy slash na koñcu stringa }
function  strEndSlash(Str: String; EmptyIfEmpty: Boolean=True): String;
begin
  Result:=strEndChar(Str, '/', EmptyIfEmpty);
end;

{ uzupe³nia brakuj¹c¹ kropkê na koñcu stringa }
function  strEndDot(Str: String; EmptyIfEmpty: Boolean=True): String;
begin
  Result:=strEndChar(Str, '.', EmptyIfEmpty);
end;

{ uzupe³nia brakuj¹cy znak na pocz¹tku stringa }
function  strBegChar(Str: String; Ch: Char; EmptyIfEmpty: Boolean=True): String;
begin
  if Length(Str)=0 then
    if EmptyIfEmpty then Result:='' else Result:=Ch
  else
  if Str[1]<>Ch then Result:=Ch+Str else Result:=Str;
end;

{ uzupe³nia brakuj¹c¹ kropkê na pocz¹tku stringa }
function  strBegBkSlash(Str: String; EmptyIfEmpty: Boolean=True): String;
begin
  Result:=strBegChar(Str, '\', EmptyIfEmpty);
end;

{ uzupe³nia brakuj¹cy slash na pocz¹tku stringa }
function  strBegSlash(Str: String; EmptyIfEmpty: Boolean=True): String;
begin
  Result:=strBegChar(Str, '/', EmptyIfEmpty);
end;

{ uzupe³nia brakuj¹c¹ kropkê na pocz¹tku stringa }
function  strBegDot(Str: String; EmptyIfEmpty: Boolean=True): String;
begin
  Result:=strBegChar(Str, '.', EmptyIfEmpty);
end;

{ uzupe³nia brakuj¹cy znak na pocz¹tku i koñcu stringa }
function  strBegEndChar(Str: String; Ch: Char; EmptyIfEmpty: Boolean=True): String;
begin
  Result:=strBegChar( strEndChar(Str, Ch, EmptyIfEmpty), Ch, EmptyIfEmpty);
end;

{ uzupe³nia brakuj¹c¹ kropkê na pocz¹tku i koñcu stringa }
function  strBegEndDot(Str: String; EmptyIfEmpty: Boolean=True): String;
begin
  Result:=strBegEndChar(Str, '.', EmptyIfEmpty);
end;

{ classic SUBSTRING function }
function  strSubString(Str: String; aPos: Integer; aLen: Integer): String;
begin
  Result := '';
  if aLen < 0 then Exit;
  if aPos = 0 then Exit;

  if aPos > 0
    then Result := Copy(Str, aPos, aLen)                    // from left side
    else Result := Copy(Str, Length(Str) + aPos + 1, aLen); // from right side
end;

{ classic LEFT function }
function  strLeft(Str: String; aLen: Integer): String;
begin
  Result := Copy(Str, 1, aLen);
end;

{ classic RIGHT function }
function  strRight(Str: String; aLen: Integer): String;
begin
  Result := Copy(Str, Length(Str) - aLen + 1, aLen);
end;

{ cytuje string }
function QuoteStr(Str: String): String;
begin
  Result:=''''+Str+'''';
end;

{ cytuje string zawarty w zmiennej wariantowej }
function QuoteVar(var V:Variant): Boolean;
begin
  Result:=False;
  if (VarType(V)=varString) or (VarType(V)=varOleStr) then begin
    Result:=True;
    V:=''''+V+'''';
  end;
end;

{ cytuje string zawarty w zmiennej wariantowej w podwójne uszy }
function DblQuoteVar(var V: Variant): Boolean;
begin
  Result:=False;
  if (VarType(V)=varString) or (VarType(V)=varOleStr) then begin
    Result:=True;
    V:='"'+V+'"';
  end;
end;

{ cytuje string zawarty w zmiennej wariantowej }
function QuoteVarStr(V:Variant): Variant;
begin
  QuoteVar(V);
  Result:=V;
end;

{ cytuje string zawarty w zmiennej wariantowej w podwójne uszy }
function DblQuoteVarStr(V: Variant): Variant;
begin
  DblQuoteVar(V);
  Result:=V;
end;

{ cytuje apostrofy w stringu }
function QuoteAphStr(Str: String): String;
begin
  Result:=StringReplace(Str, '''', ''''+'''', [rfReplaceAll, rfIgnoreCase]);
end;

{ cytuje string }
function DblQuoteStr(Str: String): String;
begin
  Result:='"'+Str+'"';
end;

{ wy³uskuje cytowany string }
function UnQuoteStr(Str: String): String;
begin
  Result:=Str;
  if (Str='') or (Length(Str)<2) then Exit;

  if (Str[1]='''') and (Str[Length(Str)]='''')
    then Result:=Copy(Str, 2, Length(Str)-2);
end;

{ wy³uskuje cytowany string }
function UnDblQuoteStr(Str: String): String;
begin
  Result:=Str;
  if (Str='') or (Length(Str)<2) then Exit;

  if (Str[1]='"') and (Str[Length(Str)]='"')
    then Result:=Copy(Str, 2, Length(Str)-2);
end;

{ wy³uskuje delimiter ze stringa }
function GtGetStrDelimiter(Str: String): Char;
begin
  Result:=';';
  if (Pos(',', Str)>0) and (Pos(';', Str)=0) then Result:=',';
end;

{ usuwa tagi ze stringa }
function strRemoveTvTags(Str: String): String;
var i: Integer;
begin
  Result := '';

  repeat
    while Pos('<', Str) = 1 do begin
      Delete(Str,1,1);
      case Str[1] of
      'B':  Delete(Str,1,1);
      'I':  Delete(Str,1,1);
      'U':  Delete(Str,1,1);
      'C':  if Copy(Str,1,5) = 'COLOR' then Delete(Str,1,5+1+1+6); // =#
      'T':  if Copy(Str,1,6) = 'TAB120' then Delete(Str,1,6) else
            if Copy(Str,1,3) = 'TAB' then Delete(Str,1,3);
      '/':  begin
              Delete(Str,1,1);
              case Str[1] of
              'B': Delete(Str,1,1);
              'I': Delete(Str,1,1);
              'U': Delete(Str,1,1);
              end;
            end;
      '>':  begin
              Result := Result + '<>';
              Delete(Str,1,1);
            end;
      end;

      if Str[1]='>' then Delete(Str,1,1);
    end;

    i := Pos('<', Str);
    if i = 0 then begin
      Result := Result + Str;
      Str := '';
    end else
    if i = 1 then begin
    end else begin
      Result := Result + Copy(Str, 1, i-1);
      Delete(Str, 1, i-1);
    end;
  until Str = '';
end;

{ zamienia variant na string, dbaj¹c o ominiêcie wyj¹tku przy konwersji typów }
function Var2Str(V: Variant): String;
begin
  if V=UnAssigned then Result:='' else Result:=V;
end;

{ zwraca mniejsza wartosc }
function IntMin(a, b: Integer): Integer;
begin
  if a < b then Result := a else Result := b;
end;

{ zwraca wieksza wartosc }
function IntMax(a, b: Integer): Integer;
begin
  if a > b then Result := a else Result := b;
end;

{ sprawdza czy parametr jest zerem, wtedy zamienia na podan¹ wartoœæ }
{ czasem przydatne przy dzieleniu w mianowniku i przy normalizacji wartoœci niezerowych }
function IfZero(aInt: Integer; aIfZero: Integer=1): Integer;
begin
  if aInt<>0
    then Result := aInt
    else Result := aIfZero;
end;

{ sprawdza czy parametr jest zerem, wtedy zamienia na podan¹ wartoœæ }
{ czasem przydatne przy dzieleniu w mianowniku i przy normalizacji wartoœci niezerowych }
function IfZero(aDbl: Double; aIfZero: Double=1): Double;
begin
  if ABS(aDbl)>1e-9
    then Result := aDbl
    else Result := aIfZero;
end;

{ sprawdza czy parametr jest pusty }
function IfNull(aVar: Variant; aIfNull: Variant): Variant;
begin
  if VarIsNull(aVar)
    then Result := aIfNull
    else Result := aVar;
end;

{ RGB: swaps R and B, zamienia endianes liczby 24-bitowej }
function RGB_SwapRandB(aRGB: Integer): Integer;
begin
  Result :=( aRGB             mod 256 { B }) * $10000 +
           ((aRGB div $100  ) mod 256 { G }) * $100   +
           ((aRGB div $10000) mod 256 { R });
end;

{ zamienia liczbê szestnastkow¹ na integer }
function HexToInt(Hex: String): Integer;
begin
  Result := 0;
  while Length(Hex)>0 do begin
    // if Hex[1] in ['0'..'9']
    if CharInSet(Hex[1], ['0'..'9'])
      then Result := 16 * Result + Ord(UpCase(Hex[1])) - Ord('0')
      else Result := 16 * Result + Ord(UpCase(Hex[1])) - Ord('A') + 10;
    Delete(Hex, 1, 1);
  end;
end;

{ zamienia liczbê szestnastkow¹ na int64 }
function HexToInt64(Hex: String): Int64;
begin
  Result := 0;
  while Length(Hex)>0 do begin
    if CharInSet(Hex[1], ['0'..'9'])
      then Result := 16 * Result + Ord(Hex[1]) - Ord('0')
      else Result := 16 * Result + Ord(Hex[1]) - Ord('A') + 10;
    Delete(Hex, 1, 1);
  end;
end;

{--------------------------------------  --------------------------------------}
{--------------------------- Funkcje liczenia czasu ---------------------------}

{ zwraca bie¿¹cy czas jako string }
function GtNowToStr: String;
begin
  Result:=GtDateTimeToStr(Now);
end;

{ zamienia datê na string w formacie YYYY-MM-DD }
function GtDateToStr(Date: TDateTime): String;
var Y,M,D: Word;
begin
  DecodeDate(Date, Y, M, D);

  Result:=strLeadChar('0', IntToStr(Y), 4)   + '-' +
          strLeadChar('0', IntToStr(M), 2)   + '-' +
          strLeadChar('0', IntToStr(D), 2);
end;

{ zamienia czas na string hh:nn:ss }
function GtTimeToStr(Time: TDateTime): String;
var H,M,Sec,MSec: Word;
begin
  DecodeTime(Time, H, M, Sec, MSec);

  Result:=strLeadChar('0', IntToStr(H), 2)   + ':' +
          strLeadChar('0', IntToStr(M), 2)   + ':' +
          strLeadChar('0', IntToStr(Sec), 2);
end;

{ zamienia czas na string z mili i mikro - sekundami }
function GtSuperToStr(Super: TDateTime): String;
begin
  Result := strLeadChar('0', IntToStr(Trunc(1e3*Frac(24*60*60*Super))), 3) + '.' +
            strLeadChar('0', IntToStr(Trunc(1e3*Frac(24*60*60*1e3*Super))), 3);
end;

{ zamienia datê i czas na string w formacie YYYY-MM-DD hh:nn:ss }
function GtDateTimeToStr(DateTime: TDateTime): String;
begin
  Result:=GtDateToStr(DateTime) + ' ' + GtTimeToStr(DateTime);
end;

{ zamienia czas na string w formacie ddd hh:nn:ss }
function GtDaysTimeToStr(DateTime: TDateTime): String;
begin
  Result:=strLeadChar('0', IntToStr(Trunc(DateTime)), 4) + ' ' +
          GtTimeToStr(DateTime);
end;

{ zamienia czas na string w formacie sss }
function GtSecsTimeToStr(DateTime: TDateTime): String;
begin
  Result:=strLeadChar('0', IntToStr(Trunc(24*60*60*DateTime)), 4);
end;

var GtQueryPerfFirst: Int64;
    GtQueryPerfFreq:  Int64;
    GtQueryDateTime: TDateTime;
    GtQueryDontCount: Integer=0;
    GtQueryCount: Integer=0;
    GtQueryCountReliable: Integer=0;
    GtQueryTimeZone: TDateTime=0;
    GtQueryReliable: Boolean;

{ ustala bazê dla metod ustalania bie¿¹cego czasu z wysok¹ precyzj¹ }
{ UWAGA: niesprawdza³em s³usnoœci - ale ponoæ warto dosyæ czêsto wo³aæ t¹ metodê }
{ w przypadku gdy komputer wchodzi w tryb uœpienia i/lub obs³uguje SpeedStep }
function GtGetSuperTimeBase: Boolean;
var TZ: TTimeZoneInformation;
    GtQueryPerfFreqLocal: Int64;
begin
  Result:=False;
  if GtQueryDontCount<>0 then Exit;

  { ustal strefê czasow¹ i zmianê czasu }
  GetTimeZoneInformation(TZ);
  GtQueryTimeZone:=TZ.Bias/24/60;

  QueryPerformanceFrequency(GtQueryPerfFreqLocal);
  if GtQueryPerfFreqLocal=GtQueryPerfFreq then Exit;

  try
    Inc(GtQueryDontCount);

    // potrzebne do super dok³adnego mierzenia czasu
    QueryPerformanceFrequency(GtQueryPerfFreq);
    QueryPerformanceCounter(GtQueryPerfFirst);
    GtQueryDateTime:=Now;

    Result:=True;
  finally
    Dec(GtQueryDontCount);
  end;
end;

{ zwraca bie¿¹cy czas z super precyzj¹ 1 mikrosekundy }
function GtNowSuper: TDateTime;
var GtQueryPerfNow: Int64;
    GtQuerySuperNow, GtQueryOrdinaryNow, GtQueryDifference: TDateTime;
begin
  Inc(CntGtNowSuper);

  try
    Inc(GtQueryDontCount);
    QueryPerformanceCounter(GtQueryPerfNow);
    GtQuerySuperNow:=GtQueryDateTime + (GtQueryPerfNow-GtQueryPerfFirst)/GtQueryPerfFreq/24/60/60;

    { weryfikuje ró¿nicê czasu - jeœli ró¿nica nie przekracza rozdzielczoœci }
    { zegara systemowego GetLocalTime - to uznajê ¿e jest OK }
    { warto przyblokowaæ oddanie czasu do systemu przed opuszczeniem tej procedury }
    GtQueryOrdinaryNow:=Now;
    GtQueryDifference:=GtQuerySuperNow - GtQueryOrdinaryNow;
    GtQueryReliable:=ABS(GtQueryDifference) < 0.015;

    { }
    if GtQueryReliable
      then Result:=GtQuerySuperNow
      else Result:=GtQueryOrdinaryNow;

    Inc(GtQueryCount);
    if GtQueryReliable then Inc(GtQueryCountReliable);
  finally
    Dec(GtQueryDontCount);
  end;
end;

{ zwraca bie¿¹cy czas z super precyzj¹ 1 mikrosekundy wg czasu uniwersalnego }
function GtNowSuperUTC: TDateTime;
begin
  Result:=GtNowSuper + GtQueryTimeZone;
end;

{ zwraca czas trwania 1000 dwukrotnych wywolan GtNowSuper }
function GtNowSuperRef1000: TDateTime;
var i: Integer;
begin
  Result := 0 - GtNowSuper;
  for i := 1 to 1000 do begin
    GtNowSuper;
    GtNowSuper;
  end;
  Result := Result + GtNowSuper;
end;

{ zwraca bie¿¹cy czas jako string z super precyzj¹ 1 mikrosekundy }
function GtNowToStrSuper: String;
begin
  Result:=GtDateTimeToStrSuper(GtNowSuper, GtQueryReliable);
end;

{ zamienia datê i czas na string w formacie YYYY-MM-DD hh:nn:ss zzzzzz }
function GtDateTimeToStrSuper(DateTime: TDateTime; Super: Boolean=True): String;
var i: Integer;
begin
  Result:=GtDateToStr(DateTime) + '   ' +
          GtTimeToStr(DateTime) + '   ' +
          GtSuperToStr(DateTime);

  { wstawia XXX w miejsca mikrosekund jeœli czas nie jest superdok³adny }
  if not Super then
    for i:=0 to 4 do
      Result[Length(Result)]:='X';
end;

{ zamienia string w formacie YYYY-MM-DD na datê }
function GtStrToDate(Str: String): TDateTime;
var S1, S2: String;
    R, M, D: Word;
begin
  Result:=0;
  if not strBreakOnFirst('-', Trim(Str), S1, S2)then Exit;
  R:=StrToInt(Copy(S1, Length(S1)-3, 4));
  if not strBreakOnFirst('-', Trim(S2), S1, S2)then Exit;
  M:=StrToInt(Copy(S1, Length(S1)-1, 2));
  D:=StrToInt(Copy(S2, 1, 2));

  Result:=EncodeDate(R, M, D);
end;

{ zamienia string w formacie hh:nn:ss na czas }
function GtStrToTime(Str: String): TDateTime;
var S1, S2: String;
    G, M, S: Word;
begin
  Result:=0;
  if not strBreakOnFirst(':', Trim(Str), S1, S2)then Exit;
  G:=StrToInt(Copy(S1, Length(S1)-1, 2));
  if not strBreakOnFirst(':', Trim(S2), S1, S2)then Exit;
  M:=StrToInt(Copy(S1, Length(S1)-1, 2));
  S:=StrToInt(Copy(S2, 1, 2));

  Result:=EncodeTime(G, M, S, 0);
end;

{ zamienia string w formacie YYYY-MM-DD hh:nn:ss na datê i czas }
function GtStrToDateTime(Str: String): TDateTime;
begin
  Result:=GtStrToDate(Str) + GtStrToTime(Str);
end;

{ zamienia string ddd hh:nn:ss na dni i czas }
function GtStrToDaysTime(Str: String): TDateTime;
begin
  Result:=StrToInt(strBreakOnFirst1(' ', Trim(Str))) + GtStrToTime(Str);
end;

{ zamienia datê i czas liczony w minutach od 2005-01-01 na numer tygodnia od 2005-01-01 }
function GtDateIntWeekNo(IntDateTime: Integer): Integer;
begin
 Result := Trunc( (IntDateTime + 5*24*60 -6*60)/60/24/7 );
end;

{ zmniejsza licznik o 1, blokuje zejœcie poni¿ej zera, zwraca True gdy ostatni dekrement }
function DecToZero(var LockCnt: Integer): Boolean;
begin
  Result:=LockCnt=1;
  if LockCnt>0 then Dec(LockCnt);
end;

{ zwiêksza licznik o 1, zwraca True gdy pierwszy inkrement }
function IncFromZero(var LockCnt: Integer): Boolean;
begin
  Result:=LockCnt=0;
  Inc(LockCnt);
  { korekta licznika, niepotrzebna bo DecToZero nieschodzi poni¿ej zera }
  { przydatne jeœli zawartoœæ licznika jest podmieniana w szczególnych przypadkach }
  { póki co niepotrzebujê }
  // if LockCnt<0 then LockCnt:=0;
end;

{ ró¿nicuje dwa zbiory wyliczeniowe, zwraca true jeœli mia³y elementy wspólne }
function GtSetSub(var aSet: ByteSet; aSub: ByteSet): Boolean;
begin
  Result := aSub * aSet <> [];
  aSet   := aSet - aSub;
end;

{ wykonuje zaokr¹glenie liczby metod¹ matematyczn¹ }
function CRound(C: Currency; Dec: Integer=2): Currency;
const WDec: array [-4..4] of Currency = ( 0.0001, 0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000 );
var T: Currency;
begin
  if Dec>4 then Dec:=4;
  if Dec<-4 then Dec:=-4;
  C := WDec[Dec] * C;
  T := Trunc(C);
  if C-T >= 0.5 then T:=T+1;
  Result := WDec[-Dec] * T;
end;

{ wykonuje zaokr¹glenie liczby metod¹ matematyczn¹ - tak jak to robimy w polsce }
function FRound(F: Double; Dec: Integer=2): Double;
const WDec: array [-6..4] of Double = ( 0.000001, 0.00001, 0.0001, 0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000 );
begin
  if (Dec>=-7) and (Dec<=4)
    then Result := Sign(F) * Trunc(Abs(F) * WDec[Dec] + 0.501) / WDec[Dec]
    else Result := Sign(F) * Trunc(Abs(F) * Power(10.0, Dec) + 0.501) / Power(10.0, Dec);
end;

begin
  CntGtNowSuper := 0;

  // potrzebne do super dok³adnego mierzenia czasu
  GtGetSuperTimeBase;
end.
