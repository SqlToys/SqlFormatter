(* $Header: /SQL Toys/units/SqlLister.pas 358   19-04-17 19:02 Tomek $
   (c) Tomasz Gierka, github.com/SqlToys, 2010.08.18                          *)
{--------------------------------------  --------------------------------------}
{$IFDEF RELEASE}
  {$DEBUGINFO OFF}
  {$LOCALSYMBOLS OFF}
{$ENDIF}
unit SqlLister;

interface

uses Classes,
     GtTokenizers, SqlNode, SqlParser;

{------------------------------ Class Hierarchy -------------------------------}

{  TGtSqlProtoLister                                                           }
{    TGtSqlTokenLister                       TokenList -> Script               }
{    TGtSqlFormatLister                      SyntaxTree -> Script - TO CHANGE  }

{------------------------------- redeclaration --------------------------------}

type
  TFontStyle = ( fsBold, fsItalic, fsUnderline, fsStrikeOut );
  TFontStyles = set of TFontStyle;

{-------------------------------- Options Types -------------------------------}

type
  TGtSqlFormattingOption   =( gtfoText, gtfoHtml, gtfoTreeView, gtfoRtf );

  TGtSqlListerOptions      =( gtloTableConstraint, gtloAlterTableConstraint, gtloSameAsPrevClause, gtloSingleColumn );
  TGtSqlListerOptionsSet   =set of TGtSqlListerOptions;

{--------------------------------- SQL Lister ---------------------------------}

type
  TGtSqlProtoLister = class
  protected //private
    FKeywordStyle: TGtLexTokenStyle;
  protected
    RawText, FormText: String;
    BracketLevel: Integer;
    CaseLevel: Integer;

    FormColors : array [TGtLexTokenStyle, gtfoHtml .. High(TGtSqlFormattingOption)] of String;
    FormStyles : array [TGtLexTokenStyle] of TFontStyles;

    { list methods }
    procedure   AddSpace(aIle: Integer = -1);
    procedure   AddCurrLine; virtual;
    procedure   AddEmptyLine(aIle: Integer = -1);

    function    BracketLevelStyle(aToken: TGtLexToken): TGtLexTokenStyle;
    function    CaseLevelStyle: TGtLexTokenStyle;

    procedure   AddStr    (aStr: String; aStyle: TGtLexTokenStyle; aAddClearSpace: Boolean = True); overload;
    procedure   AddStr    (aToken: TGtLexToken; aStyle: TGtLexTokenStyle = gtlsPlainText; aAddClearSpace: Boolean = True); overload;
    procedure   AddStr    (aNode: TGtSqlNode; aStyle: TGtLexTokenStyle); overload;

    procedure   BeginFormattedFile;
    procedure   EndFormattedFile;
  public
    SL: TStringList;
    FormattingMode: TGtSqlFormattingOption;

    constructor Create; virtual;
    destructor  Destroy; override;

    procedure   SetStyle        (aStyle: TGtLexTokenStyle; aRGB: Integer; aBold, aItalic, aUnderline: Boolean);
    function    GetColor        (aStyle: TGtLexTokenStyle): String; overload;
  end;

{----------------------------- SQL Tokens Lister ------------------------------}

  TGtSqlTokenLister = class (TGtSqlProtoLister)
  public
    procedure   List(aTokenList: TGtLexTokenList);
  end;

{----------------------------- SQL Format Lister ------------------------------}

  TGtSqlFormatLister = class (TGtSqlProtoLister)
  protected
    { list and formatting methods }
    procedure  AddCurrLine; override;
    procedure  AddClause(aClause: TGtLexToken = nil; aIntendToken: TGtLexToken = nil); overload;
    procedure  AddClauseNode(aNode: TGtSqlNode; aIntendToken: TGtLexToken = nil); overload;
    procedure  AddComma; virtual;

    procedure  AddLeftBracket(aCount: Integer=1); overload;
    procedure  AddRightBracket(aCount: Integer=1); overload;

    procedure  AddLeftBracket(aNode: TGtSqlNode; aOneLess: Boolean = False); overload;
    procedure  AddRightBracket(aNode: TGtSqlNode; aOneLess: Boolean = False); overload;

    procedure  AddNewLine(aNode: TGtSqlNode);
  private
    SubQueryLevel: Integer;
  public
    constructor Create; override;

    procedure   SaveToFile(aFileName: String);

    procedure   List         (aNode: TGtSqlNode; aListerOpt: TGtSqlListerOptionsSet);
  protected
    function   CheckSpaceNeedBeforeExpression: Boolean;

    { SQL list methods }
    procedure  List_DataType           (aNode: TGtSqlNode; aListerOpt: TGtSqlListerOptionsSet); virtual;

    procedure  List_Expr               (aNode: TGtSqlNode; aListerOpt: TGtSqlListerOptionsSet); virtual;
    procedure  List_ExprCase           (aNode: TGtSqlNode; aListerOpt: TGtSqlListerOptionsSet); virtual;
    procedure  List_ExprCast           (aNode: TGtSqlNode; aListerOpt: TGtSqlListerOptionsSet); virtual;
    procedure  List_ExprFunction       (aNode: TGtSqlNode; aListerOpt: TGtSqlListerOptionsSet); virtual;
    procedure  List_ExprColumn         (aNode: TGtSqlNode; aListerOpt: TGtSqlListerOptionsSet); virtual;

    procedure  List_ExprList           (aNode: TGtSqlNode; aListerOpt: TGtSqlListerOptionsSet); virtual;
    procedure  List_ExprTree           (aNode: TGtSqlNode; aListerOpt: TGtSqlListerOptionsSet); virtual;
    procedure  List_SetExpr            (aNode: TGtSqlNode; aListerOpt: TGtSqlListerOptionsSet); virtual;
    procedure  List_SetExprList        (aNode: TGtSqlNode; aListerOpt: TGtSqlListerOptionsSet); virtual;

    procedure  List_Cond               (aNode: TGtSqlNode; aListerOpt: TGtSqlListerOptionsSet); virtual;
    procedure  List_CondTree           (aNode: TGtSqlNode; aListerOpt: TGtSqlListerOptionsSet); virtual;

    procedure  List_ColumnDef          (aNode: TGtSqlNode; aListerOpt: TGtSqlListerOptionsSet); virtual;

    procedure  List_Constraint         (aNode: TGtSqlNode; aListerOpt: TGtSqlListerOptionsSet);
    procedure  List_PrimaryKey         (aNode: TGtSqlNode; aListerOpt: TGtSqlListerOptionsSet); virtual;
    procedure  List_ForeignKey         (aNode: TGtSqlNode; aListerOpt: TGtSqlListerOptionsSet); virtual;
    procedure  List_Unique             (aNode: TGtSqlNode; aListerOpt: TGtSqlListerOptionsSet); virtual;
    procedure  List_Check              (aNode: TGtSqlNode; aListerOpt: TGtSqlListerOptionsSet); virtual;

    procedure  List_TabRef             (aNode: TGtSqlNode; aListerOpt: TGtSqlListerOptionsSet); virtual;

    procedure  List_CreateTable        (aNode: TGtSqlNode; aListerOpt: TGtSqlListerOptionsSet); virtual;
    procedure  List_DropTable          (aNode: TGtSqlNode; aListerOpt: TGtSqlListerOptionsSet); virtual;

    procedure  List_AlterAddColumn     (aNode: TGtSqlNode; aListerOpt: TGtSqlListerOptionsSet); virtual;
    procedure  List_AlterDropColumn    (aNode: TGtSqlNode; aListerOpt: TGtSqlListerOptionsSet); virtual;
    procedure  List_AlterModifyColumn  (aNode: TGtSqlNode; aListerOpt: TGtSqlListerOptionsSet); virtual;

    procedure  List_AlterAddConstraint (aNode: TGtSqlNode; aListerOpt: TGtSqlListerOptionsSet); virtual;
    procedure  List_AlterDropConstraint(aNode: TGtSqlNode; aListerOpt: TGtSqlListerOptionsSet); virtual;

    procedure  List_AlterRenameTable   (aNode: TGtSqlNode; aListerOpt: TGtSqlListerOptionsSet); virtual;
    procedure  List_AlterRenameColumn  (aNode: TGtSqlNode; aListerOpt: TGtSqlListerOptionsSet); virtual;

    procedure  List_AlterTable         (aNode: TGtSqlNode; aListerOpt: TGtSqlListerOptionsSet); virtual;
    procedure  List_CreateIndex        (aNode: TGtSqlNode; aListerOpt: TGtSqlListerOptionsSet); virtual;
    procedure  List_DropIndex          (aNode: TGtSqlNode; aListerOpt: TGtSqlListerOptionsSet); virtual;
    procedure  List_AlterIndex         (aNode: TGtSqlNode; aListerOpt: TGtSqlListerOptionsSet); virtual;
    procedure  List_AnalyzeIndex       (aNode: TGtSqlNode; aListerOpt: TGtSqlListerOptionsSet); virtual;

    procedure  List_AlterTrigger       (aNode: TGtSqlNode; aListerOpt: TGtSqlListerOptionsSet); virtual;

    procedure  List_CreateSequence     (aNode: TGtSqlNode; aListerOpt: TGtSqlListerOptionsSet); virtual;
    procedure  List_CreateView         (aNode: TGtSqlNode; aListerOpt: TGtSqlListerOptionsSet); virtual;
    procedure  List_CreateSynonym      (aNode: TGtSqlNode; aListerOpt: TGtSqlListerOptionsSet); virtual;

    procedure  List_Grant              (aNode: TGtSqlNode; aListerOpt: TGtSqlListerOptionsSet; aKeyword: TGtLexToken); virtual;

    procedure  List_Select             (aNode: TGtSqlNode; aListerOpt: TGtSqlListerOptionsSet); virtual;
    procedure  List_ForUpdate          (aNode: TGtSqlNode; aListerOpt: TGtSqlListerOptionsSet); virtual;

    procedure  List_SetOp              (aNode: TGtSqlNode; aListerOpt: TGtSqlListerOptionsSet); virtual;
    procedure  List_On                 (aNode: TGtSqlNode; aListerOpt: TGtSqlListerOptionsSet); virtual;
    procedure  List_Values             (aNode: TGtSqlNode; aListerOpt: TGtSqlListerOptionsSet); virtual;
    procedure  List_Fields             (aNode: TGtSqlNode; aListerOpt: TGtSqlListerOptionsSet); virtual;

    procedure  List_Tables             (aNode: TGtSqlNode; aListerOpt: TGtSqlListerOptionsSet); virtual;

    procedure  List_DML                (aNode: TGtSqlNode; aListerOpt: TGtSqlListerOptionsSet); virtual;

    procedure  List_NotRecognized      (aNode: TGtSqlNode; aListerOpt: TGtSqlListerOptionsSet); virtual;

    procedure  List_Clause_Name        (aNode: TGtSqlNode; aListerOpt: TGtSqlListerOptionsSet;
                                        aClauseToken1: TGtLexToken; aClauseToken2: TGtLexToken;
                                        aName: String='';
                                        aNameStyle: TGtLexTokenStyle=gtlsPlainText;
                                        aKeywordStyle: TGtLexTokenStyle=gtlsPlainText); virtual;
    procedure  List_Clause_Expr        (aNode: TGtSqlNode; aListerOpt: TGtSqlListerOptionsSet; aClauseToken: TGtLexToken); virtual;
    procedure  List_Clause_Cond        (aNode: TGtSqlNode; aListerOpt: TGtSqlListerOptionsSet; aClauseToken: TGtLexToken); virtual;
  public
    procedure  List_SqlParser          (aNode: TGtSqlParser);
  end;


{--------------------------------- Formatting ---------------------------------}
const
  gtHtmlNewLine = '<BR>';
  gtRtfNewLine = '\par ';
  gtRtfNewLine_trim = '\par';

implementation

uses SysUtils, GtStandard, SqlCommon;

{--------------------------------- SQL Lister ---------------------------------}

{ class constructor }
constructor TGtSqlProtoLister.Create;
begin
  inherited Create;

  FKeywordStyle   := gtlsKeyword;

  RawText         := '';
  FormText        := '';
  FormattingMode  := gtfoRtf;
  BracketLevel    := 0;
  CaseLevel       := 0;

  SL := TStringList.Create;
  SL.DefaultEncoding := TEncoding.ANSI;
end;

{ class destructor }
destructor TGtSqlProtoLister.Destroy;
begin
  if Assigned(SL) then SL.Free;

  inherited Destroy;
end;

{ sets color }
procedure TGtSqlProtoLister.SetStyle (aStyle: TGtLexTokenStyle; aRGB: Integer; aBold, aItalic, aUnderline: Boolean);
var R, G, B: Byte;
begin
  R := (aRGB shr 16) and 255;
  G := (aRGB shr  8) and 255;
  B :=  aRGB         and 255;

  FormColors [aStyle, gtfoHtml]     :=  'COLOR=#' + IntToHex(R,2) + IntToHex(G,2) + IntToHex(B,2);
  FormColors [aStyle, gtfoTreeView] := '<COLOR=#' + IntToHex(R,2) + IntToHex(G,2) + IntToHex(B,2) + '>';
  FormColors [aStyle, gtfoRtf]      := '\red' + IntToStr(R) + '\green' + IntToStr(G) + '\blue' + IntToStr(B);

  if aBold then FormStyles [aStyle] := FormStyles [aStyle] + [fsBold];
  if aItalic then FormStyles [aStyle] := FormStyles [aStyle] + [fsItalic];
  if aUnderline then FormStyles [aStyle] := FormStyles [aStyle] + [fsUnderline];
end;

{ gets color for current formatting }
function  TGtSqlProtoLister.GetColor (aStyle: TGtLexTokenStyle): String;
begin
  case FormattingMode of
    gtfoText  : Result := '';
    gtfoRtf   : Result := '\cf' + IntToStr( Ord(aStyle) +1 ) + #32;
  else          Result := FormColors [aStyle, FormattingMode];
  end;
end;

{ adds clear space }
procedure TGtSqlProtoLister.AddSpace;
var sSpace: String;
    i: Integer;
begin
  if (aIle = -1) and (RawText = '') or (aIle = 0) then Exit;

  { space already added ? }
  if (aIle = -1) and (Copy(RawText, Length(RawText) - 1 + 1, 1) = ' ') then Exit;

  if FormattingMode = gtfoHtml then sSpace := '&nbsp;' else sSpace := ' ';

  if aIle = -1 then begin
    RawText  := RawText  + ' ';
    FormText := FormText + sSpace;
  end else
  for i := 0 to aIle - 1 do begin
    RawText  := RawText  + ' ';
    FormText := FormText + sSpace;
  end;
end;

{ adds current line (RawText or FormText) to output string list, commits current line }
procedure TGtSqlProtoLister.AddCurrLine;
begin
  case FormattingMode of
    gtfoHtml : SL.Add(FormText + gtHtmlNewLine);
    gtfoRtf  : SL.Add(FormText + gtRtfNewLine)
  else         SL.Add(RawText);
  end;

  RawText  := '';
  FormText := '';
end;

{ adds empty line }
procedure TGtSqlProtoLister.AddEmptyLine;
var i: Integer;
begin
  if Trim(RawText) <> '' then AddCurrLine else
  if aIle = -1 then begin
    if SL.Count = 0 then Exit;

    case FormattingMode of
      gtfoHtml : if Trim(SL[SL.Count-1]) = gtHtmlNewLine then Exit;
      gtfoRtf  : if Trim(SL[SL.Count-1]) = gtRtfNewLine_trim then Exit;
    else         if Trim(SL[SL.Count-1]) = '' then Exit;
    end;
  end;

  if aIle = -1 then begin
    AddCurrLine;
  end else begin
    for i := 1 to aIle do AddCurrLine;
  end;
end;

{ returns token style for nested brackets }
function TGtSqlProtoLister.BracketLevelStyle(aToken: TGtLexToken): TGtLexTokenStyle;
begin
  if aToken = gttkLeftBracket then begin
    case BracketLevel mod 6 of
      0 : Result := gtlsBracketOpen1;
      1 : Result := gtlsBracketOpen2;
      2 : Result := gtlsBracketOpen3;
      3 : Result := gtlsBracketOpen4;
      4 : Result := gtlsBracketOpen5;
    else  Result := gtlsBracketOpen6;
    end;
  end else
  if aToken = gttkRightBracket then begin
    case BracketLevel mod 6 of
      0 : Result := gtlsBracketClose1;
      1 : Result := gtlsBracketClose2;
      2 : Result := gtlsBracketClose3;
      3 : Result := gtlsBracketClose4;
      4 : Result := gtlsBracketClose5;
    else  Result := gtlsBracketClose6;
    end;
  end else Result:= gtlsPlainText;
end;

{ returns token style for nested CASEs }
function TGtSqlProtoLister.CaseLevelStyle: TGtLexTokenStyle;
begin
  case (CaseLevel - 1) mod 6 of
    0 : Result := gtlsCaseWhen1;
    1 : Result := gtlsCaseWhen2;
    2 : Result := gtlsCaseWhen3;
    3 : Result := gtlsCaseWhen4;
    4 : Result := gtlsCaseWhen5;
  else  Result := gtlsCaseWhen6;
  end;
end;

{ adds colored string }
procedure TGtSqlProtoLister.AddStr (aStr: String; aStyle: TGtLexTokenStyle; aAddClearSpace: Boolean = True);

  function LocalFormatStr(aStr: String; aStyle: TGtLexTokenStyle): String;
  var lbeg, lend: String;
  begin
    case FormattingMode of
      gtfoTreeView : begin
                       lbeg := GetColor(aStyle);
                       lend := '';

                       if fsBold      in FormStyles[aStyle] then begin lbeg := '<B>' + lbeg; end;
                       if fsItalic    in FormStyles[aStyle] then begin lbeg := '<I>' + lbeg; end;
                       if fsUnderline in FormStyles[aStyle] then begin lbeg := '<U>' + lbeg; end;
                     end;
      gtfoHtml : begin
                   lbeg := '<FONT ' + GetColor(aStyle) + '>';
                   lend := '</FONT>';

                   if fsBold      in FormStyles[aStyle] then begin lbeg := '<B>' + lbeg; lend := lend + '</B>'; end;
                   if fsItalic    in FormStyles[aStyle] then begin lbeg := '<I>' + lbeg; lend := lend + '</I>'; end;
                   if fsUnderline in FormStyles[aStyle] then begin lbeg := '<U>' + lbeg; lend := lend + '</U>'; end;
                 end;
      gtfoRtf  : begin
                   lbeg := '{'      + GetColor(aStyle);
                   lend := '}';

                   if fsBold      in FormStyles[aStyle] then lbeg := lbeg + '\b ';
                   if fsItalic    in FormStyles[aStyle] then lbeg := lbeg + '\i ';
                   if fsUnderline in FormStyles[aStyle] then lbeg := lbeg + '\ulth ';
                 end;
    else           lbeg := '';
                   lend := '';
    end;

    if FormattingMode = gtfoRtf
      then Result := lbeg + StringReplace( StringReplace( StringReplace(aStr, '\', '\\', []), '{', '\{', []), '}', '\}', []) + lend
      else Result := lbeg + aStr + lend;
  end;

  { adds to last line }
  procedure   Add(aRawStr: String; aFormStr: String='');
  begin
    if aRawStr='' then Exit;
    if aFormStr='' then aFormStr := aRawStr;

    RawText  := RawText  + aRawStr;
    if FormattingMode in [gtfoHtml, gtfoRtf, gtfoTreeView] then FormText := FormText + aFormStr;
  end;

begin
  if aStr = '' then Exit;

  { operator logiczny traktuje jako keyword a nie operator - musi wystapic spacja bo przykleji siê do identyfikatora }
  if aAddClearSpace or(aStyle = gtlsOperator) and((UpperCase(aStr)='AND') or (UpperCase(aStr)='OR') or (UpperCase(aStr)='NOT'))
    then AddSpace;

  Add(aStr, LocalFormatStr(aStr, aStyle));
end;

{ adds colored keyword and name with given style }
procedure TGtSqlProtoLister.AddStr{KeywordName}(aNode: TGtSqlNode; aStyle: TGtLexTokenStyle);
begin
  if not Assigned(aNode) then Exit;

  if not Assigned(aNode.KeywordExt) or (aNode.KeywordExt = gttkNone)
    then AddStr(aNode.Keyword)
    else AddStr(aNode.KeywordExt);
  AddStr(aNode.Name, aStyle);
end;

{ adds colored string }
procedure TGtSqlProtoLister.AddStr (aToken: TGtLexToken; aStyle: TGtLexTokenStyle = gtlsPlainText; aAddClearSpace: Boolean = True);
var lStyle: TGtLexTokenStyle;
    lStr: String;
begin
  if not Assigned(aToken) or (aToken = gttkNone) then Exit;
  if aStyle = gtlsPlainText then aStyle := aToken.TokenStyle;

  lStyle := aStyle;
  if (aToken.TokenKind = gtttKeyword) and (aStyle = gtlsKeyword) then lStyle := FKeywordStyle;

  if Assigned(aToken.SubToken2) and not (aToken.TokenStyle in [gtlsOperator, gtlsComment]) then begin
    AddStr(aToken.SubToken1, lStyle, aAddClearSpace);
    AddStr(aToken.SubToken2, lStyle, True);
    if not Assigned(aToken.SubToken3) then Exit;
    AddStr(aToken.SubToken3, lStyle, True);
    if not Assigned(aToken.SubToken4) then Exit;
    AddStr(aToken.SubToken4, lStyle, True);
    if not Assigned(aToken.SubToken5) then Exit;
    AddStr(aToken.SubToken5, lStyle, True);
    Exit;
  end;

  lStr := aToken.TokenText;

  case aToken.TokenKind of
    gtttKeyword : begin
                    if (aToken = gtkwCase) then begin
                      Inc(CaseLevel);
                      lStyle := CaseLevelStyle;
                    end else
                    if (CaseLevel > 0) and ((aToken = gtkwWhen) or (aToken = gtkwThen)
                                         or (aToken = gtkwElse)) then begin
                      lStyle := CaseLevelStyle;
                    end else
                    if (CaseLevel > 0) and (aToken = gtkwEnd) then begin
                      lStyle := CaseLevelStyle;
                      Dec(CaseLevel);
                    end;
                  end;
    gtttRelevant: if (aToken = gttkMinus) or (aToken = gttkSlash) or
                     (aToken = gttkPercent) or (aToken = gttkEqual) or
                     (aToken = gttkStarEqual) or (aToken = gttkEqualStar) or (aToken = gttkBracketPlusBracket)
                  then lStyle := gtlsOperator else
                  if aToken = gttkLeftBracket then begin
                    lStyle := BracketLevelStyle(aToken);
                    Inc(BracketLevel);
                    AddStr(lStr, lStyle, False);
                    Exit;
                  end else
                  if aToken = gttkRightBracket then begin
                    Dec(BracketLevel);
                    lStyle := BracketLevelStyle(aToken);
                    AddStr(lStr, lStyle, False);
                    Exit;
                  end;
  end;

  AddStr(lStr, lStyle, aAddClearSpace);
end;

{ formatted file header }
procedure TGtSqlProtoLister.BeginFormattedFile;

  { gets RTF color schema }
  function GetRtfColorSchema: String;
  var i: TGtLexTokenStyle;
  begin
    Result := '{\colortbl;';
    for i := Low(TGtLexTokenStyle) to High(TGtLexTokenStyle) do
      Result := Result + FormColors [i, gtfoRtf] + ';';
    Result := Result + '}';
  end;

begin
  case FormattingMode of
    gtfoHtml : SL.Insert(0, '<BODY><FONT FACE="Courier New">');
//  gtfoRtf  : SL.Insert(0, '{\rtf \b0 \i0 \ul0 ' + GetRtfColorSchema);

    { transition to XE7: RichEdit 2.0 compatiility }
    { TODO: setg codepage and language dynamically, now hardcoded to polish !!! }

//  gtfoRtf  : SL.Insert(0, '{\rtf1 \ansi \ansicpg1250 \deflang1045 \b0 \i0 \ul0 ' + GetRtfColorSchema);
    { minimal working version }
    gtfoRtf  : SL.Insert(0, '{\rtf \ansicpg1250 \b0 \i0 \ul0 ' + GetRtfColorSchema);
  end;
end;

{ formatted file footer }
procedure TGtSqlProtoLister.EndFormattedFile;
begin
  AddEmptyLine;

  case FormattingMode of
    gtfoHtml : SL.Add('</FONT></BODY>');
    gtfoRtf  : SL.Add('}');
  end;
end;

{----------------------------- SQL Tokens Lister ------------------------------}

{ token lister }
procedure TGtSqlTokenLister.List(aTokenList: TGtLexTokenList);
var i: Integer;
    s1, s2: String;
begin
  if not Assigned(aTokenList) then Exit;

  BeginFormattedFile;

  BracketLevel := 0;
  CaseLevel    := 0;

  { essential - token lister }
  for i := 0 to aTokenList.Count-1 do
    if Assigned(aTokenList[i]) then
      case aTokenList[i].TokenKind of
        gtttEndOfLine  : AddCurrLine;

        gtttEolComment : AddStr    (gttkMinusMinus.TokenText + aTokenList[i].TokenText,
                                    gtlsComment, False);
        gtttStdComment : begin
                           s2 :=     gttkSlashStar.TokenText + aTokenList[i].TokenText + gttkStarSlash.TokenText;
                           while s2 <> '' do begin
                             strBreakOnFirst(#$D, s2, s1, s2);
                             AddStr (s1, gtlsComment, False);
                             if s2 <> '' then AddCurrLine;
                           end;
                         end;
      else
        if aTokenList[i].TokenText = gttkSemicolon.TokenText then begin
          BracketLevel := 0;
          AddStr(gttkSemicolon, gtlsPlainText, False);
        end else
        if aTokenList[i].TokenText = gttkLeftBracket.TokenText
          then AddStr(gttkLeftBracket, gtlsPlainText, False)
          else
        if aTokenList[i].TokenText = gttkRightBracket.TokenText
          then AddStr(gttkRightBracket, gtlsPlainText, False)
          else
        if aTokenList[i].TokenText = gtkwCase.TokenText then begin
          Inc(CaseLevel);
          AddStr(aTokenList[i].TokenText, CaseLevelStyle, False);
        end else
        if (CaseLevel > 0) and ((aTokenList[i].TokenText = gtkwWhen.TokenText)
                             or (aTokenList[i].TokenText = gtkwThen.TokenText)
                             or (aTokenList[i].TokenText = gtkwElse.TokenText)) then begin
          AddStr(aTokenList[i].TokenText, CaseLevelStyle, False);
        end else
        if (CaseLevel > 0) and (aTokenList[i].TokenText = gtkwEnd.TokenText) then begin
          AddStr(aTokenList[i].TokenText, CaseLevelStyle, False);
          Dec(CaseLevel);
        end else
        AddStr(aTokenList[i].TokenText, aTokenList[i].TokenStyle, False);
      end;

  EndFormattedFile;
end;

{-------------------------------- SQL Formater --------------------------------}

{ class constructor }
constructor TGtSqlFormatLister.Create;
begin
  inherited Create;

  SubQueryLevel := 0;
end;

{ adds HTML EOL }
procedure TGtSqlFormatLister.SaveToFile(aFileName: String);
begin
  if RawText <> '' then AddCurrLine;

  SL.SaveToFile(aFileName);
end;

{ adds current line (RawText or FormText) to output string list, commits current line }
procedure TGtSqlFormatLister.AddCurrLine;
begin
  inherited AddCurrLine;
  AddSpace;
end;

{ adds clause to script }
procedure TGtSqlFormatLister.AddClauseNode(aNode:TGtSqlNode; aIntendToken: TGtLexToken = nil);
begin
  if not Assigned(aNode) then Exit;

  if not Assigned(aNode.KeywordExt) or (aNode.KeywordExt = gttkNone)
    then AddClause(aNode.Keyword, nil)
    else AddClause(aNode.KeywordExt, nil);
end;

{ adds clause to script }
procedure TGtSqlFormatLister.AddClause(aClause: TGtLexToken = nil; aIntendToken: TGtLexToken = nil);
begin
  if aClause = gttkComma then AddComma else AddStr(aClause);
  AddStr(aIntendToken);
end;

{ adds formatting options for comma }
procedure TGtSqlFormatLister.AddComma;
begin
  AddStr(gttkComma);
end;

{ adds left bracket }
procedure TGtSqlFormatLister.AddLeftBracket(aCount: Integer=1);
var i: Integer;
begin
  for i := 1 to aCount do AddStr(gttkLeftBracket);
end;

{ adds right bracket }
procedure TGtSqlFormatLister.AddRightBracket(aCount: Integer=1);
var i: Integer;
begin
  for i := 1 to aCount do AddStr(gttkRightBracket);
end;

{ adds left bracket }
procedure TGtSqlFormatLister.AddLeftBracket(aNode: TGtSqlNode; aOneLess: Boolean = False);
var lNode: TGtSqlNode;
    lcnt: Integer;
begin
  if not Assigned(aNode) then Exit;

  lNode := aNode.Find(gtsiNone, gttkLeftBracket);
  if not Assigned(lNode) then Exit;

  lcnt := 1;
  if lNode.Name <> '' then lcnt := StrToInt(lNode.Name);
  if aOneLess then lcnt := lcnt - 1;

  AddLeftBracket(lcnt);
end;

{ adds right bracket }
procedure TGtSqlFormatLister.AddRightBracket(aNode: TGtSqlNode; aOneLess: Boolean = False);
var lNode: TGtSqlNode;
    lcnt: Integer;
begin
  if not Assigned(aNode) then Exit;

  lNode := aNode.Find(gtsiNone, gttkLeftBracket);
  if not Assigned(lNode) then Exit;

  lcnt := 1;
  if lNode.Name <> '' then lcnt := StrToInt(lNode.Name);
  if aOneLess then lcnt := lcnt - 1;

  AddRightBracket(lcnt);
end;

{ adds NewLine when NewLineBefore node is found }
procedure TGtSqlFormatLister.AddNewLine(aNode: TGtSqlNode);
begin
  if not Assigned(aNode) then Exit;
  if aNode.KeywordAuxCheck(gttkNewLineBefore) then AddClause;
end;

{ lists item }
procedure TGtSqlFormatLister.List;
begin
  if not Assigned(aNode) then Exit;

  case aNode.Kind of
    gtsiExpr:                      List_Expr      (aNode, aListerOpt);
    gtsiExprTree:                  List_ExprTree  (aNode, aListerOpt);

    gtsiExprList:
      if aNode.Check(gtsiExprList, gtkwSelect)
                              then List_Select  (aNode, aListerOpt)
      else
      if aNode.Check(gtsiExprList, gtkwValues)
                              then List_Values  (aNode, aListerOpt)
      else                         List_ExprList(aNode, aListerOpt);

    gtsiCond                     : List_Cond      (aNode, aListerOpt);

    gtsiCondTree:
      if aNode.Check(gtsiCondTree, gtkwOn) or aNode.Check(gtsiCondTree, gtkwUsing)
        then List_On        (aNode, aListerOpt)
        else List_CondTree  (aNode, aListerOpt);

    gtsiDml                      : List_DML       (aNode, aListerOpt);

    gtsiDdl:
      if aNode.Check(gtsiDDL, gtkwCreate_Table) then List_CreateTable    (aNode, aListerOpt) else
      if aNode.Check(gtsiDDL, gtkwDrop_Table)   then List_DropTable      (aNode, aListerOpt) else
      if aNode.Check(gtsiDDL, gtkwAlter_Table)  then List_AlterTable     (aNode, aListerOpt) else
      if aNode.Check(gtsiDDL, gtkwCreate_Index) then List_CreateIndex    (aNode, aListerOpt) else
      if aNode.Check(gtsiDDL, gtkwDrop_Index)   then List_DropIndex      (aNode, aListerOpt) else
      if aNode.Check(gtsiDDL, gtkwAlter_Index)  then List_AlterIndex     (aNode, aListerOpt) else
      if aNode.Check(gtsiDDL, gtkwAnalyze_Index)then List_AnalyzeIndex   (aNode, aListerOpt) else
      if aNode.Check(gtsiDDL, gtkwCreate_Sequence)then List_CreateSequence(aNode,aListerOpt)else
      if aNode.Check(gtsiDDL, gtkwDrop_Sequence)then List_Clause_Name    (aNode, aListerOpt, gtkwDrop_Sequence, nil, aNode.Name, gtlsIdentifier, gtlsDdlDrop) else
      if aNode.Check(gtsiDDL, gtkwCreate_View)  then List_CreateView     (aNode, aListerOpt) else
      if aNode.Check(gtsiDDL, gtkwDrop_View)    then List_Clause_Name    (aNode, aListerOpt, gtkwDrop_View, nil, aNode.Name, gtlsView, gtlsDdlDrop) else
      if aNode.Check(gtsiDDL, gtkwCreate_Synonym)then List_CreateSynonym (aNode, aListerOpt) else
      if aNode.Check(gtsiDDL, gtkwDrop_Synonym) then List_Clause_Name    (aNode, aListerOpt, gtkwDrop_Synonym, nil, aNode.Name, gtlsSynonym, gtlsDdlDrop) else
      if aNode.Check(gtsiDDL, gtkwAlter_Trigger)then List_AlterTrigger   (aNode, aListerOpt);

    gtsiDcl:
      if aNode.Check(gtsiDCL, gtkwGrant)       then List_Grant       (aNode, aListerOpt, gtkwGrant) else
      if aNode.Check(gtsiDCL, gtkwDeny)        then List_Grant       (aNode, aListerOpt, gtkwDeny)  else
      if aNode.Check(gtsiDCL, gtkwRevoke)      then List_Grant       (aNode, aListerOpt, gtkwRevoke)else
      if aNode.Check(gtsiDCL, gtkwCreate_User) then List_Clause_Name (aNode, aListerOpt, gtkwCreate, gtkwUser,  aNode.Name, gtlsIdentifier, gtlsDcl) else
      if aNode.Check(gtsiDCL, gtkwCreate_Login)then List_Clause_Name (aNode, aListerOpt, gtkwCreate, gtkwLogin, aNode.Name, gtlsIdentifier, gtlsDcl);

    gtsiTcl:
      if aNode.Check(gtsiTCL, gtkwCommit)              then List_Clause_Name    (aNode, aListerOpt, gtkwCommit, nil, '', gtlsPlainText, gtlsTcl) else
      if aNode.Check(gtsiTCL, gtkwCommit_Work)         then List_Clause_Name    (aNode, aListerOpt, gtkwCommit, gtkwWork, '', gtlsPlainText, gtlsTcl) else
      if aNode.Check(gtsiTCL, gtkwCommit_Tran)         then List_Clause_Name    (aNode, aListerOpt, gtkwCommit, gtkwTran, aNode.Name, gtlsTransaction, gtlsTcl) else
      if aNode.Check(gtsiTCL, gtkwRollback)            then List_Clause_Name    (aNode, aListerOpt, gtkwRollback, nil, '', gtlsTransaction, gtlsTcl) else
      if aNode.Check(gtsiTCL, gtkwRollback_Tran)       then List_Clause_Name    (aNode, aListerOpt, gtkwRollback, gtkwTran, aNode.Name, gtlsTransaction, gtlsTcl) else
      if aNode.Check(gtsiTCL, gtkwRollback_To_Savepoint)then List_Clause_Name   (aNode, aListerOpt, gtkwRollback, gtkwTo_Savepoint, aNode.Name, gtlsTransaction, gtlsTcl) else
      if aNode.Check(gtsiTCL, gtkwSavepoint)           then List_Clause_Name    (aNode, aListerOpt, gtkwSavepoint, nil, aNode.Name, gtlsTransaction, gtlsTcl) else
      if aNode.Check(gtsiTCL, gtkwBegin_Tran)          then List_Clause_Name    (aNode, aListerOpt, gtkwBegin, gtkwTran, aNode.Name, gtlsTransaction, gtlsTcl) else
      if aNode.Check(gtsiTCL, gtkwBegin_Transaction)   then List_Clause_Name    (aNode, aListerOpt, gtkwBegin, gtkwTransaction, aNode.Name, gtlsTransaction, gtlsTcl) else
      if aNode.Check(gtsiTCL, gtkwStart_Transaction)   then List_Clause_Name    (aNode, aListerOpt, gtkwStart, gtkwTransaction, aNode.Name, gtlsTransaction, gtlsTcl) else
      if aNode.Check(gtsiTCL, gtkwEnd_Tran)            then List_Clause_Name    (aNode, aListerOpt, gtkwEnd, gtkwTran, aNode.Name, gtlsTransaction, gtlsTcl) else
      if aNode.Check(gtsiTCL, gtkwEnd_Transaction)     then List_Clause_Name    (aNode, aListerOpt, gtkwEnd, gtkwTransaction, aNode.Name, gtlsTransaction, gtlsTcl) else
      if aNode.Check(gtsiTCL, gtkwStop_Transaction)    then List_Clause_Name    (aNode, aListerOpt, gtkwStop, gtkwTransaction, aNode.Name, gtlsTransaction, gtlsTcl);

    gtsiClauseTables               : List_Tables         (aNode, aListerOpt);

    gtsiSetExpr:                     List_SetExpr            (aNode, aListerOpt);
    gtsiSetExprList:                 List_SetExprList        (aNode, aListerOpt);

    gtssWhenThenCondExpr,
    gtssClauseFields,
    gtssOtherColumnDef             : List_ColumnDef          (aNode, aListerOpt);

    gtsiTableRef                   : List_TabRef             (aNode, aListerOpt);

    gtsiProgram:
      if aNode.Check(gtsiProgram, gtkwBegin) then begin
           List_Clause_Name        (aNode, aListerOpt, gtkwBegin, nil);
      end else
      if aNode.Check(gtsiProgram, gtkwEnd) then List_Clause_Name (aNode, aListerOpt, gtkwEnd, nil) else
      if aNode.Check(gtsiProgram, gtkwReturn) then begin
           AddClause(gtkwReturn);
           List_ExprTree(aNode, aListerOpt);
      end;

    gtsiOther:
      if aNode.Check(gtsiOther, gtkwPurge_RecycleBin) or aNode.Check(gtsiOther, gtkwGo) or
         aNode.Check(gtsiOther, gtkwTruncate_Table) or aNode.Check(gtsiOther, gtkwUse)
        then List_Clause_Name (aNode, aListerOpt, aNode.Keyword, nil, aNode.Name);
  end;
end;

{ checks if space before expression is needed }
function TGtSqlFormatLister.CheckSpaceNeedBeforeExpression;
var lStr: String;
begin
  lStr := Trim(RawText);
  Result := (lStr <> '') and not CharInSet(lStr[ Length(lStr) ], [' ', ',', '(', ')', '+', '-', '*', '/', '%', '=']);
end;

{ lists data type }
procedure  TGtSqlFormatLister.List_DataType;
var lNode: TGtSqlNode;
begin
  AddStr(aNode.Keyword);

  lNode := aNode.Find(gtsiNone, gtkwSize);
  if Assigned(lNode) then begin
    AddStr(gttkLeftBracket, gtlsPlainText, False);

    AddStr(lNode.Name, gtlsNumber, False);

    lNode := aNode.Find(gtsiNone, gtkwPrec);
    if Assigned(lNode) then begin
      AddComma;
      AddStr(lNode.Name, gtlsNumber, False);
    end;

    AddStr(gttkRightBracket, gtlsPlainText, False);
  end;
end;

{ lists expression }
procedure TGtSqlFormatLister.List_Expr;
var s: String;
begin
  if not Assigned(aNode) then Exit;

  if aNode.KeywordAuxCheck(gtkwPrior) then AddStr(gtkwPrior);

  if aNode.Check(gtsiExpr, gtkwCase) then begin
    List_ExprCase (aNode, aListerOpt);
    Exit;
  end else
  if aNode.Check(gtsiExpr, gtkwCount) or aNode.Check(gtsiExpr, gtkwDistinct) or
     aNode.Check(gtsiExpr, gtkwFunction) or aNode.Check(gtsiExpr, gttkAggrFunction) then begin
    List_ExprFunction(aNode, aListerOpt);
    Exit;
  end else
  if aNode.Check(gtsiExpr, gtkwCast) then begin
    List_ExprCast(aNode, aListerOpt);
    Exit;
  end;

  AddLeftBracket(aNode);

  if aNode.Check(gtsiExpr, gttkColumnName) or aNode.Check(gtsiExpr, gttkStar) then begin
    List_ExprColumn(aNode, aListerOpt);
    AddRightBracket(aNode);
    Exit;
  end;

  if aNode.Check(gtsiExpr, gttkNumber) then begin
    s := strif(aNode.KeywordAuxCheck(gttkMinusMinus), gttkMinus.TokenText) + aNode.Name
  end else
  if aNode.Check(gtsiExpr, gttkIdentifier) then begin
    if aNode.KeywordAuxCheck(gttkMinus) then s := gttkMinus.TokenText;
    s := aNode.Name;
  end else
  if aNode.Check(gtsiExpr, gttkParameterName) then begin
    if aNode.KeywordAuxCheck(gttkMinus) then s := gttkMinus.TokenText;
    s := aNode.Name;
  end else
  if aNode.Check(gtsiExpr, gtkwVarchar) or aNode.Check(gtsiExpr, gttkFunParameter) or
     aNode.Check(gtsiExpr, gttkAnotherExpr) then s := aNode.Name ;

  if aNode.Check(gtsiExpr, gttkNumber) then AddStr(s, gtlsNumber,            CheckSpaceNeedBeforeExpression)
  else
  if aNode.Check(gtsiExpr, gtkwVarchar) then AddStr(s, gtlsString,            CheckSpaceNeedBeforeExpression)
  else
  if aNode.Check(gtsiExpr, gttkIdentifier) then
                         if UpperCase(aNode.Name) = gtkwNull.TokenText
                           then AddStr(s, gtlsNull,       CheckSpaceNeedBeforeExpression)
                           else AddStr(s, gtlsIdentifier, CheckSpaceNeedBeforeExpression)
  else
  if aNode.Check(gtsiExpr, gttkParameterName) or aNode.Check(gtsiExpr, gttkAnotherExpr)
  then AddStr(s, gtlsParameter,         CheckSpaceNeedBeforeExpression)
  else
  if aNode.Check(gtsiExpr, gttkFunParameter) then AddStr(s, gtlsFunParameter,      CheckSpaceNeedBeforeExpression);

  if aNode.Check(gtsiExpr, gttkFunParameter) then begin
    AddStr(gttkEqualGreater);
    List(aNode.Find(gtsiExprTree), aListerOpt);
  end;

  AddRightBracket(aNode);
end;

{ lists expression CASE }
procedure TGtSqlFormatLister.List_ExprCase;
var lCaseExpr, lElseExpr, lNode: TGtSqlNode;
    i: Integer;
begin
  lNode := nil;

  AddNewLine(aNode);
  AddStr(gtkwCase);

  { [expression] }
  lCaseExpr := aNode.Find(gtsiExprTree, gtkwCase);
  List_ExprTree(lCaseExpr, aListerOpt);

  { WHEN [expression | condition] THEN expression }
  for i := 0 to aNode.Count - 1 do
    if aNode[i].Kind = gtssWhenThenCondExpr then begin
      if Assigned(lCaseExpr) then begin
        lNode := aNode[i].Find(gtsiExprTree, gtkwWhen);
        AddNewLine(aNode);
        AddStr(gtkwWhen);
        List_ExprTree(lNode, aListerOpt);
      end else begin
        lNode := aNode[i].Find(gtsiCondTree, gtkwWhen);
        AddNewLine(lNode);
        AddStr(gtkwWhen);
        List_CondTree(lNode, aListerOpt);
      end;

      lNode := aNode[i].Find(gtsiExprTree, gtkwThen);
      AddNewLine(lNode);
      AddStr(gtkwThen);
      List_ExprTree(lNode, aListerOpt);
    end;

  { BNF: ELSE expression }
  lElseExpr := aNode.Find(gtsiExprTree, gtkwElse);
  if Assigned(lElseExpr) then begin
    AddNewLine(lElseExpr);
    AddStr(gtkwElse);
    List_ExprTree(lElseExpr, aListerOpt);
  end;

  AddNewLine(lNode);
  AddNewLine(lElseExpr);

  AddStr(gtkwEnd);
end;

{ lists expression CAST }
procedure  TGtSqlFormatLister.List_ExprCast;
begin
  AddStr(gtkwCast);
  AddStr(gttkLeftBracket, gtlsPlainText, False);

  List_ExprTree(aNode.Find(gtsiExprTree), aListerOpt);

  AddStr(gtkwAs);

  { keywords switch }
  aNode.Keyword := aNode.KeywordExt;

  List_DataType(aNode, aListerOpt);

  { keywords switch again }
  aNode.Keyword := gtkwCast; { List_DataType overwrites CAST keyword }

  AddStr(gttkRightBracket, gttkRightBracket.TokenStyle, True);
end;

{ lists expression function }
procedure TGtSqlFormatLister.List_ExprFunction;
var lExprList, lNode: TGtSqlNode;
begin
  { BNF: functions & COUNT }
  if aNode.Check(gtsiExpr, gtkwCount) or aNode.Check(gtsiExpr, gtkwDistinct)
    then AddStr(gtkwCount)
    else
  if (UpperCase(aNode.Name) = 'MIN') or (UpperCase(aNode.Name) = 'MAX') or
     (UpperCase(aNode.Name) = 'AVG') or
     (UpperCase(aNode.Name) = 'SUM') or (UpperCase(aNode.Name) = 'COUNT')
    then AddStr(aNode.Name, gtlsAggrFunction, CheckSpaceNeedBeforeExpression)
    else AddStr(aNode.Name, gtlsFunction, CheckSpaceNeedBeforeExpression);

  lExprList := aNode.Find(gtsiExprList);

  AddStr(gttkLeftBracket, gttkLeftBracket.TokenStyle, True);

  if aNode.Check(gtsiExpr, gtkwDistinct) then AddStr(gtkwDistinct, gtlsPlainText, False);

  List_ExprList(lExprList, aListerOpt);

  AddStr(gttkRightBracket, gttkRightBracket.TokenStyle, True);

  { ORACLE: KEEP DENSE RANK }
  lNode := aNode.Find(gtsiNone, gtkwKeep);
  if Assigned(lNode) then begin
    AddStr(gtkwKeep);
    AddLeftBracket;
    AddStr(gtkwDenseRank);
    AddStr(lNode.Name, gtlsAggrFunction);
    List_Clause_Expr(lNode.Find(gtsiExprList, gtkwOrder_By), aListerOpt, gtkwOrder_By);
    AddRightBracket;
  end;
end;

{ lists identifier }
procedure TGtSqlFormatLister.List_ExprColumn;

    function FindStyleForColumnPrefix(var aColumnPrefix: String): TGtLexTokenStyle;
    var i: Integer;
        lQuery, lTabClause, lAlias: TGtSqlNode;
    begin
      Result := gtlsError;
      if aColumnPrefix = '' then Exit;

      lQuery := aNode.GetQuery;

      while Assigned(lQuery) do begin
        lTabClause := lQuery.Find(gtsiClauseTables);
        if not Assigned(lTabClause) then Exit;

        for i := 0 to lTabClause.Count - 1 do
          if lTabClause[i].Check(gtsiTableRef) or lTabClause[i].Check(gtsiDml, gtkwSelect) then begin
            lAlias := lTabClause[i].Find(gtsiNone, gtkwAs);
            if Assigned(lAlias) and (AnsiUpperCase(lAlias.Name) = AnsiUpperCase(aColumnPrefix)) then begin
              if lQuery = aNode.GetQuery then Result := gtlsTableAlias else Result := gtlsExtQueryAliasOrTable;
              Exit;
            end else
            if not Assigned(lAlias) and (
              (AnsiUpperCase(lTabClause[i].Name) = AnsiUpperCase(aColumnPrefix))  or
              (Pos('.'+AnsiUpperCase(aColumnPrefix), AnsiUpperCase(lTabClause[i].Name)) > 0) ) then begin

              if lQuery = aNode.GetQuery then Result := gtlsTable else Result := gtlsExtQueryAliasOrTable;
              Exit;
            end;
          end;

        lQuery := lQuery.GetExtQuery;

        { ORACLE nested query tables identifiers are limited to 1 level }
        //if (Dialect = gtdlOracle) and (lDeep > 1) then Exit;
      end;
    end;

var lColumnPrefix, lColumnName: String;
begin
  strBreakOnLast('.', aNode.Name, lColumnPrefix, lColumnName);

  if lColumnPrefix <> '' then begin
    AddStr(lColumnPrefix, FindStyleForColumnPrefix(lColumnPrefix), CheckSpaceNeedBeforeExpression);
    AddStr(gttkDot, gtlsPlainText, False);
    AddStr(lColumnName, gtlsColumn, False);
  end else begin
    AddStr(lColumnName, gtlsColumn, CheckSpaceNeedBeforeExpression);
  end;
end;

{ lists expressions list }
procedure TGtSqlFormatLister.List_ExprList;
var i: Integer;
    lFirst: Boolean;
begin
  if not Assigned(aNode) then Exit;

  AddLeftBracket(aNode);

  lFirst := True;
  for i := 0 to aNode.Count - 1 do
    if aNode[i].Kind in [gtsiExpr, gtsiExprTree] then begin
      if not lFirst then AddComma;
      List(aNode[i], aListerOpt);
      lFirst := False;
    end;

  AddRightBracket(aNode);
end;

{ lists expression tree }
procedure TGtSqlFormatLister.List_ExprTree;
var i: Integer;
    lNode: TGtSqlNode;
begin
  if not Assigned(aNode) then Exit;

  AddLeftBracket(aNode);

  { list sub-expressions }
  for i := 0 to aNode.Count - 1 do
    if (aNode[i].Kind in [gtsiExpr, gtsiExprTree]) or aNode[i].Check(gtsiDml, gtkwSelect) then begin
      if i > 0 then begin
         if aNode.KeywordAuxCheck(gttkPlus) then begin
           if aNode[i].KeywordAuxCheck(gttkMinus) then AddStr(gttkMinus) else AddStr(gttkPlus);
         end else
         if aNode.KeywordAuxCheck(gttkStar) then begin
           if aNode[i].KeywordAuxCheck(gttkSlash) then AddStr(gttkSlash) else
           if aNode[i].KeywordAuxCheck(gttkPercent) then AddStr(gttkPercent) else AddStr(gttkStar);
         end else AddStr(gttkConcatenation);
      end;

      List(aNode[i], aListerOpt);
    end;

  AddRightBracket(aNode);

  { adds alias }
  { WARN: SkipOutput was prepared to check max expressions length }
  lNode := aNode.Find(gtsiNone, gtkwAs);
  if Assigned(lNode)     and
    ((Length(RawText) = 0) or (RawText[Length(RawText)] <> '*')) {skip alias after star expr.} then begin
    if lNode.KeywordAuxCheck(gtkwAs) then AddStr(gtkwAs) else AddSpace;

    AddStr(lNode.Name, gtlsColumnAlias);
  end;

  AddStr(aNode.KeywordAuxCheckKwd(gtkwAsc, gtkwAscending, gtkwDesc, gtkwDescending));

  { BNF: NULLS FIRST | NULLS LAST }
  AddStr(aNode.KeywordAuxCheckKwd(gtkwNulls_First, gtkwNulls_Last));

  { ORACLE outer join mark }
  if aNode.KeywordAuxCheck(gttkBracketPlusBracket) then begin
    AddStr(gttkBracketPlusBracket);
  end;
end;

{ lists set expressions }
procedure TGtSqlFormatLister.List_SetExpr;
var lCol, lVect, lSelect: TGtSqlNode;
begin
  if not Assigned(aNode) then Exit;
  lVect := nil;

  { left side, +1 because of identifier extra space }
  lCol := aNode.Find(gtsiExpr, gttkColumnName);

  if Assigned(lCol) then begin
    List_ExprColumn(lCol, aListerOpt);
  end else begin
    { TODO: check formatting }
    lVect := aNode.Find(gtsiExprList, gtkwSet);
    if Assigned(lVect) then begin
      List(lVect, aListerOpt);
    end;
  end;

  AddStr(gttkEqual);

  { right side }
  lSelect := aNode.Find(gtsiDml, gtkwSelect);
  if Assigned(lSelect) then begin
    List(lSelect, aListerOpt);
  end else begin
    lVect := aNode.Find(gtsiExprList, gtkwSet, '', lVect);
    if Assigned(lVect) then begin
      List(lVect, aListerOpt);
    end else begin
      List(aNode.Find(gtsiExprTree), aListerOpt);
    end;
  end;
end;

{ lists set expressions list }
procedure TGtSqlFormatLister.List_SetExprList;
var i: Integer;
    lFirst: Boolean;
begin
  if not Assigned(aNode) then Exit;

  { SET }
  AddClause(gtkwSet);

  lFirst := True;
  for i := 0 to aNode.Count - 1 do
    if aNode[i].Check(gtsiSetExpr) then begin
      if not lFirst then AddComma;

      List_SetExpr(aNode[i], aListerOpt);
      lFirst := False;
    end;
end;

{ lists condition }
procedure TGtSqlFormatLister.List_Cond;
var lExpr, lItem: TGtSqlNode;
begin
  if not Assigned(aNode) then Exit;

  AddStr(aNode.KeywordAuxCheckKwd(gtkwNot));
  AddLeftBracket(aNode);

  if (aNode.Keyword = gtkwExists) or (aNode.Keyword = gtkwNot_Exists) then begin
    AddStr(aNode.Keyword);
    List(aNode.Find(gtsiDml, gtkwSelect), aListerOpt);
  end else
  if (aNode.Keyword = gtkwIn) or (aNode.Keyword = gtkwNot_In) then begin
    List( aNode.Find(gtsiNone, nil, '1'), aListerOpt);

    AddStr(aNode.Keyword);
    AddSpace; // independent of space-outside-brackets !!

    lExpr := aNode.Find(gtsiNone, nil, '2');
    if Assigned(lExpr) and lExpr.Check(gtsiExprList, gtkwIn) then begin
      AddStr(gttkLeftBracket);

      List(lExpr, aListerOpt);

      AddStr(gttkRightBracket);
    end else begin
      List(lExpr, aListerOpt);
    end;
  end else begin
    { left expression }
    lItem := aNode.Find(gtsiNone, nil, '1');

    List(lItem, aListerOpt);

    { operator }
    if (aNode.Keyword = gttkEqual) then begin
        AddStr(gttkEqual);
    end else
    if (aNode.Keyword = gtkwBetween) or (aNode.Keyword = gtkwNot_Between) or
       (aNode.Keyword = gtkwLike)    or (aNode.Keyword = gtkwNot_Like) or
       (aNode.Keyword = gtkwIs_Null) or (aNode.Keyword = gtkwIs_Not_Null)
      then AddStr(aNode.Keyword)
      else AddStr(aNode.Keyword);

    { right expression }
    lItem := aNode.Find(gtsiNone, nil, '2');

    List(lItem, aListerOpt);

    { additional expression }
    if (aNode.Keyword = gtkwBetween) or (aNode.Keyword = gtkwNot_Between) then begin
      AddStr(gtkwAnd);
      List(aNode.Find(gtsiNone, nil, '3'), aListerOpt );
    end else
    if ((aNode.Keyword = gtkwLike) or (aNode.Keyword = gtkwNot_Like)) then begin
      AddStr{KeywordName}( aNode.Find(gtsiNone, gtkwEscape), gtlsString);
    end;

    { collate }
    AddStr{KeywordName}( aNode.Find(gtsiNone, gtkwCollate), gtlsString );
  end;

  AddRightBracket(aNode);
end;

{ lists condition tree }
procedure TGtSqlFormatLister.List_CondTree;
var b: Boolean;
    i: Integer;
begin
  if not Assigned(aNode) then Exit;

  if aNode.KeywordAuxCheck(gtkwNoCycle) then AddStr(gtkwNoCycle);
  AddStr(aNode.KeywordAuxCheckKwd(gtkwNot));
  AddLeftBracket(aNode);

  b := False;
  for i := 0 to aNode.Count - 1 do
    if aNode[i].Check(gtsiCond) or aNode[i].Check(gtsiCondTree) then begin
      if b then AddStr(aNode.KeywordExt);
      List(aNode[i], aListerOpt);
      b := True;
    end;

  AddRightBracket(aNode);
end;

{ lists column }
procedure TGtSqlFormatLister.List_ColumnDef;
var lUnique, lDefault, lNode: TGtSqlNode;
    i: Integer;
begin
  if not Assigned(aNode) then Exit;

  AddStr(aNode.Name, gtlsColumn);

  if aNode.Keyword = gtkwType then begin
    lNode := aNode.Find(gtsiNone, gttkTableName);
    if Assigned(lNode) then begin
      AddStr(lNode.Name, gtlsTable);
      AddStr(gttkDot, gtlsPlainText, False);
      lNode := lNode.Find(gtsiNone, gttkColumnName);
      if Assigned(lNode) then AddStr(aNode.Name, gtlsColumn, False);
    end;

    AddStr(gttkPercent, gtlsPlainText, False);
    AddStr(gtkwType, gtlsPlainText, False);
  end else begin
    List_DataType(aNode, aListerOpt);
  end;

  lNode := aNode.Find(gtsiNone, gtkwIdentity);
  if Assigned(lNode) then begin
    AddStr(gtkwIdentity);
    if Assigned(lNode.Find(gtsiNone, gtkwSize)) then begin
      AddStr(gttkLeftBracket);
      AddStr(lNode.Find(gtsiNone, gtkwSize).Name, gtlsNumber, False);

      if Assigned(lNode.Find(gtsiNone, gtkwPrec)) then begin
        AddComma;
        AddStr(lNode.Find(gtsiNone, gtkwPrec).Name, gtlsNumber, False);
      end;
      AddStr(gttkRightBracket);
    end;
  end;

  AddStr{KeywordName}( aNode.Find(gtsiNone, gtkwCollate), gtlsString );

  lDefault := aNode.Find(gtsiExprTree);
  if Assigned(lDefault) then begin
    AddStr(gtkwDefault);
    List_ExprTree(lDefault, aListerOpt);
  end;

  AddStr(aNode.KeywordAuxCheckKwd(gtkwNull, gtkwNot_Null));

  lUnique := aNode.Find(gtsiDDL, gtkwCreate_Index);
  if Assigned(lUnique) and lUnique.KeywordExt.HasSubToken(gtkwUnique) then AddStr(gtkwUnique);

  for i := 0 to aNode.Count - 1 do begin
    if (aNode[i].Kind = gtsiConstraint) then begin
      AddNewLine(aNode[i]);
      List_Constraint(aNode[i], aListerOpt + [gtloSingleColumn]);
    end;
  end;
end;

procedure TGtSqlFormatLister.List_Constraint;
begin
  if not Assigned(aNode) then Exit;

  if aNode.Check(gtsiConstraint, gtkwPrimary_Key) then List_PrimaryKey(aNode, aListerOpt) else
  if aNode.Check(gtsiConstraint, gtkwForeign_Key) then List_ForeignKey(aNode, aListerOpt) else
  if aNode.Check(gtsiConstraint, gtkwUnique)      then List_Unique    (aNode, aListerOpt) else
  if aNode.Check(gtsiConstraint, gtkwCheck)       then List_Check     (aNode, aListerOpt);
end;

{ lists primary key }
procedure TGtSqlFormatLister.List_PrimaryKey;
begin
  if not Assigned(aNode) then Exit;

  if gtloAlterTableConstraint in aListerOpt then begin
    if aNode.Name <> '' then begin
      AddClause(gtkwAdd_Constraint);
      AddStr(aNode.Name, gtlsConstraint);

      AddClause(gtkwPrimary_Key);
    end else begin
      AddClause(gtkwAdd_Primary_Key);
    end;
  end else begin
    if aNode.Name <> '' then begin
      AddStr(gtkwConstraint);
      AddStr(aNode.Name, gtlsConstraint);
    end;

    AddStr(gtkwPrimary_Key);
  end;

  if (gtloAlterTableConstraint in aListerOpt) or (gtloTableConstraint in aListerOpt) then begin
    AddStr(gttkLeftBracket);
    List_ExprList(aNode, aListerOpt);
    AddStr(gttkRightBracket);
  end;
end;

{ lists foreign key }
procedure TGtSqlFormatLister.List_ForeignKey;
var lKey: TGtSqlNode;
begin
  if not Assigned(aNode) then Exit;

  if gtloAlterTableConstraint in aListerOpt then begin
    if gtloSingleColumn in aListerOpt then begin
      if aNode.Name <> '' then begin
        AddClause(gtkwConstraint);
        AddStr(aNode.Name, gtlsConstraint);
      end;
    end else begin
      if aNode.Name <> '' then begin
        AddClause(gtkwAdd_Constraint);
        AddStr(aNode.Name, gtlsConstraint);

        AddClause(gtkwForeign_Key);
      end else begin
        AddClause(gtkwAdd_Foreign_Key);
      end;

      AddStr(gttkLeftBracket);
      List_ExprList(aNode, aListerOpt);
      AddStr(gttkRightBracket);
    end;
  end else begin
    if aNode.Name <> '' then begin
      AddStr(gtkwConstraint);
      AddStr(aNode.Name, gtlsConstraint);
    end;

    if not (gtloSingleColumn in aListerOpt) then begin
      AddStr(gtkwForeign_Key);

      AddStr(gttkLeftBracket);
      List_ExprList(aNode, aListerOpt);
      AddStr(gttkRightBracket);
    end;
  end;

  if gtloAlterTableConstraint in aListerOpt
    then AddClause(gtkwReferences)
    else AddStr(gtkwReferences);

  lKey := aNode.Find(gtsiConstraint, gtkwReferences);
  if Assigned(lKey) then begin
    AddStr(lKey.Name, gtlsTable);

    if lKey.Count > 0 then begin
      AddStr(gttkLeftBracket);
      List_ExprList(lKey, aListerOpt);
      AddStr(gttkRightBracket);
    end;
  end;

  if aNode.KeywordAuxCheck(gtkwOn_Delete_Restrict, gtkwOn_Delete_Cascade, gtkwOn_Delete_Set_Null) then begin
    if gtloAlterTableConstraint in aListerOpt
      then AddClause(gtkwOn_Delete)
      else AddStr(gtkwOn_Delete);
   if aNode.KeywordAuxCheck(gtkwOn_Delete_Restrict) then AddStr(gtkwRestrict) else
   if aNode.KeywordAuxCheck(gtkwOn_Delete_Cascade)  then AddStr(gtkwCascade)  else
   if aNode.KeywordAuxCheck(gtkwOn_Delete_Set_Null) then AddStr(gtkwSet_Null);
  end;

  if aNode.KeywordAuxCheck(gtkwOn_Update_Restrict, gtkwOn_Update_Cascade, gtkwOn_Update_Set_Null) then begin
    if gtloAlterTableConstraint in aListerOpt
      then AddClause(gtkwOn_Update)
      else AddStr(gtkwOn_Update);
   if aNode.KeywordAuxCheck(gtkwOn_Update_Restrict) then AddStr(gtkwRestrict) else
   if aNode.KeywordAuxCheck(gtkwOn_Update_Cascade)  then AddStr(gtkwCascade)  else
   if aNode.KeywordAuxCheck(gtkwOn_Update_Set_Null) then AddStr(gtkwSet_Null);
  end;
end;

{ lists index }
procedure TGtSqlFormatLister.List_Unique;
begin
  if not Assigned(aNode) then Exit;

  if gtloAlterTableConstraint in aListerOpt then begin
    if aNode.Name <> '' then begin
      AddClause(gtkwAdd_Constraint);
      AddStr(aNode.Name, gtlsConstraint);

      AddClause(gtkwUnique);
    end else begin
      AddClause(gtkwAdd_Unique);
    end;
  end else begin
    if aNode.Name <> '' then begin
      AddStr(gtkwConstraint);
      AddStr(aNode.Name, gtlsConstraint);
    end;

    AddStr(gtkwUnique);
  end;

  if (gtloTableConstraint in aListerOpt) or (gtloAlterTableConstraint in aListerOpt) then begin
    AddStr(gttkLeftBracket);
    List_ExprList(aNode, aListerOpt);
    AddStr(gttkRightBracket);
  end;
end;

{ lists check }
procedure TGtSqlFormatLister.List_Check;
begin
  if not Assigned(aNode) then Exit;

  if gtloAlterTableConstraint in aListerOpt then begin
    if aNode.Name <> '' then begin
      AddClause(gtkwAdd_Constraint);
      AddStr(aNode.Name, gtlsConstraint);

      AddClause(gtkwCheck);
    end else begin
      AddClause(gtkwAdd_Check);
    end;
  end else begin
    if aNode.Name <> '' then begin
      AddStr(gtkwConstraint);
      AddStr(aNode.Name, gtlsConstraint);
    end;

    AddStr(gtkwCheck);
  end;

  AddStr(gttkLeftBracket);
  List(aNode.Find(gtsiCondTree), aListerOpt);
  AddStr(gttkRightBracket);
end;

{ lists table reference }
procedure TGtSqlFormatLister.List_TabRef;
var lSubQuery, lNode: TGtSqlNode;
begin
  if not Assigned(aNode) then Exit;

  if (aNode.Keyword = gttkNone) and aNode.GetQuery.Check(gtsiDml, gtkwUpdate)
    then AddClause(gtkwUpdate)
    else
  if (aNode.Keyword = gtkwInto) and aNode.GetQuery.Check(gtsiDml, gtkwInsert) then begin
    AddClause(aNode.KeywordExt);
  end else
  if (aNode.Keyword = gttkComma) then begin
      AddComma;
      AddClause(nil);
  end else AddClauseNode( aNode );

  { table name or query }
  lSubQuery := aNode.Find(gtsiDml, gtkwSelect);
  if Assigned(lSubQuery) then begin
    { query }
    List(lSubQuery, aListerOpt);

    { query alias }
    lNode := aNode.Find(gtsiNone, gtkwAs);
    if Assigned(lNode) then begin
      if lNode.KeywordAuxCheck(gtkwAs) then AddStr(gtkwAs);
      AddStr(lNode.Name, gtlsTableAlias);
    end;
  end else begin
    { table name }
    AddStr(aNode.Name, gtlsTable);

    { table alias, +1 because of identifier extra space }
    lNode := aNode.Find(gtsiNone, gtkwAs);
    if Assigned(lNode) then begin
      if lNode.KeywordAuxCheck(gtkwAs) then AddStr(gtkwAs);
      AddStr(lNode.Name, gtlsTableAlias);
    end;
  end;

  { join condition }
  if (aNode.Keyword = gtkwInner) or (aNode.Keyword = gtkwLeft) or
     (aNode.Keyword = gtkwRight) or (aNode.Keyword = gtkwFull) then begin

    List(aNode.Find(gtsiCondTree, gtkwOn), aListerOpt);
    List(aNode.Find(gtsiCondTree, gtkwUsing), aListerOpt);
  end;
end;

{ lists CREATE TABLE }
procedure TGtSqlFormatLister.List_CreateTable;
var lItem: TGtSqlNode;
    j, cnt, cnt2: Integer;
begin
  if not Assigned(aNode) then Exit;
  FKeywordStyle := gtlsDdlCreate;

  { list: CREATE [[GLOBAL] TEMPORARY] TABLE table-name }
  AddStr{KeywordName}(aNode, gtlsTable);

  { BNF: [AS SELECT ...] }
  lItem := aNode.Find(gtsiDml, gtkwSelect);
  if Assigned(lItem) then begin
    AddStr(gtkwAs);
    List_DML(lItem, aListerOpt);
    Exit;
  end;

  { list: spacings }
  AddClause(gttkLeftBracket);

  { list columns }
  cnt := 0;
  cnt2 := 0;
  for j := 0 to aNode.Count - 1 do
    case aNode[j].Kind of
      gtssOtherColumnDef : begin

      if cnt > 0 then begin
          AddComma;
          AddClause(nil);
      end;

      List_ColumnDef(aNode[j], aListerOpt);

      Inc(cnt);
    end;
      gtsiConstraint : begin
        if (cnt2 = 0) then AddNewLine(aNode[j]);

        if cnt > 0 then begin
            AddComma;
            AddClause(nil);
        end;

        List_Constraint(aNode[j], aListerOpt + [gtloTableConstraint]);

        Inc(cnt);
        Inc(cnt2);
      end;
    end;

  AddClause(gttkRightBracket);

  AddStr(aNode.KeywordAuxCheckKwd(gtkwOn_Commit_Preserve_Rows, gtkwOn_Commit_Delete_Rows));
end;

{ lists DROP TABLE statement }
procedure TGtSqlFormatLister.List_DropTable;
begin
  if not Assigned(aNode) then Exit;

  List_Clause_Name (aNode, aListerOpt, gtkwDrop_Table, nil, aNode.Name, gtlsTable, gtlsDdlDrop);

  AddStr(aNode.KeywordAuxCheckKwd(gtkwCascade_Constraints));
  AddStr(aNode.KeywordAuxCheckKwd(gtkwPurge));
end;

{ lists ADD COLUMN clause }
procedure TGtSqlFormatLister.List_AlterAddColumn;
var i: Integer;
    lFirst: Boolean;
begin
  if not Assigned(aNode) then Exit;
  FKeywordStyle := gtlsDdlCreate;

  if not (gtloSameAsPrevClause in aListerOpt) then AddClause(aNode.Keyword);

  AddLeftBracket(aNode);

  lFirst := True;
  for i := 0 to aNode.Count - 1 do
    if aNode[i].Check(gtssOtherColumnDef) then begin
      if not lFirst then AddComma;
      List_ColumnDef(aNode[i], aListerOpt);
      lFirst := False;
    end;

  AddRightBracket(aNode);
end;

{ lists DROP COLUMN clause }
procedure TGtSqlFormatLister.List_AlterDropColumn;
begin
  if not Assigned(aNode) then Exit;
  FKeywordStyle := gtlsDdlDrop;

  if not (gtloSameAsPrevClause in aListerOpt) then AddClause(aNode.Keyword);

  List_ExprList(aNode[0], aListerOpt);
end;

{ lists MODIFY COLUMN clause }
procedure TGtSqlFormatLister.List_AlterModifyColumn;
var i: Integer;
    lFirst: Boolean;
begin
  if not Assigned(aNode) then Exit;
  FKeywordStyle := gtlsDdlModify;

  if not (gtloSameAsPrevClause in aListerOpt) then AddClause(aNode.KeywordExt);

  AddLeftBracket(aNode);

  lFirst := True;
  for i := 0 to aNode.Count - 1 do
    if aNode[i].Check(gtssOtherColumnDef) then begin
      if not lFirst then AddComma;
      List(aNode[i], aListerOpt);
      lFirst := False;
    end;

  AddRightBracket(aNode);
end;

{ lists ADD CONSTRAINT clause }
procedure TGtSqlFormatLister.List_AlterAddConstraint;
var lConstr: TGtSqlNode;
begin
  if not Assigned(aNode) then Exit;
  FKeywordStyle := gtlsDdlCreate;

  lConstr := aNode.Find(gtsiConstraint);
  if not Assigned(lConstr) then begin
    AddStr(gtkwAdd_Constraint);
    AddStr(aNode.Name, gtlsConstraint);
  end else begin
    List_Constraint(lConstr, aListerOpt + [gtloAlterTableConstraint]);
  end;
end;

{ lists DROP CONTRAINT clause }
procedure TGtSqlFormatLister.List_AlterDropConstraint;
begin
  if not Assigned(aNode) then Exit;
  FKeywordStyle := gtlsDdlDrop;

  if not(gtloSameAsPrevClause in aListerOpt) then AddClause(gtkwDrop_Constraint);
  AddStr(aNode.Name, gtlsConstraint);

  AddStr(aNode.KeywordAuxCheckKwd(gtkwCascade));
end;

{ lists RENAME TABLE clause }
procedure TGtSqlFormatLister.List_AlterRenameTable;
begin
  if not Assigned(aNode) then Exit;
  FKeywordStyle := gtlsDdlModify;

  if not (gtloSameAsPrevClause in aListerOpt) then AddClause(gtkwRename);
  AddStr(gtkwTo);
  AddStr(aNode.Name, gtlsTable);
end;

{ lists RENAME COLUMN clause }
procedure TGtSqlFormatLister.List_AlterRenameColumn;
begin
  if not Assigned(aNode) then Exit;
  FKeywordStyle := gtlsDdlModify;

  if not (gtloSameAsPrevClause in aListerOpt) then AddClause(gtkwRename_Column);
  AddStr(aNode.Name, gtlsColumn);

  aNode := aNode.Find(gtsiNone, gtkwTo);
  if Assigned(aNode) then begin
    AddStr(gtkwTo);
    AddStr(aNode.Name, gtlsColumn);
  end;
end;

{ lists ALTER TABLE statement }
procedure TGtSqlFormatLister.List_AlterTable;
var lPrevClause: TGtSqlNode;
    i: Integer;
begin
  if not Assigned(aNode) then Exit;
  FKeywordStyle := gtlsDdlModify;

  AddClause(gtkwAlter_Table);
  AddStr(aNode.Name, gtlsTable);

  { ALTER clauses }
  lPrevClause := nil;
  for i := 0 to aNode.Count - 1 do
    if aNode[i].Kind = gtssOtherKeyword then begin
      AddStr(aNode[i].Keyword);
    end else
    if aNode[i].Kind = gtsiClauseAlter then begin

      if aNode[i].Check(gtsiClauseAlter, gtkwModify_Column)  or
         aNode[i].Check(gtsiClauseAlter, gtkwAlter_Column)   or
         aNode[i].Check(gtsiClauseAlter, gtkwModify)         then begin

           aListerOpt := aListerOpt - [gtloSameAsPrevClause];
      end else begin

        if Assigned(lPrevClause) and (lPrevClause.Keyword = aNode[i].Keyword) or (aNode[i].Keyword = gttkComma)
        then aListerOpt := aListerOpt + [gtloSameAsPrevClause]
        else aListerOpt := aListerOpt - [gtloSameAsPrevClause];

        if gtloSameAsPrevClause in aListerOpt then AddComma;
      end;

      if aNode[i].Check(gtsiClauseAlter, gtkwAdd)            or
         aNode[i].Check(gtsiClauseAlter, gtkwAdd_Column)     then List_AlterAddColumn     (aNode[i], aListerOpt) else
      if aNode[i].Check(gtsiClauseAlter, gtkwDrop_Column)    or
         aNode[i].Check(gtsiClauseAlter, gtkwDrop)           then List_AlterDropColumn    (aNode[i], aListerOpt) else
      if aNode[i].Check(gtsiClauseAlter, gtkwModify_Column)  or
         aNode[i].Check(gtsiClauseAlter, gtkwAlter_Column)   or
         aNode[i].Check(gtsiClauseAlter, gtkwModify)         then List_AlterModifyColumn  (aNode[i], aListerOpt) else
      if aNode[i].Check(gtsiClauseAlter, gtkwAdd_Constraint) then List_AlterAddConstraint (aNode[i], aListerOpt) else
      if aNode[i].Check(gtsiClauseAlter, gtkwDrop_Constraint)then List_AlterDropConstraint(aNode[i], aListerOpt) else
      if aNode[i].Check(gtsiClauseAlter, gtkwRename_To)      then List_AlterRenameTable   (aNode[i], aListerOpt) else
      if aNode[i].Check(gtsiClauseAlter, gtkwRename_Column)  then List_AlterRenameColumn  (aNode[i], aListerOpt) else
      if aNode[i].Check(gtsiClauseAlter, gttkComma)          then begin

        if(lPrevClause.Keyword = gtkwAdd)     or
          (lPrevClause.Keyword = gtkwAdd_Column)     then List_AlterAddColumn     (aNode[i], aListerOpt) else
        if(lPrevClause.Keyword = gtkwDrop_Column)    or
          (lPrevClause.Keyword = gtkwDrop)           then List_AlterDropColumn    (aNode[i], aListerOpt) else
        if(lPrevClause.Keyword = gtkwModify_Column)  or
          (lPrevClause.Keyword = gtkwAlter_Column)   or
          (lPrevClause.Keyword = gtkwModify)         then List_AlterModifyColumn  (aNode[i], aListerOpt) else
        if lPrevClause.Keyword = gtkwAdd_Constraint  then List_AlterAddConstraint (aNode[i], aListerOpt) else
        if lPrevClause.Keyword = gtkwDrop_Constraint then List_AlterDropConstraint(aNode[i], aListerOpt) else
        if lPrevClause.Keyword = gtkwRename_To       then List_AlterRenameTable   (aNode[i], aListerOpt) else
        if lPrevClause.Keyword = gtkwRename_Column   then List_AlterRenameColumn  (aNode[i], aListerOpt) ;
      end;

      lPrevClause := aNode[i];
    end;
end;

{ lists CREATE INDEX statement }
procedure TGtSqlFormatLister.List_CreateIndex;
begin
  if not Assigned(aNode) then Exit;
  FKeywordStyle := gtlsDdlCreate;

  AddClause(aNode.KeywordExt);
  AddStr(aNode.Name, gtlsIdentifier);
  AddStr{KeywordName}( aNode.Find(gtsiNone, gtkwOn), gtlsTable);

  AddStr(gttkLeftBracket);
  List_ExprList(aNode, aListerOpt);
  AddStr(gttkRightBracket);
end;

{ lists DROP INDEX statement }
procedure TGtSqlFormatLister.List_DropIndex;
begin
  if not Assigned(aNode) then Exit;

  List_Clause_Name(aNode, aListerOpt, gtkwDrop_Index, nil, aNode.Name, gtlsIdentifier, gtlsDdlDrop);

  aNode := aNode.Find(gtsiNone, gtkwOn);
  if Assigned(aNode) then begin
    AddStr(gtkwOn);
    AddStr(aNode.Name, gtlsTable);
  end;
end;

{ lists ALTER INDEX }
procedure TGtSqlFormatLister.List_AlterIndex;
var i: Integer;
begin
  if not Assigned(aNode) then Exit;

  List_Clause_Name(aNode, aListerOpt, gtkwAlter_Index, nil, aNode.Name, gtlsIdentifier, gtlsDdlDrop);

  for i := 0 to aNode.Count - 1 do
    if aNode[i].Kind = gtssOtherKeyword then AddStr(aNode[i].Keyword);
end;

{ lists ANALYZE INDEX }
procedure TGtSqlFormatLister.List_AnalyzeIndex;
var i: Integer;
begin
  if not Assigned(aNode) then Exit;

  List_Clause_Name(aNode, aListerOpt, gtkwAnalyze_Index, nil, aNode.Name, gtlsIdentifier, gtlsDdlDrop);

  for i := 0 to aNode.Count - 1 do
    if aNode[i].Kind = gtssOtherKeyword then AddStr(aNode[i].Keyword);
end;

{ lists ALTER TRIGGER }
procedure TGtSqlFormatLister.List_AlterTrigger;
begin
  if not Assigned(aNode) then Exit;

  List_Clause_Name(aNode, aListerOpt, gtkwAlter_Trigger, nil, aNode.Name, gtlsIdentifier, gtlsDdlModify);
  AddStr(aNode.KeywordAuxCheckKwd(gtkwEnable, gtkwDisable));
end;

{ lists CREATE SEQUENCE }
procedure TGtSqlFormatLister.List_CreateSequence;
var lExpr: TGtSqlNode;
begin
  if not Assigned(aNode) then Exit;

  List_Clause_Name(aNode, aListerOpt, gtkwCreate_Sequence, nil, aNode.Name, gtlsIdentifier, gtlsDdlCreate);

  lExpr := aNode.Find(gtsiExprTree, gtkwStart_With);
  if Assigned(lExpr) then begin
    AddStr(gtkwStart_With);
    List_ExprTree(lExpr, aListerOpt);
  end;

  lExpr := aNode.Find(gtsiExprTree, gtkwIncrement_By);
  if Assigned(lExpr) then begin
    AddStr(gtkwIncrement_By);
    List_ExprTree(lExpr, aListerOpt);
  end;
end;

{ lists CREATE VIEW }
procedure  TGtSqlFormatLister.List_CreateView;
var lItem: TGtSqlNode;
begin
  if not Assigned(aNode) then Exit;
  FKeywordStyle := gtlsDdlCreateView;

  AddClause(aNode.KeywordExt);

  AddStr(aNode.Name, gtlsView);

  if aNode.KeywordAuxCheck(gtkwRefresh, gtkwRefresh_On_Demand, gtkwRefresh_Force_On_Demand, gtkwRefresh_Force) then begin
    AddStr(aNode.KeywordAuxCheckKwd(gtkwRefresh, gtkwRefresh_On_Demand, gtkwRefresh_Force_On_Demand, gtkwRefresh_Force));

    lItem := aNode.Find(gtsiExprTree, gtkwStart_With);
    if Assigned(lItem) then begin
      AddClause(gtkwStart_With);
      List_ExprTree(lItem, aListerOpt);
    end;

    lItem := aNode.Find(gtsiExprTree, gtkwNext);
    if Assigned(lItem) then begin
      AddClause(gtkwNext);
      List_ExprTree(lItem, aListerOpt);
    end;
  end;

  AddStr(gtkwAs);
  AddCurrLine;

  List_DML(aNode.Find(gtsiDml, gtkwSelect), aListerOpt);
end;

{ lists CREATE SYNONYM }
procedure  TGtSqlFormatLister.List_CreateSynonym;
begin
  if not Assigned(aNode) then Exit;
  FKeywordStyle := gtlsDdlCreate;

  AddClause(aNode.KeywordExt);
  AddStr(aNode.Name, gtlsSynonym);

  AddStr{KeywordName}( aNode.Find(gtsiNone, gtkwFor), gtlsIdentifier);
end;

{ lists GRANT statement }
procedure  TGtSqlFormatLister.List_Grant;
var i: Integer;
    lFirst: Boolean;
begin
  if not Assigned(aNode) then Exit;
  FKeywordStyle := gtlsDcl;

  AddClause(aKeyword);
  lFirst := True;
  for i := 0 to aNode.Count - 1 do
    if aNode[i].Kind = gtssGrantName then begin
      if not lFirst then AddComma;
      AddStr(aNode[i].Name, gtlsIdentifier);
      lFirst := False;
    end;

  AddStr(gtkwOn);
  lFirst := True;
  for i := 0 to aNode.Count - 1 do
    if aNode[i].Kind = gtssGrantObjectName then begin
      if not lFirst then AddComma;
      AddStr(aNode[i].Name, gtlsIdentifier);
      lFirst := False;
    end;

  if aNode.KeywordAuxCheck(gtkwTo) then AddStr(gtkwTo);
  if aNode.KeywordAuxCheck(gtkwFrom) then AddStr(gtkwFrom);

  lFirst := True;
  for i := 0 to aNode.Count - 1 do
    if aNode[i].Kind = gtssGrantUserName then begin
      if not lFirst then AddComma;
      AddStr(aNode[i].Name, gtlsIdentifier);
      lFirst := False;
    end;
end;

{ common list function }
procedure  TGtSqlFormatLister.List_Clause_Name;
begin
  // if not Assigned(aNode) then Exit;
  if aKeywordStyle <> gtlsPlainText then FKeywordStyle := aKeywordStyle;

  AddClause(aClauseToken1);
  if Assigned(aClauseToken2) then AddStr(aClauseToken2);

  if aName <> '' then AddStr(aName, aNameStyle);
end;

{ common list function }
procedure TGtSqlFormatLister.List_Clause_Expr;
begin
  if not Assigned(aNode) then Exit;

  if aNode.KeywordAuxCheck(gttkEmptyLineBefore) then AddEmptyLine;

  AddClause(aClauseToken);

  List_ExprList(aNode, aListerOpt);
end;

{ comon list function }
procedure TGtSqlFormatLister.List_Clause_Cond;
begin
  if not Assigned(aNode) then Exit;

  if aNode.KeywordAuxCheck(gttkEmptyLineBefore) then AddEmptyLine;

  AddClause(aClauseToken);

  List_CondTree(aNode, aListerOpt);
end;

{ lists SELECT clause }
procedure TGtSqlFormatLister.List_Select;
var lNode: TGtSqlNode;
begin
  if not Assigned(aNode) then Exit;

  { add new line before clause }
  if aNode.KeywordAuxCheck(gttkEmptyLineBefore) then AddEmptyLine;

  AddClause(gtkwSelect);

  { BNF: [DISTINCT] }
  if aNode.KeywordAuxCheck(gtkwDistinct) then begin
    AddStr( gtkwDistinct );
    if aNode.Count > 1 then AddClause( gttkNone );
  end;

  { BNF: [TOP n] }
  lNode := aNode.Find(gtsiNone, gtkwTop);
  if Assigned(lNode) then begin
    AddStr( gtkwTop );
    AddStr( lNode.Name, gtlsNumber );
  end;

  { list }
  List_ExprList(aNode, aListerOpt);
end;

{ lists FOR UPDATE clause }
procedure  TGtSqlFormatLister.List_ForUpdate;
begin
  if not Assigned(aNode) then Exit;

  List_Clause_Expr(aNode, aListerOpt, gtkwFor_Update_Of);
  AddStr(aNode.KeywordAuxCheckKwd(gtkwNoWait));
end;

{ lists UNION | MINUS | INTERSECT }
procedure  TGtSqlFormatLister.List_SetOp;
var lNextQuery: TGtSqlNode;
begin
  if not Assigned(aNode) then Exit;

  lNextQuery := aNode.Find(gtsiDml, gtkwSelect);
  if not Assigned(lNextQuery) or (lNextQuery.Count = 0) then Exit;

  if aNode.KeywordAuxCheck(gttkEmptyLineBefore) then AddCurrLine;

  if aNode.Check(gtsiUnions, gtkwUnion) then AddClause(gtkwUnion) else
  if aNode.Check(gtsiUnions, gtkwMinus) then AddClause(gtkwMinus) else
  if aNode.Check(gtsiUnions, gtkwIntersect) then AddClause(gtkwIntersect) else
  if aNode.Check(gtsiUnions, gtkwUnion_All) then  begin
                                                    AddClause(gtkwUnion);
                                                    AddStr(gtkwAll, gtlsUnion);
                                                  end;

  if aNode.KeywordAuxCheck(gttkEmptyLineAfter) then AddCurrLine;

  List_DML(lNextQuery, aListerOpt);
end;

{ lists ON clause }
procedure TGtSqlFormatLister.List_On;
begin
  if not Assigned(aNode) then Exit;

  if aNode.Check(gtsiCondTree, gtkwOn) then AddStr(gtkwOn) else AddStr(gtkwUsing);
  List_CondTree(aNode, aListerOpt);
end;

{ lists VALUES clause }
procedure TGtSqlFormatLister.List_Values;
begin
  if not Assigned(aNode) then Exit;

  AddClause(gtkwValues_LeftBracket);
  List_ExprList(aNode, aListerOpt);
  AddStr(gttkRightBracket);
end;

{ lists FIELDS clause }
procedure TGtSqlFormatLister.List_Fields;
begin
  if not Assigned(aNode) then Exit;
  if aNode.Kind <> gtssClauseFields then Exit;

  AddClause(gttkLeftBracket);
  List_ExprList(aNode, aListerOpt);
  AddStr(gttkRightBracket);
end;

{ lists FROM/INTO clause }
procedure TGtSqlFormatLister.List_Tables;
var i: Integer;
begin
  if not Assigned(aNode) then Exit;

  if aNode.KeywordAuxCheck(gttkEmptyLineBefore) then AddEmptyLine;

  for i := 0 to aNode.Count - 1 do begin
    if aNode[i].Check(gtsiTableRef) then List_TabRef(aNode[i], aListerOpt);
    if aNode[i].Check(gtsiDml, gtkwSelect) then List_DML(aNode[i], aListerOpt);
  end;
end;

{ lists DML statement }
procedure TGtSqlFormatLister.List_DML;
var lKeywordStyle: TGtLexTokenStyle;
    lNode: TGtSqlNode;
begin
  if not Assigned(aNode) then Exit;

  if aNode.IsSubQuery then Inc(SubQueryLevel);

  lKeywordStyle := FKeywordStyle;

  { subquery wrapper }
  if aNode.IsSubQuery and not(aNode.Owner.Check(gtsiUnions) or aNode.Owner.Check(gtsiDDL, gtkwCreate_Table)) then begin
      AddClause( aNode.KeywordExt, gttkLeftBracket );
      AddLeftBracket(aNode, True);

    { subquery at new line when in FROM/JOIN, IN condition, SELECT expr }
    if aNode.Owner.Check(gtsiClauseTables) or aNode.Owner.Check(gtsiCond) or aNode.Owner.Check(gtsiCondTree) or
       (aNode.Owner.Kind = gtsiExprTree) and Assigned((aNode.Owner.ExprTreeOwner))
       and not aNode.Owner.ExprTreeOwner.Check(gtsiExprList, gtkwSelect)
      then AddClause(nil);

    AddSpace;
  end;

  { DML essential }
  if aNode.Check(gtsiDml, gtkwSelect) then  List_Select     (aNode.Find(gtsiExprList, gtkwSelect),     aListerOpt);
  if aNode.Check(gtsiDml, gtkwInsert) then  List_TabRef     (aNode.Find(gtsiTableRef),                 aListerOpt);
  if aNode.Check(gtsiDml, gtkwInsert) then  List_Fields     (aNode.Find(gtssClauseFields),             aListerOpt);
  if aNode.Check(gtsiDml, gtkwUpdate) then  List_Tables     (aNode.Find(gtsiClauseTables, gtkwUpdate), aListerOpt);
  if aNode.Check(gtsiDml, gtkwSelect) then  List_Clause_Expr(aNode.Find(gtsiExprList, gtkwInto),       aListerOpt, gtkwInto);
  if aNode.Check(gtsiDml, gtkwInsert) then  List_Values     (aNode.Find(gtsiExprList, gtkwValues), aListerOpt);
  if aNode.Check(gtsiDml, gtkwUpdate) then  List_SetExprList(aNode.Find(gtsiSetExprList, nil), aListerOpt);

  { DELETE expr-list vs DELETE FROM }
  if aNode.Check(gtsiDml, gtkwDelete) then begin

      AddClause       (gtkwDelete);
      List_ExprList   (aNode.Find(gtsiExprList, gtkwDelete), aListerOpt);
      List_Tables     (aNode.Find(gtsiClauseTables, gtkwFrom), aListerOpt);
  end;

  if aNode.Check(gtsiDml, gtkwSelect) or
     aNode.Check(gtsiDml, gtkwUpdate) then  List_Tables     (aNode.Find(gtsiClauseTables, gtkwFrom),   aListerOpt);
  if aNode.Check(gtsiDml, gtkwSelect) or aNode.Check(gtsiDml, gtkwUpdate) or
     aNode.Check(gtsiDml, gtkwDelete) then  List_Clause_Cond(aNode.Find(gtsiCondTree, gtkwWhere),      aListerOpt, gtkwWhere);
  if aNode.Check(gtsiDml, gtkwSelect) or aNode.Check(gtsiDml, gtkwUpdate) or
     aNode.Check(gtsiDml, gtkwDelete) then  List_Clause_Cond(aNode.Find(gtsiCondTree, gtkwConnect_By), aListerOpt, gtkwConnect_By);
  if aNode.Check(gtsiDml, gtkwSelect) or aNode.Check(gtsiDml, gtkwUpdate) or
     aNode.Check(gtsiDml, gtkwDelete) then  List_Clause_Cond(aNode.Find(gtsiCondTree, gtkwStart_With), aListerOpt, gtkwStart_With);
  if aNode.Check(gtsiDml, gtkwSelect) then  List_Clause_Expr(aNode.Find(gtsiExprList, gtkwGroup_By),   aListerOpt, gtkwGroup_By);
  if aNode.Check(gtsiDml, gtkwSelect) or aNode.Check(gtsiDml, gtkwUpdate) or
     aNode.Check(gtsiDml, gtkwDelete) then  List_Clause_Cond(aNode.Find(gtsiCondTree, gtkwHaving),     aListerOpt, gtkwHaving);
  if aNode.Check(gtsiDml, gtkwInsert) then  List_DML        (aNode.Find(gtsiDml, gtkwSelect),          aListerOpt);
  if aNode.Check(gtsiDml, gtkwSelect) then  List_SetOp      (aNode.Find(gtsiUnions),                   aListerOpt);
  if aNode.Check(gtsiDml, gtkwSelect) then  List_Clause_Expr(aNode.Find(gtsiExprList, gtkwOrder_By),   aListerOpt, gtkwOrder_By);
  if aNode.Check(gtsiDml, gtkwSelect) then  List_ForUpdate  (aNode.Find(gtsiExprList, gtkwFor_Update), aListerOpt);
  if aNode.Check(gtsiDml, gtkwSelect) then  List_Clause_Expr(aNode.Find(gtsiExprList, gtkwLimit),      aListerOpt, gtkwLimit);
  if aNode.Check(gtsiDml, gtkwInsert) then  List_Clause_Expr(aNode.Find(gtsiExprList, gtkwReturning),  aListerOpt, gtkwReturning);
  if aNode.Check(gtsiDml, gtkwInsert) then  List_Clause_Expr(aNode.Find(gtsiExprList, gtkwInto),       aListerOpt, gtkwInto);

  { subquery wrapper }
  if aNode.IsSubQuery and not(aNode.Owner.Check(gtsiUnions) or aNode.Owner.Check(gtsiDDL, gtkwCreate_Table)) then begin
    AddClause(nil, gttkRightBracket);
    AddRightBracket(aNode, True);

    lNode := aNode.Find(gtsiNone, gtkwAs);
    if Assigned(lNode) then begin
      if lNode.KeywordAuxCheck(gtkwAs) then AddStr(gtkwAs);
      AddStr(lNode.Name, gtlsTableAlias);
    end;

    List(aNode.Find(gtsiCondTree, gtkwOn), aListerOpt);
    List(aNode.Find(gtsiCondTree, gtkwUsing), aListerOpt);
  end;

  FKeywordStyle := lKeywordStyle;
  if aNode.IsSubQuery then Dec(SubQueryLevel);
end;

{ lists not recognized }
procedure TGtSqlFormatLister.List_NotRecognized;
begin
  if not Assigned(aNode) then Exit;
  FKeywordStyle := gtlsError;

  AddCurrLine;
  AddStr('NOT RECOGNIZED STATEMENT: ', gtlsError);
end;

{ lists SqlParser }
procedure TGtSqlFormatLister.List_SqlParser;
var i: Integer;
begin
  if not Assigned(aNode) then Exit;

  BeginFormattedFile;

  { essential work }
  for i := 0 to aNode.QueryList.Count-1 do begin
    FKeywordStyle := gtlsKeyword;

    BracketLevel := 0;
    SubQueryLevel := 0;

    List( aNode.QueryList[ i ], [] );

    if aNode.QueryList[i].KeywordAuxCheck(gttkSemicolon) then AddStr( gttkSemicolon, gtlsPlainText, False );

    AddCurrLine;
  end;

  EndFormattedFile;
end;

end.

