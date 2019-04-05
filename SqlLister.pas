(* $Header: /SQL Toys/SqlFormat/SqlLister.pas 344   19-01-26 14:06 Tomek $
   (c) Tomasz Gierka, github.com/SqlToys, 2010.08.18                          *)
{--------------------------------------  --------------------------------------}
{$IFDEF RELEASE}
  {$DEBUGINFO OFF}
  {$LOCALSYMBOLS OFF}
{$ENDIF}
unit SqlLister;

interface

uses Classes,
     GtTokenizers, SqlStructs, SqlParser;

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
  TGtSqlFormattingOption   =( gtfoText, gtfoHtml, gtfoTreeView, gtfoRtf{, gtfoXml} );

  TGtSqlListerOptions      =( gtloTextOnly, //gtloSkipOneExprOnLineREMOVED, gtloSkipSubCaseFormatREMOVED, gtloSkipOneCondOnLineREMOVED,
                              gtloTableConstraint, gtloAlterTableConstraint,
                              gtloSameAsPrevClause, //gtloCondLeftSideOrderREMOVED, gtloCondEqualSwapREMOVED,
                              gtloSingleColumn, //gtloOnLeftSideIntendREMOVED, gtloOnRightSideIntendREMOVED, gtloExprAliasIntendREMOVED,
                              gtloSkipFrom
                            );
  TGtSqlListerOptionsSet   =set of TGtSqlListerOptions;

//type
  // uwaga na kolejnosc w tabeli wartosci domyslnych.
//  TGtListerCaseSettings =( gtlcTableCONVERTER, gtlcColumnCONVERTER, gtlcTableAliasCONVERTER
//                         , gtlcColumnAliasCONVERTER, gtlcParameterCONVERTER
//                         , gtlcIdentifierCONVERTER, gtlcKeyword
//                         , gtlcColumnQuotedAliasCONVERTER, gtlcFunctionCONVERTER );

//  TGtListerSettings = (gtstRightIntendREMOVED, gtstLineAfterQueryREMOVED,
//                       gtstSpaceBeforeCommaREMOVED, gtstSpaceBeforeSemicolonREMOVED,
//                       gtstEmptyLineBeforeClauseREMOVED, gtstUpperKeywordsREMOVED,
//                       gtstExprAsKeywordCONVERTER, gtstTableAsKeywordCONVERTER, gtstColumnConstraintREMOVED,
//                       gtstOuterJoinCONVERTER, gtstSortShortCONVERTER, gtstSkipAscendingCONVERTER,
//                       gtstOneExprOnLineREMOVED, gtstOneCondOnLineREMOVED,
//                       gtstEmptyLineAroundUnionREMOVED,
//                       gtstSpaceOutsideBracketsREMOVED, gtstSpaceInsideBracketsREMOVED,
//                       gtstSpaceAroundOperatorREMOVED, gtstSpaceAfterCommaREMOVED, gtstCommaAtNewLineREMOVED,
//                       gtstCaseAtNewLineREMOVED,
//                       gtstCaseWhenAtNewLineREMOVED, gtstCaseThenAtNewLineREMOVED,
//                       gtstCaseElseAtNewLineREMOVED, gtstCaseEndAtNewLineREMOVED,
//                       gtstTableAndAliasIntendREMOVED, gtstSetExprIntendREMOVED, gtstCreateTable_ColConsBreakLineREMOVED,
//                       gtstNoSemicolonOnSingleQueryREMOVED, gtstInnerJoinCONVERTER,
//                       gtstAliasFirstUseCaseREMOVED, gtstTableFirstUseCaseREMOVED,
//                       gtstSpaceInsideBracketsSkipFunREMOVED,
//                       gtstCreateTable_ColConsNewLineAfterREMOVED, gtstJoinCondLeftSideOrderCONVERTER,
//                       gtstCreateTable_IntendREMOVED, gtstCreateTable_EmptyLineBefComplexConstrREMOVED,
//                       gtstEmptyLineBeforeClauseSkipSubqueryREMOVED, gtstOnCondIntendREMOVED,
//                       gtstSelectAliasIntendREMOVED, gtstSpaceInsideBracketsSkipDatatypeREMOVED, gtstEmptyLineBeforeClauseSkipShortREMOVED,
//                       gtstOnCondRefsFirstCONVERTER, gtstExtQueryKeywordStyleREMOVED, gtstLinesNoAfterQueryREMOVED
//  );

//TGtListerCaseSettingsArray = array [ TGtListerCaseSettings  ] of TGtSqlCaseOption;
//TGtListerSettingsArray = array [ TGtListerSettings ] of Boolean;

{-------------------------------- Lister State --------------------------------}

type
  TGtSqlListerState = record
    RawText, FormText: String;

    KeywordStyle: TGtLexTokenStyle;
    BracketLevel, CaseLevel: Integer;

    SkipOutput: Boolean;
    SkipOutput_LineCount,
    SkipOutput_MaxLineLength: Integer;
  end;

{--------------------------------- SQL Lister ---------------------------------}

type
  TGtSqlProtoLister = class
  protected //private
    FKeywordStyle: TGtLexTokenStyle;
  protected
    RawText, FormText: String;
    BracketLevel: Integer;
    CaseLevel: Integer;
    // LastLineEmpty: Boolean;
    LastEmptyLines: Integer;

    SkipOutput: Boolean;
    SkipOutput_LineCount,
    SkipOutput_MaxLineLength: Integer;

    FormColors : array [TGtLexTokenStyle, gtfoHtml .. High(TGtSqlFormattingOption)] of String;
    FormStyles : array [TGtLexTokenStyle] of TFontStyles;

    { state methods }
    function    GetState: TGtSqlListerState;
    procedure   SetState(aState: TGtSqlListerState);

    property    State: TGtSqlListerState read GetState write SetState;

    { list methods }
    procedure   Add(aRawStr: String; aFormStr: String='');
    procedure   AddSpace(aIle: Integer = -1);
    procedure   RemSpace(aIle: Integer = -1);
    procedure   AddCurrLine; virtual;
    procedure   AddEmptyLine(aIle: Integer = -1); //(aAppend: Boolean = False);
    function    RemoveFormatting(aStr: String): String;

    function    BracketLevelStyle(aToken: TGtLexToken): TGtLexTokenStyle;
    function    CaseLevelStyle: TGtLexTokenStyle;

    function    ConvertStr(aStr: String): String;
    procedure   AddStr    (aStr: String; aStyle: TGtLexTokenStyle; aAddClearSpace: Boolean = True); overload;
    procedure   AddStr    (aToken: TGtLexToken{Def}; aAddClearSpace: Boolean = True); overload;
    procedure   AddStr    (aToken: TGtLexToken{Def}; aStyle: TGtLexTokenStyle;
                           aAddClearSpace: Boolean = True; aSingleParam: Boolean = False); overload;
    procedure   AddStrName(aNode: TGtSqlNode; aStyle: TGtLexTokenStyle);
    procedure   AddStrKeywordName(aNode: TGtSqlNode; aStyle: TGtLexTokenStyle);
    procedure   AddStrKeywordExt(aNode: TGtSqlNode);
    procedure   AddStrKeywordExtName(aNode: TGtSqlNode; aStyle: TGtLexTokenStyle);

    procedure   BeginFormattedFile;
    procedure   EndFormattedFile;
  public
    SL: TStringList;
    FormattingMode: TGtSqlFormattingOption;
  //MaxIdentifierLen: Integer;        // max length of any identifier

  //Options: TGtListerSettingsArray;
  //CaseOpt: TGtListerCaseSettingsArray;

    //Dialect: TGtSqlDialect;

    constructor Create; virtual;
    destructor  Destroy; override;

    procedure   SetStyle        (aStyle: TGtLexTokenStyle; aRGB: Integer; aBold, aItalic, aUnderline: Boolean);
    function    GetStyleTag     (aStyle: TGtLexTokenStyle): String;
    function    GetColor        (aStyle: TGtLexTokenStyle): String; overload;
    function    GetColorRGB     (aStyle: TGtLexTokenStyle): Integer; overload;
    function    GetStyleStr     (aStyle: TGtLexTokenStyle): String;
    function    GetRtfColorSchema: String;
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
    procedure  AddClause(aClause: TGtLexToken{Def} = nil; aAppend: Boolean = False); overload;
    procedure  AddClause(aClause: TGtLexToken{Def}; aIntendToken: TGtLexToken{Def};
                         aAppend: Boolean = False; aRemoveClauseBodySpace: Boolean = False); overload;
    procedure  AddClauseNode(aNode:TGtSqlNode; aAppend: Boolean = False); overload;
    procedure  AddClauseNode(aNode:TGtSqlNode; aIntendToken: TGtLexToken{Def};
                         aAppend: Boolean = False; aRemoveClauseBodySpace: Boolean = False); overload;
    procedure  AddComma; virtual;
    procedure  AddCommaAfterExpr(aListerOpt: TGtSqlListerOptionsSet);

    procedure  AddLeftBracket(aCount: Integer=1); overload;
    procedure  AddRightBracket(aCount: Integer=1); overload;

    procedure  AddLeftBracket(aNode: TGtSqlNode; aOneLess: Boolean = False); overload;
    procedure  AddRightBracket(aNode: TGtSqlNode; aOneLess: Boolean = False); overload;

    procedure  AddNewLine(aNode: TGtSqlNode; aToken: TGtLexToken);

    function   ClauseAppendCondition: Boolean;
  public
    // Dialect: TGtSqlDialect;

    { intendation }
    ClauseIntend: Boolean;
    SubQueryIntend: Boolean;
    SubQueryIntendSpace: Integer;     // >0 -> yes | =0 -> no

//  MaxSetLeftExprToIntend,           // max length of set expr left side to be intended
//  MaxTableNameToIntend,             // max length of table name to be intended
//  MaxAliasNameToIntend,             // max length of table alias to be intended
//  MaxColumnNameToIntend,            // max length of column name to be intended
//  MaxDatatypeToIntend,              // max length of datatype to be intended
//  MaxShortQueryLines: Integer;      // max lines no for short query
  //MaxClauseToIntend: Integer;       // max length of clause keyword to be intended
  //LinesNoAfterQuery: Integer;       // no of lines after query
  private
    ClauseBodySpace: Integer;         // between intended clause and its body

    NewLineIntend: Integer;
    SkipClauseNewLine,
    SkipNextNewLine,
    SkipSemicolonAfterThisQuery,
    FirstClause: Boolean;

    SubQueryLevel: Integer;

    // ML_xxx ==>> Max Length of xxx
    ML_ClauseKeyword: Integer;        // Max Length of Clause Keyword
  //ML_SetExpr_LeftSide,              // Max Length of left side of set expr (column name)
  //ML_ColumnName,                    // Max Length of column name for column def, PK, FK, UK, Check, CREATE TABLE.
  //ML_DataType,                      // Max Length of data type for column def, PK, FK, UK, Check, CREATE TABLE.
  //ML_TableName,                     // Max Length of table name for table_ref, list_tables.
  //ML_AliasName,                     // Max Length of alias name for table_ref, list_tables.
  //ML_TableAndAliasName,             // Max Length of table and alias for table_ref, list_tables.
  //ML_LeftOnExprPrefix,              // Max Length of ON condition left side column prefix.
  //ML_LeftOnExprColumn,              // Max Length of ON condition left side column name.
  //ML_RightOnExprPrefix,             // Max Length of ON condition right side column prefix.
  //ML_RightOnExprColumn,             // Max Length of ON condition right side column name.
  //ML_ExprAlias: Integer;            // Max Length of expression alias for List_SELECT.
  public
    constructor Create; override;

    procedure   SaveToFile(aFileName: String);

    procedure   List         (aNode: TGtSqlNode; aListerOpt: TGtSqlListerOptionsSet);
    procedure   List_NoOutput(aNode: TGtSqlNode; aListerOpt: TGtSqlListerOptionsSet);
  protected
    function   CheckSpaceNeedBeforeExpression: Boolean;
    function   GetClauseKeywordSpace   (aNode: TGtSqlNode): Integer;

    { SQL list methods }
    procedure  List_DataType           (aNode: TGtSqlNode; aListerOpt: TGtSqlListerOptionsSet); virtual;

    procedure  List_Expr               (aNode: TGtSqlNode; aListerOpt: TGtSqlListerOptionsSet); virtual;
    procedure  List_ExprCase           (aNode: TGtSqlNode; aListerOpt: TGtSqlListerOptionsSet); virtual;
    procedure  List_ExprCast           (aNode: TGtSqlNode; aListerOpt: TGtSqlListerOptionsSet); virtual;
    procedure  List_ExprConvert        (aNode: TGtSqlNode; aListerOpt: TGtSqlListerOptionsSet); virtual;
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

    procedure  List_Grant              (aNode: TGtSqlNode; aListerOpt: TGtSqlListerOptionsSet; aKeyword: TGtLexToken{Def}); virtual;

    procedure  List_Select             (aNode: TGtSqlNode; aListerOpt: TGtSqlListerOptionsSet); virtual;
    procedure  List_ForUpdate          (aNode: TGtSqlNode; aListerOpt: TGtSqlListerOptionsSet); virtual;

    procedure  List_SetOp              (aNode: TGtSqlNode; aListerOpt: TGtSqlListerOptionsSet); virtual;
    procedure  List_On                 (aNode: TGtSqlNode; aListerOpt: TGtSqlListerOptionsSet); virtual;
    procedure  List_Values             (aNode: TGtSqlNode; aListerOpt: TGtSqlListerOptionsSet); virtual;
    procedure  List_Fields             (aNode: TGtSqlNode; aListerOpt: TGtSqlListerOptionsSet); virtual;

    procedure  List_Tables             (aNode: TGtSqlNode; aListerOpt: TGtSqlListerOptionsSet); virtual;

    procedure  List_DML                (aNode:TGtSqlNode; aListerOpt: TGtSqlListerOptionsSet); virtual;

    procedure  List_NotRecognized      (aNode: TGtSqlNode; aListerOpt: TGtSqlListerOptionsSet); virtual;

    procedure  List_Clause_Name        (aNode: TGtSqlNode; aListerOpt: TGtSqlListerOptionsSet;
                                        aClauseToken1: TGtLexToken{Def}; aClauseToken2: TGtLexToken{Def};
                                        aName: String='';
                                        aNameStyle: TGtLexTokenStyle=gtlsPlainText;
                                        aKeywordStyle: TGtLexTokenStyle=gtlsPlainText); virtual;
    procedure  List_Clause_Expr        (aNode: TGtSqlNode; aListerOpt: TGtSqlListerOptionsSet;
                                        aClauseToken: TGtLexToken{Def}; aClauseAppend: Boolean); virtual;
    procedure  List_Clause_Cond        (aNode: TGtSqlNode; aListerOpt: TGtSqlListerOptionsSet;
                                        aClauseToken: TGtLexToken{Def}; aClauseAppend: Boolean); virtual;
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
//var //lOpt: TGtListerSettings;
//  lCase: TGtListerCaseSettings;
begin
  inherited Create;

  //Dialect := gtdlNone;
  FKeywordStyle   := gtlsKeyword;

  RawText         := '';
  FormText        := '';
  FormattingMode  := gtfoRtf; //gtfoText;
  BracketLevel    := 0;
  CaseLevel       := 0;

  LastEmptyLines  := 0;

  SkipOutput      := False;
  SkipOutput_LineCount := 0;
  SkipOutput_MaxLineLength := 0;

  SL := TStringList.Create;
  SL.DefaultEncoding := TEncoding.ANSI;

//  for lOpt := Low(TGtListerSettings) to High(TGtListerSettings)
//    do Options[ lOpt ] := False;

//  for lCase := Low(TGtListerCaseSettings) to High(TGtListerCaseSettings)
//    do CaseOpt[ lCase ] := gtcoNoChange;

  // defaults for Compact.
  //CaseOpt[ gtlcKeyword ] := gtcoUpperCase;
  // Options[ gtstUpperKeywords    ] := True;
  // Options[ gtstColumnConstraint ] := True;
//  Options[ gtstSortShortCONVERTER ] := True;
//  Options[ gtstSkipAscendingCONVERTER ] := True;

//MaxIdentifierLen := 30;
end;

{ class destructor }
destructor TGtSqlProtoLister.Destroy;
begin
  if Assigned(SL) then SL.Free;

  inherited Destroy;
end;

{ gets lister state }
function    TGtSqlProtoLister.GetState;
begin
  Result.RawText                  := RawText;
  Result.FormText                 := FormText;

  Result.KeywordStyle             := FKeywordStyle;
  Result.BracketLevel             := BracketLevel;
  Result.CaseLevel                := CaseLevel;

  Result.SkipOutput               := SkipOutput;
  Result.SkipOutput_LineCount     := SkipOutput_LineCount;
  Result.SkipOutput_MaxLineLength := SkipOutput_MaxLineLength;
end;

{ sets listet state }
procedure   TGtSqlProtoLister.SetState;
begin
  RawText                  := aState.RawText;
  FormText                 := aState.FormText;

  FKeywordStyle            := aState.KeywordStyle;
  BracketLevel             := aState.BracketLevel;
  CaseLevel                := aState.CaseLevel;

  SkipOutput               := aState.SkipOutput;
  SkipOutput_LineCount     := aState.SkipOutput_LineCount;
  SkipOutput_MaxLineLength := aState.SkipOutput_MaxLineLength;
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

{ gets HTML tag for style }
function  TGtSqlProtoLister.GetStyleTag;
begin
  Result := strif(fsBold      in FormStyles [aStyle], '<B>')
          + strif(fsItalic    in FormStyles [aStyle], '<I>')
          + strif(fsUnderline in FormStyles [aStyle], '<U>');
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

{ gets color }
function TGtSqlProtoLister.GetColorRGB (aStyle: TGtLexTokenStyle): Integer;
begin
  Result := HexToInt( FormColors [aStyle, gtfoHtml, 8] + FormColors [aStyle, gtfoHtml, 9]) shl 16 +
            HexToInt( FormColors [aStyle, gtfoHtml,10] + FormColors [aStyle, gtfoHtml,11]) shl  8 +
            HexToInt( FormColors [aStyle, gtfoHtml,12] + FormColors [aStyle, gtfoHtml,13]);
end;

{ gets style }
function TGtSqlProtoLister.GetStyleStr (aStyle: TGtLexTokenStyle): String;
begin
  Result := strif( fsBold      in FormStyles [aStyle], 'B', '_') +
            strif( fsItalic    in FormStyles [aStyle], 'I', '_') +
            strif( fsUnderline in FormStyles [aStyle], 'U', '_');
end;

{ gets RTF color schema }
function TGtSqlProtoLister.GetRtfColorSchema: String;
var i: TGtLexTokenStyle;
begin
  Result := '{\colortbl;';
  for i := Low(TGtLexTokenStyle) to High(TGtLexTokenStyle) do
    Result := Result + FormColors [i, gtfoRtf] + ';';
  Result := Result + '}';
end;

{ adds to last line }
procedure TGtSqlProtoLister.Add;
begin
  if aRawStr='' then Exit;
  if aFormStr='' then aFormStr := aRawStr;

  RawText  := RawText  + aRawStr;
  if FormattingMode in [gtfoHtml, gtfoRtf, gtfoTreeView] then FormText := FormText + aFormStr;
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

{ removes clear spaces }
procedure  TGtSqlProtoLister.RemSpace;
var i: Integer;
    sSpace: String;
begin
  if (RawText = '') or (aIle = 0) then Exit;

  { jesli wiersz zawiera jedynie intendacje }
  i := Length(RawText);
  if (i = 0) or (RawText[i] <> ' ') then Exit;

  while (i > 0) and (RawText[i] = ' ') do Dec(i);
  if i = 0 then Exit;

  if FormattingMode = gtfoHtml then sSpace := '&nbsp;' else sSpace := ' ';

  while (aIle <> 0) and (Length(RawText) > 0) and (Copy(RawText,Length(RawText),1) = ' ') do begin
    RawText  := Copy(RawText,  1, Length(RawText) - 1);
    FormText := Copy(FormText, 1, Length(FormText)- Length(sSpace));
    Dec(aIle);
  end;
end;

{ adds current line (RawText or FormText) to output string list, commits current line }
procedure TGtSqlProtoLister.AddCurrLine;
begin
  if SkipOutput then begin
    if Length(RawText) > SkipOutput_MaxLineLength then SkipOutput_MaxLineLength := Length(RawText);
    Inc(SkipOutput_LineCount);
  end else begin
    case FormattingMode of
      gtfoHtml : SL.Add(FormText + gtHtmlNewLine);
      gtfoRtf  : SL.Add(FormText + gtRtfNewLine)
    else         SL.Add(RawText);
    end;
  end;

  RawText  := '';
  FormText := '';
end;

{ adds empty line }
procedure TGtSqlProtoLister.AddEmptyLine;
var i: Integer;
begin
  if Trim(RawText) <> '' then begin
    { commit current line }
    AddCurrLine;

    LastEmptyLines := 0;
  end else
  if aIle = -1 then begin
    { it is done if last line was empty }
    if SL.Count = 0 then Exit;

    case FormattingMode of
      gtfoHtml : if RemoveFormatting(SL[SL.Count-1]) = gtHtmlNewLine then Exit;
      gtfoRtf  : if RemoveFormatting(SL[SL.Count-1]) = gtRtfNewLine_trim then Exit;
    else         if RemoveFormatting(SL[SL.Count-1]) = '' then Exit;
    end;
  end;

  if aIle = -1 then begin
    { adds single empty line }
    AddCurrLine;
    Inc(LastEmptyLines);
  end else begin
    for i := 1 to aIle do AddCurrLine;
    Inc(LastEmptyLines, aIle);
  end;
end;

{ TODO: removes formatting tags from string }
function TGtSqlProtoLister.RemoveFormatting(aStr: String): String;
begin
  Result := Trim(aStr);
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

{ convert string to proper format }
function TGtSqlProtoLister.ConvertStr (aStr: String): String;
begin
  case FormattingMode of
    gtfoRtf : Result := StringReplace( StringReplace( StringReplace(aStr, '\', '\\', []), '{', '\{', []), '}', '\}', []);
  else
    Result := aStr;
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

    Result := lbeg + ConvertStr(aStr) + lend;
  end;

begin
  if aStr = '' then Exit;

  { optional upper/lower/nochange case }
//case aStyle of
//  gtlsTable       : aStr := UpperLowerStr(aStr, CaseOpt[ gtlcTable ]);
//  gtlsColumn      : aStr := UpperLowerStr(aStr, CaseOpt[ gtlcColumn ]);
//  gtlsTableAlias  : aStr := UpperLowerStr(aStr, CaseOpt[ gtlcTableAlias ]);
//  gtlsColumnAlias : if Copy(aStr,1,1) = '"'
//                      then aStr := UpperLowerStr(aStr, CaseOpt[ gtlcColumnQuotedAlias ])
//                      else aStr := UpperLowerStr(aStr, CaseOpt[ gtlcColumnAlias ]);
//  gtlsParameter   : aStr := UpperLowerStr(aStr, CaseOpt[ gtlcParameter ]);
//  gtlsFunction,
//  gtlsAggrFunction: aStr := UpperLowerStr(aStr, CaseOpt[ gtlcFunction ]);
//  gtlsConstraint,
//  gtlsSynonym,
//  gtlsTransaction,
//  gtlsIdentifier  : aStr := UpperLowerStr(aStr, CaseOpt[ gtlcIdentifier ]);
//  gtlsDatatype .. gtlsPrior,
//  gtlsKeyword     : aStr := UpperLowerStr(aStr, CaseOpt[ gtlcKeyword ]);
//end;

  if (aStyle = gtlsOperator) then begin
    { operator logiczny traktuje jako keyword a nie operator - musi wystapic spacja bo przykleji siê do identyfikatora }
    if {Options[ gtstSpaceAroundOperator ] or} (UpperCase(aStr)='AND') or (UpperCase(aStr)='OR') or (UpperCase(aStr)='NOT')
      then AddSpace;
  end else
  if aAddClearSpace then AddSpace;

  { too long identifier list as error !!  -  only chars abowe limit }
//  if (Length(aStr) > MaxIdentifierLen) and
//     (aStyle in [gtlsTable, gtlsColumn, gtlsTableAlias, gtlsColumnAlias,
//                 gtlsFunction, gtlsAggrFunction, gtlsConstraint,
//                 gtlsSynonym, gtlsTransaction, gtlsIdentifier]) then begin
//    Add(aStr, LocalFormatStr(Copy(aStr, 1, MaxIdentifierLen), aStyle) +
//              LocalFormatStr(Copy(aStr, MaxIdentifierLen+1, 999999), gtlsError));
//  end else begin
    Add(aStr, LocalFormatStr(aStr, aStyle));
//  end;

//if (aStyle = gtlsOperator) and Options[ gtstSpaceAroundOperator ] then AddSpace;
end;

{ adds colored string }
procedure TGtSqlProtoLister.AddStr (aToken: TGtLexToken{Def}; aAddClearSpace: Boolean = True);
begin
  if not Assigned(aToken) or (aToken = gttkNone) then Exit;

  AddStr(aToken, aToken.TokenStyle, aAddClearSpace);
end;

{ adds colored name with given style }
procedure TGtSqlProtoLister.AddStrName(aNode: TGtSqlNode; aStyle: TGtLexTokenStyle);
begin
  if not Assigned(aNode) then Exit;

  AddStr(aNode.Name, aStyle);
end;

{ adds colored keyword and name with given style }
procedure TGtSqlProtoLister.AddStrKeywordName(aNode: TGtSqlNode; aStyle: TGtLexTokenStyle);
begin
  if not Assigned(aNode) then Exit;

  AddStr(aNode.Keyword);
  AddStr(aNode.Name, aStyle);
end;

{ adds colored keyword and name with given style }
procedure TGtSqlProtoLister.AddStrKeywordExt(aNode: TGtSqlNode);
begin
  if not Assigned(aNode) then Exit;

  if not Assigned(aNode.KeywordExt) or (aNode.KeywordExt = gttkNone)
    then AddStr(aNode.Keyword)
    else AddStr(aNode.KeywordExt);
end;

{ adds colored keyword and name with given style }
procedure TGtSqlProtoLister.AddStrKeywordExtName(aNode: TGtSqlNode; aStyle: TGtLexTokenStyle);
begin
  if not Assigned(aNode) then Exit;

  if not Assigned(aNode.KeywordExt) or (aNode.KeywordExt = gttkNone)
    then AddStr(aNode.Keyword)
    else AddStr(aNode.KeywordExt);
  AddStr(aNode.Name, aStyle);
end;

{ adds colored string }
procedure TGtSqlProtoLister.AddStr (aToken: TGtLexToken{Def}; aStyle: TGtLexTokenStyle;
                                    aAddClearSpace: Boolean = True; aSingleParam: Boolean = False);
var lStyle: TGtLexTokenStyle;
    lStr: String;
begin
  if not Assigned(aToken) or (aToken = gttkNone) then Exit;

  lStyle := aStyle; //aToken.TokenStyle;// gtlsPlainText;
  if (aToken.TokenKind = gtttKeyword) and (aStyle = gtlsKeyword) then lStyle := FKeywordStyle;

  if Assigned(aToken.SubToken2) and not (aToken.TokenStyle in [gtlsOperator, gtlsComment]) then begin
    AddStr(aToken.SubToken1, lStyle, aAddClearSpace);
    AddSpace;
    AddStr(aToken.SubToken2, lStyle, aAddClearSpace);
    if not Assigned(aToken.SubToken3) then RemSpace;
    if not Assigned(aToken.SubToken3) then Exit;
    AddSpace;
    AddStr(aToken.SubToken3, lStyle, aAddClearSpace);
    if not Assigned(aToken.SubToken4) then RemSpace;
    if not Assigned(aToken.SubToken4) then Exit;
    AddSpace;
    AddStr(aToken.SubToken4, lStyle, aAddClearSpace);
    if not Assigned(aToken.SubToken5) then RemSpace;
    if not Assigned(aToken.SubToken5) then Exit;
    AddSpace;
    AddStr(aToken.SubToken5, lStyle, aAddClearSpace);
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
                     (aToken = gttkPercent) {and (Dialect = gtdlMicrosoftSql)} or (aToken = gttkEqual) or
                     (aToken = gttkStarEqual) or (aToken = gttkEqualStar) or (aToken = gttkBracketPlusBracket)
                  then lStyle := gtlsOperator else
                  if aToken = gttkLeftBracket then begin
                    lStyle := BracketLevelStyle(aToken);
                    Inc(BracketLevel);
                    AddStr(lStr, lStyle, False {Options[ gtstSpaceOutsideBrackets ] and aAddClearSpace} );
//                    if Options[ gtstSpaceInsideBrackets ] and aAddClearSpace
//                      and not (aSingleParam and Options[ gtstSpaceInsideBracketsSkipFun ])
//                      then AddSpace;
                    Exit;
                  end else
                  if aToken = gttkRightBracket then begin
                    Dec(BracketLevel);
                    lStyle := BracketLevelStyle(aToken);
                    AddStr(lStr, lStyle, False);
//                           Options[ gtstSpaceInsideBrackets ] and aAddClearSpace
//                           and not (aSingleParam and Options[ gtstSpaceInsideBracketsSkipFun ]) );
                  //if Options[ gtstSpaceOutsideBrackets] and aAddClearSpace then AddSpace;
                    Exit;
                  end;
  end;

  AddStr(lStr, lStyle, aAddClearSpace);
end;

{ formatted file header }
procedure TGtSqlProtoLister.BeginFormattedFile;
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
  if Trim(RawText) <> '' then begin
    AddEmptyLine;
  end else begin
    while LastEmptyLines > 0 do begin
      SL.Delete(SL.Count -1);
      Dec(LastEmptyLines);
    end;
  end;

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
//  co: TGtListerCaseSettings;
begin
  if not Assigned(aTokenList) then Exit;

  BeginFormattedFile;

  BracketLevel := 0;
  CaseLevel    := 0;

//  for co := Low(TGtListerCaseSettings) to High(TGtListerCaseSettings) do
//    CaseOpt[ co ] := gtcoNoChange;

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
          AddStr(gttkSemicolon, False);
        end else
        if aTokenList[i].TokenText = gttkLeftBracket.TokenText
          then AddStr(gttkLeftBracket, False)
          else
        if aTokenList[i].TokenText = gttkRightBracket.TokenText
          then AddStr(gttkRightBracket, False)
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

  // Dialect := gtdlNone;

  ML_ClauseKeyword           := 6; //15;//CREATE TABLE-12,INSERT INTO-11,DROP CONSTRAINT-15
  ClauseBodySpace            := 3; //2;//1;

  SubQueryIntend             := True;
  SubQueryIntendSpace        := 0; //2;//0;

  NewLineIntend   := 0;
  SkipNextNewLine := False;
  SkipClauseNewLine:=False;
  SkipSemicolonAfterThisQuery := False;
  FirstClause     := False;

  SubQueryLevel := 0;

//ML_SetExpr_LeftSide := 0;
//ML_ColumnName := 0;
//ML_DataType   := 0;
//ML_TableName := 0;
//ML_AliasName := 0;
//ML_TableAndAliasName := 0;
//ML_LeftOnExprPrefix := 0;
//ML_LeftOnExprColumn := 0;
//ML_RightOnExprPrefix := 0;
//ML_RightOnExprColumn := 0;
//ML_ExprAlias := 0;

//MaxSetLeftExprToIntend := 20;
//MaxTableNameToIntend := 30;
//MaxAliasNameToIntend := 10;
//MaxColumnNameToIntend := 20;
//MaxDatatypeToIntend := 20;
//MaxShortQueryLines := 20;
//LinesNoAfterQuery := 1;
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
  if SkipNextNewLine then begin
    SkipNextNewLine := False;
    Exit;
  end;

  inherited AddCurrLine;
  LastEmptyLines := 0;

  AddSpace(NewLineIntend);
end;

{ adds clause to script }
procedure TGtSqlFormatLister.AddClause(aClause: TGtLexToken{Def} = nil; aAppend: Boolean = False);
begin
  AddClause(aClause, nil, aAppend);
end;

{ adds clause to script }
procedure TGtSqlFormatLister.AddClauseNode(aNode:TGtSqlNode; aAppend: Boolean = False);
begin
  AddClauseNode( aNode, nil, aAppend );
end;

{ adds clause to script }
procedure TGtSqlFormatLister.AddClauseNode(aNode:TGtSqlNode; aIntendToken: TGtLexToken{Def};
                         aAppend: Boolean = False; aRemoveClauseBodySpace: Boolean = False);
begin
  if not Assigned(aNode) then Exit;

  if not Assigned(aNode.KeywordExt) or (aNode.KeywordExt = gttkNone)
    then AddClause(aNode.Keyword, nil, aAppend, aRemoveClauseBodySpace)
    else AddClause(aNode.KeywordExt, nil, aAppend, aRemoveClauseBodySpace);
end;


{ adds clause to script }
procedure TGtSqlFormatLister.AddClause(aClause: TGtLexToken{Def}; aIntendToken: TGtLexToken{Def};
                                       aAppend: Boolean = False; aRemoveClauseBodySpace: Boolean = False);

procedure AddIntendToken;
begin
  if Assigned(aIntendToken) then begin
    AddSpace(1);
    if aIntendToken.TokenText = ''
      then AddSpace(1)
      else AddStr(aIntendToken, False); // bracket or comma
    AddSpace(ClauseBodySpace - 2);
  end else begin
    AddSpace(ClauseBodySpace);
  end;
end;

var lDoIntend: Boolean;
    lLength: Integer;
begin
  if Assigned(aClause) then lLength := Length(aClause.TokenText) else lLength := 0;

  lDoIntend := ClauseIntend and not aAppend and
              (lLength < ML_ClauseKeyword + ClauseBodySpace) and
              (aClause <> gtkwBegin) and (aClause <> gtkwEnd);

  { aRemoveClauseBodySpace (OLD: add clause back) - bez nowego wiersza np. column subquery w SELECT }
  if aRemoveClauseBodySpace and (Trim(RawText) = ',') and ClauseIntend and not aAppend
  and Assigned(aIntendToken) and (not Assigned(aClause) or (aClause = gttkNone)) then begin
    RemSpace(ClauseBodySpace);
    AddIntendToken;
    aIntendToken := nil;
  end;

  { unikamy problemu podwójnego pomijana w subqueries }
  if SkipClauseNewLine and SkipNextNewLine then SkipNextNewLine := False;

  { commit not commited text }
  if ClauseIntend and not aAppend and (Trim(RawText) <> '') {and not SkipClauseNewLine} then AddCurrLine;

  { empty line before clause }
//if Options[ gtstEmptyLineBeforeClause ] and not SkipClauseNewLine and not FirstClause and {not LastLineEmpty and}
//  if aEmptyLineBeforeClause and not SkipClauseNewLine and not FirstClause and {not LastLineEmpty and}
//   ((SubQueryLevel = 0) or
//    (SubQueryLevel > 0) and not Options[ gtstEmptyLineBeforeClauseSkipSubquery ]) and
//       ((aClause = gtkwSelect) or
//        (aClause = gtkwFrom) or (aClause = gtkwWhere) or (aClause = gtkwGroup_By) or
//        (aClause = gtkwHaving) or (aClause = gtkwOrder_By) or (aClause = gtkwConnect_By) or
//        (aClause = gtkwSet) or (aClause = gtkwValues))
//    then AddEmptyLine;

  FirstClause := False;

  { right intend clause spaces }
//if lDoIntend and Options[ gtstRightIntend ] and (ML_ClauseKeyword - lLength > 0) then AddSpace(ML_ClauseKeyword - lLength);

  { clause }
  if (aClause = gttkComma) and not lDoIntend
    then AddComma
    else AddStr(aClause, not lDoIntend);

  if not lDoIntend then begin
    AddStr(aIntendToken);
    Exit;
  end;

  { left intend clause spaces }
//if not Options[ gtstRightIntend ] then AddSpace(ML_ClauseKeyword - lLength);

  { intend token or clause body space }
  AddIntendToken;
end;

{ adds formatting options for comma }
procedure TGtSqlFormatLister.AddComma;
begin
  //if not Options[ gtstSpaceBeforeComma ] then RemSpace;

  AddStr(gttkComma);//, Options[ gtstSpaceBeforeComma ]);

//if Options[ gtstSpaceAfterComma ] then AddSpace;
end;

{ adds comma after expression inside column/expr list }
procedure TGtSqlFormatLister.AddCommaAfterExpr;
begin
//  if Options[ gtstOneExprOnLine ] and not (gtloSkipOneExprOnLine in aListerOpt) then begin
//    if not Options[ gtstCommaAtNewLine ] then begin
//      AddComma;
//      AddClause(nil);
//    end else begin
//      AddClause(gttkComma)
//    end;
//  end else begin
    AddComma;
//  end;
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

//if lNode.Name = '' then AddLeftBracket else AddLeftBracket(StrToInt(lNode.Name));

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

//if lNode.Name = '' then AddRightBracket else AddRightBracket(StrToInt(lNode.Name));

  lcnt := 1;
  if lNode.Name <> '' then lcnt := StrToInt(lNode.Name);
  if aOneLess then lcnt := lcnt - 1;

  AddRightBracket(lcnt);
end;

{ adds NewLine when NewLineBefore node is found }
procedure TGtSqlFormatLister.AddNewLine(aNode: TGtSqlNode; aToken: TGtLexToken);
begin
  if not Assigned(aNode) then Exit;
//if Assigned(aNode.Find(gtsiNone, aToken)) then AddClause;
  if aNode.KeywordAuxCheck(gttkNewLineBefore) then AddClause;
end;

{ checks if clause should appends to prev token }
function TGtSqlFormatLister.ClauseAppendCondition;
begin
  Result := (SubQueryLevel > 0) and not SubQueryIntend;
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
                                     SkipSemicolonAfterThisQuery := True;
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

{ Silent list wo. output to check max. line length }
procedure TGtSqlFormatLister.List_NoOutput;
var lState: TGtSqlListerState;
begin
  lState := State;

  try
    SkipOutput := True;

    SkipOutput_LineCount := 0;
    SkipOutput_MaxLineLength := 0;

    List (aNode, aListerOpt);

    if Length(RawText) > SkipOutput_MaxLineLength then SkipOutput_MaxLineLength := Length(RawText);

    if SkipOutput_MaxLineLength > lState.SkipOutput_MaxLineLength then lState.SkipOutput_MaxLineLength := SkipOutput_MaxLineLength;
    if SkipOutput_LineCount > lState.SkipOutput_LineCount then lState.SkipOutput_LineCount := SkipOutput_LineCount;
  finally
    State := lState;
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
  AddStr(aNode.{DataType} Keyword);
//if aNode.ColSize = gtsqlSizeOrPrecNotSpecified then Exit;
//if aNode.Int1 = gtsqlSizeOrPrecNotSpecified then Exit;

  lNode := aNode.Find(gtsiNone, gtkwSize);
  if Assigned(lNode) then begin
    AddStr(gttkLeftBracket, False{not Options[gtstSpaceInsideBracketsSkipDatatype]});

  //AddStr(IntToStr(aNode.ColSize), gtlsNumber, False);
  //AddStr(IntToStr(aNode.Int1), gtlsNumber, False);
    AddStr(lNode.Name, gtlsNumber, False);

    lNode := lNode.Find(gtsiNone, gtkwPrec);
    if Assigned(lNode) then begin
    //if aNode.ColPrec <> gtsqlSizeOrPrecNotSpecified then AddComma;
    //if aNode.ColPrec <> gtsqlSizeOrPrecNotSpecified then AddStr(IntToStr(aNode.ColPrec), gtlsNumber, False);
    //if aNode.Int2 <> gtsqlSizeOrPrecNotSpecified then AddComma;
    //if aNode.Int2 <> gtsqlSizeOrPrecNotSpecified then AddStr(IntToStr(aNode.Int2), gtlsNumber, False);
      AddComma;
      AddStr(lNode.Name, gtlsNumber, False);
    end;

    AddStr(gttkRightBracket, False{not Options[gtstSpaceInsideBracketsSkipDatatype]});
  end;
end;

{ lists expression }
procedure TGtSqlFormatLister.List_Expr;
var s: String;
begin
  if not Assigned(aNode) then Exit;

//if aNode.ExprPrior then AddStr(gtkwPrior);
//if aNode.KeywordExt = gtkwPrior then AddStr(gtkwPrior);
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
  end else
  if aNode.Check(gtsiExpr, gtkwConvert) then begin
    List_ExprConvert(aNode, aListerOpt);
    Exit;
  end;

//AddLeftBracket(aNode.BracketsCount);
  AddLeftBracket(aNode);

  if aNode.Check(gtsiExpr, gttkColumnName) or aNode.Check(gtsiExpr, gttkStar) then begin
    List_ExprColumn(aNode, aListerOpt);
  //AddRightBracket(aNode.BracketsCount);
    AddRightBracket(aNode);
    Exit;
  end;

//  if aNode.Check(gtsiExpr, gttkParameterName) then
//                         s :=   strif(aNode.ExprMinus, gttkMinus.TokenText) +
//                                aNode.Name
//  else
  if aNode.Check(gtsiExpr, gttkNumber) then begin
  //s := strif(aNode.ExprMinus, gttkMinus.TokenText) + aNode.Name
    if aNode.KeywordAuxCheck(gttkMinus) then s := gttkMinus.TokenText;
    s := aNode.Name;
  end else
  if aNode.Check(gtsiExpr, gttkIdentifier) then begin
//                         if UpperCase(aNode.Name) = gtkwNull.TokenText
//                           then s :=   strif(aNode.ExprMinus, gttkMinus.TokenText) +
//                                       {UpperLowerStr(}aNode.Name{, CaseOpt[ gtlcKeyword ])}
//                           else s :=   strif(aNode.ExprMinus, gttkMinus.TokenText) +
//                                       aNode.Name
    if aNode.KeywordAuxCheck(gttkMinus) then s := gttkMinus.TokenText;
    s := aNode.Name;
  end else
  if aNode.Check(gtsiExpr, gttkParameterName) then begin
//                         s :=   strif(aNode.ExprMinus, gttkMinus.TokenText) +
//                                aNode.Name
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

//AddRightBracket(aNode.BracketsCount);
  AddRightBracket(aNode);

  { wrong ON COND intend.sql, breakpoint condition: gtloOnRightSideIntend in aListerOpt }
//  if (gtloOnLeftSideIntend in aListerOpt) and (Length(s) < ML_LeftOnExprPrefix + ML_LeftOnExprColumn + 1) // +1 due to dot
//    then AddSpace(ML_LeftOnExprPrefix + ML_LeftOnExprColumn + 1 - Length(s) + 1); // +1 due to ON keyword
//  if (gtloOnRightSideIntend in aListerOpt) and (Length(s) < ML_RightOnExprPrefix + ML_RightOnExprColumn + 1 + 1) // +1 due to dot
//    then AddSpace(ML_RightOnExprPrefix + ML_RightOnExprColumn + 1 + 1 - Length(s)); // +1 due to dot, +1 due to space after
end;

{ lists expression CASE }
procedure TGtSqlFormatLister.List_ExprCase;
var //lSkipSubCaseFormat: Boolean;
    lCaseExpr, lElseExpr, lNode: TGtSqlNode;
    i: Integer;
begin
  lNode := nil;
//lSkipSubCaseFormat := gtloSkipSubCaseFormat in aListerOpt;
//aListerOpt := aListerOpt + [ gtloSkipOneExprOnLine {, gtloSkipSubCaseFormat, gtloSkipOneCondOnLine} ];

//if Options[ gtstCaseAtNewLine ] and not lSkipSubCaseFormat then AddClause(nil);
//if aNode.NewLineBefore then AddClause;
  AddNewLine(aNode, gttkNewLineBefore);
  AddStr(gtkwCase);

  { [expression] }
  lCaseExpr := aNode.Find(gtsiExprTree, gtkwCase);
  List_ExprTree(lCaseExpr, aListerOpt);

  { WHEN [expression | condition] THEN expression }
  for i := 0 to aNode.Count - 1 do
    if aNode[i].Kind = gtssWhenThenCondExpr then begin
    //if Options[ gtstCaseWhenAtNewLine ] and not lSkipSubCaseFormat then AddClause(nil);
    //AddStr(gtkwWhen);

      if Assigned(lCaseExpr) then begin
        lNode := aNode[i].Find(gtsiExprTree, gtkwWhen);
      //if Assigned(lNode) and lNode.NewLineBefore then AddClause;
        AddNewLine(aNode, gttkNewLineBefore);
        AddStr(gtkwWhen);
        List_ExprTree(lNode, aListerOpt);
      end else begin
        lNode := aNode[i].Find(gtsiCondTree, gtkwWhen);
      //if Assigned(lNode) and lNode.NewLineBefore then AddClause;
        AddNewLine(lNode, gttkNewLineBefore);
        AddStr(gtkwWhen);
        List_CondTree(lNode, aListerOpt);
      end;

//    if Options[ gtstCaseThenAtNewLine ] and not lSkipSubCaseFormat then AddClause(nil);
//    AddStr(gtkwThen);

      lNode := aNode[i].Find(gtsiExprTree, gtkwThen);
    //if Assigned(lNode) and lNode.NewLineBefore then AddClause;
      AddNewLine(lNode, gttkNewLineBefore);
      AddStr(gtkwThen);
      List_ExprTree(lNode, aListerOpt);
    end;

  { BNF: ELSE expression }
  lElseExpr := aNode.Find(gtsiExprTree, gtkwElse);
  if Assigned(lElseExpr) then begin
  //if Options[ gtstCaseElseAtNewLine ] and not lSkipSubCaseFormat then AddClause(nil);
  //if lElseExpr.NewLineBefore then AddClause;
    AddNewLine(lElseExpr, gttkNewLineBefore);
    AddStr(gtkwElse);
    List_ExprTree(lElseExpr, aListerOpt);
  end;

//if Options[ gtstCaseEndAtNewLine ] and not lSkipSubCaseFormat then AddClause(nil);
//if Assigned(lNode) and lNode.NewLineBefore or
//   Assigned(lElseExpr) and lElseExpr.NewLineBefore then AddClause;
  AddNewLine(lNode, gttkNewLineBefore);
  AddNewLine(lElseExpr, gttkNewLineBefore);

  AddStr(gtkwEnd);
end;

{ lists expression CAST }
procedure  TGtSqlFormatLister.List_ExprCast;
begin
  AddStr(gtkwCast);
  AddStr(gttkLeftBracket, False{not Options[ gtstSpaceInsideBracketsSkipFun ]});

  List_ExprTree(aNode.Find{ByKind}(gtsiExprTree), aListerOpt);

  AddStr(gtkwAs);

  List_DataType(aNode, aListerOpt);

  AddStr(gttkRightBracket, gttkRightBracket.TokenStyle, True, False {Options[ gtstSpaceInsideBracketsSkipFun ]});
end;

{ lists expression CONVERT }
procedure  TGtSqlFormatLister.List_ExprConvert;
var lExprTree: TGtSqlNode;
begin
  AddStr(gtkwConvert);
  AddStr(gttkLeftBracket);

  List_DataType(aNode, aListerOpt);

  AddComma;
  lExprTree := aNode.Find{ByKind}(gtsiExprTree);
  List_ExprTree(lExprTree, aListerOpt);
  lExprTree := aNode.Find{ByKind}(gtsiExprTree, nil, '', lExprTree);
  if Assigned(lExprTree) then AddComma;
  List_ExprTree(lExprTree, aListerOpt);

  AddStr(gttkRightBracket);
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

  lExprList := aNode.Find{ByKind}(gtsiExprList);

  AddStr(gttkLeftBracket, gttkLeftBracket.TokenStyle, True, Assigned(lExprList) and (lExprList.Count = 1));

  if aNode.Check(gtsiExpr, gtkwDistinct) then AddStr(gtkwDistinct, False);

  List_ExprList(lExprList, aListerOpt); // + [gtloSkipOneExprOnLine]);

  AddStr(gttkRightBracket, gttkRightBracket.TokenStyle, True, Assigned(lExprList) and (lExprList.Count = 1));

  { ORACLE: KEEP DENSE RANK }
  lNode := aNode.Find(gtsiNone, gtkwKeep);
  if Assigned(lNode) then begin
//if {(Dialect = gtdlOracle) and} (aNode.KeepName <> '') then begin
//if {(Dialect = gtdlOracle) and} (aNode.Name1 <> '') then begin
    AddStr(gtkwKeep);
    AddLeftBracket;
    AddStr(gtkwDenseRank);
  //AddStr(aNode.KeepName, gtlsAggrFunction);
  //AddStr(aNode.Name1, gtlsAggrFunction);
    AddStr(lNode.Name, gtlsAggrFunction);
    List_Clause_Expr(lNode.Find(gtsiExprList, gtkwOrder_By),
                     aListerOpt {+ [gtloSkipOneExprOnLine]}, gtkwOrder_By, True {ClauseAppendCondition});
    AddRightBracket;
  end;
end;

{ lists identifier }
procedure TGtSqlFormatLister.List_ExprColumn;

    function FindStyleForColumnPrefix(var aColumnPrefix: String): TGtLexTokenStyle;
    var i{, lDeep}: Integer;
        lQuery, lTabClause, lAlias: TGtSqlNode;
    begin
      Result := gtlsError;
      if aColumnPrefix = '' then Exit;

      lQuery := aNode.GetQuery;
      //lDeep := 0;

      while Assigned(lQuery) do begin
        lTabClause := lQuery.Find{ByKind}(gtsiClauseTables);
        if not Assigned(lTabClause) then Exit;

        for i := 0 to lTabClause.Count - 1 do
          if lTabClause[i].Check(gtsiTableRef) or lTabClause[i].Check(gtsiDml, gtkwSelect) then begin
            lAlias := lTabClause[i].Find(gtsiNone, gtkwAs);
//          if AnsiUpperCase(lTabClause[i].AliasName) = AnsiUpperCase(aColumnPrefix) then begin
          //if AnsiUpperCase(lTabClause[i].Name1) = AnsiUpperCase(aColumnPrefix) then begin
            if Assigned(lAlias) and (AnsiUpperCase(lAlias.Name) = AnsiUpperCase(aColumnPrefix)) then begin
              if lQuery = aNode.GetQuery then Result := gtlsTableAlias else Result := gtlsExtQueryAliasOrTable;
//            if CaseOpt[ gtlcTableAlias ] = gtcoFirstUseCase then aColumnPrefix := lTabClause[i].AliasName;

              Exit;
            end else
//          if (AnsiUpperCase(lTabClause[i].Name) = AnsiUpperCase(aColumnPrefix)) and (lTabClause[i].AliasName = '') or
          //if (AnsiUpperCase(lTabClause[i].Name) = AnsiUpperCase(aColumnPrefix)) and (lTabClause[i].Name1 = '') or
               // column prefix is a full-part of an table name ie. lejek_projekt.numer for drk.lejek_projekt
//             (Pos('.'+AnsiUpperCase(aColumnPrefix), AnsiUpperCase(lTabClause[i].Name)) > 0) and (lTabClause[i].AliasName = '') then begin
             //(Pos('.'+AnsiUpperCase(aColumnPrefix), AnsiUpperCase(lTabClause[i].Name)) > 0) and (lTabClause[i].Name1 = '') then begin
            if not Assigned(lAlias) and (
              (AnsiUpperCase(lTabClause[i].Name) = AnsiUpperCase(aColumnPrefix))  or
              (Pos('.'+AnsiUpperCase(aColumnPrefix), AnsiUpperCase(lTabClause[i].Name)) > 0) ) then begin

              if lQuery = aNode.GetQuery then Result := gtlsTable else Result := gtlsExtQueryAliasOrTable;
//            if CaseOpt[ gtlcTable ] = gtcoFirstUseCase then aColumnPrefix := lTabClause[i].Name;

              Exit;
            end;
          end;

        lQuery := lQuery.GetExtQuery;
        //Inc(lDeep);

        { ORACLE nested query tables identifiers are limited to 1 level }
        //if (Dialect = gtdlOracle) and (lDeep > 1) then Exit;
      end;
    end;

var lColumnPrefix, lColumnName: String;
//  cnt: Integer;
begin
  strBreakOnLast('.', aNode.Name, lColumnPrefix, lColumnName);
//if lColumnPrefix = '' then cnt := 0 else cnt := 1 + strCountChars('.', lColumnPrefix);

//  if (gtloOnLeftSideIntend in aListerOpt) and (Length(lColumnPrefix) < ML_LeftOnExprPrefix)
//    then AddSpace(ML_LeftOnExprPrefix - Length(lColumnPrefix) + 1); // +1 due to ON keyword
//  if (gtloOnRightSideIntend in aListerOpt) and (Length(lColumnPrefix) < ML_RightOnExprPrefix)
//    then AddSpace(ML_RightOnExprPrefix - Length(lColumnPrefix));

  if lColumnPrefix <> '' then begin
    AddStr(lColumnPrefix, FindStyleForColumnPrefix(lColumnPrefix), CheckSpaceNeedBeforeExpression);
    AddStr(gttkDot, False);
    AddStr(lColumnName, gtlsColumn, False);
  end else begin
    AddStr(lColumnName, gtlsColumn, CheckSpaceNeedBeforeExpression);
  end;

//  if (gtloOnLeftSideIntend in aListerOpt) and (ML_LeftOnExprColumn - Length(lColumnName) > 0) then begin
//    if cnt=0 then AddSpace(1); // +1 due to no dot
//    AddSpace(ML_LeftOnExprColumn - Length(lColumnName) + 1); // +1 due to operator
//  end;
//  if (gtloOnRightSideIntend in aListerOpt) and (ML_RightOnExprColumn - Length(lColumnName) > 0) then begin
//    if cnt=0 then AddSpace(1); // +1 due to no dot
//    AddSpace(ML_RightOnExprColumn - Length(lColumnName) + 1); // +1 due to space after
//  end;
end;

{ lists expressions list }
procedure TGtSqlFormatLister.List_ExprList;
var i: Integer;
    lFirst: Boolean;
begin
  if not Assigned(aNode) then Exit;

//AddLeftBracket(aNode.BracketsCount);
  AddLeftBracket(aNode);

  lFirst := True;
  for i := 0 to aNode.Count - 1 do
    if aNode[i].Kind in [gtsiExpr, gtsiExprTree] then begin
      if not lFirst then AddCommaAfterExpr(aListerOpt);
      List(aNode[i], aListerOpt);

//      if (i < aNode.Count -1) and not aNode[i].Check(gtsiExpr, gttkFunParameter)
//        then AddCommaAfterExpr(aListerOpt);
      lFirst := False;
    end;

//AddRightBracket(aNode.BracketsCount);
  AddRightBracket(aNode);
end;

{ lists expression tree }
procedure TGtSqlFormatLister.List_ExprTree;
var i: Integer;
    lNode: TGtSqlNode;
begin
  if not Assigned(aNode) then Exit;

//AddLeftBracket(aNode.BracketsCount);
  AddLeftBracket(aNode);

  { list sub-expressions }
  for i := 0 to aNode.Count - 1 do
    if (aNode[i].Kind in [gtsiExpr, gtsiExprTree]) or aNode[i].Check(gtsiDml, gtkwSelect) then begin
      if i > 0 then begin
//        if (aNode[i].ExprReverseOp) and (aNode.Keyword {Operand} = gttkPlus) then AddStr(gttkMinus) else
//        if (aNode[i].ExprReverseOp) and (aNode.Keyword {Operand} = gttkStar) then AddStr(gttkSlash) else
//        if (aNode[i].ExprReverseOp2)and (aNode.Keyword {Operand} = gttkStar) then AddStr(gttkPercent)

//        if (aNode.Keyword {Operand} = gttkPlus) and aNode[i].KeywordAuxCheck(gttkMinus)  then AddStr(gttkMinus) else
//        if (aNode.Keyword {Operand} = gttkStar) and aNode[i].KeywordAuxCheck(gttkSlash)  then AddStr(gttkSlash) else
//        if (aNode.Keyword {Operand} = gttkStar) and aNode[i].KeywordAuxCheck(gttkPercent)then AddStr(gttkPercent)
//        else AddStr(aNode.Keyword {Operand});

//      aListerOpt := aListerOpt - [ gtloOnLeftSideIntend, gtloOnRightSideIntend ];

         if aNode[i].KeywordAuxCheck(gttkPlus) then begin
           if aNode[i].KeywordAuxCheck(gttkMinus) then AddStr(gttkMinus) else AddStr(gttkPlus);
         end else
         if aNode[i].KeywordAuxCheck(gttkStar) then begin
           if aNode[i].KeywordAuxCheck(gttkSlash) then AddStr(gttkSlash) else
           if aNode[i].KeywordAuxCheck(gttkPercent) then AddStr(gttkPercent) else AddStr(gttkStar);
         end else AddStr(gttkConcatenation);
      end;

      if aNode[i].Check(gtsiExprTree) or
         aNode[i].Check(gtsiExpr, gttkNumber) or aNode[i].Check(gtsiExpr, gtkwVarchar) or
         aNode[i].Check(gtsiExpr, gttkIdentifier) or aNode[i].Check(gtsiExpr, gttkParameterName) or
         aNode[i].Check(gtsiExpr, gttkColumnName) or aNode[i].Check(gtsiExpr, gttkStar)
        then List(aNode[i], aListerOpt)
        else List(aNode[i], aListerOpt); // - [{gtloOnLeftSideIntend, gtloCondLeftSideOrderREMOVED{, gtloOnRightSideIntend}]);
    end;

//AddRightBracket(aNode.BracketsCount);
  AddRightBracket(aNode);

  { adds alias }
  { WARN: SkipOutput was prepared to check max expressions length }
  lNode := aNode.Find(gtsiNone, gtkwAs);
//if (aNode.AliasName <> '') and not SkipOutput and
//if (aNode.Name1 <> '') and not SkipOutput and
  if Assigned(lNode)     and not SkipOutput and
    ((Length(RawText) = 0) or (RawText[Length(RawText)] <> '*')) {skip alias after star expr.} then begin
//    if (gtloExprAliasIntend in aListerOpt) and (SkipOutput_MaxLineLength > Length(RawText))
//      then AddSpace(SkipOutput_MaxLineLength - Length(RawText) + 1);

//    if Options[ gtstExprAsKeyword ] then AddStr(gtkwAs) else AddSpace;
//  if aNode.AliasAsToken then AddStr(gtkwAs) else AddSpace;
//  if aNode.KeywordAfter1 = gtkwAs then AddStr(gtkwAs) else AddSpace;
//  if aNode.KeywordAuxCheck(gtkwAs) then AddStr(gtkwAs) else AddSpace;
//  if lNode.KeywordExt = gtkwAs then AddStr(gtkwAs) else AddSpace;
    if lNode.KeywordAuxCheck(gtkwAs) then AddStr(gtkwAs) else AddSpace;

//    if gtloExprAliasIntend in aListerOpt
//      then AddSpace(ML_ExprAlias - Length(aNode.AliasName) + 1);

//  AddStr(aNode.AliasName, gtlsColumnAlias);
//  AddStr(aNode.Name1, gtlsColumnAlias);
    AddStr(lNode.Name, gtlsColumnAlias);
  end;

  { adds sort order }
//AddStr(aNode.KeywordAfter1 {SortOrder});
//  if Assigned(aNode.Owner) then begin
//      if (aNode.Owner.Kind = gtsiExprList) and (aNode.Owner.Keyword = gtkwOrder_By) then begin
//
//        if Options[ gtstSortShort ] then begin
//          if(aNode.SortOrder = gtkwDesc)or(aNode.SortOrder = gtkwDescending) then AddStr(gtkwDesc) else
//          if{((aNode.SortOrder = gtkwAsc)or(aNode.SortOrder = gtkwAscending)) and} not Options[ gtstSkipAscending ] then AddStr(gtkwAsc);
//        end else begin
//          if(aNode.SortOrder = gtkwDesc)or(aNode.SortOrder = gtkwDescending)then AddStr(gtkwDescending) else
//          if{((aNode.SortOrder = gtkwAsc)or(aNode.SortOrder = gtkwAscending)) and} not Options[ gtstSkipAscending ] then AddStr(gtkwAscending);
//        end;
//
//      end;
//  end;
  AddStr(aNode.KeywordAuxCheckKwd(gtkwAsc, gtkwAscending, gtkwDesc, gtkwDescending));

  { BNF: NULLS FIRST | NULLS LAST }
//AddStr(aNode.KeywordAfter1);
//  if aNode.NullsFirst then AddStr(gtkwNulls_First) else
//  if aNode.NullsLast  then AddStr(gtkwNulls_Last);
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
  //if Options[ gtstSetExprIntend ] then AddSpace(ML_SetExpr_LeftSide + 1 - Length(lCol.Name));
  end else begin
    { TODO: check formatting }
    lVect := aNode.Find(gtsiExprList, gtkwSet);
    if Assigned(lVect) then begin
      List(lVect, aListerOpt);
//    end else begin
//      if Options[ gtstSetExprIntend ] then AddSpace(ML_SetExpr_LeftSide + 1 );
    end;
  end;

  AddStr(gttkEqual);

  { right side }
  lSelect := aNode.Find{BySubKind}(gtsiDml, gtkwSelect);
  if Assigned(lSelect) then begin
    List(lSelect, aListerOpt);
  end else begin
    lVect := aNode.Find(gtsiExprList, gtkwSet, '', lVect);
    if Assigned(lVect) then begin
      List(lVect, aListerOpt);
    end else begin
      List(aNode.Find{ByKind}(gtsiExprTree), aListerOpt);
    end;
  end;
end;

{ lists set expressions list }
procedure TGtSqlFormatLister.List_SetExprList;
var i {, lML_SetExpr_LeftSide}: Integer;
    b: Boolean;
//  lCol: TGtSqlNode;
begin
  if not Assigned(aNode) then Exit;

  { max left size to intend }
//lML_SetExpr_LeftSide := ML_SetExpr_LeftSide;

//  if Options[ gtstSetExprIntend ] then begin
//    ML_SetExpr_LeftSide := 0;
//
//    for i := 0 to aNode.Count - 1 do begin
//      lCol := aNode[i].Find(gtsiExpr, gttkColumnName);
//      if Assigned(lCol) //and (Length(lCol.Name) < MaxColumnNameToIntend) //MaxSetLeftExprToIntend)
//                        and (Length(lCol.Name) > ML_SetExpr_LeftSide)
//        then ML_SetExpr_LeftSide := Length(lCol.Name);
//    end;
//  end;

  { SET }
  AddClause(gtkwSet);

  b := False;
  for i := 0 to aNode.Count - 1 do
    if aNode[i].Check(gtsiSetExpr) then begin
      if b then AddCommaAfterExpr(aListerOpt);

      List_SetExpr(aNode[i], aListerOpt);
      b := True;
    end;

//ML_SetExpr_LeftSide := lML_SetExpr_LeftSide;
end;

{ lists condition }
procedure TGtSqlFormatLister.List_Cond;
var lExpr, lItem: TGtSqlNode;
begin
  if not Assigned(aNode) then Exit;

//if aNode.Negation then AddStr(gtkwNot);
  AddStr(aNode.KeywordAuxCheckKwd(gtkwNot));
//AddLeftBracket(aNode.BracketsCount);
  AddLeftBracket(aNode);

  if (aNode.Keyword {Operand} = gtkwExists) or (aNode.Keyword {Operand} = gtkwNot_Exists) then begin
    AddStr(aNode.Keyword {Operand});
    List(aNode.Find(gtsiDml, gtkwSelect), aListerOpt);
  end else
  if (aNode.Keyword {Operand} = gtkwIn) or (aNode.Keyword {Operand} = gtkwNot_In) then begin
    List( aNode.Find(gtsiNone, nil, '1'), aListerOpt {- [ gtloOnRightSideIntend ]});

    AddStr(aNode.Keyword {Operand});
    AddSpace; // independent of space-outside-brackets !!

    lExpr := aNode.Find(gtsiNone, nil, '2');
    if Assigned(lExpr) and lExpr.Check(gtsiExprList, gtkwIn) then begin
      AddStr(gttkLeftBracket);

      List(lExpr, aListerOpt {+ [gtloSkipOneExprOnLine] - [gtloOnLeftSideIntend, gtloOnRightSideIntend]});

      AddStr(gttkRightBracket);
    end else begin
      List(lExpr, aListerOpt);
    end;
  end else begin
    { left expression }
//    if (gtloCondEqualSwap in aListerOpt) and (aNode.{CompOp} Operand = gttkEqual)
//      then lItem := aNode.Find(gtsiNone, nil, '2')
//      else
    lItem := aNode.Find(gtsiNone, nil, '1');

    List(lItem, aListerOpt {- [ gtloOnRightSideIntend ]} );

    { operator }
    if (aNode.Keyword {Operand} = gttkEqual) then begin
//    if aNode.OuterMark1Oracle then begin
//    if aNode.KeywordExt = gttkBracketPlusBracket then begin
//      AddStr(gttkBracketPlusBracket);
//      AddStr(gttkEqual);
//    end else begin
//      if aNode.OuterMark1MSSQL
//        then AddStr(gttkStarEqual)
//        else AddStr(gttkEqual);
        AddStrKeywordExt(aNode);
//    end;
    end else
    if (aNode.Keyword {Operand} = gtkwBetween) or (aNode.Keyword {Operand} = gtkwNot_Between) or
       (aNode.Keyword {Operand} = gtkwLike)    or (aNode.Keyword {Operand} = gtkwNot_Like) or
       (aNode.Keyword {Operand} = gtkwIs_Null) or (aNode.Keyword {Operand} = gtkwIs_Not_Null)
      then AddStr(aNode.Keyword {Operand})
      else AddStr(aNode.Keyword {Operand});

    { right expression }
//  if aNode.OuterMark2MSSQL and (aNode.Keyword {Operand} = gttkEqual) {and (Dialect = gtdlMicrosoftSql)} then AddStr(gttkEqualStar);
//    if (gtloCondEqualSwap in aListerOpt) and (aNode.Keyword {Operand} = gttkEqual)
//      then lItem := aNode.Find(gtsiNone, nil, '1')
//      else
    lItem := aNode.Find(gtsiNone, nil, '2');

    List(lItem, aListerOpt {- [gtloOnLeftSideIntend]});
//  if aNode.OuterMark2Oracle and (aNode.Keyword {Operand} = gttkEqual) {and (Dialect = gtdlOracle)}
//    if (aNode.KeywordExt = gttkBracketPlusBracket) and (aNode.Keyword {Operand} = gttkEqual) {and (Dialect = gtdlOracle)}
//      then AddStr(gttkBracketPlusBracket);

    { additional expression }
    if (aNode.Keyword {Operand} = gtkwBetween) or (aNode.Keyword {Operand} = gtkwNot_Between) then begin
      AddStr(gtkwAnd);
      List(aNode.Find(gtsiNone, nil, '3'), aListerOpt {- [gtloOnLeftSideIntend, gtloOnRightSideIntend]});
    end else
  //if ((aNode.Keyword {Operand} = gtkwLike) or (aNode.Keyword {Operand} = gtkwNot_Like)) and (aNode.CondEscape <> '') then begin
  //if ((aNode.Keyword {Operand} = gtkwLike) or (aNode.Keyword {Operand} = gtkwNot_Like)) and (aNode.Name2 <> '') then begin
    if ((aNode.Keyword {Operand} = gtkwLike) or (aNode.Keyword {Operand} = gtkwNot_Like)) then begin
      //AddStr(gtkwEscape);
      //AddStr(aNode.CondEscape, gtlsString);
      //AddStr(aNode.Name2, gtlsString);
      AddStrKeywordName( aNode.Find(gtsiNone, gtkwEscape), gtlsString);
    end;

    { collate }
  //if aNode.CollateName <> '' then begin
//    if aNode.Name1 <> '' then begin
//      AddStr(gtkwCollate);
//    //AddStr(aNode.CollateName, gtlsString);
//      AddStr(aNode.Name1, gtlsString);
//    end;
    AddStrKeywordName( aNode.Find(gtsiNone, gtkwCollate), gtlsString );
  end;

//AddRightBracket(aNode.BracketsCount);
  AddRightBracket(aNode);
end;

{ lists condition tree }
procedure TGtSqlFormatLister.List_CondTree;
var b: Boolean;
    i: Integer;
begin
  if not Assigned(aNode) then Exit;

//if aNode.NoCycle  then AddStr(gtkwNoCycle);
//if aNode.Negation then AddStr(gtkwNot);
//if aNode.KeywordExt = gtkwNoCycle then AddStr(aNode.KeywordExt);
  if aNode.KeywordAuxCheck(gtkwNoCycle) then AddStr(gtkwNoCycle);
  AddStr(aNode.KeywordAuxCheckKwd(gtkwNot));
//AddLeftBracket(aNode.BracketsCount);
  AddLeftBracket(aNode);

  b := False;
  for i := 0 to aNode.Count - 1 do
    if aNode[i].Check(gtsiCond) or aNode[i].Check(gtsiCondTree) then begin
      if b then begin
      //if Options[ gtstOneCondOnLine ] and not (gtloSkipOneCondOnLine in aListerOpt)
      //  then AddClause(aNode.Keyword {Operand})
          {else} AddStr(aNode.KeywordExt {Operand});
      end;

//      if not b then begin
//        { first condition }
//        if (gtloCondLeftSideOrder in aListerOpt) and (aNode[i].CompOp = gttkEqual) and (aNode[i].Count = 2)
//        and aNode[i][1].ExprHasReferenceTo(aNode.OwnerTableNameOrAlias)
//        and not aNode[i][0].ExprHasReferenceTo(aNode.OwnerTableNameOrAlias)
//          then List(aNode[i], aListerOpt + [gtloCondEqualSwap])
//          else List(aNode[i], aListerOpt - [gtloCondEqualSwap]);
//      end else begin
//        { next conditions }
        List(aNode[i], aListerOpt {- [gtloOnLeftSideIntend, gtloCondLeftSideOrderREMOVED, gtloOnRightSideIntend]});
//      end;

      b := True;
    end;

//AddRightBracket(aNode.BracketsCount);
  AddRightBracket(aNode);
end;

{ lists column }
procedure TGtSqlFormatLister.List_ColumnDef;
var lUnique, lDefault, lNode: TGtSqlNode;
    {lLen,} i: Integer;
//  lIntend: Boolean;
begin
  if not Assigned(aNode) then Exit;

  AddStr(aNode.Name, gtlsColumn);

//  lLen := 0;
//  lIntend := Options [ gtstCreateTable_Intend ];
//  if lIntend then begin
//    lIntend := ML_ColumnName - Length(aNode.Name) >= 0;
//
//    if ML_ColumnName - Length(aNode.Name) > 0
//      then AddSpace(ML_ColumnName - Length(aNode.Name) + 1);
//
//    lLen := Length(RawText);
//    if (Copy(RawText, Length(RawText) - 1 + 1, 1) <> ' ') then Inc(lLen); // AddClearSpace will add a space
//  end;

  if aNode.Keyword {DataType} = gtkwType then begin
  //AddStr(aNode.TableName, gtlsTable);
  //AddStr(aNode.Name1, gtlsTable);
    lNode := aNode.Find(gtsiNone, gttkTableName);
    if Assigned(lNode) then begin
      AddStr(lNode.Name, gtlsTable);
      AddStr(gttkDot, False);
    //AddStr(aNode.ColumnName, gtlsColumn, False);
    //AddStr(aNode.Name2, gtlsColumn, False);
      lNode := lNode.Find(gtsiNone, gttkColumnName);
      if Assigned(lNode) then AddStr(aNode.Name, gtlsColumn);
    end;

    AddStr(gttkPercent, False);
    //AddStr(aNode.DataType, False);
    AddStr(gtkwType, False);
  end else begin
    List_DataType(aNode, aListerOpt);
  end;

//  if lIntend and (ML_DataType - Length(RawText) + lLen > 0)
//    then AddSpace(ML_DataType - Length(RawText) + lLen + 1);

//if aNode.Identity then begin
  if aNode.KeywordExt = gtkwIdentity then begin
    AddStr(gtkwIdentity);
    lNode := aNode.Find(gtsiNone, gtkwSize);
//  if aNode.ColIdentitySeed >0 then begin
//  if aNode.Int1 >0 then begin
    if Assigned(lNode) then begin
      AddStr(gttkLeftBracket);
    //AddStr(IntToStr(aNode.ColIdentitySeed), gtlsNumber, False);
    //AddStr(IntToStr(aNode.Int1), gtlsNumber, False);
      AddStr(lNode.Name, gtlsNumber, False);
    //if aNode.ColIdentityInc >0 then AddComma;
    //if aNode.ColIdentityInc >0 then AddStr(IntToStr(aNode.ColIdentityInc), gtlsNumber, False);

      lNode := lNode.Find(gtsiNone, gtkwPrec);
      if Assigned(lNode) then begin
      //if aNode.Int2 >0 then AddComma;
      //if aNode.Int2 >0 then AddStr(IntToStr(aNode.Int2), gtlsNumber, False);
        AddComma;
        AddStr(lNode.Name, gtlsNumber, False);
      end;
      AddStr(gttkRightBracket);
    end;
  end;

//if aNode.CollateName <> '' then begin
//  if aNode.Name1 <> '' then begin
//    AddStr(gtkwCollate);
//    AddSpace;
//  //AddStr(aNode.CollateName, gtlsIdentifier);
//    AddStr(aNode.Name1, gtlsIdentifier);
//  end;
  AddStrKeywordName( aNode.Find(gtsiNone, gtkwCollate), gtlsString );

  lDefault := aNode.Find(gtsiExprTree);
  if Assigned(lDefault) then begin
    AddStr(gtkwDefault);
    List_ExprTree(lDefault, aListerOpt);
  end;

//  case aNode.Nullable of
//    gtopNull:    AddStr(gtkwNull);
//    gtopNotNull: AddStr(gtkwNot_Null);
//  end;
//AddStr(aNode.KeywordAfter1);
  AddStr(aNode.KeywordAuxCheckKwd(gtkwNull, gtkwNot_Null));

  lUnique := aNode.Find(gtsiDDL, gtkwCreate_Index);
  if Assigned(lUnique) and lUnique.KeywordExt.HasSubToken(gtkwUnique) then AddStr(gtkwUnique);
//if Assigned(lUnique) and lUnique.Unique then AddStr(gtkwUnique);

  for i := 0 to aNode.Count - 1 do begin
    if (aNode[i].Kind = gtsiConstraint) then begin
    //if Options[ gtstCreateTable_ColConsBreakLine ] then AddClause;
    //if aNode[i].NewLineBefore then AddClause;
      AddNewLine(aNode[i], gttkNewLineBefore);
      List_Constraint(aNode[i], aListerOpt + [gtloSingleColumn]);
//    if aNode[i].NewLineAfter then AddCurrLine;
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
//      if Options [ gtstColumnConstraint ] and Options [ gtstCreateTable_Intend ] and (ML_ColumnName - Length(gtkwConstraint.Text) > 0)
//        then AddSpace(ML_ColumnName - Length(gtkwConstraint.Text) + 1);
      AddStr(aNode.Name, gtlsConstraint);
//      if Options [ gtstColumnConstraint ] and Options [ gtstCreateTable_Intend ] and (ML_DataType - Length(aNode.Name) >= 0)
//        then AddSpace(ML_DataType - Length(aNode.Name) + 1);
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

//      if Options[ gtstColumnConstraint ] and Options [ gtstCreateTable_Intend ] and (ML_ColumnName - Length(gtkwConstraint.Text) > 0)
//        then AddSpace(ML_ColumnName - Length(gtkwConstraint.Text) + 1);

      AddStr(aNode.Name, gtlsConstraint);

//      if Options[ gtstColumnConstraint ] and Options [ gtstCreateTable_Intend ] and (ML_DataType - Length(aNode.Name) > 0)
//        then AddSpace(ML_DataType - Length(aNode.Name) + 1);
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
  //AddStr(lKey.ObjectName, gtlsTable);
    AddStr(lKey.Name, gtlsTable);

    if lKey.Count > 0 then begin
      AddStr(gttkLeftBracket);
      List_ExprList(lKey, aListerOpt);
      AddStr(gttkRightBracket);
    end;
  end;

//if aNode.OnDelete <> gttkNone then begin
//if aNode.KeywordAfter1 <> gttkNone then begin
//  if aNode.KeywordAuxCheck(gtkwOn_Delete_Preserve) then begin
//    if gtloAlterTableConstraint in aListerOpt
//      then AddClause(gtkwOn_Delete)
//      else AddStr(gtkwOn_Delete);
//  //AddStr(aNode.OnDelete);
//    AddStr(aNode.KeywordAfter1);
//  end;
 if aNode.KeywordAuxCheck(gtkwOn_Delete_Restrict, gtkwOn_Delete_Cascade, gtkwOn_Delete_Set_Null) then begin
    if gtloAlterTableConstraint in aListerOpt
      then AddClause(gtkwOn_Delete)
      else AddStr(gtkwOn_Delete);
   if aNode.KeywordAuxCheck(gtkwOn_Delete_Restrict) then AddStr(gtkwRestrict) else
   if aNode.KeywordAuxCheck(gtkwOn_Delete_Cascade)  then AddStr(gtkwCascade)  else
   if aNode.KeywordAuxCheck(gtkwOn_Delete_Set_Null) then AddStr(gtkwSet_Null);
 end;

//if aNode.OnUpdate <> gttkNone then begin
//  if aNode.KeywordAfter2 <> gttkNone then begin
//    if gtloAlterTableConstraint in aListerOpt
//      then AddClause(gtkwOn_Update)
//      else AddStr(gtkwOn_Update);
//  //AddStr(aNode.OnUpdate);
//    AddStr(aNode.KeywordAfter2);
//  end;
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
//      if Options[ gtstColumnConstraint ] and Options [ gtstCreateTable_Intend ] and (ML_ColumnName - Length(gtkwConstraint.Text) > 0)
//        then AddSpace(ML_ColumnName - Length(gtkwConstraint.Text) + 1);
      AddStr(aNode.Name, gtlsConstraint);
//      if Options[ gtstColumnConstraint ] and Options [ gtstCreateTable_Intend ] and (ML_DataType - Length(aNode.Name) > 0)
//        then AddSpace(ML_DataType - Length(aNode.Name) + 1);
    end;

    AddStr(gtkwUnique);
  end;

  if (gtloTableConstraint in aListerOpt) or (gtloAlterTableConstraint in aListerOpt) then begin
    AddStr(gttkLeftBracket);
    List_ExprList(aNode, aListerOpt {+ [gtloSkipOneExprOnLine]});
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
//      if Options[ gtstColumnConstraint ] and Options [ gtstCreateTable_Intend ] and (ML_ColumnName - Length(gtkwConstraint.Text) > 0)
//        then AddSpace(ML_ColumnName - Length(gtkwConstraint.Text) + 1);
      AddStr(aNode.Name, gtlsConstraint);
//      if Options[ gtstColumnConstraint ] and Options [ gtstCreateTable_Intend ] and (ML_DataType - Length(aNode.Name) > 0)
//        then AddSpace(ML_DataType - Length(aNode.Name) + 1);
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
//  lDoIntend: Boolean;
begin
  if not Assigned(aNode) then Exit;

  if (aNode.Keyword {Operand} = gttkNone) and aNode.GetQuery.Check(gtsiDml, gtkwUpdate)
    then AddClause(gtkwUpdate, ClauseAppendCondition)
    else
  if (aNode.Keyword {Operand} = gtkwInto) and aNode.GetQuery.Check(gtsiDml, gtkwInsert) then begin
    AddClause(aNode.KeywordExt);
//    if aNode.GetQuery.OrReplace
//      then AddClause( gtkwInsert_Or_Replace_Into, ClauseAppendCondition )
//      else AddClause( gtkwInsert_Into, ClauseAppendCondition);
  end else
  if (aNode.Keyword {Operand} = gttkComma) then begin
//    if not Options[ gtstCommaAtNewLine ] then begin
      AddComma;
      AddClause(nil, ClauseAppendCondition);
//    end else begin
//      AddClause(gttkComma, ClauseAppendCondition);
//    end;
  end else
  if (aNode.Keyword {Operand} = gtkwFrom) and (gtloSkipFrom in aListerOpt)
    then
  //else AddClause( JoinOperatorToToken( aNode.Keyword {Operand}, aNode.JoinInnerKeyword, aNode.JoinOuterKeyword ), ClauseAppendCondition );
  //else AddClause( aNode.KeywordExt, ClauseAppendCondition );
    else AddClauseNode( aNode, ClauseAppendCondition );

  { table name or query }
  lSubQuery := aNode.Find(gtsiDml, gtkwSelect);
  if Assigned(lSubQuery) then begin
    { query }
    List(lSubQuery, aListerOpt);

    { query alias }
    lNode := aNode.Find(gtsiNone, gtkwAs);
//  if aNode.AliasName <> '' then begin
//  if aNode.Name1 <> '' then begin
    if Assigned(lNode) then begin
    //if Options[ gtstTableAsKeyword ] then AddStr(gtkwAs);
    //if aNode.AliasAsToken then AddStr(gtkwAs);
    //if aNode.KeywordAfter1 = gtkwAs then AddStr(gtkwAs);
    //AddStr(aNode.KeywordAuxCheckKwd(gtkwAs));
    //AddStr(lNode.KeywordExt);
      if lNode.KeywordAuxCheck(gtkwAs) then AddStr(gtkwAs);
    //AddStr(aNode.AliasName, gtlsTableAlias);
    //AddStr(aNode.Name1, gtlsTableAlias);
      AddStr(lNode.Name, gtlsTableAlias);
    end;
  end else begin
    { table name }
    AddStr(aNode.Name, gtlsTable);

    //lDoIntend := Options[ gtstTableAndAliasIntend ];
    //       and (Length(aNode.Name) < MaxTableNameToIntend)
    //       and (Length(aNode.AliasName) < MaxAliasNameToIntend);

    { table alias, +1 because of identifier extra space }
  //AddStrKeywordExtName(aNode.Find(gtsiNone, gtkwAs), gtlsTableAlias);
    lNode := aNode.Find(gtsiNone, gtkwAs);
    if Assigned(lNode) then begin
      if lNode.KeywordAuxCheck(gtkwAs) then AddStr(gtkwAs);
      AddStr(lNode.Name, gtlsTableAlias);
    end;

//  if not lDoIntend then begin
    //if Options[ gtstTableAsKeyword ] then AddStr(gtkwAs);
    //if aNode.AliasAsToken then AddStr(gtkwAs);
    //if aNode.KeywordAfter1 = gtkwAs then AddStr(gtkwAs);
    //AddStr(aNode.KeywordAuxCheckKwd(gtkwAs));
    //AddStr(aNode.AliasName, gtlsTableAlias);
    //AddStr(aNode.Name1, gtlsTableAlias);
//    end else begin
//    //if Options[ gtstTableAsKeyword ] then begin
//      if aNode.AliasAsToken then begin
//        AddSpace(ML_TableName + 1 - Length(aNode.Name));
//        if aNode.AliasName = '' then AddSpace(3) else AddStr(gtkwAs);
//        AddSpace(ML_AliasName + 1 - Length(aNode.AliasName));
//        AddStr(aNode.AliasName, gtlsTableAlias);
//      end else
//      if ML_AliasName > 0 then begin
//        AddSpace(ML_TableAndAliasName + 1 - Length(aNode.Name) - Length(aNode.AliasName));
//        if aNode.AliasName = '' then begin
//          AddSpace(2);
//        end else begin
//          AddSpace(1);
//          AddStr(aNode.AliasName, gtlsTableAlias);
//        end;
//      end else begin
//        AddSpace(ML_TableAndAliasName - Length(aNode.Name) - Length(aNode.AliasName));
//      end;
//    end;
  end;

  { join condition }
  if (aNode.Keyword {Operand} = gtkwInner) or (aNode.Keyword {Operand} = gtkwLeft) or
     (aNode.Keyword {Operand} = gtkwRight) or (aNode.Keyword {Operand} = gtkwFull) then begin

    List(aNode.Find(gtsiCondTree, gtkwOn), {lTreeNode,} aListerOpt);
    List(aNode.Find(gtsiCondTree, gtkwUsing), {lTreeNode,} aListerOpt);
  end;
end;

{ lists CREATE TABLE }
procedure TGtSqlFormatLister.List_CreateTable;
var lItem: TGtSqlNode;
    {i,} j, cnt, cnt2, lIntend{, lDataTypeLen}: Integer;
  //lNewLine: Boolean;
begin
  if not Assigned(aNode) then Exit;
  FKeywordStyle := gtlsDdlCreate;
  lIntend := NewLineIntend;

  { list: CREATE [[GLOBAL] TEMPORARY] TABLE table-name }
//if aNode.Temporary then begin
//  if aNode.KeywordExt.HasSubToken(gtkwTemporary) then begin
//  //if aNode.Global then begin
//    if aNode.KeywordExt.HasSubToken(gtkwGlobal) then begin
//      AddClause(gtkwCreate_Global);
//      AddStr(gtkwTemporary_Table);
//    end else begin
//      AddClause(gtkwCreate);
//      AddStr(gtkwTemporary_Table);
//    end;
//  end else begin
//    AddClause(gtkwCreate_Table);
//  end;
//AddStr(aNode.Name, gtlsTable);
  AddStrKeywordExtName(aNode, gtlsTable);

  { BNF: [AS SELECT ...] }
  lItem := aNode.Find(gtsiDml, gtkwSelect);
  if Assigned(lItem) then begin
    AddStr(gtkwAs);
    List_DML(lItem, aListerOpt);
    Exit;
  end;

  { list: spacings }
  AddClause(gttkLeftBracket);
  if ClauseIntend then NewLineIntend := Length( RawText ) - ML_ClauseKeyword - ClauseBodySpace;

  { check max column name and datatype length }
//ML_ColumnName := 0;
//ML_DataType := 0;
//  if Options [ gtstCreateTable_Intend ] then begin
//    for j := 0 to aNode.Count - 1 do
//      if aNode[j].Kind = gtssOtherColumnDef then begin
//        { column name }
//        if (Length(aNode[j].Name) > ML_ColumnName) //and (Length(aNode[j].Name) <= MaxColumnNameToIntend)
//          then ML_ColumnName := Length(aNode[j].Name);
//
//        { data type }
//        if aNode[j].DataType = gtkwType then begin
//          lDataTypeLen := Length(aNode[j].TableName + '.' + aNode[j].ColumnName + '%' + aNode[j].DataType.Text);
//        end else begin
//          lDataTypeLen := Length(aNode[j].DataType.Text);
//          if aNode[j].ColSize <> gtsqlSizeOrPrecNotSpecified then begin
//            Inc(lDataTypeLen,2); // gttkLeftBracket
//            Inc(lDataTypeLen, Length(IntToStr(aNode[j].ColSize)));
//            if aNode[j].ColPrec <> gtsqlSizeOrPrecNotSpecified then Inc(lDataTypeLen,2); // AddComma
//            if aNode[j].ColPrec <> gtsqlSizeOrPrecNotSpecified then Inc(lDataTypeLen, Length(IntToStr(aNode[j].ColPrec)));
//            Inc(lDataTypeLen,2); // gttkRightBracket
//          end;
//        end;
//        if (lDataTypeLen > ML_DataType) //and (lDataTypeLen <= MaxDatatypeToIntend)
//          then ML_DataType := lDataTypeLen;
//
//        { constraint at new line }
////        if Options[ gtstColumnConstraint ] then begin
////          for i := 0 to aNode.Count - 1 do begin
////            lItem := aNode[i];
////            if (lItem.Kind = gtsiConstraint) and lItem.SingleColumnConstraint
////            and (lItem.Find(gtsiExpr, gttkColumnName).Name = aNode[j].Name) then begin
////              if (Length(gtkwConstraint.Text) > ML_ColumnName) //and (Length(gtkwConstraint.Text) <= MaxColumnNameToIntend)
////                then ML_ColumnName := Length(gtkwConstraint.Text);
////              if (Length(lItem.Name) > ML_DataType) //and (Length(lItem.Name) <= MaxDatatypeToIntend)
////                then ML_DataType := Length(lItem.Name);
////            end;
////          end;
////        end;
//      end;
//  end;

  { list columns }
  cnt := 0;
  cnt2 := 0;
  for j := 0 to aNode.Count - 1 do
    case aNode[j].Kind of
      gtssOtherColumnDef : begin
    //lNewLine := False;

      if cnt > 0 then begin
//        if Options[ gtstCommaAtNewLine ] then begin
//          AddClause(gttkComma);
//        end else begin
          AddComma;
          AddClause(nil);
//        end;
      end;

        List_ColumnDef(aNode[j], {lTreeNode,} aListerOpt);

      { single column constraint }
//        if Options[ gtstColumnConstraint ] then begin
//          for i := 0 to aNode.Count - 1 do begin
//            lItem := aNode[i];
//            if (lItem.Kind = gtsiConstraint) and lItem.SingleColumnConstraint
//            and (lItem.Find(gtsiExpr, gttkColumnName).Name = aNode[j].Name) then begin
//              if Options[ gtstCreateTable_ColConsBreakLine ] then AddClause;
//              List_Constraint(lItem, lTreeNode, aListerOpt + [gtloSingleColumn]);
//              lNewLine := True;
//            end;
//          end;
//        end;

//      if lNewLine and Options[gtstCreateTable_ColConsNewLineAfter] then AddCurrLine;

      Inc(cnt);
    end;
      gtsiConstraint : begin
      //if not Options[ gtstColumnConstraint ] or not aNode[j].SingleColumnConstraint then begin
//      if (cnt2 = 0) and Options [ gtstCreateTable_EmptyLineBefComplexConstr ]
//          then AddCurrLine;
      //if (cnt2 = 0) and aNode[j].EmptyLineBefore
      //    then AddEmptyLine;
        if (cnt2 = 0) then AddNewLine(aNode[j], gttkEmptyLineBefore);

        if cnt > 0 then begin
//          if Options[ gtstCommaAtNewLine ] then begin
//            AddClause(gttkComma);
//          end else begin
            AddComma;
            AddClause(nil);
//          end;
        end;

          List_Constraint(aNode[j], {lTreeNode,} aListerOpt + [gtloTableConstraint]);

        Inc(cnt);
        Inc(cnt2);
      //end;
    end;
    end;

  { list complex constraints }
//  cnt2 := 0;
//  for i := 0 to aNode.Count - 1 do
//    if aNode[i].Kind = gtsiConstraint then begin
//
//      if not Options[ gtstColumnConstraint ] or not aNode[i].SingleColumnConstraint then begin
//        if (cnt2 = 0) and Options [ gtstCreateTable_EmptyLineBefComplexConstr ]
//          then AddCurrLine;
//
//        if cnt > 0 then begin
//          if Options[ gtstCommaAtNewLine ] then begin
//            AddClause(gttkComma);
//          end else begin
//            AddComma;
//            AddClause(nil);
//          end;
//        end;
//
//        List_Constraint(aNode[i], lTreeNode, aListerOpt + [gtloTableConstraint]);
//
//        Inc(cnt);
//        Inc(cnt2);
//      end;
//    end;

  { prevents empty line before closing bracket }
  SkipNextNewLine := RawText = '';

  AddClause(gttkRightBracket);

//  AddStr(aNode.KeywordAfter1);
//  if aNode.OnCommitPreserveRows or aNode.OnCommitDeleteRows then begin
//    AddStr(gtkwOn_Commit);
//    if aNode.OnCommitPreserveRows then AddStr(gtkwPreserve_Rows) else
//    if aNode.OnCommitDeleteRows   then AddStr(gtkwDelete_Rows);
//  end;
  AddStr(aNode.KeywordAuxCheckKwd(gtkwOn_Commit_Preserve_Rows, gtkwOn_Commit_Delete_Rows));

  NewLineIntend := lIntend;
//ML_ColumnName := 0;
//ML_DataType   := 0;
end;

{ lists DROP TABLE statement }
procedure TGtSqlFormatLister.List_DropTable;
begin
  if not Assigned(aNode) then Exit;

  List_Clause_Name (aNode, aListerOpt, gtkwDrop_Table, nil, aNode.Name, gtlsTable, gtlsDdlDrop);

//if aNode.Cascade then AddStr(gtkwCascade_Constraints);
//if aNode.Purge   then AddStr(gtkwPurge);
//AddStr(aNode.KeywordAfter1);
//AddStr(aNode.KeywordAfter2);
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

//AddLeftBracket(aNode.BracketsCount);
  AddLeftBracket(aNode);

  lFirst := True;
  for i := 0 to aNode.Count - 1 do
    if aNode[i].Check(gtssOtherColumnDef) then begin
      if not lFirst then AddComma;
      List_ColumnDef(aNode[i], aListerOpt);
      lFirst := False;
    end;

//AddRightBracket(aNode.BracketsCount);
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

  if not (gtloSameAsPrevClause in aListerOpt) then AddClause(aNode.Keyword);

//AddLeftBracket(aNode.BracketsCount);
  AddLeftBracket(aNode);

  lFirst := True;
  for i := 0 to aNode.Count - 1 do
    if aNode[i].Check(gtssOtherColumnDef) then begin
      if not lFirst then AddComma;
      List(aNode[i], aListerOpt);
      lFirst := False;
    end;

//AddRightBracket(aNode.BracketsCount);
  AddRightBracket(aNode);
end;

{ lists ADD CONSTRAINT clause }
procedure TGtSqlFormatLister.List_AlterAddConstraint;
var lConstr: TGtSqlNode;
begin
  if not Assigned(aNode) then Exit;
  FKeywordStyle := gtlsDdlCreate;

  lConstr := aNode.Find{ByKind}(gtsiConstraint);
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

//if aNode.Cascade then AddStr(gtkwCascade);
//if aNode.KeywordAfter1 = gtkwCascade then AddStr(gtkwCascade);
  AddStr(aNode.KeywordAuxCheckKwd(gtkwCascade));
end;

{ lists RENAME TABLE clause }
procedure TGtSqlFormatLister.List_AlterRenameTable;
begin
  if not Assigned(aNode) then Exit;
  FKeywordStyle := gtlsDdlModify;

  if not (gtloSameAsPrevClause in aListerOpt) then AddClause(gtkwRename);
  //AddStr(aNode.OldName, gtlsTable);
  //AddStr(aNode.Name1, gtlsTable);
  AddStr(gtkwTo);
  //AddStr(aNode.NewName, gtlsTable);
  //AddStr(aNode.Name2, gtlsTable);
  AddStr(aNode.Name, gtlsTable);
end;

{ lists RENAME COLUMN clause }
procedure TGtSqlFormatLister.List_AlterRenameColumn;
begin
  if not Assigned(aNode) then Exit;
  FKeywordStyle := gtlsDdlModify;

  if not (gtloSameAsPrevClause in aListerOpt) then AddClause(gtkwRename_Column);
  //AddStr(aNode.OldName, gtlsColumn);
  //AddStr(aNode.Name1, gtlsColumn);
  AddStr(aNode.Name, gtlsColumn);

  aNode := aNode.Find(gtsiNone, gtkwTo);
  if Assigned(aNode) then begin
    AddStr(gtkwTo);
    //AddStr(aNode.NewName, gtlsColumn);
    //AddStr(aNode.Name2, gtlsColumn);
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

      if gtloSameAsPrevClause in aListerOpt then begin
      //if not Options[ gtstCommaAtNewLine ] then AddComma else AddClause(gttkComma);
        AddComma;
      end;
      end;

      if aNode[i].Check(gtsiClauseAlter, gtkwAdd)            or
         aNode[i].Check(gtsiClauseAlter, gtkwAdd_Column)     then List_AlterAddColumn     (aNode[i], {lTreeNode,} aListerOpt) else
      if aNode[i].Check(gtsiClauseAlter, gtkwDrop_Column)    or
         aNode[i].Check(gtsiClauseAlter, gtkwDrop)           then List_AlterDropColumn    (aNode[i], {lTreeNode,} aListerOpt) else
      if aNode[i].Check(gtsiClauseAlter, gtkwModify_Column)  or
         aNode[i].Check(gtsiClauseAlter, gtkwAlter_Column)   or
         aNode[i].Check(gtsiClauseAlter, gtkwModify)         then List_AlterModifyColumn  (aNode[i], {lTreeNode,} aListerOpt) else
      if aNode[i].Check(gtsiClauseAlter, gtkwAdd_Constraint) then List_AlterAddConstraint (aNode[i], {lTreeNode,} aListerOpt) else
      if aNode[i].Check(gtsiClauseAlter, gtkwDrop_Constraint)then List_AlterDropConstraint(aNode[i], {lTreeNode,} aListerOpt) else
      if aNode[i].Check(gtsiClauseAlter, gtkwRename_To)      then List_AlterRenameTable   (aNode[i], {lTreeNode,} aListerOpt) else
      if aNode[i].Check(gtsiClauseAlter, gtkwRename_Column)  then List_AlterRenameColumn  (aNode[i], {lTreeNode,} aListerOpt) else
      if aNode[i].Check(gtsiClauseAlter, gttkComma)          then begin

        if(lPrevClause.Keyword = gtkwAdd)     or
          (lPrevClause.Keyword = gtkwAdd_Column)     then List_AlterAddColumn     (aNode[i], {lTreeNode,} aListerOpt) else
        if(lPrevClause.Keyword = gtkwDrop_Column)    or
          (lPrevClause.Keyword = gtkwDrop)           then List_AlterDropColumn    (aNode[i], {lTreeNode,} aListerOpt) else
        if(lPrevClause.Keyword = gtkwModify_Column)  or
          (lPrevClause.Keyword = gtkwAlter_Column)   or
          (lPrevClause.Keyword = gtkwModify)         then List_AlterModifyColumn  (aNode[i], {lTreeNode,} aListerOpt) else
        if lPrevClause.Keyword = gtkwAdd_Constraint  then List_AlterAddConstraint (aNode[i], {lTreeNode,} aListerOpt) else
        if lPrevClause.Keyword = gtkwDrop_Constraint then List_AlterDropConstraint(aNode[i], {lTreeNode,} aListerOpt) else
        if lPrevClause.Keyword = gtkwRename_To       then List_AlterRenameTable   (aNode[i], {lTreeNode,} aListerOpt) else
        if lPrevClause.Keyword = gtkwRename_Column   then List_AlterRenameColumn  (aNode[i], {lTreeNode,} aListerOpt) ;
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
//  if aNode.Unique
//    then AddClause(gtkwCreate_Unique_Index)
//    else AddClause(gtkwCreate_Index);

  AddStr(aNode.Name, gtlsIdentifier);

//  AddStr(gtkwOn);
////AddStr(aNode.ObjectName, gtlsTable);
//  AddStr(aNode.Name1, gtlsTable);
  AddStrKeywordName( aNode.Find(gtsiNone, gtkwOn), gtlsTable);

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
//if aNode.ObjectName <> '' then begin
//if aNode.Name1 <> '' then begin
    AddStr(gtkwOn);
  //AddStr(aNode.ObjectName, gtlsTable);
  //AddStr(aNode.Name1, gtlsTable);
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

//  if aNode.Enable then AddStr(gtkwEnable) else
//  if aNode.Disable then AddStr(gtkwDisable);
//  if aNode.KeywordAfter1 = gtkwEnable then AddStr(gtkwEnable) else
//  if aNode.KeywordAfter1 = gtkwDisable then AddStr(gtkwDisable);
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

//  if aNode.OrReplace then begin
//    if aNode.Materialized
//      then AddClause(gtkwCreate_Or_Replace_Materialized_View)
//      else AddClause(gtkwCreate_Or_Replace_View);
//  end else begin
//    if aNode.Materialized
//      then AddClause(gtkwCreate_Materialized_View)
//      else AddClause(gtkwCreate_View);
//  end;

  AddClause(aNode.KeywordExt);

  AddStr(aNode.Name, gtlsView);

//if aNode.Materialized then begin
//if aNode.KeywordExt.HasSubToken(gtkwMaterialized) then begin
//if Assigned(aNode.KeywordAfter1) and aNode.KeywordAfter1.HasSubToken(gtkwRefresh) then begin
  if aNode.KeywordAuxCheck(gtkwRefresh) then begin
//    if aNode.Force or aNode.OnDemand then AddStr(gtkwRefresh);
//    if aNode.Force then AddStr(gtkwForce);
//    if aNode.OnDemand then AddStr(gtkwOn_Demand);

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
//  if aNode.OrReplace then begin
//    if aNode.Public
//      then AddClause(gtkwCreate_Or_Replace_Public_Synonym)
//      else AddClause(gtkwCreate_Or_Replace_Synonym);
//  end else begin
//    if aNode.Public
//      then AddClause(gtkwCreate_Public_Synonym)
//      else AddClause(gtkwCreate_Synonym);
//  end;

  AddStr(aNode.Name, gtlsSynonym);

//  AddStr(gtkwFor);
////AddStr(aNode.ObjectName, gtlsIdentifier);
//  AddStr(aNode.Name1, gtlsIdentifier);

  AddStrKeywordName( aNode.Find(gtsiNone, gtkwFor), gtlsIdentifier);
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

  AddStr(gtkwTo);
//if aKeyword = gtkwRevoke then AddStr(gtkwFrom) else AddStr(gtkwTo);

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

//if aNode.EmptyLineBefore and not SkipClauseNewLine then AddEmptyLine;
  if aNode.KeywordAuxCheck(gttkEmptyLineBefore) and not SkipClauseNewLine then AddEmptyLine;

  AddClause(aClauseToken, aClauseAppend);

  List_ExprList(aNode, aListerOpt);
end;

{ comon list function }
procedure TGtSqlFormatLister.List_Clause_Cond;
begin
  if not Assigned(aNode) then Exit;

//if aNode.EmptyLineBefore and not SkipClauseNewLine then AddEmptyLine;
  if aNode.KeywordAuxCheck(gttkEmptyLineBefore) and not SkipClauseNewLine then AddEmptyLine;

  AddClause(aClauseToken, aClauseAppend);

  List_CondTree(aNode, aListerOpt);
end;

{ lists SELECT clause }
procedure TGtSqlFormatLister.List_Select;
var lMaxLineLength: Integer;
    lNode: TGtSqlNode;

  { calculates }
//  procedure CalcExprAlias(aNode: TGtSqlNode);
//  var i, lMaxLineLength: Integer;
//      lItem, lItem2: TGtSqlNode;
//      lState: TGtSqlListerState;
//  begin
//    aListerOpt := aListerOpt + [gtloExprAliasIntend];
//
//    for i := 0 to aNode.Count - 1 do
//      if (aNode[i].Kind = gtsiExprTree) and (Length(aNode[i].AliasName) > ML_ExprAlias)
//        then ML_ExprAlias := Length(aNode[i].AliasName);
//
//    { are there set-ops like UNION, MINUS etc }
//    lItem := aNode.GetQuery.Find{ByKind}(gtsiUnions);
//    while Assigned(lItem) do begin
//      lItem2 := lItem.Find(gtsiDml, gtkwSelect);
//      if Assigned(lItem2) then lItem2 := lItem2.Find(gtsiExprList, gtkwSelect);
//      if Assigned(lItem2) then begin
//        // this line takes so much time - each union twice the amount of time
//        // because of ... aNode.GetQuery before while loop
//        // CalcExprAlias(lItem2);
//
//        lState := GetState;
//        SkipOutput := True;
//        List_Select(lItem2, aListerOpt);
//        lMaxLineLength := SkipOutput_MaxLineLength;
//        SetState(lState);
//        SkipOutput_MaxLineLength := lMaxLineLength;
//      end;
//
//      lItem := aNode.GetQuery.Find{ByKind}(gtsiUnions, nil, '', lItem);
//    end;
//  end;

begin
  if not Assigned(aNode) then Exit;

  { store latest }
  lMaxLineLength := SkipOutput_MaxLineLength;
  if (aNode.Owner.Owner.Kind <> gtsiUnions) and
  not Assigned(aNode.Owner.Find{ByKind}(gtsiUnions)) then begin
    SkipOutput_MaxLineLength := 0;
  end;
  // lMaxExprAlias := ML_ExprAlias;
  // ML_ExprAlias  := 0;

  { commit not commited text }
  if ClauseIntend and (Trim(RawText) <> '') then AddCurrLine;

  { add new line before clause }
//if Options[ gtstEmptyLineBeforeClause ] and not SkipClauseNewLine and
//if aNode.EmptyLineBefore and not SkipClauseNewLine //and
//   ((SubQueryLevel = 0) or
//    (SubQueryLevel > 0) and not Options[ gtstEmptyLineBeforeClauseSkipSubquery ]) and
//     aNode.GetQuery.Owner.Check(gtsiDml, gtkwInsert)
  if aNode.KeywordAuxCheck(gttkEmptyLineBefore) and not SkipClauseNewLine //and
    then AddEmptyLine;

  AddClause(gtkwSelect, ClauseAppendCondition);

  { BNF: [DISTINCT] }
//if aNode.KeywordExt = gtkwDistinct then begin
//if aNode.Distinct then begin
  if aNode.KeywordAuxCheck(gtkwDistinct) then begin
    AddStr( gtkwDistinct );
    if aNode.Count > 1 then AddClause( gttkNone );
  end;

  { BNF: [TOP n] }
//if aNode.Top > 0 then begin
//if aNode.Int1 > 0 then begin
  lNode := aNode.Find(gtsiNone, gtkwTop);
  if Assigned(lNode) then begin
    AddStr( gtkwTop );
  //AddStr( IntToStr(aNode.Top), gtlsNumber );
  //AddStr( IntToStr(aNode.Int1), gtlsNumber );
    AddStr( lNode.Name, gtlsNumber );
  end;

//if Options [ gtstSelectAliasIntend ] then CalcExprAlias(aNode);

  { counts max expr line length }
//  if gtloExprAliasIntend in aListerOpt then begin
//    aNode.Keyword := nil;
//
//    List_NoOutput(aNode, aListerOpt);
//
//    aNode.Kind := gtsiExprList;
//    aNode.Keyword := gtkwSelect;
//  end;

  { list }
  List_ExprList(aNode, aListerOpt);

  if (aNode.Owner.Owner.Kind <> gtsiUnions) and
  not Assigned(aNode.Owner.Find{ByKind}(gtsiUnions)) then begin
    SkipOutput_MaxLineLength := lMaxLineLength;
  end;
end;

{ lists FOR UPDATE clause }
procedure  TGtSqlFormatLister.List_ForUpdate;
begin
  if not Assigned(aNode) then Exit;

  List_Clause_Expr(aNode, aListerOpt, gtkwFor_Update_Of, ClauseAppendCondition);

//AddStr(aNode.KeywordAfter1);
//if aNode.NoWait then AddStr(gtkwNoWait);
  AddStr(aNode.KeywordAuxCheckKwd(gtkwNoWait));
end;

{ lists UNION | MINUS | INTERSECT }
procedure  TGtSqlFormatLister.List_SetOp;
var lNextQuery: TGtSqlNode;
    lSkipClauseNewLine: Boolean;
begin
  if not Assigned(aNode) then Exit;

  lNextQuery := aNode.Find(gtsiDml, gtkwSelect);
  if not Assigned(lNextQuery) or (lNextQuery.Count = 0) then Exit;

  { commit not commited text }
  if ClauseIntend and (Trim(RawText) <> '') then AddCurrLine;

//if {Options[gtstEmptyLineBeforeClause] and} Options[gtstEmptyLineAroundUnion] then AddCurrLine;
//if aNode.EmptyLineBefore then AddCurrLine;
  if aNode.KeywordAuxCheck(gttkEmptyLineBefore) then AddCurrLine;

  lSkipClauseNewLine := SkipClauseNewLine;
  SkipClauseNewLine := True;

  if aNode.Check(gtsiUnions, gtkwUnion) then AddClause(gtkwUnion,    ClauseAppendCondition) else
  if aNode.Check(gtsiUnions, gtkwMinus) then AddClause(gtkwMinus,    ClauseAppendCondition) else
  if aNode.Check(gtsiUnions, gtkwIntersect) then AddClause(gtkwIntersect,ClauseAppendCondition) else
  if aNode.Check(gtsiUnions, gtkwUnion_All) then  begin
                                                    AddClause(gtkwUnion,  ClauseAppendCondition);
                                                    AddStr(gtkwAll, gtlsUnion);
                                                  end;

  SkipClauseNewLine := lSkipClauseNewLine;

  { commit not commited text }
  if ClauseIntend and (Trim(RawText) <> '') then AddCurrLine;

//if {Options[gtstEmptyLineBeforeClause] and} Options[gtstEmptyLineAroundUnion] then AddCurrLine;
//if aNode.EmptyLineAfter then AddCurrLine;
  if aNode.KeywordAuxCheck(gttkEmptyLineAfter) then AddCurrLine;

  FirstClause := True;

  List_DML(lNextQuery, aListerOpt);
end;

{ lists ON clause }
procedure TGtSqlFormatLister.List_On;
begin
  if not Assigned(aNode) then Exit;

  if aNode.Check(gtsiCondTree, gtkwOn) then AddStr(gtkwOn) else AddStr(gtkwUsing);

//  if Options[gtstJoinCondLeftSideOrder] then aListerOpt := aListerOpt + [gtloCondLeftSideOrder];
//  if Options[gtstOnCondIntend] then aListerOpt := aListerOpt + [gtloOnLeftSideIntend, gtloOnRightSideIntend];

//  if Options[gtstOnCondRefsFirst] then begin
//    if aNode.Owner.AliasName <> ''
//      then aNode.OnCondMoveRefsFirst(aNode.Owner.AliasName)
//      else aNode.OnCondMoveRefsFirst(aNode.Owner.Name);
//  end;

  List_CondTree(aNode, aListerOpt {+ [gtloSkipOneCondOnLine]});
end;

{ lists VALUES clause }
procedure TGtSqlFormatLister.List_Values;
begin
  if not Assigned(aNode) then Exit;

  AddClause(gtkwValues_LeftBracket);
  List_ExprList(aNode, aListerOpt);
  if ClauseIntend then AddSpace(ClauseBodySpace);
  AddStr(gttkRightBracket);
end;

{ lists FIELDS clause }
procedure TGtSqlFormatLister.List_Fields;
begin
  if not Assigned(aNode) then Exit;
  if aNode.Kind <> gtssClauseFields then Exit;

  AddClause(gttkLeftBracket);
  List_ExprList(aNode, aListerOpt);
  if ClauseIntend then AddSpace(ClauseBodySpace);
  AddStr(gttkRightBracket);
end;

{ lists FROM/INTO clause }
procedure TGtSqlFormatLister.List_Tables;
var i: Integer;
//  lMaxTableName, lMaxAliasName, lMaxTableAndAliasName,
//  lMaxLeftOnExprAlias, lMaxLeftOnExprColumn, lMaxRightOnExprAlias, lMaxRightOnExprColumn: Integer;
//  lItem: TGtSqlNode;
begin
  if not Assigned(aNode) then Exit;

//if aNode.EmptyLineBefore and not SkipClauseNewLine then AddEmptyLine;
  if aNode.KeywordAuxCheck(gttkEmptyLineBefore) and not SkipClauseNewLine then AddEmptyLine;

//lMaxTableName         := ML_TableName;
//lMaxAliasName         := ML_AliasName;
//lMaxTableAndAliasName := ML_TableAndAliasName;
//lMaxLeftOnExprAlias   := ML_LeftOnExprPrefix;
//lMaxLeftOnExprColumn  := ML_LeftOnExprColumn;
//lMaxRightOnExprAlias  := ML_RightOnExprPrefix;
//lMaxRightOnExprColumn := ML_RightOnExprColumn;

//  if Options[ gtstTableAndAliasIntend ] then begin
//    ML_TableName         := 0;
//    ML_AliasName         := 0;
//    ML_TableAndAliasName := 0;
//
//    for i := 0 to aNode.Count - 1 do
//      if aNode[i].Check(gtsiTableRef) then begin
//    //and (Length(aNode[i].Name) < MaxTableNameToIntend)
//    //and (Length(aNode[i].AliasName) < MaxAliasNameToIntend) then begin
//        if Length(aNode[i].Name) > ML_TableName then ML_TableName := Length(aNode[i].Name);
//        if Length(aNode[i].AliasName) > ML_AliasName then ML_AliasName := Length(aNode[i].AliasName);
//        if Length(aNode[i].Name + aNode[i].AliasName) > ML_TableAndAliasName then ML_TableAndAliasName := Length(aNode[i].Name + aNode[i].AliasName);
//      end;
//  end;

//  if Options [ gtstOnCondIntend ] then begin
//    ML_LeftOnExprPrefix := 0;
//    ML_LeftOnExprColumn := 0;
//    ML_RightOnExprPrefix := 0;
//    ML_RightOnExprColumn := 0;
//
//    for i := 0 to aNode.Count - 1 do
//      if aNode[i].Check(gtsiTableRef) then begin
//        lItem := aNode[i].Find(gtsiCondTree, gtkwOn);
//        if Assigned(lItem)
//          then lItem.CalcConditionArgsLen(ML_LeftOnExprPrefix, ML_LeftOnExprColumn,
//                                          ML_RightOnExprPrefix, ML_RightOnExprColumn);
//      end;
//  end;

  for i := 0 to aNode.Count - 1 do begin
    if aNode[i].Check(gtsiTableRef) then List_TabRef(aNode[i], aListerOpt);
    if aNode[i].Check(gtsiDml, gtkwSelect) then List_DML(aNode[i], aListerOpt);
  end;

//ML_TableName := lMaxTableName;
//ML_AliasName := lMaxAliasName;
//ML_TableAndAliasName := lMaxTableAndAliasName;
//ML_LeftOnExprPrefix := lMaxLeftOnExprAlias;
//ML_LeftOnExprColumn := lMaxLeftOnExprColumn;
//ML_RightOnExprPrefix := lMaxRightOnExprAlias;
//ML_RightOnExprColumn := lMaxRightOnExprColumn;
end;

{ lists DML statement }
procedure TGtSqlFormatLister.List_DML;
var lIntend{, i, lQueryLines}: Integer;
    lSkipClauseNewLine{, lLongQuery}: Boolean;
    lKeywordStyle: TGtLexTokenStyle;
    lItem, lNode: TGtSqlNode;
begin
  if not Assigned(aNode) then Exit;
  lIntend := NewLineIntend;
  lSkipClauseNewLine := SkipClauseNewLine;
  SkipClauseNewLine := False;

  if aNode.IsSubQuery then Inc(SubQueryLevel);

  lKeywordStyle := FKeywordStyle;
//  if (FKeywordStyle = gtlsKeyword) or not Options[gtstExtQueryKeywordStyle] then begin
//    if aQuery.Check(gtsiDml, gtkwSelect)  then FKeywordStyle := gtlsDmlSelect else
//    if aQuery.Check(gtsiDml, gtkwInsert)  then FKeywordStyle := gtlsDmlInsert else
//    if aQuery.Check(gtsiDml, gtkwUpdate)  then FKeywordStyle := gtlsDmlUpdate else
//    if aQuery.Check(gtsiDml, gtkwDelete)  then FKeywordStyle := gtlsDmlDelete;
//  end;

  { subquery wrapper }
  if aNode.IsSubQuery and not(aNode.Owner.Check(gtsiUnions) or aNode.Owner.Check(gtsiDDL, gtkwCreate_Table)) then begin

    if (aNode.KeywordExt {Operand} = gtkwFrom) and (gtloSkipFrom in aListerOpt) then else begin
      AddClause( {JoinOperatorToToken(} aNode.KeywordExt {Operand} {)},
                 gttkLeftBracket,
                 ClauseAppendCondition,
                 aNode.Owner.Check(gtsiExprTree) or aNode.Owner.Check(gtsiCond) or aNode.Owner.Check(gtsiCondTree));

    //AddLeftBracket(aQuery.BracketsCount - 1);
      AddLeftBracket(aNode, True);
    end;

    { subquery at new line when in FROM/JOIN, IN condition, SELECT expr }
    if aNode.Owner.Check(gtsiClauseTables) or aNode.Owner.Check(gtsiCond) or aNode.Owner.Check(gtsiCondTree) or
       (aNode.Owner.Kind = gtsiExprTree) and Assigned((aNode.Owner.ExprTreeOwner))
       and not aNode.Owner.ExprTreeOwner.Check(gtsiExprList, gtkwSelect)
      then AddClause(nil);

    AddSpace;
    if SubQueryIntendSpace > 0 then AddSpace(SubQueryIntendSpace);
    SkipNextNewLine := False; { chyba tak ju¿ ma teraz byæ }
//  SkipNextNewLine := True; //not SubQueryNewLine;
    if SubQueryIntend then NewLineIntend := Length(RawText);

    SkipClauseNewLine := False;
//  SkipClauseNewLine := Options [ gtstEmptyLineBeforeClauseSkipSubquery ];
  end;
  aListerOpt := aListerOpt - [gtloSkipFrom];

  { calculate query lines no }
//  if Options [ gtstEmptyLineBeforeClauseSkipShort ] {and not SkipClauseNewLine} then begin
//    lLongQuery := False;
//    lQueryLines := 0;
//
//    if aQuery.Check(gtsiDml, gtkwSelect) then  aQuery.CalcClauseLines(gtsiExprList, gtkwSelect,     10, Options[ gtstOneExprOnLine ], lLongQuery, lQueryLines);
//    if aQuery.Check(gtsiDml, gtkwInsert) then  aQuery.CalcClauseLines(gtssClauseFields, nil {?},      10, Options[ gtstOneExprOnLine ], lLongQuery, lQueryLines);
//    if aQuery.Check(gtsiDml, gtkwSelect)   then  aQuery.CalcClauseLines(gtsiExprList, gtkwInto,       10, Options[ gtstOneExprOnLine ], lLongQuery, lQueryLines);
//    if aQuery.Check(gtsiDml, gtkwInsert) then  aQuery.CalcClauseLines(gtsiExprList, gtkwValues,     10, Options[ gtstOneExprOnLine ], lLongQuery, lQueryLines);
//    if aQuery.Check(gtsiDml, gtkwUpdate) then  aQuery.CalcClauseLines(gtsiSetExprList, nil, 10, True {???},                   lLongQuery, lQueryLines);
//    if aQuery.Check(gtsiDml, gtkwSelect) or aQuery.Check(gtsiDml, gtkwDelete) or aQuery.Check(gtsiDml, gtkwUpdate) then  begin
//      aQuery.CalcClauseLines(gtsiClauseTables, gtkwFrom, 6, True {???}, lLongQuery, lQueryLines);
//      lItem := aQuery.Find(gtsiClauseTables, gtkwFrom);
//      if Assigned(lItem) then
//        for i := 0 to lItem.Count - 1 do
//          lItem[i].CalcClauseLines(gtsiCondTree, gtkwOn, 6, Options[ gtstOneCondOnLine ], lLongQuery, lQueryLines);
//    end;
//    if aQuery.Check(gtsiDml, gtkwSelect) or aQuery.Check(gtsiDml, gtkwUpdate) or
//       aQuery.Check(gtsiDml, gtkwDelete) then  aQuery.CalcClauseLines(gtsiCondTree, gtkwWhere,              10, Options[ gtstOneCondOnLine ], lLongQuery, lQueryLines);
//    if aQuery.Check(gtsiDml, gtkwSelect) or aQuery.Check(gtsiDml, gtkwUpdate) or
//       aQuery.Check(gtsiDml, gtkwDelete) then  aQuery.CalcClauseLines(gtsiCondTree, gtkwConnect_By,          10, Options[ gtstOneCondOnLine ], lLongQuery, lQueryLines);
//    if aQuery.Check(gtsiDml, gtkwSelect) or aQuery.Check(gtsiDml, gtkwUpdate) or
//       aQuery.Check(gtsiDml, gtkwDelete) then  aQuery.CalcClauseLines(gtsiCondTree, gtkwStart_With,          10, Options[ gtstOneCondOnLine ], lLongQuery, lQueryLines);
//    if aQuery.Check(gtsiDml, gtkwSelect) then  aQuery.CalcClauseLines(gtsiExprList, gtkwGroup_By,    10, Options[ gtstOneExprOnLine ], lLongQuery, lQueryLines);
//    if aQuery.Check(gtsiDml, gtkwSelect) or aQuery.Check(gtsiDml, gtkwUpdate) or
//       aQuery.Check(gtsiDml, gtkwDelete) then  aQuery.CalcClauseLines(gtsiCondTree, gtkwHaving,             10, Options[ gtstOneCondOnLine ], lLongQuery, lQueryLines);
//    if aQuery.Check(gtsiDml, gtkwSelect) then  aQuery.CalcClauseLines(gtsiExprList, gtkwOrder_By,    10, Options[ gtstOneExprOnLine ], lLongQuery, lQueryLines);
//
//    lLongQuery := lLongQuery or (lQueryLines > MaxShortQueryLines);
//    SkipClauseNewLine := not lLongQuery;
//  end;

  { DML essential }
  if aNode.Check(gtsiDml, gtkwSelect) then  List_Select     (aNode.Find(gtsiExprList, gtkwSelect),     aListerOpt);
  if aNode.Check(gtsiDml, gtkwInsert) then  List_TabRef     (aNode.Find(gtsiTableRef),                 aListerOpt);
  if aNode.Check(gtsiDml, gtkwInsert) then  List_Fields     (aNode.Find(gtssClauseFields),             aListerOpt);
  if aNode.Check(gtsiDml, gtkwUpdate) then  List_Tables     (aNode.Find(gtsiClauseTables, gtkwUpdate), aListerOpt);
  if aNode.Check(gtsiDml, gtkwSelect) then  List_Clause_Expr(aNode.Find(gtsiExprList, gtkwInto),       aListerOpt, gtkwInto, ClauseAppendCondition);
//  if aQuery.Check(gtsiDml, gtkwSelect) and
//  // (aQuery.ObjectName <> '')         then  List_Clause_Name(nil, aListerOpt, gtkwInto, nil, aQuery.ObjectName, gtlsTable);
//     (aQuery.Name1 <> '')              then  List_Clause_Name(nil, aListerOpt, gtkwInto, nil, aQuery.Name1,      gtlsTable);
  if aNode.Check(gtsiDml, gtkwInsert) then  List_Values     (aNode.Find(gtsiExprList, gtkwValues), aListerOpt);
  if aNode.Check(gtsiDml, gtkwUpdate) then  List_SetExprList(aNode.Find(gtsiSetExprList, nil), aListerOpt);

  { DELETE expr-list vs DELETE FROM }
  if aNode.Check(gtsiDml, gtkwDelete) then begin
    lItem := aNode.Find(gtsiExprList, gtkwDelete);

    if not Assigned(lItem) then begin
      AddClause       (gtkwDelete_From, ClauseAppendCondition);
      List_Tables     (aNode.Find(gtsiClauseTables, gtkwFrom), aListerOpt + [gtloSkipFrom]);
    end else begin
      AddClause       (gtkwDelete, ClauseAppendCondition);
      List_ExprList   (lItem, aListerOpt);
      List_Tables     (aNode.Find(gtsiClauseTables, gtkwFrom), aListerOpt);
    end;
  end;

  if aNode.Check(gtsiDml, gtkwSelect) or
     aNode.Check(gtsiDml, gtkwUpdate) then  List_Tables     (aNode.Find(gtsiClauseTables, gtkwFrom),   aListerOpt);
  if aNode.Check(gtsiDml, gtkwSelect) or aNode.Check(gtsiDml, gtkwUpdate) or
     aNode.Check(gtsiDml, gtkwDelete) then  List_Clause_Cond(aNode.Find(gtsiCondTree, gtkwWhere),      aListerOpt, gtkwWhere, ClauseAppendCondition);
  if aNode.Check(gtsiDml, gtkwSelect) or aNode.Check(gtsiDml, gtkwUpdate) or
     aNode.Check(gtsiDml, gtkwDelete) then  List_Clause_Cond(aNode.Find(gtsiCondTree, gtkwConnect_By), aListerOpt, gtkwConnect_By, ClauseAppendCondition);
  if aNode.Check(gtsiDml, gtkwSelect) or aNode.Check(gtsiDml, gtkwUpdate) or
     aNode.Check(gtsiDml, gtkwDelete) then  List_Clause_Cond(aNode.Find(gtsiCondTree, gtkwStart_With), aListerOpt, gtkwStart_With, ClauseAppendCondition);
  if aNode.Check(gtsiDml, gtkwSelect) then  List_Clause_Expr(aNode.Find(gtsiExprList, gtkwGroup_By),   aListerOpt, gtkwGroup_By, ClauseAppendCondition);
  if aNode.Check(gtsiDml, gtkwSelect) or aNode.Check(gtsiDml, gtkwUpdate) or
     aNode.Check(gtsiDml, gtkwDelete) then  List_Clause_Cond(aNode.Find(gtsiCondTree, gtkwHaving),     aListerOpt, gtkwHaving, ClauseAppendCondition);
  if aNode.Check(gtsiDml, gtkwInsert) then  List_DML        (aNode.Find(gtsiDml, gtkwSelect),          aListerOpt);
  if aNode.Check(gtsiDml, gtkwSelect) then  List_SetOp      (aNode.Find{ByKind} (gtsiUnions),          aListerOpt);
  if aNode.Check(gtsiDml, gtkwSelect) then  begin
    lItem := aNode.Find(gtsiExprList, gtkwOrder_By);
    if Assigned(lItem) then begin
      // odstêp identyczny jak dla UNION
//      if Options[ gtstEmptyLineAroundUnion ] and Assigned(aQuery.Find{ByKind} (gtsiUnions)) then AddEmptyLine;

      List_Clause_Expr(lItem, aListerOpt, gtkwOrder_By, ClauseAppendCondition);
    end;
  end;
  if aNode.Check(gtsiDml, gtkwSelect) then  List_ForUpdate  (aNode.Find(gtsiExprList, gtkwFor_Update), aListerOpt);
  if aNode.Check(gtsiDml, gtkwSelect) then  List_Clause_Expr(aNode.Find(gtsiExprList, gtkwLimit),      aListerOpt, gtkwLimit, ClauseAppendCondition);
  if aNode.Check(gtsiDml, gtkwInsert) then  List_Clause_Expr(aNode.Find(gtsiExprList, gtkwReturning),  aListerOpt, gtkwReturning, ClauseAppendCondition);
  if aNode.Check(gtsiDml, gtkwInsert) then  List_Clause_Expr(aNode.Find(gtsiExprList, gtkwInto),       aListerOpt, gtkwInto,      ClauseAppendCondition);

  { subquery wrapper }
  if aNode.IsSubQuery and not(aNode.Owner.Check(gtsiUnions) or aNode.Owner.Check(gtsiDDL, gtkwCreate_Table)) then begin

    NewLineIntend := lIntend;
    SkipClauseNewLine := lSkipClauseNewLine;

      AddClause(nil, gttkRightBracket, ClauseAppendCondition);

    //AddRightBracket(aQuery.BracketsCount - 1);
      AddRightBracket(aNode, True);

  //if aQuery.AliasName <> '' then begin
  //if aQuery.Name1 <> '' then begin
    lNode := aNode.Find(gtsiNone, gtkwAs);
    if Assigned(lNode) then begin
    //if Options[ gtstTableAsKeyword ] then AddStr(gtkwAs);
    //if aQuery.AliasAsToken then AddStr(gtkwAs);
    //if aQuery.KeywordAfter1 = gtkwAs then AddStr(gtkwAs);
    //AddStr(aQuery.KeywordAuxCheckKwd(gtkwAs));
    //AddStr(aQuery.AliasName, gtlsTableAlias);
    //AddStr(aQuery.Name1, gtlsTableAlias);
    //AddStr(lNode.KeywordExt);
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

{ calculates ML_ClauseKeyword }
function TGtSqlFormatLister.GetClauseKeywordSpace;
var i, j, l: Integer;
begin
  Result := 0;
  if not Assigned(aNode) then Exit;

  { klauzula wiodaca }
  if ((aNode.Kind = gtsiDml) or (aNode.Kind = gtsiDdl)) and Assigned(aNode.Keyword) and
      (Length(aNode.Keyword.TokenText) > Result) //and (Length(aNode.Keyword.Text) <= MaxClauseToIntend)
    then Result := Length(aNode.Keyword.TokenText);

  for i := 0 to aNode.Count - 1 do begin
    { keyword sub klauzul }
    if Assigned(aNode[i].Keyword) and (Length(aNode[i].Keyword.TokenText) > Result)
        then Result := Length(aNode[i].Keyword.TokenText);

    { INSERT INTO }
    if aNode[i].Check(gtsiTableRef) then
      if Assigned(aNode[i].Keyword {Operand}) then begin
    //  l := Length(JoinOperatorToToken( aNode[i].Keyword {Operand}, aNode[i].JoinInnerKeyword, aNode[i].JoinOuterKeyword ).TokenText);
        l := Length(aNode[i].KeywordExt.TokenText);
        if l > Result then Result := l;
      end;

    { JOIN clauses }
    if aNode[i].Check(gtsiClauseTables, gtkwFrom) then begin
      for j := 0 to aNode[i].Count - 1 do begin
        if Assigned(aNode[i][j].Keyword {Operand}) then begin
      //  l := Length(JoinOperatorToToken( aNode[i][j].Keyword {Operand}, aNode[i][j].JoinInnerKeyword, aNode[i][j].JoinOuterKeyword ).TokenText);
          l := Length(aNode[i][j].KeywordExt.TokenText);
          if l > Result then Result := l;
        end;

        { JOIN sub-queries }
        if aNode[i][j].Check(gtsiDml, gtkwSelect) then begin
          l := GetClauseKeywordSpace(aNode[i][j]);
          if l > Result then Result := l;
        end;
      end;
    end;

    { SELECT & ExprTree sub-queries }
    if aNode[i].Check(gtsiExprList, gtkwSelect) or (aNode[i].Kind = gtsiExprTree) then begin
      for j := 0 to aNode[i].Count - 1 do begin
        l := GetClauseKeywordSpace(aNode[i][j]);
        if l > Result then Result := l;
      end;
    end;

    { subqueries }
    if aNode[i].Check(gtsiDml, gtkwSelect) then begin
      l := GetClauseKeywordSpace(aNode[i]);
      if l > Result then Result := l;
    end;

    { conditions }
    if aNode[i].Check(gtsiCond) or aNode[i].Check(gtsiCondTree) then begin
      l := GetClauseKeywordSpace(aNode[i]);
      if l > Result then Result := l;
    end;

    { SET-OP queries }
    if aNode[i].Kind = gtsiUnions then begin
      l := GetClauseKeywordSpace(aNode[i].Find(gtsiDml, gtkwSelect));
      if l > Result then Result := l;
    end;

    { ADD COLUMN }
    if aNode[i].Check(gtsiClauseAlter, gtkwAdd_Column) then begin
      l := GetClauseKeywordSpace(aNode[i]);
      if l > Result then Result := l;
    end;
  end;
end;

{ lists SqlParser }
procedure TGtSqlFormatLister.List_SqlParser;
var i, lMaxKeywordSpace, lKeywordSpace: Integer;
begin
  if not Assigned(aNode) then Exit;

  lMaxKeywordSpace := 0;
  for i := 0 to aNode.QueryList.Count-1 do begin
    lKeywordSpace := GetClauseKeywordSpace(aNode.QueryList[i]);
    if lKeywordSpace > lMaxKeywordSpace then lMaxKeywordSpace := lKeywordSpace;
  end;
  if lMaxKeywordSpace > 0 then ML_ClauseKeyword := lMaxKeywordSpace;

  { Max Clause Intend + Clause Body Space powinna byæ wielokrotnoœci¹ 2, ¿eby ³adnie siê uk³ada³y blokowe intendacje }
  if (ML_ClauseKeyword + ClauseBodySpace) mod 2 > 0 then Inc(ML_ClauseKeyword);

  BeginFormattedFile;

  { essential work }
  for i := 0 to aNode.QueryList.Count-1 do
//    if aNode.QueryList[i].Kind in [gtsiDml, gtsiDdl, gtsiDcl, gtsiTcl, gtsiProgram] then begin
    if true then begin
      FKeywordStyle := gtlsKeyword;

      BracketLevel := 0;
      SubQueryLevel := 0;
      FirstClause := True;

      List( aNode.QueryList[ i ], [] );
      SkipNextNewLine := False;

//    if (aNode.QueryList.Count > 1) or not Options[ gtstNoSemicolonOnSingleQuery ] then begin
      //if aNode.QueryList[i].Semicolon and not SkipSemicolonAfterThisQuery then begin
      //if aNode.QueryList[i].KeywordAfter1.HasSubToken(gttkSemicolon) and not SkipSemicolonAfterThisQuery then begin
        if aNode.QueryList[i].KeywordAuxCheck(gttkSemicolon) and not SkipSemicolonAfterThisQuery then begin
        //if Options[ gtstSpaceBeforeSemicolon ] then AddSpace else RemSpace;
          AddStr( gttkSemicolon, False );
        end;
//    end;
      SkipSemicolonAfterThisQuery := False;

      AddCurrLine;
//      if LinesNoAfterQuery > 0
//        then AddEmptyLine( IntIf(Options[gtstLinesNoAfterQuery], LinesNoAfterQuery, -1) )
//        else AddCurrLine;

      SkipNextNewLine := False;
    end;

  EndFormattedFile;
end;

end.

