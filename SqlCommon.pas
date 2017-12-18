(* $Header: /SQL Toys/SqlFormat/SqlCommon.pas 24    17-12-16 19:57 Tomek $
   (c) Tomasz Gierka, github.com/SqlToys, 2014.08.26                          *)
{--------------------------------------  --------------------------------------}
{$IFDEF RELEASE}
  {$DEBUGINFO OFF}
  {$LOCALSYMBOLS OFF}
{$ENDIF}
unit SqlCommon;

interface

uses SqlTokenizers, SqlLister;

{--------------------------------- Status Log ---------------------------------}

procedure StatusLogStartTime;
procedure StatusLogStopTime(aCaption: String);
function  StatusLogText: String;
function  StatusLogLastTime: TDateTime;
function  StatusLogLastTimeStr: String;

{------------------------------- Registry Keys --------------------------------}
const
  YA_COMMON_KEY = '\Software\SQL Formatter\';
  YA_SETTINGS_KEY = YA_COMMON_KEY + 'Settings\';
  YA_RECENT_FILES_KEY = YA_COMMON_KEY + 'RecentFiles\';

  YA_VERSION_KEY = YA_COMMON_KEY + 'Version';

  { editor }
  YA_SET_KEY_TAB_MAX_LEN            : String = YA_SETTINGS_KEY + 'TableMaxLen';
  YA_SET_KEY_ALIAS_MAX_LEN          : String = YA_SETTINGS_KEY + 'AliasMaxLen';
  YA_SET_KEY_SET_MAX_LEN            : String = YA_SETTINGS_KEY + 'SetMaxLen';
  YA_SET_KEY_COL_MAX_LEN            : String = YA_SETTINGS_KEY + 'ColumnMaxLen';
  YA_SET_KEY_TYPE_MAX_LEN           : String = YA_SETTINGS_KEY + 'DatatypeMaxLen';
  YA_SET_KEY_SHORT_QUERY            : String = YA_SETTINGS_KEY + 'ShortQuery';
  YA_SET_KEY_MAX_CLAUSE_KEYWORD_INT : String = YA_SETTINGS_KEY + 'MaxClauseKeywordIntend';
  YA_SET_KEY_MAX_IDENTIFIER_LEN     : String = YA_SETTINGS_KEY + 'MaxIdentifierLen';
  YA_SET_KEY_LINES_AFTER_QUERY      : String = YA_SETTINGS_KEY + 'MoreLinesAfterQuery';

{------------------------------- Registry Keys --------------------------------}

const
  GT_FIND_REPLACE_KEY = YA_COMMON_KEY + 'FindReplace\';

var
  GT_FIND_REPLACE_KEY_FIND_TEXT           : String = GT_FIND_REPLACE_KEY + 'FindText';
  GT_FIND_REPLACE_KEY_REPLACE_TEXT        : String = GT_FIND_REPLACE_KEY + 'ReplaceText';

  GT_FIND_REPLACE_KEY_OPT_CASE_SENSITIVE  : String = GT_FIND_REPLACE_KEY + 'CaseSensitive';
  GT_FIND_REPLACE_KEY_OPT_WHOLE_WORDS     : String = GT_FIND_REPLACE_KEY + 'WholeWordsOnly';
  GT_FIND_REPLACE_KEY_OPT_PROMPT_REPLACE  : String = GT_FIND_REPLACE_KEY + 'PromptOnReplace';

  GT_FIND_REPLACE_KEY_OPT_SCOPE           : String = GT_FIND_REPLACE_KEY + 'Scope';
  GT_FIND_REPLACE_KEY_OPT_DIRECTION       : String = GT_FIND_REPLACE_KEY + 'Direction';

{----------------------------- Settings registry ------------------------------}

type
  TYaRegKey = ( yarkCommon, yarkSettings );

var
  GT_SET_BOOL_ARR : array [ TGtListerSettings ]
  of record Key: TYaRegKey; Reg: String; Def: Boolean end
  = ( (Key: yarkSettings; Reg: 'RightIntend';            Def: True),
      (Key: yarkSettings; Reg: 'EmptyLineAfterQuery';    Def: True),
      (Key: yarkSettings; Reg: 'SpaceBeforeComma';       Def: False),
      (Key: yarkSettings; Reg: 'SpaceBeforeSemicolon';   Def: True), // False
      (Key: yarkSettings; Reg: 'EmptyLineBeforeClause';  Def: True), // False
      (Key: yarkSettings; Reg: 'UpperKeywords';          Def: True),
      (Key: yarkSettings; Reg: 'ExpressionAsKeyword';    Def: True),
      (Key: yarkSettings; Reg: 'TableAsKeyword';         Def: False), // True
      (Key: yarkSettings; Reg: 'ColumnConstraint';       Def: True),
      (Key: yarkSettings; Reg: 'OuterJoin';              Def: False),
      (Key: yarkSettings; Reg: 'SortShortKeyword';       Def: True),
      (Key: yarkSettings; Reg: 'SkipAscending';          Def: True), // False
      (Key: yarkSettings; Reg: 'OneExprOnLine';          Def: True),
      (Key: yarkSettings; Reg: 'OneCondOnLine';          Def: True),
      (Key: yarkSettings; Reg: 'EmptyLineArounUnion';    Def: True),

      (Key: yarkSettings; Reg: 'SpaceOutsideBrackets';   Def: False), // True
      (Key: yarkSettings; Reg: 'SpaceInsideBrackets';    Def: True),
      (Key: yarkSettings; Reg: 'SpaceAroundOperator';    Def: True),
      (Key: yarkSettings; Reg: 'SpaceAfterComma';        Def: True),
      (Key: yarkSettings; Reg: 'CommaAtNewLine';         Def: True),

      (Key: yarkSettings; Reg: 'CaseIntend';             Def: False),
      (Key: yarkSettings; Reg: 'CaseWhenAtNewLine';      Def: True),
      (Key: yarkSettings; Reg: 'CaseThenAtNewLine';      Def: True),
      (Key: yarkSettings; Reg: 'CaseElseAtNewLine';      Def: True),
      (Key: yarkSettings; Reg: 'CaseEndAtNewLine';       Def: True),

      (Key: yarkSettings; Reg: 'TableAndAliasIntend';    Def: True),
      (Key: yarkSettings; Reg: 'SetExprIntend';          Def: True),
      (Key: yarkSettings; Reg: 'CreateTableColConsBreakine';Def: True),
      (Key: yarkSettings; Reg: 'NoSemicolonOnSingleQuery';Def:True),
      (Key: yarkSettings; Reg: 'InnerJoin';              Def: False),
      (Key: yarkSettings; Reg: 'AliasFirstUseCase';      Def: True),
      (Key: yarkSettings; Reg: 'TableFirstUseCase';      Def: True),
      (Key: yarkSettings; Reg: 'SpaceInsideBracketsSkipFunOneParam'; Def: True),
      (Key: yarkSettings; Reg: 'CreateTableColConsNewLineAfter'; Def: True),
      (Key: yarkSettings; Reg: 'JoinCondOrder';          Def: True),
      (Key: yarkSettings; Reg: 'CreateTableIntend';      Def: True),
      (Key: yarkSettings; Reg: 'CreateTableEmptyLineBeforeComplexConstraints'; Def: True),
      (Key: yarkSettings; Reg: 'EmptyLineBeforeClauseSkipSubquery'; Def: True),
      (Key: yarkSettings; Reg: 'OnCondIntend';           Def: True),
      (Key: yarkSettings; Reg: 'SelectAliasIntend';      Def: True),
      (Key: yarkSettings; Reg: 'SpaceInsideBracketsSkipDatatype'; Def: True),
      (Key: yarkSettings; Reg: 'EmptyLineBeforeClauseSkipShort'; Def: True),
      (Key: yarkSettings; Reg: 'OnCondRefsFirst';        Def: True),
      (Key: yarkSettings; Reg: 'ExtQueryKeywordStyle';   Def: True),
      (Key: yarkSettings; Reg: 'LinesNoAfterQuery';      Def: False)
    );

var
  GT_SET_CASE_ARR : array [ TGtListerCaseSettings ]
  of record Key: TYaRegKey; Reg: String; Def: TGtSqlCaseOption end
  = ( (Key: yarkSettings; Reg: 'TableCase';             Def: gtcoFirstUseCase  ),
      (Key: yarkSettings; Reg: 'ColumnCase';            Def: gtcoNoChange ),
      (Key: yarkSettings; Reg: 'TableAliasCase';        Def: gtcoFirstUseCase ),
      (Key: yarkSettings; Reg: 'ColumnAliasCase';       Def: gtcoNoChange ),
      (Key: yarkSettings; Reg: 'ParameterCase';         Def: gtcoNoChange ),
      (Key: yarkSettings; Reg: 'IdentifierCase';        Def: gtcoNoChange ),
      (Key: yarkSettings; Reg: 'KeywordCase';           Def: gtcoUpperCase ),
      (Key: yarkSettings; Reg: 'ColumnQuotedAliasCase'; Def: gtcoNoChange ),
      (Key: yarkSettings; Reg: 'FunctionCase';          Def: gtcoNoChange )
    );

type
  TYaBoolSettings = ( yastSpaceSavingAliases_OBSOLETE, yastPrefixNonUniqueCols_OBSOLETE,
                      yastBringToFrontOnFileDrag_OBSOLETE,
                      yastExtColorIdentifiers, yastExtColorKeywords, yastExtColorBrackets, yastExtColorCases,
                      yastShowFullScreen, yastShowGrid, yastShowTrees, {yastShowGutter,} yastShowStatusBar, yastShowQuickSettings,
                      yastDontMaximizeOnStart_OBSOLETE);

var
  YA_SET_BOOL_ARR : array [ TYaBoolSettings ]
  of record Key: TYaRegKey; Reg: String; Def: Boolean end
  = ( (Key: yarkSettings; Reg: 'SpaceSavingAliases';     Def: False),
      (Key: yarkSettings; Reg: 'PrefixNonUniqueColumns'; Def: False),
      (Key: yarkSettings; Reg: 'BringToFrontOnFileDrag'; Def: True),
      (Key: yarkSettings; Reg: 'ExtColorIdentifiers';    Def: True),
      (Key: yarkSettings; Reg: 'ExtColorKeywords';       Def: True),
      (Key: yarkSettings; Reg: 'ExtColorBrackets';       Def: True),
      (Key: yarkSettings; Reg: 'ExtColorCases';          Def: True),

      (Key: yarkCommon;   Reg: 'ShowFullScreen';         Def: False),
      (Key: yarkCommon;   Reg: 'ShowGrid';               Def: False),
      (Key: yarkCommon;   Reg: 'ShowNavigator';          Def: False),
      // (Key: yarkCommon;   Reg: 'ShowGutter';             Def: True),
      (Key: yarkCommon;   Reg: 'ShowStatusBar';          Def: True),
      (Key: yarkCommon;   Reg: 'ShowQuickSettings';      Def: True),
      (Key: yarkCommon;   Reg: 'DontMaximizeOnStart';    Def: True)
  );

var
  YA_SET_COLOR_ARR : array [ TGtLexTokenStyle ]
  of record Reg: String; Color: Integer; Style: String; end
  = ( // basic
      (  Reg: 'PlainText';     Color: $000000;   Style: '___'),
      (  Reg: 'Keywords';      Color: $4444EE;   Style: 'B__'),
      (  Reg: 'Identifiers';   Color: $888800;   Style: '___'), // CCCC00
      (  Reg: 'Numbers';       Color: $666666;   Style: 'B__'), // 880000
      (  Reg: 'Strings';       Color: $CC44CC;   Style: 'B__'), // 6600AA, BI
      (  Reg: 'Operators';     Color: $000000;   Style: 'B__'), // 880088
      (  Reg: 'Comments';      Color: $888888;   Style: '_I_'), // FF8888, BI_
      (  Reg: 'Errors';        Color: $FF0000;   Style: '__U'),
      (  Reg: 'Disabled';      Color: $888888;   Style: '___'),

      // EXT. IDENTIFIERS
      (  Reg: 'Tables';        Color: $008888;   Style: 'B__'),
      (  Reg: 'Views';         Color: $00AA88;   Style: 'B__'), // 00CC88
      (  Reg: 'TableAliases';  Color: $FF8800;   Style: 'B__'), // EE8800
      (  Reg: 'Columns';       Color: $CCAA88;   Style: 'B__'),
      // (  Reg: 'RefColumns';    Color: $880000;   Style: '_I_'),
      (  Reg: 'ColumnAliases'; Color: $FF4444;   Style: 'B__'), // EE8888, BI_
      (  Reg: 'Functions';     Color: $0055AA;   Style: 'B__'), // 0000FF, 0088FF, 00CCFF
      (  Reg: 'AggrFunctions'; Color: $4400AA;   Style: 'B__'),
      (  Reg: 'Constraints';   Color: $0066AA;   Style: '___'), // CC8800, _I_
      (  Reg: 'Synonyms';      Color: $008800;   Style: '_I_'),
      (  Reg: 'Transactions';  Color: $CC4444;   Style: '_I_'),
      (  Reg: 'Parameters';    Color: $0088FF;   Style: 'B__'), // 0000FF, BI_
      (  Reg: 'FunParams';     Color: $0088FF;   Style: 'B__'),
      (  Reg: 'ExtQueryAliasOrTable';Color:$FF8800;Style:'B_U'), // FF0088

      // EXT. KEYWORDS
      (  Reg: 'Datatypes';     Color: $00CCFF;   Style: 'B__'),
      (  Reg: 'DmlSelect';     Color: $44AA44;   Style: 'B__'),
      (  Reg: 'DmlInsert';     Color: $4488CC;   Style: 'B__'),
      (  Reg: 'DmlUpdate';     Color: $CC4400;   Style: 'B__'), // CC00CC
      (  Reg: 'DmlDelete';     Color: $CC00CC;   Style: 'B__'), // FF0088
      (  Reg: 'DdlCreate';     Color: $EE4488;   Style: 'B__'), // FF8800
      (  Reg: 'DdlCreateView'; Color: $88AA44;   Style: 'B__'),
      (  Reg: 'DdlDrop';       Color: $FF00FF;   Style: 'B__'), // FF4488
      (  Reg: 'DdlModify';     Color: $EE6644;   Style: 'B__'), // EE6600
      (  Reg: 'Tcl';           Color: $CC0066;   Style: 'B__'),
      (  Reg: 'Dcl';           Color: $CC44CC;   Style: 'B__'),
      (  Reg: 'Union';         Color: $CC44CC;   Style: 'B__'),
      (  Reg: 'Null';          Color: $444444;   Style: 'B__'), // 000000, CC44CC
      (  Reg: 'Prior';         Color: $CC0000;   Style: 'B__'),

      // EXT. CASES
      (  Reg: 'Cases1';        Color: $AA4444;   Style: 'B__'), // CC6666
      (  Reg: 'Cases2';        Color: $4444AA;   Style: 'B__'), // 6666CC
      (  Reg: 'Cases3';        Color: $44AA44;   Style: 'B__'), // 66CC66
      (  Reg: 'Cases4';        Color: $44AAAA;   Style: 'B__'), // 66CCCC
      (  Reg: 'Cases5';        Color: $AA44AA;   Style: 'B__'), // CC66CC
      (  Reg: 'Cases6';        Color: $AAAA44;   Style: 'B__'), // CCCC66

      // EXT. BRACKETS
      (  Reg: 'Brackets1';     Color: $EE2222;   Style: 'B__'),
      (  Reg: 'Brackets2';     Color: $22EE22;   Style: 'B__'), // EE22EE, 22EE22
      (  Reg: 'Brackets3';     Color: $2222EE;   Style: 'B__'),
      (  Reg: 'Brackets4';     Color: $EEEE22;   Style: 'B__'), // 22EEEE
      (  Reg: 'Brackets5';     Color: $EE22EE;   Style: 'B__'), // 22EE22, EE22EE
      (  Reg: 'Brackets6';     Color: $22EEEE;   Style: 'B__')  // EEEE22
    );

function YaRegKey(aKey: TYaRegKey): String;

procedure GtRegistryPutBool(aSett: TGtListerSettings; aValue: Boolean; aForce: Boolean = False);
function  GtRegistryGetBool(aSett: TGtListerSettings): Boolean;

procedure GtRegistryPutCase(aSett: TGtListerCaseSettings; aValue: Integer; aForce: Boolean = False);
function  GtRegistryGetCase(aSett: TGtListerCaseSettings): TGtSqlCaseOption;

procedure YaRegistryPutBool(aSett: TYaBoolSettings; aValue: Boolean; aForce: Boolean = False);
function  YaRegistryGetBool(aSett: TYaBoolSettings): Boolean;

function YaRegistryGetColor    (aStyle: TGtLexTokenStyle): String;
function YaRegistryGetBold     (aStyle: TGtLexTokenStyle): Boolean;
function YaRegistryGetItalic   (aStyle: TGtLexTokenStyle): Boolean;
function YaRegistryGetUnderline(aStyle: TGtLexTokenStyle): Boolean;

procedure YaRegistryPutColor   (aStyle: TGtLexTokenStyle; aColor: String);
procedure YaRegistryPutBIU     (aStyle: TGtLexTokenStyle; aBold, aItalic, aUnderline: Boolean);

procedure SetScriptListerOptions(aScriptLister: TGtSqlProtoLister);
procedure SetScriptFormatOptions(aScriptFormater: TGtSqlFormatLister; aScriptFormat: Boolean);

implementation

uses Classes, {Forms, Dialogs,} SysUtils,
     GtStandard, GtRegistry, SqlParser;

var StatusLogStartTime1, StatusLogLastTimeInt: TDateTime;
var StatusLogTextInt: String;

{ status log - stores start time }
procedure StatusLogStartTime;
begin
  StatusLogStartTime1 := GtNowSuper;
end;

{ status log - shows time }
procedure StatusLogStopTime(aCaption: String);
begin
  StatusLogLastTimeInt := GtNowSuper - StatusLogStartTime1;

  StatusLogTextInt := aCaption +
       strif(aCaption <> '|',
             ': ' + StatusLogLastTimeStr +
             strif((StatusLogTextInt <> ''), ', ') ) +
       StatusLogTextInt;
end;

{ returns status log text }
function  StatusLogText: String;
begin
  Result := StatusLogTextInt;
end;

{ returns status log last time }
function  StatusLogLastTime: TDateTime;
begin
  Result := StatusLogLastTimeInt;
end;

{ returns status log last time }
function  StatusLogLastTimeStr: String;
begin
  Result := strif(1000 * 24*60*60 * StatusLogLastTimeInt < 10, Format('%.2f ms', [1000 * 24*60*60*StatusLogLastTimeInt]),
            strif(1000 * 24*60*60 * StatusLogLastTimeInt < 100, Format('%.1f ms', [1000 * 24*60*60*StatusLogLastTimeInt]),
            strif(1000 * 24*60*60 * StatusLogLastTimeInt >= 100, Format('%.0f ms', [1000 * 24*60*60*StatusLogLastTimeInt])))) ;
end;

{ gets YA registry key }
function YaRegKey(aKey: TYaRegKey): String;
begin
  if aKey = yarkSettings then Result := YA_SETTINGS_KEY else Result := YA_COMMON_KEY;
end;

{ gets YA Boolean Setting }
function  GtRegistryGetBool(aSett: TGtListerSettings): Boolean;
begin
  Result := rguGetBool(YaRegKey(GT_SET_BOOL_ARR [ aSett ].Key) + GT_SET_BOOL_ARR [ aSett ].Reg, GT_SET_BOOL_ARR [ aSett ].Def);
end;

{ puts YA Boolean Setting }
procedure GtRegistryPutBool(aSett: TGtListerSettings; aValue: Boolean; aForce: Boolean = False);
begin
  if aForce or (aValue <> GT_SET_BOOL_ARR [ aSett ].Def)
    then rguPutBool(YaRegKey(GT_SET_BOOL_ARR [ aSett ].Key) + GT_SET_BOOL_ARR [ aSett ].Reg, aValue)
    else rguDeleteVal(YaRegKey(GT_SET_BOOL_ARR [ aSett ].Key) + GT_SET_BOOL_ARR [ aSett ].Reg);
end;

{ gets YA Case Setting }
function  GtRegistryGetCase(aSett: TGtListerCaseSettings): TGtSqlCaseOption;
begin
  case rguGetInt(YaRegKey(GT_SET_CASE_ARR [ aSett ].Key) + GT_SET_CASE_ARR [ aSett ].Reg,
                 Ord(GT_SET_CASE_ARR [ aSett ].Def)) of
    1: Result := gtcoUpperCase;
    2: Result := gtcoLowerCase;
    3: Result := gtcoFirstCharUpper;
    4: Result := gtcoFirstUseCase;
  else Result := gtcoNoChange;
  end;
end;

{ puts YA Case Setting }
procedure GtRegistryPutCase(aSett: TGtListerCaseSettings; aValue: Integer; aForce: Boolean = False);
begin
  if aForce or (aValue <> Ord(GT_SET_CASE_ARR [ aSett ].Def))
    then rguPutInt(YaRegKey(GT_SET_CASE_ARR [ aSett ].Key) + GT_SET_CASE_ARR [ aSett ].Reg, aValue)
    else rguDeleteVal(YaRegKey(GT_SET_CASE_ARR [ aSett ].Key) + GT_SET_CASE_ARR [ aSett ].Reg);
end;

{ puts YA Boolean Setting }
procedure YaRegistryPutBool(aSett: TYaBoolSettings; aValue: Boolean; aForce: Boolean = False);
begin
  if aForce or (aValue <> YA_SET_BOOL_ARR [ aSett ].Def)
    then rguPutBool(YaRegKey(YA_SET_BOOL_ARR [ aSett ].Key) + YA_SET_BOOL_ARR [ aSett ].Reg, aValue)
    else rguDeleteVal(YaRegKey(YA_SET_BOOL_ARR [ aSett ].Key) + YA_SET_BOOL_ARR [ aSett ].Reg);
end;

{ gets YA Boolean Setting }
function  YaRegistryGetBool(aSett: TYaBoolSettings): Boolean;
begin
  Result := rguGetBool(YaRegKey(YA_SET_BOOL_ARR [ aSett ].Key) + YA_SET_BOOL_ARR [ aSett ].Reg, YA_SET_BOOL_ARR [ aSett ].Def);
end;

{ gets token color from registry }
function YaRegistryGetColor(aStyle: TGtLexTokenStyle): String;
begin
  Result := IntToHex( HexToInt(
            rguGetStr (YA_SETTINGS_KEY + YA_SET_COLOR_ARR [aStyle].Reg + 'Color',
                       IntToHex( YA_SET_COLOR_ARR [aStyle].Color, 6 ) )
            ), 6 );
end;

{ puts token color to registry }
procedure YaRegistryPutColor    (aStyle: TGtLexTokenStyle; aColor: String);
begin
  aColor := IntToHex( HexToInt( aColor ), 6 );

  if aColor <> IntToHex( YA_SET_COLOR_ARR[aStyle].Color, 6 )
    then rguPutStr (YA_SETTINGS_KEY + YA_SET_COLOR_ARR[aStyle].Reg + 'Color', aColor );
end;

{ gets token bold from registry }
function YaRegistryGetBold     (aStyle: TGtLexTokenStyle): Boolean;
begin
  Result := Pos('B', rguGetStr( YA_SETTINGS_KEY + YA_SET_COLOR_ARR [aStyle].Reg + 'Style',
                                YA_SET_COLOR_ARR [aStyle].Style) ) > 0;
end;

{ gets token italic from registry }
function YaRegistryGetItalic   (aStyle: TGtLexTokenStyle): Boolean;
begin
  Result := Pos('I', rguGetStr( YA_SETTINGS_KEY + YA_SET_COLOR_ARR [aStyle].Reg + 'Style',
                                YA_SET_COLOR_ARR [aStyle].Style) ) > 0;
end;

{ gets token underline from registry }
function YaRegistryGetUnderline(aStyle: TGtLexTokenStyle): Boolean;
begin
  Result := Pos('U', rguGetStr( YA_SETTINGS_KEY + YA_SET_COLOR_ARR [aStyle].Reg + 'Style',
                                YA_SET_COLOR_ARR [aStyle].Style) ) > 0;
end;

{ puts token BIU to registry }
procedure YaRegistryPutBIU     (aStyle: TGtLexTokenStyle; aBold, aItalic, aUnderline: Boolean);
var s: String;
begin
  s := StrIf(aBold,      'B', '_') +
       StrIf(aItalic,    'I', '_') +
       StrIf(aUnderline, 'U', '_');

  if s <> YA_SET_COLOR_ARR[aStyle].Style
    then rguPutStr (YA_SETTINGS_KEY + YA_SET_COLOR_ARR[aStyle].Reg + 'Style', s );
end;

{ set style options for script lister }
procedure SetScriptListerOptions(aScriptLister: TGtSqlProtoLister);

  procedure ColorAndStyleSetLister(aLister: TGtSqlProtoLister; aStyle: TGtLexTokenStyle; aStyle2: TGtLexTokenStyle);
  begin
    if not Assigned(aLister) then Exit;

    aLister.SetStyle( aStyle,
                      HexToInt( YaRegistryGetColor(aStyle2) ),
                      YaRegistryGetBold(aStyle2),
                      YaRegistryGetItalic(aStyle2),
                      YaRegistryGetUnderline(aStyle2)
                    );
  end;

var lStyle: TGtLexTokenStyle;
begin
  for lStyle := Low(TGtLexTokenStyle) to High(TGtLexTokenStyle) do begin
    if lStyle in [gtlsTable, gtlsView, gtlsTableAlias, gtlsColumn, //gtlsRefColumn,
                  gtlsColumnAlias, gtlsFunction, gtlsConstraint, gtlsSynonym,
                  gtlsTransaction, gtlsParameter, gtlsExtQueryAliasOrTable] then begin
      if YaRegistryGetBool (yastExtColorIdentifiers)
        then ColorAndStyleSetLister(aScriptLister, lStyle, lStyle)
        else ColorAndStyleSetLister(aScriptLister, lStyle, gtlsIdentifier)
    end else
    if lStyle in [gtlsDatatype, gtlsDmlSelect, gtlsDmlInsert, gtlsDmlUpdate, gtlsDmlDelete,
                  gtlsDdlCreate, gtlsDdlDrop, gtlsDdlModify, gtlsTcl, gtlsDcl,
                  gtlsUnion, gtlsNull] then begin
      if YaRegistryGetBool (yastExtColorKeywords)
        then ColorAndStyleSetLister(aScriptLister, lStyle, lStyle)
        else ColorAndStyleSetLister(aScriptLister, lStyle, gtlsKeyword)
    end else
    if lStyle in [gtlsBracket1, gtlsBracket2, gtlsBracket3, gtlsBracket4, gtlsBracket5, gtlsBracket6] then begin
      if YaRegistryGetBool (yastExtColorBrackets)
        then ColorAndStyleSetLister(aScriptLister, lStyle, lStyle)
        else ColorAndStyleSetLister(aScriptLister, lStyle, gtlsPlainText)
    end else
    if lStyle in [gtlsCaseWhen1, gtlsCaseWhen2, gtlsCaseWhen3, gtlsCaseWhen4, gtlsCaseWhen5, gtlsCaseWhen6] then begin
      if YaRegistryGetBool (yastExtColorCases)
        then ColorAndStyleSetLister(aScriptLister, lStyle, lStyle)
        else ColorAndStyleSetLister(aScriptLister, lStyle, gtlsPlainText)
    end else begin
      ColorAndStyleSetLister(aScriptLister, lStyle, lStyle);
    end;
  end;
end;

{ set format options for script lister }
procedure SetScriptFormatOptions(aScriptFormater: TGtSqlFormatLister; aScriptFormat: Boolean);
var lOpt: TGtListerSettings;
    lCase: TGtListerCaseSettings;
begin
  for lCase := Low(TGtListerCaseSettings) to High(TGtListerCaseSettings)
    do aScriptFormater.CaseOpt[ lCase ] := GtRegistryGetCase (lCase);

  if aScriptFormat then begin { Format }
    for lOpt := Low(TGtListerSettings) to High(TGtListerSettings)
      do aScriptFormater.Options [lOpt] := GtRegistryGetBool (lOpt);

    aScriptFormater.ClauseIntend               := True;
    aScriptFormater.SubQueryIntend             := True;
    aScriptFormater.SubQueryIntendSpace        := 0; //2;

    aScriptFormater.MaxTableNameToIntend := StrToInt(rguGetStr(YA_SET_KEY_TAB_MAX_LEN,   '30'));
    aScriptFormater.MaxAliasNameToIntend := StrToInt(rguGetStr(YA_SET_KEY_ALIAS_MAX_LEN, '10'));

    aScriptFormater.MaxSetLeftExprToIntend := StrToInt(rguGetStr(YA_SET_KEY_SET_MAX_LEN, '30')); // 20

    aScriptFormater.MaxColumnNameToIntend := StrToInt(rguGetStr(YA_SET_KEY_COL_MAX_LEN,  '20'));
    aScriptFormater.MaxDatatypeToIntend   := StrToInt(rguGetStr(YA_SET_KEY_TYPE_MAX_LEN, '20'));

    aScriptFormater.MaxShortQueryLines    := StrToInt(rguGetStr(YA_SET_KEY_SHORT_QUERY,  '20'));
    aScriptFormater.MaxClauseToIntend     := StrToInt(rguGetStr(YA_SET_KEY_MAX_CLAUSE_KEYWORD_INT,  '15'));
    aScriptFormater.MaxIdentifierLen      := StrToInt(rguGetStr(YA_SET_KEY_MAX_IDENTIFIER_LEN,  '30'));

    aScriptFormater.LinesNoAfterQuery     := StrToInt(rguGetStr(YA_SET_KEY_LINES_AFTER_QUERY, '1'));
  end else begin { Compact }
    aScriptFormater.Options [ gtstColumnConstraint ] := GtRegistryGetBool (gtstColumnConstraint);
    aScriptFormater.Options [ gtstNoSemicolonOnSingleQuery ] := GtRegistryGetBool (gtstNoSemicolonOnSingleQuery);

    aScriptFormater.SubQueryIntend             := False;
  end;
end;

begin
  StatusLogStartTime1 := 0;
  StatusLogLastTimeInt   := 0;
  StatusLogTextInt    := '';
end.

