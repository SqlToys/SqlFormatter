(* $Header: /SQL Toys/SqlFormat/SqlCommon.pas 36    19-01-10 19:05 Tomek $
   (c) Tomasz Gierka, github.com/SqlToys, 2014.08.26                          *)
{--------------------------------------  --------------------------------------}
{$IFDEF RELEASE}
  {$DEBUGINFO OFF}
  {$LOCALSYMBOLS OFF}
{$ENDIF}
unit SqlCommon;

interface

uses GtTokenizers, SqlLister;

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
//YA_SET_KEY_TAB_MAX_LEN            : String = YA_SETTINGS_KEY + 'TableMaxLen';
//YA_SET_KEY_ALIAS_MAX_LEN          : String = YA_SETTINGS_KEY + 'AliasMaxLen';
//YA_SET_KEY_SET_MAX_LEN            : String = YA_SETTINGS_KEY + 'SetMaxLen';
//YA_SET_KEY_COL_MAX_LEN            : String = YA_SETTINGS_KEY + 'ColumnMaxLen';
//YA_SET_KEY_TYPE_MAX_LEN           : String = YA_SETTINGS_KEY + 'DatatypeMaxLen';
//YA_SET_KEY_SHORT_QUERY            : String = YA_SETTINGS_KEY + 'ShortQuery';
//YA_SET_KEY_MAX_CLAUSE_KEYWORD_INT : String = YA_SETTINGS_KEY + 'MaxClauseKeywordIntend';
//YA_SET_KEY_MAX_IDENTIFIER_LEN     : String = YA_SETTINGS_KEY + 'MaxIdentifierLen';
//YA_SET_KEY_LINES_AFTER_QUERY      : String = YA_SETTINGS_KEY + 'MoreLinesAfterQuery';

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

//var
//  GT_SET_BOOL_ARR : array [ TGtListerSettings ]
//  of record Key: TYaRegKey; Reg: String; Def: Boolean end
//  = ( (Key: yarkSettings; Reg: 'RightIntend';            Def: True),
//      (Key: yarkSettings; Reg: 'EmptyLineAfterQuery';    Def: True),
//      (Key: yarkSettings; Reg: 'SpaceBeforeComma';       Def: False),
//      (Key: yarkSettings; Reg: 'SpaceBeforeSemicolon';   Def: True), // False
//      (Key: yarkSettings; Reg: 'EmptyLineBeforeClause';  Def: True), // False
//      (Key: yarkSettings; Reg: 'UpperKeywords';          Def: True),
//      (Key: yarkSettings; Reg: 'ExpressionAsKeyword';    Def: True),
//      (Key: yarkSettings; Reg: 'TableAsKeyword';         Def: False), // True
//      (Key: yarkSettings; Reg: 'ColumnConstraint';       Def: True),
//      (Key: yarkSettings; Reg: 'OuterJoin';              Def: False),
//      (Key: yarkSettings; Reg: 'SortShortKeyword';       Def: True),
//      (Key: yarkSettings; Reg: 'SkipAscending';          Def: True), // False
//      (Key: yarkSettings; Reg: 'OneExprOnLine';          Def: True),
//      (Key: yarkSettings; Reg: 'OneCondOnLine';          Def: True),
//      (Key: yarkSettings; Reg: 'EmptyLineArounUnion';    Def: True),
//
//      (Key: yarkSettings; Reg: 'SpaceOutsideBrackets';   Def: False), // True
//      (Key: yarkSettings; Reg: 'SpaceInsideBrackets';    Def: True),
//      (Key: yarkSettings; Reg: 'SpaceAroundOperator';    Def: True),
//      (Key: yarkSettings; Reg: 'SpaceAfterComma';        Def: True),
//      (Key: yarkSettings; Reg: 'CommaAtNewLine';         Def: True),
//
//      (Key: yarkSettings; Reg: 'CaseIntend';             Def: False),
//      (Key: yarkSettings; Reg: 'CaseWhenAtNewLine';      Def: True),
//      (Key: yarkSettings; Reg: 'CaseThenAtNewLine';      Def: True),
//      (Key: yarkSettings; Reg: 'CaseElseAtNewLine';      Def: True),
//      (Key: yarkSettings; Reg: 'CaseEndAtNewLine';       Def: True),
//
//      (Key: yarkSettings; Reg: 'TableAndAliasIntend';    Def: True),
//      (Key: yarkSettings; Reg: 'SetExprIntend';          Def: True),
//      (Key: yarkSettings; Reg: 'CreateTableColConsBreakine';Def: True),
//      (Key: yarkSettings; Reg: 'NoSemicolonOnSingleQuery';Def:True),
//      (Key: yarkSettings; Reg: 'InnerJoin';              Def: False),
//      (Key: yarkSettings; Reg: 'AliasFirstUseCase';      Def: True),
//      (Key: yarkSettings; Reg: 'TableFirstUseCase';      Def: True),
//      (Key: yarkSettings; Reg: 'SpaceInsideBracketsSkipFunOneParam'; Def: True),
//      (Key: yarkSettings; Reg: 'CreateTableColConsNewLineAfter'; Def: True),
//      (Key: yarkSettings; Reg: 'JoinCondOrder';          Def: True),
//      (Key: yarkSettings; Reg: 'CreateTableIntend';      Def: True),
//      (Key: yarkSettings; Reg: 'CreateTableEmptyLineBeforeComplexConstraints'; Def: True),
//      (Key: yarkSettings; Reg: 'EmptyLineBeforeClauseSkipSubquery'; Def: True),
//      (Key: yarkSettings; Reg: 'OnCondIntend';           Def: True),
//      (Key: yarkSettings; Reg: 'SelectAliasIntend';      Def: True),
//      (Key: yarkSettings; Reg: 'SpaceInsideBracketsSkipDatatype'; Def: True),
//      (Key: yarkSettings; Reg: 'EmptyLineBeforeClauseSkipShort'; Def: True),
//      (Key: yarkSettings; Reg: 'OnCondRefsFirst';        Def: True),
//      (Key: yarkSettings; Reg: 'ExtQueryKeywordStyle';   Def: True),
//      (Key: yarkSettings; Reg: 'LinesNoAfterQuery';      Def: False)
//    );

//var
//  GT_SET_CASE_ARR : array [ TGtListerCaseSettings ]
//  of record Key: TYaRegKey; Reg: String; Def: TGtSqlCaseOption end
//  = ( (Key: yarkSettings; Reg: 'TableCase';             Def: gtcoFirstUseCase  ),
//      (Key: yarkSettings; Reg: 'ColumnCase';            Def: gtcoNoChange ),
//      (Key: yarkSettings; Reg: 'TableAliasCase';        Def: gtcoFirstUseCase ),
//      (Key: yarkSettings; Reg: 'ColumnAliasCase';       Def: gtcoNoChange ),
//      (Key: yarkSettings; Reg: 'ParameterCase';         Def: gtcoNoChange ),
//      (Key: yarkSettings; Reg: 'IdentifierCase';        Def: gtcoNoChange ),
//      (Key: yarkSettings; Reg: 'KeywordCase';           Def: gtcoUpperCase ),
//      (Key: yarkSettings; Reg: 'ColumnQuotedAliasCase'; Def: gtcoNoChange ),
//      (Key: yarkSettings; Reg: 'FunctionCase';          Def: gtcoNoChange )
//    );

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
      (  Reg: 'TableAliasDef'; Color: $FF8800;   Style: 'B__'), // EE8800
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
      (  Reg: 'BracketOpen1';  Color: $EE2222;   Style: 'B__'),
      (  Reg: 'BracketOpen2';  Color: $22EE22;   Style: 'B__'), // EE22EE, 22EE22
      (  Reg: 'BracketOpen3';  Color: $2222EE;   Style: 'B__'),
      (  Reg: 'BracketOpen4';  Color: $EEEE22;   Style: 'B__'), // 22EEEE
      (  Reg: 'BracketOpen5';  Color: $EE22EE;   Style: 'B__'), // 22EE22, EE22EE
      (  Reg: 'BracketOpen6';  Color: $22EEEE;   Style: 'B__'), // EEEE22
      (  Reg: 'BracketClose1'; Color: $EE2222;   Style: 'B__'),
      (  Reg: 'BracketClose2'; Color: $22EE22;   Style: 'B__'), // EE22EE, 22EE22
      (  Reg: 'BracketClose3'; Color: $2222EE;   Style: 'B__'),
      (  Reg: 'BracketClose4'; Color: $EEEE22;   Style: 'B__'), // 22EEEE
      (  Reg: 'BracketClose5'; Color: $EE22EE;   Style: 'B__'), // 22EE22, EE22EE
      (  Reg: 'BracketClose6'; Color: $22EEEE;   Style: 'B__'), // EEEE22

      // EXT. RELEVANT
      (  Reg: 'Semicolon';     Color: $000000;   Style: '___'),
      (  Reg: 'Comma';         Color: $000000;   Style: '___')
    );

function YaRegKey(aKey: TYaRegKey): String;

//procedure GtRegistryPutBool(aSett: TGtListerSettings; aValue: Boolean; aForce: Boolean = False);
//function  GtRegistryGetBool(aSett: TGtListerSettings): Boolean;

//procedure GtRegistryPutCase(aSett: TGtListerCaseSettings; aValue: Integer; aForce: Boolean = False);
//function  GtRegistryGetCase(aSett: TGtListerCaseSettings): TGtSqlCaseOption;

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

{----------------------------- SQL Keyword Tokens -----------------------------}
var
  { ---- A ---- }
  gtkwAdd,            gtkwAfter,          gtkwAlter,          gtkwAll,
  gtkwAnalyze,
  gtkwAnd,            gtkwAny,            gtkwAs,             gtkwAsc,
  gtkwAscending,
  { ---- B ---- }
  gtkwBefore,         gtkwBegin,          gtkwBetween,        gtkwBigint,
  gtkwBinary,         gtkwBit,            gtkwBlob,           gtkwBoolean,
  gtkwBy,
  { ---- C ---- }
  gtkwCall,           gtkwCascade,        gtkwCase,           gtkwCast,
  gtkwChar,           gtkwCheck,          gtkwClob,           gtkwClose,
  gtkwCoalesce,
  gtkwCollate,        gtkwColumn,         gtkwCommit,         gtkwConnect,
  gtkwConstraint,     gtkwConstraints,    gtkwConvert,        gtkwCount,
  gtkwCreate,         gtkwCross,          gtkwCursor,
  { ---- D ---- }
  gtkwDatabase,       gtkwDate,           gtkwDatetime,       gtkwDeadlock,
  gtkwDeallocate,
  gtkwDec,            gtkwDecimal,        gtkwDeclare,        gtkwDefault,
  gtkwDenseRank,      gtkwDeny,           gtkwDemand,
  gtkwDelete,         gtkwDesc,           gtkwDescending,     gtkwDescribe,
  gtkwDisable,        gtkwDistinct,       gtkwDouble,         gtkwDrop,
  { ---- E ---- }
  gtkwEach,           gtkwElse,           gtkwElseIf,         gtkwEnable,
  gtkwEncryption,     gtkwEnd,            gtkwEscape,         gtkwExcept,
  gtkwException,      gtkwExec,           gtkwExecute,        gtkwExit,
  gtkwExists,
  { ---- F ---- }
  gtkwFirst,
  gtkwFloat,          gtkwFor,            gtkwForeign,        gtkwForward,
  gtkwForce,
  gtkwFrom,           gtkwFull,           gtkwFunction,
  { ---- G ---- }
  gtkwGlobal,         gtkwGo,             gtkwGoto,           gtkwGrant,
  gtkwGroup,
  { ---- H ---- }
  gtkwHaving,         gtkwHint,
  { ---- I ---- }
  gtkwIdentity,       gtkwIf,             gtkwImmediate,      gtkwIn,
  gtkwIncrement,      gtkwIndex,          gtkwInner,          gtkwInstead,
  gtkwInt,            gtkwInt4,           gtkwInt8,           gtkwInteger,
  gtkwInto,           gtkwInsert,         gtkwIntersect,      gtkwIs,
  { ---- J ---- }
  gtkwJoin,
  { ---- K ---- }
  gtkwKeep,           gtkwKey,
  { ---- L ---- }
  gtkwLabel,          gtkwLast,
  gtkwLeft,           gtkwLike,           gtkwLimit,
  gtkwLock,           gtkwLogin,          gtkwLoop,
  { ---- M ---- }
  gtkwMaterialized,   gtkwMinus,          gtkwModify,         gtkwMovement,
  { ---- N ---- }
  gtkwNchar,          gtkwNew,            gtkwNext,
  gtkwNoCycle,        gtkwNot,            gtkwNoWait,
  gtkwNull,           gtkwNulls,
  gtkwNumber,         gtkwNumeric,        gtkwNvarchar,
  { ---- O ---- }
  gtkwOf,             gtkwOld,            gtkwOn,             gtkwOpen,
  gtkwOption,         gtkwOr,             gtkwOrder,          gtkwOthers,
  gtkwOut,            gtkwOutput,         gtkwOuter,          gtkwOver,
  { ---- P ---- }
  gtkwPartition,      gtkwPassword,       gtkwPragma,         gtkwPreserve,
  gtkwPrimary,
  gtkwPrint,          gtkwPrior,          gtkwProcedure,      gtkwPublic,
  gtkwPurge,
  { ---- Q ---- }
  gtkwQuick,
  { ---- R ---- }
  gtkwRank,           gtkwReal,           gtkwReferences,     gtkwRename,
  gtkwRecycleBin,     gtkwRefresh,        gtkwRestrict,
  gtkwRepeat,         gtkwReplace,        gtkwReturn,         gtkwReturns,
  gtkwReturning,      gtkwRevoke,
  gtkwRight,          gtkwRollback,       gtkwRow,            gtkwRows,
  { ---- S ---- }
  gtkwSave,           gtkwSavepoint,      gtkwSchema,         gtkwSelect,
  gtkwSet,            gtkwSequence,       gtkwSign,           gtkwSingle,
  gtkwShrink,
  gtkwSmallint,       gtkwSome,           gtkwStart,          gtkwStop,
  gtkwStructure,      gtkwSynonym,        gtkwSpace,
  { ---- T ---- }
  gtkwTable,          gtkwTemp,           gtkwTemporary,      gtkwTinyint,
  gtkwTime,           gtkwTimeout,        gtkwThen,           gtkwTo,
  gtkwTop,            gtkwTran,           gtkwTrans,          gtkwTransaction,
  gtkwTrigger,        gtkwTriggers,       gtkwTruncate,       gtkwType,
  { ---- U ---- }
  gtkwUnion,          gtkwUnique,         gtkwUnsigned,       gtkwUntil,
  gtkwUnused,
  gtkwUpdate,         gtkwUse,            gtkwUser,           gtkwUsing,
  { ---- V ---- }
  gtkwValidate,
  gtkwValues,         gtkwVarchar,        gtkwVarchar2,       gtkwVersion,
  gtkwView,
  { ---- W ---- }
  gtkwWait,           gtkwWith,           gtkwWhen,           gtkwWhere,
  gtkwWhile,          gtkwWork,
  { ---- X ---- }
  gtkwXor
  { ---- Y ---- }
  { ---- Z ---- }
                                                              : TGtLexToken{Def};
{---------------------------- SQL Complex Keywords ----------------------------}
var
  { ---- A ---- }
  gtkwAdd_Check,        gtkwAdd_Column,       gtkwAdd_Constraint,  gtkwAdd_Foreign_Key,
  gtkwAdd_Primary_Key,  gtkwAdd_Unique,       gtkwAlter_Column,    gtkwAlter_Table,
  gtkwAlter_Trigger,    gtkwAlter_Index,      gtkwAnalyze_Index,
  { ---- B ---- }
  gtkwBegin_Tran,       gtkwBegin_Transaction,
  { ---- C ---- }
  gtkwCascade_Constraints,
  gtkwCommit_Tran,      gtkwCommit_Work,      gtkwCommit_Transaction,
  gtkwConnect_By,       gtkwCreate_Index,     gtkwCreate_Sequence, gtkwCreate_Table,
  gtkwCreate_Temporary_Table,                 gtkwCreate_Global_Temporary_Table,
  gtkwCreate_Global,    gtkwTemporary_Table,
  gtkwCreate_Login,
  gtkwCreate_Synonym,   gtkwCreate_Public_Synonym,                 gtkwCreate_Or_Replace_Synonym,
  gtkwCreate_Or_Replace_Public_Synonym,
  gtkwCreate_Unique_Index,                    gtkwCreate_User,
  gtkwCreate_View,      gtkwCreate_Or_Replace,gtkwCreate_Or_Replace_View,
  gtkwCreate_Materialized_View,               gtkwCreate_Or_Replace_Materialized_View,
  gtkwCross_Join,
  { ---- D ---- }
  gtkwDeallocate_Unused,
  gtkwDelete_From,      gtkwDelete_Rows,
  gtkwDrop_Table,       gtkwDrop_Column,     gtkwDrop_Constraint,
  gtkwDrop_Index,       gtkwDrop_Sequence,    gtkwDrop_Synonym,    gtkwDrop_View,
  { ---- E ---- }
  gtkwEnable_Row_Movement,
  gtkwEnd_Tran,         gtkwEnd_Transaction,
  { ---- F ---- }
  gtkwForeign_Key,      gtkwFor_Update,       gtkwFor_Update_Of,
  gtkwFull_Join,        gtkwFull_Outer_Join,
  { ---- G ---- }
  gtkwGroup_By,
  { ---- H ---- }
  { ---- I ---- }
  gtkwIncrement_By,     gtkwInner_Join,       gtkwInsert_Into,     gtkwInsert_Or_Replace_Into,
  gtkwIs_Not_Null,      gtkwIs_Null,
  { ---- J ---- }
  { ---- K ---- }
  { ---- L ---- }
  gtkwLeft_Join,        gtkwLeft_Outer_Join,
  { ---- M ---- }
  gtkwModify_Column,
  { ---- N ---- }
  gtkwNot_Between,      gtkwNot_Exists,       gtkwNot_In,          gtkwNot_Like,
  gtkwNot_Null,         gtkwNulls_First,      gtkwNulls_Last,
  { ---- O ---- }
  gtkwOn_Commit,        gtkwOn_Commit_Preserve_Rows,               gtkwOn_Commit_Delete_Rows,
  gtkwOn_Delete,        gtkwOn_Demand,        gtkwOn_Update,       gtkwOrder_By,
  gtkwOn_Delete_Cascade,gtkwOn_Delete_Restrict,                    gtkwOn_Delete_Set_Null,
  gtkwOn_Update_Cascade,gtkwOn_Update_Restrict,                    gtkwOn_Update_Set_Null,

  { ---- P ---- }
  gtkwPreserve_Rows,    gtkwPrimary_Key,      gtkwPurge_RecycleBin,
  { ---- Q ---- }
  { ---- R ---- }
  gtkwRefresh_Force,    gtkwRefresh_On_Demand,gtkwRefresh_Force_On_Demand,
  gtkwRename_Column,    gtkwRename_Table,     gtkwRename_To,       gtkwRight_Join,
  gtkwRight_Outer_Join, gtkwRollback_Tran,    gtkwRollback_Transaction,
  gtkwRollback_To_Savepoint,
  { ---- S ---- }
  gtkwSet_Null,         gtkwSelect_Into,
  gtkwShrink_Space,     gtkwStart_With,      gtkwStart_Transaction,
  gtkwStop_Transaction,
  { ---- T ---- }
  gtkwTo_Savepoint ,    gtkwTruncate_Table,
  { ---- U ---- }
  gtkwUnion_All,
  { ---- V ---- }
  gtkwValidate_Structure,
  gtkwValues_LeftBracket
  { ---- W ---- }
  { ---- X ---- }
  { ---- Y ---- }
  { ---- Z ---- }
                                                              : TGtLexToken{Def};
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
//function  GtRegistryGetBool(aSett: TGtListerSettings): Boolean;
//begin
//  Result := rguGetBool(YaRegKey(GT_SET_BOOL_ARR [ aSett ].Key) + GT_SET_BOOL_ARR [ aSett ].Reg, GT_SET_BOOL_ARR [ aSett ].Def);
//end;

{ puts YA Boolean Setting }
//procedure GtRegistryPutBool(aSett: TGtListerSettings; aValue: Boolean; aForce: Boolean = False);
//begin
//  if aForce or (aValue <> GT_SET_BOOL_ARR [ aSett ].Def)
//    then rguPutBool(YaRegKey(GT_SET_BOOL_ARR [ aSett ].Key) + GT_SET_BOOL_ARR [ aSett ].Reg, aValue)
//    else rguDeleteVal(YaRegKey(GT_SET_BOOL_ARR [ aSett ].Key) + GT_SET_BOOL_ARR [ aSett ].Reg);
//end;

{ gets YA Case Setting }
//function  GtRegistryGetCase(aSett: TGtListerCaseSettings): TGtSqlCaseOption;
//begin
//  case rguGetInt(YaRegKey(GT_SET_CASE_ARR [ aSett ].Key) + GT_SET_CASE_ARR [ aSett ].Reg,
//                 Ord(GT_SET_CASE_ARR [ aSett ].Def)) of
//    1: Result := gtcoUpperCase;
//    2: Result := gtcoLowerCase;
//    3: Result := gtcoFirstCharUpper;
//    4: Result := gtcoFirstUseCase;
//  else Result := gtcoNoChange;
//  end;
//end;

{ puts YA Case Setting }
//procedure GtRegistryPutCase(aSett: TGtListerCaseSettings; aValue: Integer; aForce: Boolean = False);
//begin
//  if aForce or (aValue <> Ord(GT_SET_CASE_ARR [ aSett ].Def))
//    then rguPutInt(YaRegKey(GT_SET_CASE_ARR [ aSett ].Key) + GT_SET_CASE_ARR [ aSett ].Reg, aValue)
//    else rguDeleteVal(YaRegKey(GT_SET_CASE_ARR [ aSett ].Key) + GT_SET_CASE_ARR [ aSett ].Reg);
//end;

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
    if lStyle in [gtlsBracketOpen1, gtlsBracketOpen2, gtlsBracketOpen3, gtlsBracketOpen4, gtlsBracketOpen5, gtlsBracketOpen6] then begin
      if YaRegistryGetBool (yastExtColorBrackets)
        then ColorAndStyleSetLister(aScriptLister, lStyle, lStyle)
        else ColorAndStyleSetLister(aScriptLister, lStyle, gtlsPlainText)
    end else
    if lStyle in [gtlsBracketClose1, gtlsBracketClose2, gtlsBracketClose3, gtlsBracketClose4, gtlsBracketClose5, gtlsBracketClose6] then begin
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
//var //lOpt: TGtListerSettings;
    //lCase: TGtListerCaseSettings;
begin
//  for lCase := Low(TGtListerCaseSettings) to High(TGtListerCaseSettings)
//    do aScriptFormater.CaseOpt[ lCase ] := GtRegistryGetCase (lCase);

  if aScriptFormat then begin { Format }
//    for lOpt := Low(TGtListerSettings) to High(TGtListerSettings)
//      do aScriptFormater.Options [lOpt] := GtRegistryGetBool (lOpt);

    aScriptFormater.ClauseIntend               := True;
    aScriptFormater.SubQueryIntend             := True;
    aScriptFormater.SubQueryIntendSpace        := 0; //2;

  //aScriptFormater.MaxTableNameToIntend := StrToInt(rguGetStr(YA_SET_KEY_TAB_MAX_LEN,   '30'));
  //aScriptFormater.MaxAliasNameToIntend := StrToInt(rguGetStr(YA_SET_KEY_ALIAS_MAX_LEN, '10'));

  //aScriptFormater.MaxSetLeftExprToIntend := StrToInt(rguGetStr(YA_SET_KEY_SET_MAX_LEN, '30')); // 20

  //aScriptFormater.MaxColumnNameToIntend := StrToInt(rguGetStr(YA_SET_KEY_COL_MAX_LEN,  '20'));
  //aScriptFormater.MaxDatatypeToIntend   := StrToInt(rguGetStr(YA_SET_KEY_TYPE_MAX_LEN, '20'));

  //aScriptFormater.MaxShortQueryLines    := StrToInt(rguGetStr(YA_SET_KEY_SHORT_QUERY,  '20'));
  //aScriptFormater.MaxClauseToIntend     := StrToInt(rguGetStr(YA_SET_KEY_MAX_CLAUSE_KEYWORD_INT,  '15'));
  //aScriptFormater.MaxIdentifierLen      := StrToInt(rguGetStr(YA_SET_KEY_MAX_IDENTIFIER_LEN,  '30'));

  //aScriptFormater.LinesNoAfterQuery     := StrToInt(rguGetStr(YA_SET_KEY_LINES_AFTER_QUERY, '1'));
  end else begin { Compact }
  //aScriptFormater.Options [ gtstColumnConstraint ] := GtRegistryGetBool (gtstColumnConstraint);
  //aScriptFormater.Options [ gtstNoSemicolonOnSingleQuery ] := GtRegistryGetBool (gtstNoSemicolonOnSingleQuery);

    aScriptFormater.SubQueryIntend             := False;
  end;
end;

begin
  StatusLogStartTime1 := 0;
  StatusLogLastTimeInt   := 0;
  StatusLogTextInt    := '';


{------------------------------- Keyword Tokens -------------------------------}

  { ---- A ---- }
  gtkwAdd                   := LexKeywordTokenDefs.AddToken( 'ADD',         '', gtttKeyword );
  gtkwAfter                 := LexKeywordTokenDefs.AddToken( 'AFTER',       '', gtttKeyword );
  gtkwAlter                 := LexKeywordTokenDefs.AddToken( 'ALTER',       '', gtttKeyword );
  gtkwAll                   := LexKeywordTokenDefs.AddToken( 'ALL',         '', gtttKeyword );
  gtkwAnalyze               := LexKeywordTokenDefs.AddToken( 'ANALYZE',     '', gtttKeyword );
  gtkwAnd                   := LexKeywordTokenDefs.AddToken( 'AND',         '', gtttKeyword );
  gtkwAny                   := LexKeywordTokenDefs.AddToken( 'ANY',         '', gtttKeyword );
  gtkwAs                    := LexKeywordTokenDefs.AddToken( 'AS',          '', gtttKeyword );
  gtkwAsc                   := LexKeywordTokenDefs.AddToken( 'ASC',         '', gtttKeyword );
  gtkwAscending             := LexKeywordTokenDefs.AddToken( 'ASCENDING',   '', gtttKeyword );
  { ---- B ---- }
  gtkwBefore                := LexKeywordTokenDefs.AddToken( 'BEFORE',      '', gtttKeyword );
  gtkwBegin                 := LexKeywordTokenDefs.AddToken( 'BEGIN',       '', gtttKeyword );
  gtkwBetween               := LexKeywordTokenDefs.AddToken( 'BETWEEN',     '', gtttKeyword );
  gtkwBigint                := LexKeywordTokenDefs.AddToken( 'BIGINT',      '', gtttKeyword, gtlsDatatype );
  gtkwBinary                := LexKeywordTokenDefs.AddToken( 'BINARY',      '', gtttKeyword, gtlsDatatype );
  gtkwBit                   := LexKeywordTokenDefs.AddToken( 'BIT',         '', gtttKeyword, gtlsDatatype );
  gtkwBlob                  := LexKeywordTokenDefs.AddToken( 'BLOB',        '', gtttKeyword, gtlsDatatype );
  gtkwBoolean               := LexKeywordTokenDefs.AddToken( 'BOOLEAN',     '', gtttKeyword, gtlsDatatype );
  gtkwBy                    := LexKeywordTokenDefs.AddToken( 'BY',          '', gtttKeyword );
  { ---- C ---- }
  gtkwCall                  := LexKeywordTokenDefs.AddToken( 'CALL',        '', gtttKeyword );
  gtkwCascade               := LexKeywordTokenDefs.AddToken( 'CASCADE',     '', gtttKeyword );
  gtkwCase                  := LexKeywordTokenDefs.AddToken( 'CASE',        '', gtttKeyword );
  gtkwCast                  := LexKeywordTokenDefs.AddToken( 'CAST',        '', gtttKeyword );
  gtkwChar                  := LexKeywordTokenDefs.AddToken( 'CHAR',        '', gtttKeyword, gtlsDatatype );
  gtkwCheck                 := LexKeywordTokenDefs.AddToken( 'CHECK',       '', gtttKeyword );
  gtkwClob                  := LexKeywordTokenDefs.AddToken( 'CLOB',        '', gtttKeyword, gtlsDatatype );
  gtkwClose                 := LexKeywordTokenDefs.AddToken( 'CLOSE',       '', gtttKeyword );
  gtkwCoalesce              := LexKeywordTokenDefs.AddToken( 'COALESCE',    '', gtttKeyword );
  gtkwCollate               := LexKeywordTokenDefs.AddToken( 'COLLATE',     '', gtttKeyword );
  gtkwColumn                := LexKeywordTokenDefs.AddToken( 'COLUMN',      '', gtttKeyword );
  gtkwCommit                := LexKeywordTokenDefs.AddToken( 'COMMIT',      '', gtttKeyword, gtlsTcl );
  gtkwConnect               := LexKeywordTokenDefs.AddToken( 'CONNECT',     '', gtttKeyword );
  gtkwConstraint            := LexKeywordTokenDefs.AddToken( 'CONSTRAINT',  '', gtttKeyword );
  gtkwConstraints           := LexKeywordTokenDefs.AddToken( 'CONSTRAINTS', '', gtttKeyword );
  gtkwConvert               := LexKeywordTokenDefs.AddToken( 'CONVERT',     '', gtttKeyword );
  gtkwCount                 := LexKeywordTokenDefs.AddToken( 'COUNT',       '', gtttKeyword, gtlsAggrFunction );
  gtkwCreate                := LexKeywordTokenDefs.AddToken( 'CREATE',      '', gtttKeyword );
  gtkwCross                 := LexKeywordTokenDefs.AddToken( 'CROSS',       '', gtttKeyword );
  gtkwCursor                := LexKeywordTokenDefs.AddToken( 'CURSOR',      '', gtttKeyword );
  { ---- D ---- }
  gtkwDatabase              := LexKeywordTokenDefs.AddToken( 'DATABASE',    '', gtttKeyword );
  gtkwDate                  := LexKeywordTokenDefs.AddToken( 'DATE',        '', gtttKeyword, gtlsDatatype );
  gtkwDatetime              := LexKeywordTokenDefs.AddToken( 'DATETIME',    '', gtttKeyword, gtlsDatatype );
  gtkwDeallocate            := LexKeywordTokenDefs.AddToken( 'DEALLOCATE',  '', gtttKeyword );
  gtkwDeadlock              := LexKeywordTokenDefs.AddToken( 'DEADLOCK',    '', gtttKeyword );
  gtkwDec                   := LexKeywordTokenDefs.AddToken( 'DEC',         '', gtttKeyword, gtlsDatatype );
  gtkwDecimal               := LexKeywordTokenDefs.AddToken( 'DECIMAL',     '', gtttKeyword, gtlsDatatype );
  gtkwDeclare               := LexKeywordTokenDefs.AddToken( 'DECLARE',     '', gtttKeyword );
  gtkwDefault               := LexKeywordTokenDefs.AddToken( 'DEFAULT',     '', gtttKeyword );
  gtkwDelete                := LexKeywordTokenDefs.AddToken( 'DELETE',      '', gtttKeyword );
  gtkwDemand                := LexKeywordTokenDefs.AddToken( 'DEMAND',      '', gtttKeyword );
  gtkwDenseRank             := LexKeywordTokenDefs.AddToken( 'DENSE_RANK', '', gtttKeyword );
  gtkwDeny                  := LexKeywordTokenDefs.AddToken( 'DENY',        '', gtttKeyword, gtlsDcl );
  gtkwDesc                  := LexKeywordTokenDefs.AddToken( 'DESC',        '', gtttKeyword );
  gtkwDescending            := LexKeywordTokenDefs.AddToken( 'DESCENDING',  '', gtttKeyword );
  gtkwDescribe              := LexKeywordTokenDefs.AddToken( 'DESCRIBE',    '', gtttKeyword );
  gtkwDisable               := LexKeywordTokenDefs.AddToken( 'DISABLE',     '', gtttKeyword );
  gtkwDistinct              := LexKeywordTokenDefs.AddToken( 'DISTINCT',    '', gtttKeyword, gtlsFunction );
  gtkwDouble                := LexKeywordTokenDefs.AddToken( 'DOUBLE',      '', gtttKeyword, gtlsDatatype );
  gtkwDrop                  := LexKeywordTokenDefs.AddToken( 'DROP',        '', gtttKeyword );
  { ---- E ---- }
  gtkwEach                  := LexKeywordTokenDefs.AddToken( 'EACH',        '', gtttKeyword );
  gtkwElse                  := LexKeywordTokenDefs.AddToken( 'ELSE',        '', gtttKeyword );
  gtkwElseIf                := LexKeywordTokenDefs.AddToken( 'ELSEIF',      '', gtttKeyword );
  gtkwEnable                := LexKeywordTokenDefs.AddToken( 'ENABLE',      '', gtttKeyword );
  gtkwEncryption            := LexKeywordTokenDefs.AddToken( 'ENCRYPTION',  '', gtttKeyword );
  gtkwEnd                   := LexKeywordTokenDefs.AddToken( 'END',         '', gtttKeyword );
  gtkwEscape                := LexKeywordTokenDefs.AddToken( 'ESCAPE',      '', gtttKeyword );
  gtkwExcept                := LexKeywordTokenDefs.AddToken( 'EXCEPT',      '', gtttKeyword, gtlsUnion );
  gtkwException             := LexKeywordTokenDefs.AddToken( 'EXCEPTION',   '', gtttKeyword );
  gtkwExec                  := LexKeywordTokenDefs.AddToken( 'EXEC',        '', gtttKeyword );
  gtkwExecute               := LexKeywordTokenDefs.AddToken( 'EXECUTE',     '', gtttKeyword );
  gtkwExit                  := LexKeywordTokenDefs.AddToken( 'EXIT',        '', gtttKeyword );
  gtkwExists                := LexKeywordTokenDefs.AddToken( 'EXISTS',      '', gtttKeyword );
  { ---- F ---- }
  gtkwFirst                 := LexKeywordTokenDefs.AddToken( 'FIRST',       '', gtttKeyword );
  gtkwFloat                 := LexKeywordTokenDefs.AddToken( 'FLOAT',       '', gtttKeyword, gtlsDatatype );
  gtkwFor                   := LexKeywordTokenDefs.AddToken( 'FOR',         '', gtttKeyword );
  gtkwForce                 := LexKeywordTokenDefs.AddToken( 'FORCE',       '', gtttKeyword );
  gtkwForeign               := LexKeywordTokenDefs.AddToken( 'FOREIGN',     '', gtttKeyword );
  gtkwForward               := LexKeywordTokenDefs.AddToken( 'FORWARD',     '', gtttKeyword );
  gtkwFrom                  := LexKeywordTokenDefs.AddToken( 'FROM',        '', gtttKeyword );
  gtkwFull                  := LexKeywordTokenDefs.AddToken( 'FULL',        '', gtttKeyword );
  gtkwFunction              := LexKeywordTokenDefs.AddToken( 'FUNCTION',    '', gtttKeyword );
  { ---- G ---- }
  gtkwGlobal                := LexKeywordTokenDefs.AddToken( 'GLOBAL',      '', gtttKeyword );
  gtkwGo                    := LexKeywordTokenDefs.AddToken( 'GO',          '', gtttKeyword );
  gtkwGoto                  := LexKeywordTokenDefs.AddToken( 'GOTO',        '', gtttKeyword );
  gtkwGrant                 := LexKeywordTokenDefs.AddToken( 'GRANT',       '', gtttKeyword, gtlsDcl );
  gtkwGroup                 := LexKeywordTokenDefs.AddToken( 'GROUP',       '', gtttKeyword );
  { ---- H ---- }
  gtkwHaving                := LexKeywordTokenDefs.AddToken( 'HAVING',      '', gtttKeyword );
  gtkwHint                  := LexKeywordTokenDefs.AddToken( 'HINT',        '', gtttKeyword );
  { ---- I ---- }
  gtkwIdentity              := LexKeywordTokenDefs.AddToken( 'IDENTITY',    '', gtttKeyword );
  gtkwIf                    := LexKeywordTokenDefs.AddToken( 'IF',          '', gtttKeyword );
  gtkwImmediate             := LexKeywordTokenDefs.AddToken( 'IMMEDIATE',   '', gtttKeyword );
  gtkwIn                    := LexKeywordTokenDefs.AddToken( 'IN',          '', gtttKeyword );
  gtkwIncrement             := LexKeywordTokenDefs.AddToken( 'INCREMENT',   '', gtttKeyword );
  gtkwIndex                 := LexKeywordTokenDefs.AddToken( 'INDEX',       '', gtttKeyword );
  gtkwInner                 := LexKeywordTokenDefs.AddToken( 'INNER',       '', gtttKeyword );
  gtkwInstead               := LexKeywordTokenDefs.AddToken( 'INSTEAD',     '', gtttKeyword );
  gtkwInt                   := LexKeywordTokenDefs.AddToken( 'INT',         '', gtttKeyword, gtlsDatatype );
  gtkwInt4                  := LexKeywordTokenDefs.AddToken( 'INT4',        '', gtttKeyword, gtlsDatatype );
  gtkwInt8                  := LexKeywordTokenDefs.AddToken( 'INT8',        '', gtttKeyword, gtlsDatatype );
  gtkwInteger               := LexKeywordTokenDefs.AddToken( 'INTEGER',     '', gtttKeyword, gtlsDatatype );
  gtkwInto                  := LexKeywordTokenDefs.AddToken( 'INTO',        '', gtttKeyword );
  gtkwInsert                := LexKeywordTokenDefs.AddToken( 'INSERT',      '', gtttKeyword );
  gtkwIntersect             := LexKeywordTokenDefs.AddToken( 'INTERSECT',   '', gtttKeyword, gtlsUnion );
  gtkwIs                    := LexKeywordTokenDefs.AddToken( 'IS',          '', gtttKeyword );
  { ---- J ---- }
  gtkwJoin                  := LexKeywordTokenDefs.AddToken( 'JOIN',        '', gtttKeyword );
  { ---- K ---- }
  gtkwKeep                  := LexKeywordTokenDefs.AddToken( 'KEEP',        '', gtttKeyword );
  gtkwKey                   := LexKeywordTokenDefs.AddToken( 'KEY',         '', gtttKeyword );
  { ---- L ---- }
  gtkwLast                  := LexKeywordTokenDefs.AddToken( 'LAST',        '', gtttKeyword );
  gtkwLabel                 := LexKeywordTokenDefs.AddToken( 'LABEL',       '', gtttKeyword );
  gtkwLeft                  := LexKeywordTokenDefs.AddToken( 'LEFT',        '', gtttKeyword );
  gtkwLike                  := LexKeywordTokenDefs.AddToken( 'LIKE',        '', gtttKeyword );
  gtkwLimit                 := LexKeywordTokenDefs.AddToken( 'LIMIT',       '', gtttKeyword );
  gtkwLock                  := LexKeywordTokenDefs.AddToken( 'LOCK',        '', gtttKeyword );
  gtkwLogin                 := LexKeywordTokenDefs.AddToken( 'LOGIN',       '', gtttKeyword );
  gtkwLoop                  := LexKeywordTokenDefs.AddToken( 'LOOP',        '', gtttKeyword );
  { ---- M ---- }
  gtkwMaterialized          := LexKeywordTokenDefs.AddToken( 'MATERIALIZED','', gtttKeyword );
  gtkwMinus                 := LexKeywordTokenDefs.AddToken( 'MINUS',       '', gtttKeyword, gtlsUnion );
  gtkwModify                := LexKeywordTokenDefs.AddToken( 'MODIFY',      '', gtttKeyword );
  gtkwMovement              := LexKeywordTokenDefs.AddToken( 'MOVEMENT',    '', gtttKeyword );
  { ---- N ---- }
  gtkwNChar                 := LexKeywordTokenDefs.AddToken( 'NCHAR',       '', gtttKeyword, gtlsDatatype );
  gtkwNew                   := LexKeywordTokenDefs.AddToken( 'NEW',         '', gtttKeyword );
  gtkwNext                  := LexKeywordTokenDefs.AddToken( 'NEXT',        '', gtttKeyword );
  gtkwNoCycle               := LexKeywordTokenDefs.AddToken( 'NOCYCLE',     '', gtttKeyword );
  gtkwNoWait                := LexKeywordTokenDefs.AddToken( 'NOWAIT',      '', gtttKeyword );
  gtkwNot                   := LexKeywordTokenDefs.AddToken( 'NOT',         '', gtttKeyword );
  gtkwNull                  := LexKeywordTokenDefs.AddToken( 'NULL',        '', gtttKeyword, gtlsNull);
  gtkwNulls                 := LexKeywordTokenDefs.AddToken( 'NULLS',       '', gtttKeyword, gtlsNull);
  gtkwNumber                := LexKeywordTokenDefs.AddToken( 'NUMBER',      '', gtttKeyword, gtlsDatatype );
  gtkwNumeric               := LexKeywordTokenDefs.AddToken( 'NUMERIC',     '', gtttKeyword, gtlsDatatype );
  gtkwNVarchar              := LexKeywordTokenDefs.AddToken( 'NVARCHAR',    '', gtttKeyword, gtlsDatatype );
  { ---- O ---- }
  gtkwOf                    := LexKeywordTokenDefs.AddToken( 'OF',          '', gtttKeyword );
  gtkwOld                   := LexKeywordTokenDefs.AddToken( 'OLD',         '', gtttKeyword );
  gtkwOn                    := LexKeywordTokenDefs.AddToken( 'ON',          '', gtttKeyword );
  gtkwOpen                  := LexKeywordTokenDefs.AddToken( 'OPEN',        '', gtttKeyword );
  gtkwOption                := LexKeywordTokenDefs.AddToken( 'OPTION',      '', gtttKeyword );
  gtkwOr                    := LexKeywordTokenDefs.AddToken( 'OR',          '', gtttKeyword );
  gtkwOrder                 := LexKeywordTokenDefs.AddToken( 'ORDER',       '', gtttKeyword );
  gtkwOthers                := LexKeywordTokenDefs.AddToken( 'OTHERS',      '', gtttKeyword );
  gtkwOut                   := LexKeywordTokenDefs.AddToken( 'OUT',         '', gtttKeyword );
  gtkwOutput                := LexKeywordTokenDefs.AddToken( 'OUTPUT',      '', gtttKeyword );
  gtkwOuter                 := LexKeywordTokenDefs.AddToken( 'OUTER',       '', gtttKeyword );
  gtkwOver                  := LexKeywordTokenDefs.AddToken( 'OVER',        '', gtttKeyword );
  { ---- P ---- }
  gtkwPartition             := LexKeywordTokenDefs.AddToken( 'PARTITION',   '', gtttKeyword );
  gtkwPassword              := LexKeywordTokenDefs.AddToken( 'PASSWORD',    '', gtttKeyword );
  gtkwPragma                := LexKeywordTokenDefs.AddToken( 'PRAGMA',      '', gtttKeyword );
  gtkwPreserve              := LexKeywordTokenDefs.AddToken( 'PRESERVE',    '', gtttKeyword );
  gtkwPrimary               := LexKeywordTokenDefs.AddToken( 'PRIMARY',     '', gtttKeyword );
  gtkwPrint                 := LexKeywordTokenDefs.AddToken( 'PRINT',       '', gtttKeyword );
  gtkwPrior                 := LexKeywordTokenDefs.AddToken( 'PRIOR',       '', gtttKeyword, gtlsPrior );
  gtkwProcedure             := LexKeywordTokenDefs.AddToken( 'PROCEDURE',   '', gtttKeyword );
  gtkwPublic                := LexKeywordTokenDefs.AddToken( 'PUBLIC',      '', gtttKeyword );
  gtkwPurge                 := LexKeywordTokenDefs.AddToken( 'PURGE',       '', gtttKeyword );
  { ---- Q ---- }
  gtkwQuick                 := LexKeywordTokenDefs.AddToken( 'QUICK',       '', gtttKeyword );
  { ---- R ---- }
  gtkwRank                  := LexKeywordTokenDefs.AddToken( 'RANK',        '', gtttKeyword );
  gtkwReal                  := LexKeywordTokenDefs.AddToken( 'REAL',        '', gtttKeyword, gtlsDatatype );
  gtkwRecycleBin            := LexKeywordTokenDefs.AddToken( 'RECYCLEBIN',  '', gtttKeyword );
  gtkwReferences            := LexKeywordTokenDefs.AddToken( 'REFERENCES',  '', gtttKeyword );
  gtkwRefresh               := LexKeywordTokenDefs.AddToken( 'REFRESH',     '', gtttKeyword );
  gtkwRename                := LexKeywordTokenDefs.AddToken( 'RENAME',      '', gtttKeyword );
  gtkwRepeat                := LexKeywordTokenDefs.AddToken( 'REPEAT',      '', gtttKeyword );
  gtkwReplace               := LexKeywordTokenDefs.AddToken( 'REPLACE',     '', gtttKeyword );
  gtkwRestrict              := LexKeywordTokenDefs.AddToken( 'RESTRICT',    '', gtttKeyword );
  gtkwReturn                := LexKeywordTokenDefs.AddToken( 'RETURN',      '', gtttKeyword );
  gtkwReturning             := LexKeywordTokenDefs.AddToken( 'RETURNING',   '', gtttKeyword );
  gtkwReturns               := LexKeywordTokenDefs.AddToken( 'RETURNS',     '', gtttKeyword );
  gtkwRevoke                := LexKeywordTokenDefs.AddToken( 'REVOKE',      '', gtttKeyword, gtlsDcl );
  gtkwRight                 := LexKeywordTokenDefs.AddToken( 'RIGHT',       '', gtttKeyword );
  gtkwRollback              := LexKeywordTokenDefs.AddToken( 'ROLLBACK',    '', gtttKeyword, gtlsTcl );
  gtkwRow                   := LexKeywordTokenDefs.AddToken( 'ROW',         '', gtttKeyword );
  gtkwRows                  := LexKeywordTokenDefs.AddToken( 'ROWS',        '', gtttKeyword );
  { ---- S ---- }
  gtkwSave                  := LexKeywordTokenDefs.AddToken( 'SAVE',        '', gtttKeyword );
  gtkwSavepoint             := LexKeywordTokenDefs.AddToken( 'SAVEPOINT',   '', gtttKeyword, gtlsTcl );
  gtkwSchema                := LexKeywordTokenDefs.AddToken( 'SCHEMA',      '', gtttKeyword );
  gtkwSelect                := LexKeywordTokenDefs.AddToken( 'SELECT',      '', gtttKeyword );
  gtkwSet                   := LexKeywordTokenDefs.AddToken( 'SET',         '', gtttKeyword );
  gtkwSequence              := LexKeywordTokenDefs.AddToken( 'SEQUENCE',    '', gtttKeyword );
  gtkwShrink                := LexKeywordTokenDefs.AddToken( 'SHRINK',      '', gtttKeyword );
  gtkwSign                  := LexKeywordTokenDefs.AddToken( 'SIGN',        '', gtttKeyword );
  gtkwSingle                := LexKeywordTokenDefs.AddToken( 'SINGLE',      '', gtttKeyword, gtlsDatatype );
  gtkwSmallint              := LexKeywordTokenDefs.AddToken( 'SMALLINT',    '', gtttKeyword, gtlsDatatype );
  gtkwSome                  := LexKeywordTokenDefs.AddToken( 'SOME',        '', gtttKeyword );
  gtkwSpace                 := LexKeywordTokenDefs.AddToken( 'SPACE',       '', gtttKeyword );
  gtkwStart                 := LexKeywordTokenDefs.AddToken( 'START',       '', gtttKeyword );
  gtkwStop                  := LexKeywordTokenDefs.AddToken( 'STOP',        '', gtttKeyword );
  gtkwStructure             := LexKeywordTokenDefs.AddToken( 'STRUCTURE',   '', gtttKeyword );
  gtkwSynonym               := LexKeywordTokenDefs.AddToken( 'SYNONYM',     '', gtttKeyword );
  { ---- T ---- }
  gtkwTable                 := LexKeywordTokenDefs.AddToken( 'TABLE',       '', gtttKeyword );
  gtkwTemp                  := LexKeywordTokenDefs.AddToken( 'TEMP',        '', gtttKeyword );
  gtkwTemporary             := LexKeywordTokenDefs.AddToken( 'TEMPORARY',   '', gtttKeyword );
  gtkwTinyint               := LexKeywordTokenDefs.AddToken( 'TINYINT',     '', gtttKeyword, gtlsDatatype );
  gtkwTime                  := LexKeywordTokenDefs.AddToken( 'TIME',        '', gtttKeyword, gtlsDatatype );
  gtkwTimeout               := LexKeywordTokenDefs.AddToken( 'TIMEOUT',     '', gtttKeyword );
  gtkwThen                  := LexKeywordTokenDefs.AddToken( 'THEN',        '', gtttKeyword );
  gtkwTo                    := LexKeywordTokenDefs.AddToken( 'TO',          '', gtttKeyword );
  gtkwTop                   := LexKeywordTokenDefs.AddToken( 'TOP',         '', gtttKeyword );
  gtkwTran                  := LexKeywordTokenDefs.AddToken( 'TRAN',        '', gtttKeyword );
  gtkwTrans                 := LexKeywordTokenDefs.AddToken( 'TRANS',       '', gtttKeyword );
  gtkwTransaction           := LexKeywordTokenDefs.AddToken( 'TRANSACTION', '', gtttKeyword );
  gtkwTrigger               := LexKeywordTokenDefs.AddToken( 'TRIGGER',     '', gtttKeyword );
  gtkwTriggers              := LexKeywordTokenDefs.AddToken( 'TRIGGERS',    '', gtttKeyword );
  gtkwTruncate              := LexKeywordTokenDefs.AddToken( 'TRUNCATE',    '', gtttKeyword );
  gtkwType                  := LexKeywordTokenDefs.AddToken( 'TYPE',        '', gtttKeyword );
  { ---- U ---- }
  gtkwUnion                 := LexKeywordTokenDefs.AddToken( 'UNION',       '', gtttKeyword, gtlsUnion );
  gtkwUnique                := LexKeywordTokenDefs.AddToken( 'UNIQUE',      '', gtttKeyword );
  gtkwUnsigned              := LexKeywordTokenDefs.AddToken( 'UNSIGNED',    '', gtttKeyword, gtlsDatatype );
  gtkwUnused                := LexKeywordTokenDefs.AddToken( 'UNUSED',      '', gtttKeyword );
  gtkwUntil                 := LexKeywordTokenDefs.AddToken( 'UNTIL',       '', gtttKeyword );
  gtkwUpdate                := LexKeywordTokenDefs.AddToken( 'UPDATE',      '', gtttKeyword );
  gtkwUse                   := LexKeywordTokenDefs.AddToken( 'USE',         '', gtttKeyword );
  gtkwUser                  := LexKeywordTokenDefs.AddToken( 'USER',        '', gtttKeyword );
  gtkwUsing                 := LexKeywordTokenDefs.AddToken( 'USING',       '', gtttKeyword );
  { ---- V ---- }
  gtkwValidate              := LexKeywordTokenDefs.AddToken( 'VALIDATE',    '', gtttKeyword );
  gtkwValues                := LexKeywordTokenDefs.AddToken( 'VALUES',      '', gtttKeyword );
  gtkwVarchar               := LexKeywordTokenDefs.AddToken( 'VARCHAR',     '', gtttKeyword, gtlsDatatype );
  gtkwVarchar2              := LexKeywordTokenDefs.AddToken( 'VARCHAR2',    '', gtttKeyword, gtlsDatatype );
  gtkwVersion               := LexKeywordTokenDefs.AddToken( 'VERSION',     '', gtttKeyword );
  gtkwView                  := LexKeywordTokenDefs.AddToken( 'VIEW',        '', gtttKeyword );
  { ---- W ---- }
  gtkwWait                  := LexKeywordTokenDefs.AddToken( 'WAIT',        '', gtttKeyword );
  gtkwWith                  := LexKeywordTokenDefs.AddToken( 'WITH',        '', gtttKeyword );
  gtkwWhen                  := LexKeywordTokenDefs.AddToken( 'WHEN',        '', gtttKeyword );
  gtkwWhere                 := LexKeywordTokenDefs.AddToken( 'WHERE',       '', gtttKeyword );
  gtkwWhile                 := LexKeywordTokenDefs.AddToken( 'WHILE',       '', gtttKeyword );
  gtkwWork                  := LexKeywordTokenDefs.AddToken( 'WORK',        '', gtttKeyword );
  { ---- X ---- }
  gtkwXor                   := LexKeywordTokenDefs.AddToken( 'XOR',         '', gtttKeyword );
  { ---- Y ---- }
  { ---- Z ---- }

{---------------------------- SQL Complex Keywords ----------------------------}

  { ---- A ---- }
  gtkwAdd_Check              := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtlsDdlCreate, gtkwAdd,      gtkwCheck );
  gtkwAdd_Column             := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtlsDdlCreate, gtkwAdd,      gtkwColumn );
  gtkwAdd_Constraint         := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtlsDdlCreate, gtkwAdd,      gtkwConstraint );
  gtkwAdd_Foreign_Key        := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtlsDdlCreate, gtkwAdd,      gtkwForeign,  gtkwKey);
  gtkwAdd_Primary_Key        := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtlsDdlCreate, gtkwAdd,      gtkwPrimary,  gtkwKey);
  gtkwAdd_Unique             := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtlsDdlCreate, gtkwAdd,      gtkwUnique );
  gtkwAlter_Column           := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtkwAlter,     gtkwColumn );
  gtkwAlter_Index            := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtlsDdlModify, gtkwAlter,    gtkwIndex);
  gtkwAlter_Table            := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtlsDdlModify, gtkwAlter,    gtkwTable );
  gtkwAlter_Trigger          := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtlsDdlModify, gtkwAlter,    gtkwTrigger );
  gtkwAnalyze_Index          := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtkwAnalyze,   gtkwIndex);
  { ---- B ---- }
  gtkwBegin_Tran             := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtlsTcl,       gtkwBegin,    gtkwTran);
  gtkwBegin_Transaction      := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtkwBegin,     gtkwTransaction );
  { ---- C ---- }
  gtkwCascade_Constraints    := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtkwCascade,   gtkwConstraints);
  gtkwCommit_Tran            := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtkwCommit,    gtkwTran );
  gtkwCommit_Transaction     := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtkwCommit,    gtkwTransaction );
  gtkwCommit_Work            := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtkwCommit,    gtkwWork );
  gtkwConnect_By             := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtkwConnect,   gtkwBy );
  gtkwCreate_Index           := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtlsDdlCreate, gtkwCreate,   gtkwIndex );
  gtkwCreate_Or_Replace      := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtkwCreate,    gtkwOr,       gtkwReplace );
  gtkwCreate_Or_Replace_View := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtlsDdlCreateView,gtkwCreate,   gtkwOr,       gtkwReplace, gtkwView );
  gtkwCreate_Login           := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtlsDcl,       gtkwCreate,   gtkwLogin );
  gtkwCreate_Sequence        := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtlsDdlCreate, gtkwCreate,   gtkwSequence );
  gtkwCreate_Synonym         := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtlsDdlCreate, gtkwCreate,   gtkwSynonym);
  gtkwCreate_Public_Synonym  := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtkwCreate,    gtkwPublic,   gtkwSynonym);
  gtkwCreate_Or_Replace_Synonym
                             := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtkwCreate,    gtkwOr,       gtkwReplace, gtkwSynonym);
  gtkwCreate_Or_Replace_Public_Synonym
                             := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtlsDdlCreate, gtkwCreate,    gtkwOr,        gtkwReplace,   gtkwPublic, gtkwSynonym);
  gtkwCreate_Table           := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtlsDdlCreate, gtkwCreate,    gtkwTable );
  gtkwCreate_Temporary_Table := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtlsDdlCreate, gtkwCreate,    gtkwTemporary, gtkwTable );
  gtkwCreate_Global_Temporary_Table
                             := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtlsDdlCreate, gtkwCreate,    gtkwGlobal,    gtkwTemporary, gtkwTable );
  gtkwCreate_Global          := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtlsDdlCreate, gtkwCreate,    gtkwGlobal );
  gtkwTemporary_Table        := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtlsDdlCreate, gtkwTemporary, gtkwTable );
  gtkwCreate_Unique_Index    := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtlsDdlCreate, gtkwCreate,    gtkwUnique,    gtkwIndex );
  gtkwCreate_User            := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtlsDcl,       gtkwCreate,    gtkwUser );
  gtkwCreate_View            := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtlsDdlCreateView,gtkwCreate,    gtkwView );
  gtkwCreate_Materialized_View:=LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtlsDdlCreateView,gtkwCreate,    gtkwMaterialized, gtkwView );
  gtkwCreate_Or_Replace_Materialized_View
                             := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtlsDdlCreateView,gtkwCreate,    gtkwOr, gtkwReplace, gtkwMaterialized, gtkwView );

  gtkwCross_Join             := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtkwCross,     gtkwJoin );
  { ---- D ---- }
  gtkwDeallocate_Unused      := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtkwDeallocate,gtkwUnused);
  gtkwDelete_From            := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtkwDelete,    gtkwFrom );
  gtkwDelete_Rows            := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtkwDelete,    gtkwRows );
  gtkwDrop_Table             := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtlsDdlDrop, gtkwDrop,      gtkwTable );
  gtkwDrop_Column            := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtlsDdlDrop,   gtkwDrop,     gtkwColumn );
  gtkwDrop_Constraint        := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtlsDdlDrop,   gtkwDrop,     gtkwConstraint );
  gtkwDrop_Index             := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtkwDrop,      gtkwIndex );
  gtkwDrop_Sequence          := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtlsDdlDrop,   gtkwDrop,     gtkwSequence );
  gtkwDrop_Synonym           := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtlsDdlDrop,   gtkwDrop,     gtkwSynonym );
  gtkwDrop_View              := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtlsDdlDrop,   gtkwDrop,     gtkwView );
  { ---- E ---- }
  gtkwEnable_Row_Movement    := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtkwEnable,    gtkwRow,      gtkwMovement);
  gtkwEnd_Tran               := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtlsTcl,       gtkwEnd,      gtkwTran);
  gtkwEnd_Transaction        := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtkwEnd,       gtkwTransaction );
  { ---- F ---- }
  gtkwForeign_Key            := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtkwForeign,   gtkwKey );
  gtkwFor_Update             := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtkwFor,       gtkwUpdate );
  gtkwFor_Update_Of          := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtkwFor,       gtkwUpdate,   gtkwOf );
  gtkwFull_Join              := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtkwFull,      gtkwJoin );
  gtkwFull_Outer_Join        := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtkwFull,      gtkwOuter,    gtkwJoin );
  { ---- G ---- }
  gtkwGroup_By               := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtkwGroup,     gtkwBy );
  { ---- H ---- }
  { ---- I ---- }
  gtkwIncrement_By           := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtkwIncrement, gtkwBy );
  gtkwInner_Join             := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtkwInner,     gtkwJoin );
  gtkwInsert_Into            := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtkwInsert,    gtkwInto );
  gtkwInsert_Or_Replace_Into := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtkwInsert,    gtkwOr,        gtkwReplace,  gtkwInto);
  gtkwIs_Not_Null            := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtlsNull,      gtkwIs,        gtkwNot,      gtkwNull);
  gtkwIs_Null                := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtlsNull,      gtkwIs,        gtkwNull );
  { ---- J ---- }
  { ---- K ---- }
  { ---- L ---- }
  gtkwLeft_Join              := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtkwLeft,      gtkwJoin );
  gtkwLeft_Outer_Join        := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtkwLeft,      gtkwOuter,    gtkwJoin );
  { ---- M ---- }
  gtkwModify_Column          := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtkwModify,    gtkwColumn );
  { ---- N ---- }
  gtkwNot_Between            := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtkwNot,       gtkwBetween );
  gtkwNot_Exists             := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtkwNot,       gtkwExists );
  gtkwNot_In                 := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtkwNot,       gtkwIn );
  gtkwNot_Like               := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtkwNot,       gtkwLike );
  gtkwNot_Null               := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtlsNull,      gtkwNot,       gtkwNull );
  gtkwNulls_First            := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtlsNull,      gtkwNulls,     gtkwFirst );
  gtkwNulls_Last             := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtlsNull,      gtkwNulls,     gtkwLast  );
  { ---- O ---- }
  gtkwOn_Commit              := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtkwOn,        gtkwCommit );
  gtkwOn_Commit_Preserve_Rows:= LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtkwOn,        gtkwCommit,    gtkwPreserve, gtkwRows );
  gtkwOn_Commit_Delete_Rows  := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtkwOn,        gtkwCommit,    gtkwDelete,   gtkwRows );
  gtkwOn_Demand              := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtkwOn,        gtkwDemand );
  gtkwOn_Delete              := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtkwOn,        gtkwDelete );
  gtkwOn_Delete_Cascade      := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtkwOn,        gtkwDelete,    gtkwCascade );
  gtkwOn_Delete_Restrict     := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtkwOn,        gtkwDelete,    gtkwRestrict );
  gtkwOn_Delete_Set_Null     := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtkwOn,        gtkwDelete,    gtkwSet,      gtkwNull );
  gtkwOn_Update              := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtkwOn,        gtkwUpdate );
  gtkwOn_Update_Cascade      := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtkwOn,        gtkwUpdate,    gtkwCascade );
  gtkwOn_Update_Restrict     := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtkwOn,        gtkwUpdate,    gtkwRestrict );
  gtkwOn_Update_Set_Null     := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtkwOn,        gtkwUpdate,    gtkwSet,      gtkwNull );

  gtkwOrder_By               := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtkwOrder,     gtkwBy );
  { ---- P ---- }
  gtkwPreserve_Rows          := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtkwPreserve,  gtkwRows );
  gtkwPrimary_Key            := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtkwPrimary,   gtkwKey );
  gtkwPurge_RecycleBin       := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtkwPurge,     gtkwRecycleBin );
  { ---- Q ---- }
  { ---- R ---- }
  gtkwRefresh_Force          := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtlsDdlModify, gtkwRefresh,  gtkwForce);
  gtkwRefresh_On_Demand      := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtlsDdlModify, gtkwRefresh,  gtkwOn,  gtkwDemand);
  gtkwRefresh_Force_On_Demand:= LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtlsDdlModify, gtkwRefresh,  gtkwForce, gtkwOn,  gtkwDemand);
  gtkwRename_Column          := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtlsDdlModify, gtkwRename,   gtkwColumn );
  gtkwRename_Table           := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtlsDdlModify, gtkwRename,    gtkwTable );
  gtkwRename_To              := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtlsDdlModify, gtkwRename,   gtkwTo );
  gtkwRight_Join             := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtkwRight,     gtkwJoin );
  gtkwRight_Outer_Join       := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtkwRight,     gtkwOuter,    gtkwJoin );
  gtkwRollback_Tran          := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtkwRollback,  gtkwTran );
  gtkwRollback_Transaction   := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtkwRollback,  gtkwTransaction );
  gtkwRollback_To_Savepoint  := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtkwRollback,  gtkwTo,       gtkwSavepoint );
  { ---- S ---- }
  gtkwSet_Null               := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtkwSet,       gtkwNull );
  gtkwSelect_Into            := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtkwSelect,    gtkwInto );
  gtkwShrink_Space           := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtkwShrink,    gtkwSpace);
  gtkwStart_With             := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtkwStart,     gtkwWith );
  gtkwStart_Transaction      := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtkwStart,     gtkwTransaction );
  gtkwStop_Transaction       := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtkwStop,      gtkwTransaction );
  { ---- T ---- }
  gtkwTo_Savepoint           := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtkwTo,        gtkwSavepoint);
  gtkwTruncate_Table         := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtkwTruncate,  gtkwTable);
  { ---- U ---- }
  gtkwUnion_All              := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtlsUnion,     gtkwUnion,     gtkwAll );
  { ---- V ---- }
  gtkwValidate_Structure     := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtkwValidate,  gtkwStructure);
  gtkwValues_LeftBracket     := LexKeywordTokenDefs.AddToken( '', '', gtttKeyword, gtkwValues,    gttkLeftBracket);
  { ---- W ---- }
  { ---- X ---- }
  { ---- Y ---- }
  { ---- Z ---- }
end.

