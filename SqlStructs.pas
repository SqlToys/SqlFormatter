(* $Header: /SQL Toys/SqlFormat/SqlStructs.pas 310   19-01-13 15:22 Tomek $
   (c) Tomasz Gierka, github.com/SqlToys, 2010.10.15                          *)
{--------------------------------------  --------------------------------------}
{$IFDEF RELEASE}
  {$DEBUGINFO OFF}
  {$LOCALSYMBOLS OFF}
{$ENDIF}
unit SqlStructs;

interface

uses Classes, GtContainers, GtTokenizers;

{------------------------------ SQL dialect type ------------------------------}

//type
//  TGtSqlDialect = (
//    gtdlNone,
//    gtdlMicrosoftSql,
//    gtdlOracle
//  );

{-------------------------------- Join Operator -------------------------------}

function  JoinOperatorToToken( jo: TGtLexToken{Def}; InnerPreff: Boolean = False; OuterPreff: Boolean = False ): TGtLexToken{Def};

{-------------------------------- Options Types -------------------------------}

type
  TGtSqlBracketOption     = ( gtopInsideBrackets,  gtopNotInsideBrackets     );
  TGtSqlNegationOption    = ( gtopNegation,        gtopNotNegation           );
  TGtSqlNullableOption    = ( gtopNullNotSpecified,gtopNull, gtopNotNull     );

{---------------------------------- SQL Item ----------------------------------}

  TGtSqlNodeKind = ( gtsiNone,
                     gtsiExpr,          gtsiExprList,      gtsiExprTree,
                     gtsiSetExpr,       gtsiSetExprList,
                     gtsiCond,          gtsiCondTree,
                     gtsiConstraint,    gtsiUnions,
                     gtsiDml,           gtsiDdl,
                     gtsiDcl,           gtsiTcl,
                     gtsiClauseTables,  gtsiClauseAlter,
                     gtsiProgram,
                     gtsiOther,
                     // nowe, do pouk³adania
                     gtsiTableRef,

                     gtssWhenThenCondExpr,   gtssClauseFields,
                     gtssOtherColumnDef,
                     gtssGrantName,          gtssGrantObjectName,     gtssGrantUserName, // 2013-05-29
                     gtssOtherKeyword,
                     gtssOtherNotRecognized,

                     gtsiQueryList { na razie jako lista kwerend parsera }
                   );

  function GtSqlNodeKindToName( aKind: TGtSqlNodeKind ): string;
  function GtSqlNodeNameToKind( aName: String ): TGtSqlNodeKind;

const
  cSqlNodeTokenMax = 16;

type
  TGtSqlNode = class;
  TSqlNodeProcedure = procedure (aNode: TGtSqlNode);

//type
  TGtSqlNode = class (TGtLexNode)
  private
    FKind: TGtSqlNodeKind;
  //FValues: TStringList;
    FTokens: array [1..cSqlNodeTokenMax] of TGtLexToken{Def};

    function        GetOwner_SqlNode: TGtSqlNode; //virtual;
    procedure       SetOwner_SqlNode(aOwner: TGtSqlNode); //virtual;

    procedure       SetSqlItemKind   (aKind: TGtSqlNodeKind);
    function        FindByKind(aKind: TGtSqlNodeKind; aNode: TGtSqlNode=nil): TGtSqlNode;

    procedure       SetFTokens(aIndex: Integer; aToken: TGtLexToken{Def}); //virtual;

    function        OwnerOfAnotherKind(aKind: TGtSqlNodeKind): TGtSqlNode;

  //function        GetValStr (aIndex: Integer): String;
  //procedure       SetValStr (aIndex: Integer; aValue: String);
  //function        GetValBool(aIndex: Integer): Boolean;
  //procedure       SetValBool(aIndex: Integer; aValue: Boolean);
  //function        GetValInt (aIndex: Integer): Integer;
  //procedure       SetValInt (aIndex: Integer; aValue: Integer);

  //procedure       RemoveVal (aIndex: Integer);
  public
  //function        GetValName(aIndex: Integer): String;
  //function        GetNameIndex(aName: String): Integer;
  //procedure       SetValueByName(aName, aValue: String);
  public // general SQL Item methods
    constructor     Create(aOwner: TGtItem); override;
    constructor     Create(aOwner: TGtItem; aKind: TGtSqlNodeKind; aName: String=''); overload; virtual;
  //destructor      Destroy; override;

    function        NewNode(aKind: TGtSqlNodeKind; aKeyword: TGtLexToken{Def}=nil; aName: String=''): TGtSqlNode; virtual;

    function        GetNode(aIndex: Integer): TGtSqlNode;

    function        Check    (aKind: TGtSqlNodeKind=gtsiNone; aKeyword: TGtLexToken{Def}=nil; aName: String=''): Boolean;
    function        Find     (aKind: TGtSqlNodeKind=gtsiNone; aKeyword: TGtLexToken{Def}=nil; aName: String=''; aNode: TGtSqlNode=nil): TGtSqlNode;
    function        FindOwner(aKind: TGtSqlNodeKind=gtsiNone; aKeyword: TGtLexToken{Def}=nil; aName: String=''): TGtSqlNode;

    property        Owner: TGtSqlNode read GetOwner_SqlNode write SetOwner_SqlNode;

    property        Kind: TGtSqlNodeKind read FKind write SetSqlItemKind;
    property        Nodes[Index: Integer]: TGtSqlNode read GetNode; default;

  //property        Values: TStringList read FValues;
  public // function specific methods
//  Nullable: TGtSqlNullableOption;

//  property        LogicOp   : TGtLexToken{Def} index 1 read FTokens[1] write SetFTokens;
//  property        ExprOp    : TGtLexToken{Def} index 2 read FTokens[2] write SetFTokens;
//  property        CompOp    : TGtLexToken{Def} index 3 read FTokens[3] write SetFTokens;
//  property        JoinOp    : TGtLexToken{Def} index 4 read FTokens[4] write SetFTokens;

//    property        DataType  : TGtLexToken{Def} index 5 read FTokens[5] write SetFTokens;
//    property        OnDelete  : TGtLexToken{Def} index 6 read FTokens[6] write SetFTokens;
//    property        OnUpdate  : TGtLexToken{Def} index 7 read FTokens[7] write SetFTokens;
//    property        SortOrder : TGtLexToken{Def} index 8 read FTokens[8] write SetFTokens;
    property        Keyword   : TGtLexToken{Def} index 9 read FTokens[9] write SetFTokens;
//    property        Operand   : TGtLexToken{Def} index 10 read FTokens[10] write SetFTokens;

    property        KeywordExt:    TGtLexToken{Def} index 11 read FTokens[11] write SetFTokens;
//protected
    property        KeywordAux1:   TGtLexToken      index 12 read FTokens[12] write SetFTokens;
    property        KeywordAux2:   TGtLexToken      index 13 read FTokens[13] write SetFTokens;
    property        KeywordAux3:   TGtLexToken      index 14 read FTokens[14] write SetFTokens;
    property        KeywordAux4:   TGtLexToken      index 15 read FTokens[15] write SetFTokens;
    property        KeywordAux5:   TGtLexToken      index 16 read FTokens[16] write SetFTokens;
//public
//  property        KeywordAfter1: TGtLexToken{Def} index 12 read FTokens[12] write SetFTokens;
//  property        KeywordAfter2: TGtLexToken{Def} index 13 read FTokens[13] write SetFTokens;
//  property        KeywordAfter3: TGtLexToken{Def} index 14 read FTokens[14] write SetFTokens;
//  property        KeywordBefore: TGtLexToken{Def} index 15 read FTokens[15] write SetFTokens;

//  property        OldName              : String index 101 read GetValStr write SetValStr;
//  property        NewName              : String index 102 read GetValStr write SetValStr;
//  property        AliasName            : String index 103 read GetValStr write SetValStr;
//  property        CollateName          : String index 104 read GetValStr write SetValStr;
//  property        CondEscape           : String index 105 read GetValStr write SetValStr;
//  property        ObjectName           : String index 106 read GetValStr write SetValStr;
//  property        TableName            : String index 107 read GetValStr write SetValStr;
//  property        ColumnName           : String index 108 read GetValStr write SetValStr;
//  property        KeepName             : String index 109 read GetValStr write SetValStr;

//  property        Name1                : String index 110 read GetValStr write SetValStr;
//  property        Name2                : String index 111 read GetValStr write SetValStr;
//  property        Name3                : String index 112 read GetValStr write SetValStr;

//    property        Negation             : Boolean index 201 read GetValBool write SetValBool;
//    property        Unique               : Boolean index 202 read GetValBool write SetValBool;
//    property{Create}OrReplace            : Boolean index 203 read GetValBool write SetValBool;
//    property        Public               : Boolean index 204 read GetValBool write SetValBool;
//    property        Global               : Boolean index 205 read GetValBool write SetValBool;
//    property        Temporary            : Boolean index 206 read GetValBool write SetValBool;
//    property        Identity             : Boolean index 207 read GetValBool write SetValBool;
//  property        Distinct             : Boolean index 208 read GetValBool write SetValBool;
//    property        NoWait               : Boolean index 209 read GetValBool write SetValBool;
//  property        Materialized         : Boolean index 210 read GetValBool write SetValBool;
//    property        NoCycle              : Boolean index 211 read GetValBool write SetValBool;
//  //property        OuterMark1           : Boolean index 212 read GetValBool write SetValBool;
//  //property        OuterMark2           : Boolean index 213 read GetValBool write SetValBool;
//  property        ExprMinus            : Boolean index 214 read GetValBool write SetValBool;
//  property        ExprReverseOp        : Boolean index 215 read GetValBool write SetValBool;
//  property        ExprReverseOp2       : Boolean index 216 read GetValBool write SetValBool;
//    property        ExprPrior            : Boolean index 217 read GetValBool write SetValBool;
//    property        Enable               : Boolean index 218 read GetValBool write SetValBool;
//    property        Disable              : Boolean index 219 read GetValBool write SetValBool;
//    property        Cascade              : Boolean index 220 read GetValBool write SetValBool;
//    property        NullsFirst           : Boolean index 221 read GetValBool write SetValBool;
//    property        NullsLast            : Boolean index 222 read GetValBool write SetValBool;
//    property        Purge                : Boolean index 223 read GetValBool write SetValBool;
//    property        OnDemand             : Boolean index 224 read GetValBool write SetValBool;
//    property        Force                : Boolean index 225 read GetValBool write SetValBool;
//    property        OnCommitPreserveRows : Boolean index 226 read GetValBool write SetValBool;
//    property        OnCommitDeleteRows   : Boolean index 227 read GetValBool write SetValBool;
//    property        Semicolon            : Boolean index 228 read GetValBool write SetValBool;
 //   property        OuterMark1Oracle     : Boolean index 229 read GetValBool write SetValBool;
//    property        OuterMark1MSSQL      : Boolean index 230 read GetValBool write SetValBool;
//    property        OuterMark2Oracle     : Boolean index 231 read GetValBool write SetValBool;
//    property        OuterMark2MSSQL      : Boolean index 232 read GetValBool write SetValBool;
//    property        AliasAsToken         : Boolean index 233 read GetValBool write SetValBool;

//  property        JoinOuterKeyword     : Boolean index 234 read GetValBool write SetValBool;
//  property        JoinInnerKeyword     : Boolean index 235 read GetValBool write SetValBool;

//  property        NewLineBefore        : Boolean index 236 read GetValBool write SetValBool;
//  property        NewLineAfter         : Boolean index 237 read GetValBool write SetValBool;
//  property        EmptyLineBefore      : Boolean index 238 read GetValBool write SetValBool;
//  property        EmptyLineAfter       : Boolean index 239 read GetValBool write SetValBool;

//  property        ColSize              : Integer index 301 read GetValInt  write SetValInt;
//  property        ColIdentitySeed      : Integer index 302 read GetValInt  write SetValInt;
//  property        ColIdentityInc       : Integer index 303 read GetValInt  write SetValInt;
//  property        Top                  : Integer index 304 read GetValInt  write SetValInt;
//  property        ColPrec              : Integer index 305 read GetValInt  write SetValInt;
//  property        BracketsCount        : Integer index 306 read GetValInt  write SetValInt;

//  property        Int1                 : Integer index 310 read GetValInt  write SetValInt;
//  property        Int2                 : Integer index 311 read GetValInt  write SetValInt;
  public // service methods
    function        GetQuery: TGtSqlNode;
    function        SingleColumnConstraint: Boolean;
    function        IsSubQuery: Boolean;
    function        IsShortQuery: Boolean;
    function        IsClauseKeyword: Boolean;
    function        GetExtQuery: TGtSqlNode;
    function        TablesCount: Integer;

    function        ExprTreeOwner: TGtSqlNode;
    function        ConditionsCount: Integer;

    procedure       KeywordAuxAdd     (aKeywordAux: TGtLexToken);
    procedure       KeywordAuxRemove  (aKeywordAux: TGtLexToken);
    function        KeywordAuxCheck   (aKeywordAux1: TGtLexToken;
                                       aKeywordAux2: TGtLexToken = nil;
                                       aKeywordAux3: TGtLexToken = nil;
                                       aKeywordAux4: TGtLexToken = nil;
                                       aKeywordAux5: TGtLexToken = nil): Boolean;
    function        KeywordAuxCheckKwd(aKeywordAux1: TGtLexToken;
                                       aKeywordAux2: TGtLexToken = nil;
                                       aKeywordAux3: TGtLexToken = nil;
                                       aKeywordAux4: TGtLexToken = nil;
                                       aKeywordAux5: TGtLexToken = nil): TGtLexToken;
  public
    function        OwnerTableNameOrAlias: String;

    function        ExprHasReferenceTo(aColPrefix: String): Boolean;
    function        CondHasReferenceTo(aColPrefix: String): Boolean;
    procedure       OnCondMoveRefsFirst(aColPrefix: String);

    procedure       CalcConditionArgsLen(var ML_LeftOnExprPrefix, ML_LeftOnExprColumn,
                                             ML_RightOnExprPrefix, ML_RightOnExprColumn: Integer);
    procedure       CalcClauseLines(aKind: TGtSqlNodeKind; aKeyword: TGtLexToken{Def};
                                    aLinesLimit: Integer; aSeparateLines: Boolean;
                                    var aLongQuery: Boolean; var aQueryLines: Integer); overload;

    procedure       ForEach ( aProc: TSqlNodeProcedure;       aDeep: Boolean = False;
                              aKind: TGtSqlNodeKind=gtsiNone; aKeyword: TGtLexToken{Def}=nil; aName: String='' ); overload;
  end;

var GtSqlNodeCount: Integer = 0;

{------------------------------ Resource strings ------------------------------}
resourcestring
  (* NOT EXISTS *)
  gtstrNotExistsDatatype           = 'Datatype %s does not exists.';

const
  gtsqlSizeOrPrecNotSpecified = -999999;
  gtsqlSizeOrPrecAny          = -1;

implementation

uses SysUtils, GtStandard, SqlCommon;

{------------------------------ Compare Operator ------------------------------}

{ negates compare operator }
//function  CompareOperatorNegation( co: TGtSqlCompareOperator; NotEqualPreff: Boolean = True ): TGtSqlCompareOperator;
//begin
//  case co of
//    gtcoEqual      : if NotEqualPreff
//                       then Result := gtcoNotEqual
//                       else Result := gtcoDifferent;
//    gtcoLess       : Result := gtcoGreatEq;
//    gtcoGreat      : Result := gtcoLessEq;
//    gtcoLessEq     : Result := gtcoGreat;
//    gtcoGreatEq    : Result := gtcoLess;
//    gtcoNotEqual   : Result := gtcoEqual;
//    gtcoDifferent  : Result := gtcoEqual;
//    gtcoBetween    : Result := gtcoNotBetween;
//    gtcoNotBetween : Result := gtcoBetween;
//    gtcoIn         : Result := gtcoNotIn;
//    gtcoNotIn      : Result := gtcoIn;
//    gtcoLike       : Result := gtcoNotLike;
//    gtcoNotLike    : Result := gtcoLike;
//    gtcoIsNull     : Result := gtcoIsNotNull;
//    gtcoIsNotNull  : Result := gtcoIsNull;
//    gtcoExists     : Result := gtcoNotExists;
//    gtcoNotExists  : Result := gtcoExists;
//  else
//                     Result := gtcoNone;
//  end;
//end;

{-------------------------------- Join Operator -------------------------------}

{ converts JoinOperator to its name string }
function JoinOperatorToToken;
begin
  if jo = gtkwInto     then Result := gtkwInsert_Into else
  if jo = gtkwUpdate   then Result := gtkwUpdate else
  if jo = gtkwFrom     then Result := gtkwFrom else

  if jo = gtkwInner    then begin
                            if InnerPreff
                              then Result := gtkwInner_Join
                              else Result := gtkwJoin;
                          end else
  if jo = gtkwLeft     then begin
                            if OuterPreff
                              then Result := gtkwLeft_Outer_Join
                              else Result := gtkwLeft_Join;
                          end else
  if jo = gtkwRight    then begin
                            if OuterPreff
                              then Result := gtkwRight_Outer_Join
                              else Result := gtkwRight_Join;
                          end else
  if jo = gtkwFull     then begin
                            if OuterPreff
                              then Result := gtkwFull_Outer_Join
                              else Result := gtkwFull_Join;
                          end else

  if jo = gtkwCross    then Result := gtkwCross_Join else
  if jo = gttkComma       then Result := gttkComma
                          else Result := gttkNone;
end;

{---------------------------------- SQL Item ----------------------------------}

{ TGtSqlNodeKind to name }
function GtSqlNodeKindToName( aKind: TGtSqlNodeKind ): string;
begin
  case aKind of
    gtsiNone                           : Result := 'None';
    gtsiExpr                           : Result := 'Expr';
    gtsiExprList                       : Result := 'ExprList';
    gtsiExprTree                       : Result := 'ExprTree';
    gtsiSetExpr                        : Result := 'SetExpr';
    gtsiSetExprList                    : Result := 'SetExprList';
    gtsiCond                           : Result := 'Cond';
    gtsiCondTree                       : Result := 'CondTree';
    gtsiConstraint                     : Result := 'Constraint';
    gtsiUnions                         : Result := 'Unions';
    gtsiDml                            : Result := 'Dml';
    gtsiDdl                            : Result := 'Ddl';
    gtsiDcl                            : Result := 'Dcl';
    gtsiTcl                            : Result := 'Tcl';
    gtsiClauseTables                   : Result := 'Tables';
    gtsiClauseAlter                    : Result := 'ClauseAlter';
    gtsiProgram                        : Result := 'Program';
    gtsiOther                          : Result := 'Other';
    gtsiTableRef                       : Result := 'Table';
    gtssWhenThenCondExpr               : Result := 'WhenThenCondExpr';
    gtssClauseFields                   : Result := 'ClauseFields';
    gtssOtherColumnDef                 : Result := 'OtherColumnDef';
    gtssGrantName                      : Result := 'GrantName';
    gtssGrantObjectName                : Result := 'GrantObjectName';
    gtssGrantUserName                  : Result := 'GrantUserName';
    gtssOtherKeyword                   : Result := 'OtherKeyword';
    gtssOtherNotRecognized             : Result := 'OtherNotRecognized';
    gtsiQueryList                      : Result := 'QueryList';
  else                                   Result := 'unnamed';
  end;
end;

{ TGtSqlNode name to kind }
function GtSqlNodeNameToKind( aName: String ): TGtSqlNodeKind;
var lKind: TGtSqlNodeKind;
begin
  Result := gtsiNone;
  try
    lKind := gtsiNone;
    while aName <> GtSqlNodeKindToName( lKind ) do lKind := Succ(lKind);
    if aName = GtSqlNodeKindToName( lKind ) then Result := lKind;
  except
    Result := gtsiNone;
  end;
end;

{---------------------------------- SQL Item ----------------------------------}

{ class constructor }
constructor     TGtSqlNode.Create(aOwner: TGtItem);
var i: Integer;
begin
  inherited Create(aOwner);

//FValues := nil;

  Inc(GtSqlNodeCount);

  SetSqlItemKind( gtsiNone );

//Nullable := gtopNullNotSpecified; //True; // TODO: database default value ??

  for i := 1 to cSqlNodeTokenMax do FTokens[i] := gttkNone;
end;

{ class constructor }
constructor TGtSqlNode.Create(aOwner: TGtItem; aKind: TGtSqlNodeKind; aName: String='');
begin
  inherited Create(aOwner, aName);

  SetSqlItemKind( aKind );
end;

{ class destructor }
//destructor TGtSqlNode.Destroy;
//begin
//  if Assigned(FValues) then FreeAndNil(FValues);
//end;

{ creates new node }
function TGtSqlNode.NewNode;
begin
  Result := TGtSqlNode.Create(Self, aName);

  Result.SetSqlItemKind( aKind );
  Result.Keyword := aKeyword;
end;

{ gets owner of sql item }
function  TGtSqlNode.GetOwner_SqlNode: TGtSqlNode;
begin
  if inherited Owner is TGtSqlNode
    then Result := inherited Owner as TGtSqlNode
    else Result := nil;
end;

{ sets new owner for sql item }
procedure TGtSqlNode.SetOwner_SqlNode(aOwner: TGtSqlNode);
begin
  SetOwner(aOwner);
end;

{ gets property value }
//function  TGtSqlNode.GetValStr(aIndex: Integer): String;
//begin
//  Result := '';
//  if Assigned(FValues) then Result := FValues.Values[ IntToStr(aIndex) ];
////if Assigned(FValues) then Result := FValues.Values[ GetValName(aIndex) ];
//
//  if aIndex > 500 then Result := Owner.GetValStr(aIndex);
//end;

{ sets property value }
//procedure TGtSqlNode.SetValStr(aIndex: Integer; aValue: String);
//begin
//  if aValue = '' then begin
//    if not Assigned(FValues) then Exit;
////  if FValues.IndexOfName( IntToStr(aIndex) ) = -1 then Exit;
////  if FValues.IndexOfName( GetValName(aIndex) ) = -1 then Exit;
//  end;
//
//  if not Assigned(FValues) then FValues := TStringList.Create(True);
//
//  FValues.Values[ IntToStr(aIndex) ] := aValue;
////FValues.Values[ GetValName(aIndex) ] := aValue;
//end;

{ gets boolean property value }
//function  TGtSqlNode.GetValBool (aIndex: Integer): Boolean;
//begin
//  Result := GetValStr(aIndex) = '1';
//end;

{ sets boolean property value }
//procedure TGtSqlNode.SetValBool (aIndex: Integer; aValue: Boolean);
//begin
////  if aValue = False then begin
////    if not Assigned(FValues) then Exit;
////    if FValues.IndexOfName( GetValName(aIndex) ) = -1 then Exit;
////  end;
//
//  if aValue then SetValStr(aIndex, '1') else SetValStr(aIndex, '');
//end;

{ gets int property value }
//function  TGtSqlNode.GetValInt (aIndex: Integer): Integer;
//begin
//  try
//    Result := StrToInt( GetValStr(aIndex) );
//  except
//    Result := 0;
//  end;
//end;

{ sets int property value }
//procedure TGtSqlNode.SetValInt (aIndex: Integer; aValue: Integer);
//begin
////  if aValue = 0 then begin
////    if not Assigned(FValues) then Exit;
////    if FValues.IndexOfName( GetValName(aIndex) ) = -1 then Exit;
////  end;
//
//  if aValue = 0 then SetValStr(aIndex, '') else SetValStr(aIndex, IntToStr(aValue));
//end;

{ removes value from list }
//procedure TGtSqlNode.RemoveVal (aIndex: Integer);
//var i: Integer;
//begin
//  if not Assigned(FValues) then Exit;
//
//  try
//    i := FValues.IndexOfName( GetValName(aIndex) );
//    if i >= 0 then FValues.Delete( i );
//  finally
//    //
//  end;
//end;

{ returns property name }
//function TGtSqlNode.GetValName(aIndex: Integer): String;
//begin
//  case aIndex of
////    1: Result := 'LogicOp';
////    2: Result := 'ExprOp';
////    3: Result := 'CompOp';
////    4: Result := 'JoinOp';
////    5: Result := 'DataType';
////    6: Result := 'OnDelete';
////    7: Result := 'OnUpdate';
////    8: Result := 'SortOrder';
//    9: Result := 'Keyword';
//   11: Result := 'KeywordExt';
//   12: Result := 'KeywordAux1';
//   13: Result := 'KeywordAux2';
//   14: Result := 'KeywordAux3';
//   15: Result := 'KeywordAux4';
//   16: Result := 'KeywordAux5';
//
////    101: Result := 'OldName';
////    102: Result := 'NewName';
////    103: Result := 'AliasName';
////    104: Result := 'CollateName';
////    105: Result := 'CondEscape';
////    106: Result := 'ObjectName';
////    107: Result := 'TableName';
////    108: Result := 'ColumnName';
////    109: Result := 'KeepName';
//
//    110: Result := 'Name1';
//    111: Result := 'Name2';
//    112: Result := 'Name3';
//
////    201: Result := 'Negation';
////    202: Result := 'Unique';
////    203: Result := 'OrReplace';
////    204: Result := 'Public';
////    205: Result := 'Global';
////    206: Result := 'Temporary';
////    207: Result := 'Identity';
////    208: Result := 'Distinct';
////    209: Result := 'NoWait';
////    210: Result := 'Materialized';
////    211: Result := 'NoCycle';
//////  212: Result := 'OuterMark1';
//////  213: Result := 'OuterMark2';
////    214: Result := 'ExprMinus';
////    215: Result := 'ExprReverseOp';
////    216: Result := 'ExprReverseOp2';
////    217: Result := 'ExprPrior';
////    218: Result := 'Enable';
////    219: Result := 'Disable';
////    220: Result := 'Cascade';
////    221: Result := 'NullsFirst';
////    222: Result := 'NullsLast';
////    223: Result := 'Purge';
////    224: Result := 'OnDemand';
////    225: Result := 'Force';
////    226: Result := 'OnCommitPreserveRows';
////    227: Result := 'OnCommitDeleteRows';
////    228: Result := 'Semicolon';
////    229: Result := 'OuterMark1Oracle';
////    230: Result := 'OuterMark1MSSQL';
////    231: Result := 'OuterMark2Oracle';
////    232: Result := 'OuterMark2MSSQL';
////    233: Result := 'AliasAsToken';
////    234: Result := 'JoinOuterKeyword';
////    235: Result := 'JoinInnerKeyword';
//
//    236: Result := 'NewLineBefore';
////    237: Result := 'NewLineAfter';
//    238: Result := 'EmptyLineBefore';
//    239: Result := 'EmptyLineAfter';
//
////    301: Result := 'ColSize';
////    302: Result := 'ColIdentitySeed';
////    303: Result := 'ColIdentityInc';
////    304: Result := 'Top';
////    305: Result := 'ColPrec';
//    306: Result := 'BracketsCount';
//    310: Result := 'Int1';
//    311: Result := 'Int2';
//  end;
//end;

{ returns value name as its index }
//function TGtSqlNode.GetNameIndex(aName: String): Integer;
//var i: Integer;
//begin
//  i:= 1;
//  while (GetValName(i) <> '') and (GetValName(i) <> aName) do Inc(i);
//  if aName = GetValName(i) then begin
//    Result := i;
//    Exit;
//  end;
//
//  i:= 101;
//  while (GetValName(i) <> '') and (GetValName(i) <> aName) do Inc(i);
//  if aName = GetValName(i) then begin
//    Result := i;
//    Exit;
//  end;
//
//  i:= 201;
//  while (GetValName(i) <> '') and (GetValName(i) <> aName) do Inc(i);
//  if aName = GetValName(i) then begin
//    Result := i;
//    Exit;
//  end;
//
//  i:= 301;
//  while (GetValName(i) <> '') and (GetValName(i) <> aName) do Inc(i);
//  if aName = GetValName(i) then begin
//    Result := i;
//    Exit;
//  end;
//
//  Result := 0;
//end;

{ sets node value }
//procedure TGtSqlNode.SetValueByName(aName, aValue: String);
//begin
//  SetValStr( GetNameIndex( aName ), aValue );
//end;

{ sets item kind }
procedure TGtSqlNode.SetSqlItemKind;
begin
  FKind := aKind;
end;

{ gets item }
function TGtSqlNode.GetNode;
var lItem: TGtItem;
begin
  lItem := inherited GetItem(aIndex);
  if lItem is TGtSqlNode
    then Result := lItem as TGtSqlNode
    else Result := nil;
end;

{ finds item of a given kind }
function TGtSqlNode.FindByKind;
var i: Integer;
begin
  Result := nil;

  i := 0;
  if Assigned(aNode) then i := GetIndex(aNode) +1;

  while not Assigned(Result) and (i >= 0) and (i < Count) do
    if (aKind = gtsiNone) or (GetItem(i) is TGtSqlNode) and (GetNode(i).Kind = aKind)
      then Result := GetNode(i)
      else Inc(i);
end;

{ checks node against given values }
function TGtSqlNode.Check;
begin
  Result := ((Self.Kind = aKind) or (aKind = gtsiNone))
        and ((Self.Keyword = aKeyword) or not assigned(aKeyword))
        and ((Self.Name = aName) or (aName = ''));
end;

{ finds item of a given kind, keyword and name }
function TGtSqlNode.Find;
begin
  Result := nil;
  repeat
    aNode := FindByKind(aKind, aNode);
    if Assigned(aNode) and aNode.Check(aKind, aKeyword, aName) then Result := aNode;
  until Assigned(Result) or not Assigned(aNode);
end;

{ find owner of a given kind, keyword and name }
function TGtSqlNode.FindOwner;
var lOwner: TGtSqlNode;
begin
  Result := nil;
  lOwner := Self.Owner;
  while not Assigned(Result) and Assigned(lOwner) and (lOwner is TGtSqlNode) do
    if lOwner.Check(aKind, aKeyword, aName)
      then Result := lOwner as TGtSqlNode
      else lOwner := lOwner.Owner;
end;

{ returns parent query for this item }
function TGtSqlNode.GetQuery;
begin
  Result := nil;
  if Self.Kind in [gtsiDml, gtsiDdl, gtsiTcl, gtsiDcl] then Result := Self else
  { ON condition should return external query, because its owner is the subquery }
  if Assigned(Owner) then begin
    if Check(gtsiCondTree, gtkwOn) and Owner.Check(gtsiDml) then Result := Owner.GetExtQuery else
    if (Owner.Kind in [gtsiDml, gtsiDdl, gtsiTcl, gtsiDcl]) then Result := Owner else
    Result := Owner.GetQuery;
  end;
  if not Assigned(Result) then Exit;

  if not (Result.Kind in [gtsiDml, gtsiDdl, gtsiTcl, gtsiDcl]) then Result := nil;
end;

{ true if constraint related to only one column }
function TGtSqlNode.SingleColumnConstraint: Boolean;
var i, cnt: Integer;
begin
  cnt := 0;
  for i := 0 to Count - 1 do
    if Nodes[i].Check(gtsiExpr, gttkColumnName) then Inc(cnt);

  Result := cnt = 1;
end;

{ returns true if this query is a subquery }
{ INSERT from SELECT - SELECT nie jest subquery dla INSERT }
function TGtSqlNode.IsSubQuery: Boolean;
var lQuery, lOwner: TGtSqlNode;
begin
  Result := False;
  if not Check(gtsiDml, gtkwSelect) then Exit;

  lOwner := Owner;
  lQuery := GetExtQuery;

  { dla UNION/MINUS/EXCEPT GetExtQuery nie jest kwerenda nadrzedna }
  while (lOwner is TGtSqlNode) and (lOwner.Kind = gtsiUnions) do begin
    lOwner := lQuery.Owner;
    lQuery := lQuery.GetExtQuery;
  end;

  Result := Assigned( lQuery ) and
            not( (Owner is TGtSqlNode) and (Owner.FKind = gtsiUnions) and (lQuery.Owner.Name = 'Parsed Queries List')) and
            not( lQuery.Check(gtsiDml, gtkwInsert) and (Owner = lQuery) ) and
            not((lQuery.Check(gtsiDDL) and (lQuery.Check(gtsiDDL, gtkwCreate_View))));
end;

{ returns true if this query is a short query }
function TGtSqlNode.IsShortQuery: Boolean;
begin
  Result := False; // True; // TEST ONLY !!!

  if not Check(gtsiDml) then begin
    if Assigned(Owner) then Result := Owner.IsShortQuery;
    Exit;
  end;

//    lQueryLines := 0;

//  if Check(gtsiDml, gtkwSelect) then CalcClauseLines(gtsiExprList, gtkwSelect,     10, Options[ gtstOneExprOnLine ], lLongQuery, lQueryLines);
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
end;

{ returns true if node keyword is an clause keyword }
function TGtSqlNode.IsClauseKeyword: Boolean;
begin
  Result := (Keyword = gtkwSelect) or (Keyword = gtkwFrom) or (Keyword = gtkwWhere) or
            (Keyword = gtkwGroup_By) or (Keyword = gtkwHaving) or (Keyword = gtkwOrder_By) or
            (Keyword = gtkwConnect_By) or (Keyword = gtkwStart_With) or
            (Keyword = gtkwSet) or (Keyword = gtkwValues) ;
end;

{ returns external query for this query, if it is a subquery }
function TGtSqlNode.GetExtQuery;
begin
  Result := nil;
  if not Check(gtsiDml, gtkwSelect) then Exit;

  if Owner is TGtSqlNode then Result := Owner.GetQuery;
end;

{ returns number of tables used by query in UPDATE, INSERT, FROM clauses }
function TGtSqlNode.TablesCount: Integer;
var i: Integer;
begin
  Result := 0;

  for i := 0 to Count - 1 do
    if Nodes[i].Kind = gtsiClauseTables
      then Inc(Result, Nodes[i].Count);
end;

{ returns top level owner of an expr tree }
function TGtSqlNode.ExprTreeOwner: TGtSqlNode;
begin
  Result := nil;
  if Self.Kind <> gtsiExprTree then Exit;

  Result := OwnerOfAnotherKind(gtsiExprTree);
end;

{ drills down cond tree to count single conditions }
function TGtSqlNode.ConditionsCount: Integer;
var i: Integer;
begin
  Result := 0;

  case Self.Kind of
    gtsiCond     : Result := 1;
    gtsiCondTree : for i := 0 to Self.Count - 1 do
                     Inc(Result, Self[i].ConditionsCount);
  end;
end;

{ drills up to find owner with another Kind than given }
function TGtSqlNode.OwnerOfAnotherKind(aKind: TGtSqlNodeKind): TGtSqlNode;
var lOwner: TGtSqlNode;
begin
  Result := nil;
  lOwner := Self.Owner;
  while not Assigned(Result) and Assigned(lOwner) and (lOwner is TGtSqlNode) do
    if lOwner.Kind = aKind
      then lOwner := lOwner.Owner
      else Result := lOwner;
end;

{}
function TGtSqlNode.OwnerTableNameOrAlias: String;
var lOwner, lAlias: TGtSqlNode;
begin
  Result := '';
  lOwner := FindOwner(gtsiTableRef);
  if not Assigned(lOwner) then lOwner := FindOwner(gtsiDml, gtkwSelect);
  if not Assigned(lOwner) then Exit;

//if lOwner.AliasName <> ''
//  then Result := lOwner.AliasName
//  if lOwner.Name1 <> ''
//    then Result := lOwner.Name1
//    else Result := lOwner.Name;
  lAlias := lOwner.Find(gtsiNone, gtkwAs);
  if Assigned(lAlias) then Result := lAlias.Name else Result := lOwner.Name;
end;

{ sets FTokens property }
procedure TGtSqlNode.SetFTokens(aIndex: Integer; aToken: TGtLexToken{Def});
begin
//  if (aToken = gttkNone) or
//     (aIndex = 1) and ((aToken = gtkwOr) or (aToken = gtkwAnd) or (aToken = gtkwNot)) or
//     (aIndex = 2) and ((aToken = gttkPlus) or (aToken = gttkMinus) or (aToken = gttkConcatenation) or
//                       (aToken = gttkStar) or (aToken = gttkSlash) or (aToken = gttkPercent)) or
//     (aIndex = 3) and ((aToken = gttkEqual) or (aToken = gttkLessThan) or (aToken = gttkGreaterThan) or
//                       (aToken = gttkLessEqual) or (aToken = gttkGreaterEqual) or
//                       (aToken = gttkNotEqual) or (aToken = gttkDiffer) or
//                       (aToken = gtkwBetween) or (aToken = gtkwNot_Between) or
//                       (aToken = gtkwIn) or (aToken = gtkwNot_In) or
//                       (aToken = gtkwLike) or (aToken = gtkwNot_Like) or
//                       (aToken = gtkwIs_Null) or (aToken = gtkwIs_Not_Null) or
//                       (aToken = gtkwExists) or (aToken = gtkwNot_Exists)) or
//     (aIndex = 4) and ((aToken = gttkNone) or (aToken = gtkwFrom) or (aToken = gtkwInto) or (aToken = gtkwUpdate) or
//                       (aToken = gtkwInner) or (aToken = gtkwLeft) or (aToken = gtkwRight) or
//                       (aToken = gtkwFull) or (aToken = gtkwCross) or (aToken = gttkComma)) or
//     (aIndex = 5) and ((aToken = gtkwBit) or (aToken = gtkwInt) or (aToken = gtkwInteger) or (aToken = gtkwTinyInt) or (aToken = gtkwSmallInt) or
//                       (aToken = gtkwBigInt) or (aToken = gtkwInt4) or (aToken = gtkwInt8) or (aToken = gtkwFloat) or
//                       (aToken = gtkwSingle) or (aToken = gtkwDouble) or (aToken = gtkwReal) or (aToken = gtkwDecimal) or
//                       (aToken = gtkwNumeric) or (aToken = gtkwNumber) or (aToken = gtkwDate) or (aToken = gtkwTime) or
//                       (aToken = gtkwDateTime) or (aToken = gtkwChar) or (aToken = gtkwVarChar) or (aToken = gtkwVarChar2) or
//                       (aToken = gtkwNVarChar) or (aToken = gtkwBlob) or (aToken = gtkwClob) or (aToken = gtkwType)) or
//     (aIndex = 6) and ((aToken = gtkwCascade) or (aToken = gtkwSet_Null)) or
//     (aIndex = 7) and ((aToken = gtkwCascade) or (aToken = gtkwSet_Null)) or
//     (aIndex = 8) and ((aToken = gtkwAsc) or (aToken = gtkwDesc) or
//                       (aToken = gtkwAscending) or (aToken = gtkwDescending)) or
//     (aIndex = 9) and  Assigned(aToken) or
//     (aIndex = 10)and ((aToken = gtkwOr) or (aToken = gtkwAnd) or (aToken = gtkwNot) or
//                       (aToken = gttkPlus) or (aToken = gttkMinus) or (aToken = gttkConcatenation) or
//                       (aToken = gttkStar) or (aToken = gttkSlash) or (aToken = gttkPercent) or
//                       (aToken = gttkEqual) or (aToken = gttkLessThan) or (aToken = gttkGreaterThan) or
//                       (aToken = gttkLessEqual) or (aToken = gttkGreaterEqual) or
//                       (aToken = gttkNotEqual) or (aToken = gttkDiffer) or
//                       (aToken = gtkwBetween) or (aToken = gtkwNot_Between) or
//                       (aToken = gtkwIn) or (aToken = gtkwNot_In) or
//                       (aToken = gtkwLike) or (aToken = gtkwNot_Like) or
//                       (aToken = gtkwIs_Null) or (aToken = gtkwIs_Not_Null) or
//                       (aToken = gtkwExists) or (aToken = gtkwNot_Exists) or
//                       (aToken = gttkNone) or (aToken = gtkwFrom) or (aToken = gtkwInto) or (aToken = gtkwUpdate) or
//                       (aToken = gtkwInner) or (aToken = gtkwLeft) or (aToken = gtkwRight) or
//                       (aToken = gtkwFull) or (aToken = gtkwCross) or (aToken = gttkComma)) or
//     (aIndex = 11) or
//     (aIndex = 12) and Assigned(aToken) or
//     (aIndex = 13) and Assigned(aToken) or
//     (aIndex = 14) and Assigned(aToken) or
//     (aIndex = 15) and Assigned(aToken)
  if Assigned(aToken)
  then begin
    FTokens [aIndex] := aToken;
//  if aIndex = 9 {keyword} then FTokens[11] {keyword ext} := aToken;
  end else begin
    FTokens [aIndex] := gttkNone;
    //raise
  end;
end;

{ checks if expression has reference to column of given table or alias name }
function TGtSqlNode.ExprHasReferenceTo(aColPrefix: String): Boolean;
var i: Integer;
    s1, s2: String;
begin
  Result := False;
  if aColPrefix = '' then Exit;

  if Check(gtsiExpr, gttkColumnName) then begin
    strBreakOnLast('.', Name, s1, s2);
    Result := AnsiUpperCase(s1) = AnsiUpperCase(aColPrefix);
  end else
  if Kind = gtsiExprTree then begin
    i := 0;
    while not Result and (i < Count) do begin
      Result := Self[i].ExprHasReferenceTo(aColPrefix);
      Inc(i);
    end;
  end;
end;

{ checks if condition has reference to column of given table or alias name }
function TGtSqlNode.CondHasReferenceTo(aColPrefix: String): Boolean;
var lNode: TGtSqlNode;
begin
  Result := False;
  if aColPrefix = '' then Exit;
  if not Check(gtsiCond) then Exit;
  if (Keyword {Operand} = gtkwExists) or (Keyword {Operand} = gtkwNot_Exists) then Exit;

  lNode := Find(gtsiNone, nil, '1');
  if Assigned(lNode) then Result := lNode.ExprHasReferenceTo(aColPrefix);
  if Result then Exit;

  lNode := Find(gtsiNone, nil, '2');
  if Assigned(lNode) then Result := lNode.ExprHasReferenceTo(aColPrefix);
  if Result then Exit;

  if ((Keyword {Operand} = gtkwBetween) or (Keyword {Operand} = gtkwNot_Between)) then begin
    lNode := Find(gtsiNone, nil, '3');
    if Assigned(lNode) then Result := lNode.ExprHasReferenceTo(aColPrefix);
  end;
end;

{ ON cond - moves to end conditions wo reference to given table or alias name }
procedure TGtSqlNode.OnCondMoveRefsFirst(aColPrefix: String);
var i: Integer;
begin
  if aColPrefix = '' then Exit;
  if not Check(gtsiCondTree, gtkwOn) and not Check(gtsiCondTree, gtkwUsing) and not Check(gtsiCond) and not Check(gtsiCondTree) then Exit;

  if Check(gtsiCond) and not CondHasReferenceTo(aColPrefix) then begin
    Owner.RemoveItem(Self);
    Owner.AddItem(Self);
  end else
  if Check(gtsiCondTree, gtkwOn) or Check(gtsiCondTree, gtkwUsing) or Check(gtsiCondTree) then begin
    for i := 0 to Count - 1 do
      Self[i].OnCondMoveRefsFirst(aColPrefix);
  end;
end;

{ calculates condition args len }
procedure TGtSqlNode.CalcConditionArgsLen(var ML_LeftOnExprPrefix, ML_LeftOnExprColumn,
                                              ML_RightOnExprPrefix, ML_RightOnExprColumn: Integer);
var lNode: TGtSqlNode;
    i: Integer;
    b: Boolean;
    s1, s2: String;
begin
  if Kind = gtsiCond then begin
    lNode := Find(gtsiNone, nil, '1');
    while Assigned(lNode) and (lNode.Kind = gtsiExprTree) and (lNode.Count = 1) do lNode := lNode[0];
    if Assigned(lNode) and lNode.Check(gtsiExpr, gttkColumnName) then begin
      strBreakOnLast('.', lNode.Name, s1, s2);

      if Length(s1) > ML_LeftOnExprPrefix then ML_LeftOnExprPrefix := Length(s1);
      if Length(s2) > ML_LeftOnExprColumn then ML_LeftOnExprColumn := Length(s2);
    end;

    lNode := Find(gtsiNone, nil, '2');
    while Assigned(lNode) and (lNode.Kind = gtsiExprTree) and (lNode.Count = 1) do lNode := lNode[0];
    if Assigned(lNode) and lNode.Check(gtsiExpr, gttkColumnName) then begin
      strBreakOnLast('.', lNode.Name, s1, s2);

      if Length(s1) > ML_RightOnExprPrefix then ML_RightOnExprPrefix := Length(s1);
      if Length(s2) > ML_RightOnExprColumn then ML_RightOnExprColumn := Length(s2);
    end;
  end else
  if Kind = gtsiCondTree then begin
    b := False;
    for i := 0 to Count - 1 do
      if not b then begin
        Self[i].CalcConditionArgsLen(ML_LeftOnExprPrefix, ML_LeftOnExprColumn,
                                     ML_RightOnExprPrefix, ML_RightOnExprColumn);
        b := True;
      end;
  end;
end;

{ calculates approx. no of lines for clause }
procedure TGtSqlNode.CalcClauseLines(aKind: TGtSqlNodeKind; aKeyword: TGtLexToken{Def};
                                     aLinesLimit: Integer; aSeparateLines: Boolean;
                                     var aLongQuery: Boolean; var aQueryLines: Integer);
var lNode: TGtSqlNode;
    cnt: Integer;
begin
  if not aSeparateLines then Exit;

  lNode := Find(aKind, aKeyword);
  if not Assigned(lNode) then Exit;

  case lNode.Kind of
    gtsiExprList : begin
      aLongQuery := aLongQuery or (lNode.Count > aLinesLimit) and (aLinesLimit > 0);
      Inc(aQueryLines, lNode.Count);
    end;
    gtsiCond, gtsiCondTree: begin
      cnt := lNode.ConditionsCount;

      aLongQuery := aLongQuery or (cnt > aLinesLimit) and (aLinesLimit > 0);
      Inc(aQueryLines, cnt);

      if Check(gtsiCondTree, gtkwOn) then Dec(aQueryLines);
    end;
  end;
end;

{ calls aProc for each node in aNode list }
procedure TGtSqlNode.ForEach ( aProc: TSqlNodeProcedure;       aDeep: Boolean = False;
                               aKind: TGtSqlNodeKind=gtsiNone; aKeyword: TGtLexToken{Def}=nil; aName: String='' );
var i: Integer;
begin
  for i := 0 to Count -1 do begin
    if Nodes[i].Check(aKind, aKeyword, aName) then aProc(Nodes[i]);

    { recursive call for each node }
    if aDeep then Nodes[i].ForEach ( aProc, aDeep, aKind, aKeyword, aName );
  end;
end;

{ adds auxilinary keyword }
procedure TGtSqlNode.KeywordAuxAdd  (aKeywordAux: TGtLexToken);
begin
  if KeywordAuxCheck(aKeywordAux) then Exit;

  if KeywordAux1 = gttkNone then KeywordAux1 := aKeywordAux else
  if KeywordAux2 = gttkNone then KeywordAux2 := aKeywordAux else
  if KeywordAux3 = gttkNone then KeywordAux3 := aKeywordAux else
  if KeywordAux4 = gttkNone then KeywordAux4 := aKeywordAux else
  if KeywordAux5 = gttkNone then KeywordAux5 := aKeywordAux else
//raise 'Too many auxilinar keywords...'
end;

{ removes auxilinary keyword }
procedure TGtSqlNode.KeywordAuxRemove(aKeywordAux: TGtLexToken);
begin
  if KeywordAuxCheck(aKeywordAux) then Exit;

  if KeywordAux1 = aKeywordAux then KeywordAux1 := gttkNone else
  if KeywordAux2 = aKeywordAux then KeywordAux2 := gttkNone else
  if KeywordAux3 = aKeywordAux then KeywordAux3 := gttkNone else
  if KeywordAux4 = aKeywordAux then KeywordAux4 := gttkNone else
  if KeywordAux5 = aKeywordAux then KeywordAux5 := gttkNone ;
end;

{ check for auxilinary keyword }
function  TGtSqlNode.KeywordAuxCheck  (aKeywordAux1: TGtLexToken;
                                       aKeywordAux2: TGtLexToken = nil;
                                       aKeywordAux3: TGtLexToken = nil;
                                       aKeywordAux4: TGtLexToken = nil;
                                       aKeywordAux5: TGtLexToken = nil): Boolean;
begin
  Result := Assigned(aKeywordAux1) and ( (KeywordAux1 = aKeywordAux1) or (KeywordAux2 = aKeywordAux1) or
                                         (KeywordAux3 = aKeywordAux1) or (KeywordAux4 = aKeywordAux1) or
                                         (KeywordAux5 = aKeywordAux1))or
            Assigned(aKeywordAux2) and ( (KeywordAux1 = aKeywordAux2) or (KeywordAux2 = aKeywordAux2) or
                                         (KeywordAux3 = aKeywordAux2) or (KeywordAux4 = aKeywordAux2) or
                                         (KeywordAux5 = aKeywordAux2))or
            Assigned(aKeywordAux3) and ( (KeywordAux1 = aKeywordAux3) or (KeywordAux2 = aKeywordAux3) or
                                         (KeywordAux3 = aKeywordAux3) or (KeywordAux4 = aKeywordAux3) or
                                         (KeywordAux5 = aKeywordAux3))or
            Assigned(aKeywordAux4) and ( (KeywordAux1 = aKeywordAux4) or (KeywordAux2 = aKeywordAux4) or
                                         (KeywordAux3 = aKeywordAux4) or (KeywordAux4 = aKeywordAux4) or
                                         (KeywordAux5 = aKeywordAux4))or
            Assigned(aKeywordAux5) and ( (KeywordAux1 = aKeywordAux5) or (KeywordAux2 = aKeywordAux5) or
                                         (KeywordAux3 = aKeywordAux5) or (KeywordAux4 = aKeywordAux5) or
                                         (KeywordAux5 = aKeywordAux5));
end;

{ check for auxilinary keyword }
function  TGtSqlNode.KeywordAuxCheckKwd (aKeywordAux1: TGtLexToken;
                                         aKeywordAux2: TGtLexToken = nil;
                                         aKeywordAux3: TGtLexToken = nil;
                                         aKeywordAux4: TGtLexToken = nil;
                                         aKeywordAux5: TGtLexToken = nil): TGtLexToken;
begin
  if Assigned(aKeywordAux1) and KeywordAuxCheck(aKeywordAux1) then Result := aKeywordAux1 else
  if Assigned(aKeywordAux2) and KeywordAuxCheck(aKeywordAux2) then Result := aKeywordAux2 else
  if Assigned(aKeywordAux3) and KeywordAuxCheck(aKeywordAux3) then Result := aKeywordAux3 else
  if Assigned(aKeywordAux4) and KeywordAuxCheck(aKeywordAux4) then Result := aKeywordAux4 else
  if Assigned(aKeywordAux5) and KeywordAuxCheck(aKeywordAux5) then Result := aKeywordAux5 else Result := nil ;
end;

end.

