(* $Header: /SQL Toys/units/SqlNode.pas 316   19-03-24 21:50 Tomek $
   (c) Tomasz Gierka, github.com/SqlToys, 2010.10.15                          *)
{--------------------------------------  --------------------------------------}
{$IFDEF RELEASE}
  {$DEBUGINFO OFF}
  {$LOCALSYMBOLS OFF}
{$ENDIF}
unit SqlNode;

interface

uses Classes, GtContainers, GtTokenizers;

{-------------------------------- Join Operator -------------------------------}

function  JoinOperatorToToken( jo: TGtLexToken; InnerPreff: Boolean = False; OuterPreff: Boolean = False ): TGtLexToken;


{---------------------------------- SQL Item ----------------------------------}

type
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

  TGtSqlNode = class (TGtLexNode)
  private
    FKind: TGtSqlNodeKind;
    FTokens: array [1..cSqlNodeTokenMax] of TGtLexToken;

    function        GetNodeOwner: TGtSqlNode;
    procedure       SetNodeOwner(aOwner: TGtSqlNode);

    function        FindByKind(aKind: TGtSqlNodeKind; aNode: TGtSqlNode=nil): TGtSqlNode;

    procedure       SetFTokens(aIndex: Integer; aToken: TGtLexToken);
  public // general SQL Item methods
    constructor     Create(aOwner: TGtItem); override;
    constructor     Create(aOwner: TGtItem; aKind: TGtSqlNodeKind; aName: String=''); overload; virtual;

    function        NewNode(aKind: TGtSqlNodeKind; aKeyword: TGtLexToken=nil; aName: String=''): TGtSqlNode; virtual;

    function        GetNode(aIndex: Integer): TGtSqlNode;

    function        Check    (aKind: TGtSqlNodeKind=gtsiNone; aKeyword: TGtLexToken=nil; aName: String=''): Boolean;
    function        Find     (aKind: TGtSqlNodeKind=gtsiNone; aKeyword: TGtLexToken=nil; aName: String=''; aNode: TGtSqlNode=nil): TGtSqlNode;
    function        FindOwner(aKind: TGtSqlNodeKind=gtsiNone; aKeyword: TGtLexToken=nil; aName: String=''): TGtSqlNode;

    property        Owner: TGtSqlNode read GetNodeOwner  write SetNodeOwner;

    property        Kind: TGtSqlNodeKind read FKind write FKind;
    property        Nodes[Index: Integer]: TGtSqlNode read GetNode; default;
  public // function specific methods
    property        Keyword   : TGtLexToken index 9 read FTokens[9] write SetFTokens;
    property        KeywordExt:    TGtLexToken index 11 read FTokens[11] write SetFTokens;

    // KeywordExt shoud be used to extend meaning of Keyword.
    // As Keyword should be used to point node class type, KeywordExt should be used to point what keyword was exactly used.
    // In other cases KeywordAux should be used.
//protected
    property        KeywordAux1:   TGtLexToken      index 12 read FTokens[12] write SetFTokens;
    property        KeywordAux2:   TGtLexToken      index 13 read FTokens[13] write SetFTokens;
    property        KeywordAux3:   TGtLexToken      index 14 read FTokens[14] write SetFTokens;
    property        KeywordAux4:   TGtLexToken      index 15 read FTokens[15] write SetFTokens;
    property        KeywordAux5:   TGtLexToken      index 16 read FTokens[16] write SetFTokens;
  public // service methods
    function        GetQuery: TGtSqlNode;
    function        SingleColumnConstraint: Boolean;
    function        IsSubQuery: Boolean;
  //function        IsShortQuery: Boolean;
    function        IsClauseKeyword: Boolean;
    function        GetExtQuery: TGtSqlNode;
  //function        TablesCount: Integer;

    function        ExprTreeOwner: TGtSqlNode;
    function        ExprTreeOperator: TGtLexToken;
  //function        ConditionsCount: Integer;

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

    procedure       ForEach ( aProc: TSqlNodeProcedure;       aDeep: Boolean = False;
                              aKind: TGtSqlNodeKind=gtsiNone; aKeyword: TGtLexToken=nil; aName: String='' ); overload;
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

uses SysUtils, GtStandard, SqlCommon, GtExternals;

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

  Inc(GtSqlNodeCount);
  Kind := gtsiNone;

  for i := 1 to cSqlNodeTokenMax do FTokens[i] := gttkNone;
end;

{ class constructor }
constructor TGtSqlNode.Create(aOwner: TGtItem; aKind: TGtSqlNodeKind; aName: String='');
begin
  inherited Create(aOwner, aName);

  Kind := aKind;
end;

{ creates new node }
function TGtSqlNode.NewNode;
begin
  Result := TGtSqlNode.Create(Self, aName);

  Result.Kind := aKind ;
  Result.Keyword := aKeyword;
end;

{ gets owner of sql item }
function  TGtSqlNode.GetNodeOwner: TGtSqlNode;
begin
  if inherited Owner is TGtSqlNode
    then Result := inherited Owner as TGtSqlNode
    else Result := nil;
end;

{ sets new owner for sql item }
procedure TGtSqlNode.SetNodeOwner(aOwner: TGtSqlNode);
begin
  SetOwner(aOwner);
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

{ returns top level owner of an expr tree }
function TGtSqlNode.ExprTreeOwner: TGtSqlNode;
begin
  Result := nil;
  if Self.Kind <> gtsiExprTree then Exit;

  Result := FindOwner(gtsiExprTree);
end;

{ returns operator }
function TGtSqlNode.ExprTreeOperator: TGtLexToken;
begin
  Result := gttkConcatenation ;
  if KeywordAuxCheck(gttkPlus) then Result := gttkPlus else
  if KeywordAuxCheck(gttkStar) then Result := gttkStar ;
end;

{}
function TGtSqlNode.OwnerTableNameOrAlias: String;
var lOwner, lAlias: TGtSqlNode;
begin
  Result := '';
  lOwner := FindOwner(gtsiTableRef);
  if not Assigned(lOwner) then lOwner := FindOwner(gtsiDml, gtkwSelect);
  if not Assigned(lOwner) then Exit;

  lAlias := lOwner.Find(gtsiNone, gtkwAs);
  if Assigned(lAlias) then Result := lAlias.Name else Result := lOwner.Name;
end;

{ sets FTokens property }
procedure TGtSqlNode.SetFTokens(aIndex: Integer; aToken: TGtLexToken{Def});
begin
  if Assigned(aToken) then begin
    FTokens [aIndex] := aToken;
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

{ calls aProc for each node in aNode list }
procedure TGtSqlNode.ForEach ( aProc: TSqlNodeProcedure;       aDeep: Boolean = False;
                               aKind: TGtSqlNodeKind=gtsiNone; aKeyword: TGtLexToken=nil; aName: String='' );
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

  { MOVE THIS PROCEDURE TO SqlCommon !!!! }
  function SameKeywordTokens(aTokenDef, aToken: TGtLexToken): Boolean;
  begin
    Result := False;
    if not Assigned(aTokenDef) then Exit;
    if not Assigned(aToken) then Exit;

    if aTokenDef = gttkNone then Exit;
    if aToken = gttkNone then Exit;

    if aTokenDef.Complex and aToken.Complex then begin
      Result := True;
      if Result and Assigned(aTokenDef.SubToken1) then Result := Result and SameKeywordTokens(aTokenDef.SubToken1, aToken.SubToken1);
      if Result and Assigned(aTokenDef.SubToken2) then Result := Result and SameKeywordTokens(aTokenDef.SubToken2, aToken.SubToken2);
      if Result and Assigned(aTokenDef.SubToken3) then Result := Result and SameKeywordTokens(aTokenDef.SubToken3, aToken.SubToken3);
      if Result and Assigned(aTokenDef.SubToken4) then Result := Result and SameKeywordTokens(aTokenDef.SubToken4, aToken.SubToken4);
      if Result and Assigned(aTokenDef.SubToken5) then Result := Result and SameKeywordTokens(aTokenDef.SubToken5, aToken.SubToken5);
      Exit;
    end;

    Result := (aTokenDef.TokenKind = gtttKeyword)    and (aToken.TokenKind in [gtttWord, gtttKeyword])
                                                     and (Hash_DJB_TGI_U32(AnsiUpperCase(aTokenDef.TokenText))
                                                        = Hash_DJB_TGI_U32(aToken.TokenText))
                                                     and (AnsiUpperCase(aTokenDef.TokenText) = aToken.TokenText) or
              (aTokenDef.TokenKind = gtttIdentifier) and (aToken.TokenKind in [gtttWord, gtttIdentifier])     or
              (aTokenDef.TokenKind = gtttRelevant)   and (Hash_DJB_TGI_U32(AnsiUpperCase(aTokenDef.TokenText))
                                                        = Hash_DJB_TGI_U32(AnsiUpperCase(aToken.TokenText))) or
              (aTokenDef.TokenKind = gtttNumber)     and (aToken.TokenKind = gtttNumber)   or
              (aTokenDef.TokenKind = gtttString)     and (aToken.TokenKind = gtttString);
  end;

begin
  Result := SameKeywordTokens(aKeywordAux1, KeywordAux1) or SameKeywordTokens(aKeywordAux1, KeywordAux2) or SameKeywordTokens(aKeywordAux1, KeywordAux3) or SameKeywordTokens(aKeywordAux1, KeywordAux4) or SameKeywordTokens(aKeywordAux1, KeywordAux5) or
            SameKeywordTokens(aKeywordAux2, KeywordAux1) or SameKeywordTokens(aKeywordAux2, KeywordAux2) or SameKeywordTokens(aKeywordAux2, KeywordAux3) or SameKeywordTokens(aKeywordAux2, KeywordAux4) or SameKeywordTokens(aKeywordAux2, KeywordAux5) or
            SameKeywordTokens(aKeywordAux3, KeywordAux1) or SameKeywordTokens(aKeywordAux3, KeywordAux2) or SameKeywordTokens(aKeywordAux3, KeywordAux3) or SameKeywordTokens(aKeywordAux3, KeywordAux4) or SameKeywordTokens(aKeywordAux3, KeywordAux5) or
            SameKeywordTokens(aKeywordAux4, KeywordAux1) or SameKeywordTokens(aKeywordAux4, KeywordAux2) or SameKeywordTokens(aKeywordAux4, KeywordAux3) or SameKeywordTokens(aKeywordAux4, KeywordAux4) or SameKeywordTokens(aKeywordAux4, KeywordAux5) or
            SameKeywordTokens(aKeywordAux5, KeywordAux1) or SameKeywordTokens(aKeywordAux5, KeywordAux2) or SameKeywordTokens(aKeywordAux5, KeywordAux3) or SameKeywordTokens(aKeywordAux5, KeywordAux4) or SameKeywordTokens(aKeywordAux5, KeywordAux5) ;
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

