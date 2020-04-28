(* $Header: /SQL Toys/units/SqlNode.pas 320   19-12-14 12:31 Tomek $
   (c) Tomasz Gierka, github.com/SqlToys, 2010.10.15                          *)
{--------------------------------------  --------------------------------------}
{$IFDEF RELEASE}
  {$DEBUGINFO OFF}
  {$LOCALSYMBOLS OFF}
{$ENDIF}
unit SqlNode;

interface

uses Classes, GtContainers, GtTokenizers;

{---------------------------------- SQL Node ----------------------------------}

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

  function IsKeywordClause(aKeyword: TGtLexToken): Boolean;
  function GtSqlNodeKindToName( aKind: TGtSqlNodeKind ): string;
  function GtSqlNodeNameToKind( aName: String ): TGtSqlNodeKind;

const
  cSqlNodeTokenMax = 7;

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
    property        Keyword   :    TGtLexToken index  1 read FTokens[ 1] write SetFTokens;
    property        KeywordExt:    TGtLexToken index  2 read FTokens[ 2] write SetFTokens;

    // KeywordExt shoud be used to extend meaning of Keyword.
    // As Keyword should be used to point node class type, KeywordExt should be used to point what keyword was exactly used.
    // In other cases KeywordAux should be used.
//protected
    property        KeywordAux1:   TGtLexToken index  3 read FTokens[ 3] write SetFTokens;
    property        KeywordAux2:   TGtLexToken index  4 read FTokens[ 4] write SetFTokens;
    property        KeywordAux3:   TGtLexToken index  5 read FTokens[ 5] write SetFTokens;
    property        KeywordAux4:   TGtLexToken index  6 read FTokens[ 6] write SetFTokens;
    property        KeywordAux5:   TGtLexToken index  7 read FTokens[ 7] write SetFTokens;
  private
    function        SubQueryCond: Boolean;
  public // service methods
    function        GetQuery: TGtSqlNode;
    function        SingleColumnConstraint: Boolean;
    function        IsQuery: Boolean;
    function        IsSubQuery: Boolean;
    function        SubQueryDeep: Integer;
    function        IsClause: Boolean;
    function        GetExtQuery: TGtSqlNode;
    function        OwnerTableNameOrAlias: String;

    function        ExprTreeOwner: TGtSqlNode;
    function        ExprTreeOperator: TGtLexToken;
    function        ExprHasReferenceTo(aColPrefix: String): Boolean;

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

    procedure       ForEach ( aProc: TSqlNodeProcedure;       aDeep: Boolean = False;
                              aKind: TGtSqlNodeKind=gtsiNone; aKeyword: TGtLexToken=nil; aName: String='' ); overload;
  end;

var GtSqlNodeCount: Integer = 0;

implementation

uses SysUtils, GtStandard, SqlCommon, GtExternals;

function IsKeywordClause(aKeyword: TGtLexToken): Boolean;
begin
  Result := (aKeyword = gtkwSelect) or (aKeyword = gtkwInto) or (aKeyword = gtkwFrom) or
            (aKeyword = gtkwJoin) or (aKeyword = gtkwInner_Join) or
            (aKeyword = gtkwLeft_Join) or (aKeyword = gtkwLeft_Outer_Join) or
            (aKeyword = gtkwRight_Join) or (aKeyword = gtkwRight_Outer_Join) or
            (aKeyword = gtkwWhere) or (aKeyword = gtkwGroup_By) or (aKeyword = gtkwHaving) or (aKeyword = gtkwOrder_By) or
            (aKeyword = gtkwConnect_By) or (aKeyword = gtkwStart_With) or
            (aKeyword = gtkwUpdate) or (aKeyword = gtkwSet) or
            (aKeyword = gtkwDelete) or (aKeyword = gtkwDelete_From) or
            (aKeyword = gtkwInsert) or (aKeyword = gtkwInsert_Into) or (aKeyword = gtkwValues) or (aKeyword = gtkwReturning) or
            (aKeyword = gtkwCreate_Table) or (aKeyword = gtkwDrop_Table) or (aKeyword = gtkwAlter_Table) or
            (aKeyword = gtkwCreate_Index) or (aKeyword = gtkwCreate_Unique_Index) or (aKeyword = gtkwDrop_Index) or
            (aKeyword = gtkwSavepoint) or
            (aKeyword = gtkwCommit) or (aKeyword = gtkwCommit_Work) or (aKeyword = gtkwCommit_Tran) or
            (aKeyword = gtkwRollback) or (aKeyword = gtkwRollback_Tran) or (aKeyword = gtkwRollback_To_Savepoint) or
            (aKeyword = gtkwCreate_Sequence) or (aKeyword = gtkwDrop_Sequence) or
            (aKeyword = gtkwCreate_View) or (aKeyword = gtkwCreate_Or_Replace_View) or (aKeyword = gtkwDrop_View) or
            (aKeyword = gtkwTruncate_Table) or
            (aKeyword = gtkwUse) ;
//          (aKeyword = gtkwCreate_Synonym) or (aKeyword = gtkwDrop_Synonym) or
//          (aKeyword = gtkwGrant) or (aKeyword = gtkwDeny) or (aKeyword = gtkwRevoke);
end;

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

{---------------------------------- SQL Node ----------------------------------}

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

{ returns true if node is a query or subquery }
function TGtSqlNode.IsQuery: Boolean;
begin
  Result := Check(gtsiDml, gtkwSelect);
end;

{ subquery condition }
function TGtSqlNode.SubQueryCond: Boolean;
begin
  Result := not( (Owner is TGtSqlNode) and (Owner.FKind = gtsiUnions) and (Owner.Name = 'Parsed Queries List')) and
            not(  Check(gtsiDml, gtkwInsert) and (Owner = Self) ) and
            not( (Check(gtsiDDL) and (Check(gtsiDDL, gtkwCreate_View))));
end;

{ returns true if node is a subquery }
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

  Result := Assigned( lQuery ) and lQuery.SubQueryCond ;
end;

{ TODO: returns nested query deep level }
function  TGtSqlNode.SubQueryDeep: Integer;
var lQuery, lOwner: TGtSqlNode;
begin
  Result := 0;
  if not Check(gtsiDml, gtkwSelect) then Exit;

  lOwner := Owner;
  lQuery := GetExtQuery;

  { dla UNION/MINUS/EXCEPT GetExtQuery nie jest kwerenda nadrzedna }
  while (lOwner is TGtSqlNode) and (lOwner.Kind = gtsiUnions) do begin
    lOwner := lQuery.Owner;
    lQuery := lQuery.GetExtQuery;

    if Assigned( lQuery ) and lQuery.SubQueryCond then Inc(Result);
  end;
end;

{ returns true if node keyword is an clause keyword }
function TGtSqlNode.IsClause: Boolean;
begin
  Result := IsKeywordClause( Keyword ) or IsKeywordClause( KeywordExt );
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
  for i := 0 to Count -1 do
    if Assigned(Nodes[i]) then begin
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

