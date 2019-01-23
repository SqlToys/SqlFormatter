(* $Header: /SQL Toys/SqlFormat/SqlXmlTree.pas 14    19-01-10 19:05 Tomek $
   (c) Tomasz Gierka, github.com/SqlToys, 2017.12.12                          *)
{--------------------------------------  --------------------------------------}
unit SqlXmlTree;

interface

uses SqlStructs;

function  XmlQuote(aStr: String): String;

procedure ParseTreeToXml (aSqlNode: TGtSqlNode; aFileName: String);
procedure XmlToParseTree (aFileName: String; aTopNode: TGtSqlNode);

implementation

uses Classes, SysUtils, XmlIntf, XmlDoc, Variants,
     GtTokenizers;

{ quotes string to use in xml }
function  XmlQuote(aStr: String): String;
begin
  Result := StringReplace(
            StringReplace(
                           aStr
                         , '''', '&apos;', [rfReplaceAll])
                         , '"',  '&quot;', [rfReplaceAll])
            ;
end;

{ unquotes string used in xml }
function  XmlUnQuote(aStr: String): String;
begin
  Result := StringReplace(
            StringReplace(
                           aStr
                         , '&apos;', '''', [rfReplaceAll])
                         , '&quot;', '"',  [rfReplaceAll])
            ;
end;

{ exports tree nodes to an xml file }
procedure ParseTreeToXml (aSqlNode: TGtSqlNode; aFileName: String);
var lSL: TStringList;

    procedure NodeToXml(aNode: TGtSqlNode);
    var i: Integer;
        s: String;
    begin
      if not Assigned(aNode) then Exit;
      s := '';

      if(aNode.Name <> '') //and (aNode.Kind <> gtsiExprTree)
        then s := s + ' Name="' + XmlQuote(aNode.Name) + '"';

      { node attributes }
//      case aNode.Nullable of
//        gtopNullNotSpecified : ;
//        gtopNull             : s := s + ' Nullable="NULL"';
//        gtopNotNull          : s := s + ' Nullable="NOT NULL"';
//      end;

//    if(aNode.LogicOp       <> gttkNone)and(aNode.Count > 1)
//                                       then s := s + ' LogicOp="'       + XmlQuote(aNode.LogicOp.Text)   + '"';
//    if(aNode.ExprOp        <> gttkNone)and(aNode.Count > 1)
//                                       then s := s + ' ExprOp="'        + XmlQuote(aNode.ExprOp.Text)    + '"';
//    if(aNode.CompOp        <> gttkNone)and(aNode.Count > 1)
//                                       then s := s + ' CompOp="'        + XmlQuote(aNode.CompOp.Text)    + '"';
//    if aNode.JoinOp        <> gttkNone then s := s + ' JoinOp="'        + XmlQuote(aNode.JoinOp.Text)    + '"';
//    if(aNode.Operand       <> gttkNone)then s := s + ' Operand="'       + XmlQuote(aNode.Operand.TokenText)   + '"';
//    if aNode.DataType      <> gttkNone then s := s + ' DataType="'      + XmlQuote(aNode.DataType.TokenText)  + '"';
//    if aNode.OnDelete      <> gttkNone then s := s + ' OnDelete="'      + XmlQuote(aNode.OnDelete.TokenText)  + '"';
//    if aNode.OnUpdate      <> gttkNone then s := s + ' OnUpdate="'      + XmlQuote(aNode.OnUpdate.TokenText)  + '"';
//    if aNode.SortOrder     <> gttkNone then s := s + ' SortOrder="'     + XmlQuote(aNode.SortOrder.TokenText) + '"';
      if Assigned(aNode.Keyword) and
        (aNode.Keyword       <> gttkNone)//and(aNode.Keyword <> gttkIdentifier)and(aNode.Keyword <> gttkColumnName)
  //                                       and(aNode.Keyword <> gttkNumber)
                                         then s := s + ' Keyword="'       + XmlQuote(aNode.Keyword.TokenText)   + '"';
      if Assigned(aNode.KeywordExt) and
        (aNode.KeywordExt    <> gttkNone)then s := s + ' KeywordExt="'    + XmlQuote(aNode.KeywordExt.TokenText)+ '"';

//      if Assigned(aNode.KeywordAfter1) and
//        (aNode.KeywordAfter1 <> gttkNone)then s := s + ' KeywordAfter1="' + XmlQuote(aNode.KeywordAfter1.TokenText)+ '"';
//      if Assigned(aNode.KeywordAfter2) and
//        (aNode.KeywordAfter2 <> gttkNone)then s := s + ' KeywordAfter2="' + XmlQuote(aNode.KeywordAfter1.TokenText)+ '"';
//      if Assigned(aNode.KeywordAfter3) and
//        (aNode.KeywordAfter3 <> gttkNone)then s := s + ' KeywordAfter3="' + XmlQuote(aNode.KeywordAfter1.TokenText)+ '"';

      if Assigned(aNode.KeywordAux1) and (aNode.KeywordAux1 <> gttkNone) then s := s + ' KeywordAux1="' + XmlQuote(aNode.KeywordAux1.TokenText)+ '"';
      if Assigned(aNode.KeywordAux2) and (aNode.KeywordAux2 <> gttkNone) then s := s + ' KeywordAux2="' + XmlQuote(aNode.KeywordAux2.TokenText)+ '"';
      if Assigned(aNode.KeywordAux3) and (aNode.KeywordAux3 <> gttkNone) then s := s + ' KeywordAux3="' + XmlQuote(aNode.KeywordAux3.TokenText)+ '"';
      if Assigned(aNode.KeywordAux4) and (aNode.KeywordAux4 <> gttkNone) then s := s + ' KeywordAux4="' + XmlQuote(aNode.KeywordAux4.TokenText)+ '"';
      if Assigned(aNode.KeywordAux5) and (aNode.KeywordAux5 <> gttkNone) then s := s + ' KeywordAux5="' + XmlQuote(aNode.KeywordAux5.TokenText)+ '"';

      if Assigned(aNode.Values) then
        for i := 0 to aNode.Values.Count -1 do
          s := s + ' ' + aNode.GetValName( StrToInt(aNode.Values.Names[i]) ) + '="' + XmlQuote(aNode.Values.ValueFromIndex[i]) + '"';

//      if (not aFullList) and (aNode.Count = 1) and (s = '') and (aNode.Kind in [gtsiExprTree, gtsiCondTree]) then begin
//        NodeToXml(aNode[0]);
//        Exit;
//      end;

      if aNode.Count = 0 then begin
        lSL.Add( '<' + GtSqlNodeKindToName(aNode.Kind) + s + ' />');
      end else begin
        lSL.Add( '<' + GtSqlNodeKindToName(aNode.Kind) + s + '>' );
        for i := 0 to aNode.Count-1 do NodeToXml(aNode[i]);
        lSL.Add( '</' + GtSqlNodeKindToName(aNode.Kind) + '>' );
      end;
    end;

begin
  lSL := TStringList.Create;
  try
    NodeToXml(aSqlNode);
    lSL.SaveToFile(aFileName);
  finally
    lSL.Free;
  end;
end;

{ imports xml file into tree nodes }
procedure XmlToParseTree (aFileName: String; aTopNode: TGtSqlNode);

    function XmlToNode( aXmlNode: IXmlNode; aParentSqlNode: TGtSqlNode ): TGtSqlNode;
    var i: Integer;
    begin
      if GtSqlNodeNameToKind(aXmlNode.NodeName) = gtsiQueryList then begin
        Result := aParentSqlNode;
      end else
      if not Assigned(aParentSqlNode) then begin
        Result := TGtSqlNode.Create(nil, GtSqlNodeNameToKind(aXmlNode.NodeName));
      end else begin
        Result := aParentSqlNode.NewNode(GtSqlNodeNameToKind(aXmlNode.NodeName), nil, '');
      end;

      for i := 0 to aXmlNode.AttributeNodes.Count - 1 do
        if not VarIsNull(aXmlNode.AttributeNodes[i].NodeValue) then begin
          if aXmlNode.AttributeNodes[i].NodeName = 'Name' then Result.Name := XmlUnQuote( aXmlNode.AttributeNodes[i].NodeValue ) else
          if aXmlNode.AttributeNodes[i].NodeName = 'Nullable' then begin
//            if aXmlNode.AttributeNodes[i].NodeValue = 'NULL' then Result.Nullable := gtopNull else
//            if aXmlNode.AttributeNodes[i].NodeValue = 'NOT NULL' then Result.Nullable := gtopNotNull
//                                                                 else Result.Nullable := gtopNullNotSpecified;
          end else
//        if aXmlNode.AttributeNodes[i].NodeName = 'LogicOp'   then Result.LogicOp  := LexKeywordTokenFind( XmlUnQuote( aXmlNode.AttributeNodes[i].NodeValue )) else
//        if aXmlNode.AttributeNodes[i].NodeName = 'ExprOp'    then Result.ExprOp   := LexKeywordTokenFind( XmlUnQuote( aXmlNode.AttributeNodes[i].NodeValue )) else
//        if aXmlNode.AttributeNodes[i].NodeName = 'CompOp'    then Result.CompOp   := LexKeywordTokenFind( XmlUnQuote( aXmlNode.AttributeNodes[i].NodeValue )) else
//        if aXmlNode.AttributeNodes[i].NodeName = 'JoinOp'    then Result.JoinOp   := LexKeywordTokenFind( XmlUnQuote( aXmlNode.AttributeNodes[i].NodeValue )) else
//        if aXmlNode.AttributeNodes[i].NodeName = 'Operand'   then Result.Operand  := LexKeywordTokenFind( XmlUnQuote( aXmlNode.AttributeNodes[i].NodeValue )) else
//        if aXmlNode.AttributeNodes[i].NodeName = 'DataType'  then Result.DataType := LexKeywordTokenFind( XmlUnQuote( aXmlNode.AttributeNodes[i].NodeValue )) else
//          if aXmlNode.AttributeNodes[i].NodeName = 'OnDelete'  then Result.OnDelete := LexKeywordTokenFind( XmlUnQuote( aXmlNode.AttributeNodes[i].NodeValue )) else
//          if aXmlNode.AttributeNodes[i].NodeName = 'OnUpdate'  then Result.OnUpdate := LexKeywordTokenFind( XmlUnQuote( aXmlNode.AttributeNodes[i].NodeValue )) else
//        if aXmlNode.AttributeNodes[i].NodeName = 'SortOrder' then Result.SortOrder:= LexKeywordTokenFind( XmlUnQuote( aXmlNode.AttributeNodes[i].NodeValue )) else
          if aXmlNode.AttributeNodes[i].NodeName = 'Keyword'   then Result.Keyword  := LexKeywordTokenFind( XmlUnQuote( aXmlNode.AttributeNodes[i].NodeValue )) else
          if aXmlNode.AttributeNodes[i].NodeName = 'KeywordExt'      then Result.KeywordExt  := LexKeywordTokenFind( XmlUnQuote( aXmlNode.AttributeNodes[i].NodeValue )) else
//          if aXmlNode.AttributeNodes[i].NodeName = 'KeywordAfter1'   then Result.KeywordAfter1  := LexKeywordTokenFind( XmlUnQuote( aXmlNode.AttributeNodes[i].NodeValue )) else
//          if aXmlNode.AttributeNodes[i].NodeName = 'KeywordAfter2'   then Result.KeywordAfter2  := LexKeywordTokenFind( XmlUnQuote( aXmlNode.AttributeNodes[i].NodeValue )) else
          if aXmlNode.AttributeNodes[i].NodeName = 'KeywordAux1'   then Result.KeywordAux1 := LexKeywordTokenFind( XmlUnQuote( aXmlNode.AttributeNodes[i].NodeValue )) else
          if aXmlNode.AttributeNodes[i].NodeName = 'KeywordAux2'   then Result.KeywordAux2 := LexKeywordTokenFind( XmlUnQuote( aXmlNode.AttributeNodes[i].NodeValue )) else
          if aXmlNode.AttributeNodes[i].NodeName = 'KeywordAux3'   then Result.KeywordAux3 := LexKeywordTokenFind( XmlUnQuote( aXmlNode.AttributeNodes[i].NodeValue )) else
          if aXmlNode.AttributeNodes[i].NodeName = 'KeywordAux4'   then Result.KeywordAux4 := LexKeywordTokenFind( XmlUnQuote( aXmlNode.AttributeNodes[i].NodeValue )) else
          if aXmlNode.AttributeNodes[i].NodeName = 'KeywordAux5'   then Result.KeywordAux5 := LexKeywordTokenFind( XmlUnQuote( aXmlNode.AttributeNodes[i].NodeValue )) else

//          if aXmlNode.AttributeNodes[i].NodeName = 'KeywordAfter3'   then Result.KeywordAfter3  := LexKeywordTokenFind( XmlUnQuote( aXmlNode.AttributeNodes[i].NodeValue )) else
//             Result.SetValueByName( aXmlNode.AttributeNodes[i].NodeName, XmlUnQuote( aXmlNode.AttributeNodes[i].NodeValue ) );
        end;

      { child nodes }
      for i := 0 to aXmlNode.ChildNodes.Count - 1 do
        XmlToNode( aXmlNode.ChildNodes[i], Result );
    end;

var lXml: IXmlDocument;
    i: Integer;
begin
  if aFileName = '' then Exit;
  if not FileExists(aFileName) then Exit;

  lXml := LoadXmlDocument( aFileName );
  try
    if lXml.IsEmptyDoc then Exit;
    if not Assigned(lXml.ChildNodes) then Exit;

    { child nodes }
    for i := 0 to lXml.ChildNodes.Count - 1 do
      XmlToNode( lXml.ChildNodes[i], aTopNode );
  finally
  //  lXml.Free;
  end;
end;

end.
