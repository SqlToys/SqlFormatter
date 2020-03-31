(* $Header: /SQL Toys/units/SqlXmlTree.pas 17    19-03-24 21:50 Tomek $
   (c) Tomasz Gierka, github.com/SqlToys, 2017.12.12                          *)
{--------------------------------------  --------------------------------------}
unit SqlXmlTree;

interface

uses SqlNode;

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

      if(aNode.Name <> '') then s := s + ' Name="' + XmlQuote(aNode.Name) + '"';

      { node attributes }
      if Assigned(aNode.Keyword) and (aNode.Keyword       <> gttkNone)
        then s := s + ' Keyword="'       + XmlQuote(aNode.Keyword.TokenText)   + '"';
      if Assigned(aNode.KeywordExt) and (aNode.KeywordExt <> gttkNone)
        then s := s + ' KeywordExt="'    + XmlQuote(aNode.KeywordExt.TokenText)+ '"';

      if Assigned(aNode.KeywordAux1) and (aNode.KeywordAux1 <> gttkNone) then s := s + ' KeywordAux1="' + XmlQuote(aNode.KeywordAux1.TokenText)+ '"';
      if Assigned(aNode.KeywordAux2) and (aNode.KeywordAux2 <> gttkNone) then s := s + ' KeywordAux2="' + XmlQuote(aNode.KeywordAux2.TokenText)+ '"';
      if Assigned(aNode.KeywordAux3) and (aNode.KeywordAux3 <> gttkNone) then s := s + ' KeywordAux3="' + XmlQuote(aNode.KeywordAux3.TokenText)+ '"';
      if Assigned(aNode.KeywordAux4) and (aNode.KeywordAux4 <> gttkNone) then s := s + ' KeywordAux4="' + XmlQuote(aNode.KeywordAux4.TokenText)+ '"';
      if Assigned(aNode.KeywordAux5) and (aNode.KeywordAux5 <> gttkNone) then s := s + ' KeywordAux5="' + XmlQuote(aNode.KeywordAux5.TokenText)+ '"';

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
          end else
          if aXmlNode.AttributeNodes[i].NodeName = 'Keyword'   then Result.Keyword  := LexKeywordTokenFind( XmlUnQuote( aXmlNode.AttributeNodes[i].NodeValue )) else
          if aXmlNode.AttributeNodes[i].NodeName = 'KeywordExt'      then Result.KeywordExt  := LexKeywordTokenFind( XmlUnQuote( aXmlNode.AttributeNodes[i].NodeValue )) else
          if aXmlNode.AttributeNodes[i].NodeName = 'KeywordAux1'   then Result.KeywordAux1 := LexKeywordTokenFind( XmlUnQuote( aXmlNode.AttributeNodes[i].NodeValue )) else
          if aXmlNode.AttributeNodes[i].NodeName = 'KeywordAux2'   then Result.KeywordAux2 := LexKeywordTokenFind( XmlUnQuote( aXmlNode.AttributeNodes[i].NodeValue )) else
          if aXmlNode.AttributeNodes[i].NodeName = 'KeywordAux3'   then Result.KeywordAux3 := LexKeywordTokenFind( XmlUnQuote( aXmlNode.AttributeNodes[i].NodeValue )) else
          if aXmlNode.AttributeNodes[i].NodeName = 'KeywordAux4'   then Result.KeywordAux4 := LexKeywordTokenFind( XmlUnQuote( aXmlNode.AttributeNodes[i].NodeValue )) else
          if aXmlNode.AttributeNodes[i].NodeName = 'KeywordAux5'   then Result.KeywordAux5 := LexKeywordTokenFind( XmlUnQuote( aXmlNode.AttributeNodes[i].NodeValue )) else
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
