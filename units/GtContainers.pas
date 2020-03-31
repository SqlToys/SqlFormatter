(* $Header: /SQL Toys/units/GtContainers.pas 115   19-03-24 21:50 Tomek $
   (c) Tomasz Gierka, github.com/SqlToys, 2010.10.15                          *)
{--------------------------------------  --------------------------------------}
{ This unit provides simple item class and an items list class                 }
{ Each item and class can have individual name                                 }
{ For each item and list can be enabled class reference tracking               }
{--------------------------------------  --------------------------------------}
{ DEBUGINFO OFF}
{ LOCALSYMBOLS OFF}
unit GtContainers;

interface

uses Classes, Contnrs, SysUtils
{$IFDEF GtGarbageCollector}
   , GtGarbageCollector
{$ENDIF} ;

{------------------------------ Class Hierarchy -------------------------------}

{  TGtItem                                                                     }
{    TGtUniList                                                                }
{      TGtTemporaryItemList                                                    }

{--------------------------------- Item Class ---------------------------------}
{ FReferencedBy - lista klas odwolujacych sie do tej klasy, z wylaczeniem Owner}
{    klasa jest dodawana do listy referencji ownera, jesli Owner nie jest lista}

type
  TGtItemClass = class of TGtItem;
  TGtItem = class {$IFDEF GtGarbageCollector} (TGtGcItem) {$ENDIF}
  private
    FOwner: TGtItem;
    FReferencedBy: TObjectList;
  protected
    EnableReferences: Boolean;

    { name management }
    FName: String;

    function    GetName: String; virtual;
    procedure   SetName(aName: String);
  protected
    { item could be destroyed when orphaned AND not referenced }
    function    CanDestroy: Boolean; {$IFDEF GtGarbageCollector} override; {$ELSE} virtual; {$ENDIF}

    { childs management }
    function    RemoveItem(aItem: TGtItem): Boolean; virtual;

    { adds and removes items referencing this item }
    procedure   AddReference(aReferencedBy: TGtItem); virtual;
    procedure   RemoveReference(aReferencedBy: TGtItem); virtual;
  public
    { class constructors and destructors }
    constructor Create; overload; {$IFDEF GtGarbageCollector} override; {$ELSE} virtual; {$ENDIF}
    constructor Create(aOwner: TGtItem); reintroduce; overload; virtual;
    constructor Create(aOwner: TGtItem; aName: String); reintroduce; overload; virtual;
    destructor  Destroy; override;

    { ownership management }
    procedure   SetOwner(aOwner: TGtItem); virtual;
    procedure   RemoveOwner; virtual;

    function    IsReferenced: Boolean; virtual;

    { properties }
    property    Name: String read GetName write SetName;
    property    Owner: TGtItem read FOwner write SetOwner;
    property    ReferencedBy: TObjectList read FReferencedBy;
  end;

{----------------------------- Unified List Class -----------------------------}

  TGtItemProc = procedure (aItem: TGtItem);

  TGtUniList = class (TGtItem)
  private
    FObjectList: TObjectList;
    FStringList: TStringList;
  protected
    FUseAsStringList: Boolean;

    procedure   CreateList;
    function    GetCount: Integer;
    function    GetCapacity: Integer;
    procedure   SetCapacity(aCapacity: Integer);
  public
    constructor Create(aOwner: TGtItem); override;
    constructor CreateAsNameList(aOwner: TGtItem); virtual;
    destructor  Destroy; override;

    { list management }
    procedure   Clear;

    procedure   AddItem(aItem: TGtItem); virtual;
    procedure   AddItemBefore(aNewItem, aExistingItem: TGtItem); virtual;
    procedure   AddItemAfter(aNewItem, aExistingItem: TGtItem); virtual;
    function    RemoveItem(aItem: TGtItem): Boolean; override;
    function    RemoveIndex(aIndex: Integer): Boolean; virtual;
    function    GetItem(aIndex: Integer): TGtItem;
    function    GetIndex(aItem: TGtItem): Integer;
    function    FindItem(aItem: TGtItem): Boolean;

    { name list management }
    procedure   AddName(aItem: TGtItem; aName: String=''); overload;
    procedure   ChangeItemName(aItem: TGtItem; aName: String);

    { properties }
    property    Count: Integer read GetCount;
    property    Capacity: Integer read GetCapacity write SetCapacity;
    property    Items[Index: Integer]: TGtItem read GetItem; default;
  end;

{-------------------------- Temporary Item List Class -------------------------}

  TGtTemporaryItemList = class (TGtUniList);

{---------------------------------- General -----------------------------------}

type
  EUniListDuplicate = class (Exception);

{------------------------------ Resource strings ------------------------------}

resourcestring
  gtstrInvalidClassConstructorCall     = 'Invalid class construtor call.';
  gtstrClassOwnerAlreadySet            = 'Class owner already set.';

{$IFDEF GtUniListDebug}
  gtstrObjListDuplicate                = ' -- ObjectList duplicate';
  gtstrStrListDuplicate                = ' -- StringList duplicate';
{$ENDIF}

implementation

uses System.Types;

{--------------------------------- Item Class ---------------------------------}

{ class constructor }
constructor TGtItem.Create;
begin
  raise Exception.Create( gtstrInvalidClassConstructorCall );
end;

{ class constructor }
constructor TGtItem.Create(aOwner: TGtItem);
begin
  inherited Create;

  FReferencedBy := nil;
  EnableReferences := False;

  SetOwner(aOwner);
end;

{ class constructor }
constructor TGtItem.Create(aOwner: TGtItem; aName: String);
begin
  Create(aOwner);

  SetName( aName );
end;

{ class destructor }
destructor  TGtItem.Destroy;
begin
  { removes owner reference }
  RemoveOwner;

  { removes parents reference }
  if Assigned(FReferencedBy) then begin
    while FReferencedBy.Count > 0 do RemoveReference( FReferencedBy[0] as TGtItem );
    FReferencedBy.Free;
  end;

  inherited Destroy;
end;

{ gets name }
function    TGtItem.GetName: String;
begin
  Result := FName;
end;

{ sets name }
procedure   TGtItem.SetName(aName: String);
begin
  if aName = FName then Exit;
  FName := aName;

  {$IFDEF GtGarbageCollector}
  GcLogOp( gtgcSetName );
  {$ENDIF}

  { zmiana na liscie Owner-a }
  if Assigned(Owner) and (Owner is TGtUniList) and (TGtUniList(Owner).FUseAsStringList)
    then TGtUniList(Owner).ChangeItemName(Self, aName);
end;

{ check if class could be easily destroyed }
{ only orphaned classes not referenced by others }
function    TGtItem.CanDestroy: Boolean;
begin
  Result := not Assigned(FOwner) and
           (not Assigned(FReferencedBy) or (FReferencedBy.Count = 0));
end;

{ removes item reference to this class }
function    TGtItem.RemoveItem(aItem: TGtItem): Boolean;
begin
  Result := False;
  if not Assigned(aItem) then Exit;

  aItem.RemoveReference( Self );

  {$IFDEF GtGarbageCollector}
  GcLogOp( gtgcRemove, aItem );
  {$ENDIF}
end;

{ links item to another list, saves that linkage for further use }
procedure   TGtItem.AddReference(aReferencedBy: TGtItem);
begin
  if not EnableReferences then Exit;
  if aReferencedBy is TGtTemporaryItemList then Exit;
  if aReferencedBy = FOwner then Exit;

  if Assigned(FReferencedBy) and (FReferencedBy.IndexOf(aReferencedBy) <> -1) then Exit;
  if not Assigned(FReferencedBy) then FReferencedBy := TObjectList.Create(False);
  FReferencedBy.Add(aReferencedBy);

  {$IFDEF GtGarbageCollector}
  GcLogOp( gtgcSetReference, aReferencedBy );
  {$ENDIF}
end;

{ removes link to parent class, returns true if there are no parents for that item }
procedure TGtItem.RemoveReference(aReferencedBy: TGtItem);
begin
  if not Assigned(FReferencedBy) or (FReferencedBy.Count = 0)then Exit;
  if FReferencedBy.Remove(aReferencedBy) =-1 then Exit;

  if aReferencedBy is TGtUniList then aReferencedBy.RemoveItem( Self );

  {$IFDEF GtGarbageCollector}
  GcLogOp( gtgcRemoveReference, aReferencedBy );
  {$ENDIF}
end;

{ checks if item is referenced }
function TGtItem.IsReferenced: Boolean;
begin
  Result := Assigned(FReferencedBy) and (FReferencedBy.Count > 0);
end;

{ sets new owner for item }
procedure TGtItem.SetOwner(aOwner: TGtItem);
begin
  if FOwner = aOwner then Exit;
  if Assigned(FOwner) then raise Exception.Create( gtstrClassOwnerAlreadySet );

  FOwner := aOwner;

  if aOwner is TGtUniList
    then TGtUniList(aOwner).AddItem(Self)
    else aOwner.AddReference(Self);

  {$IFDEF GtGarbageCollector}
  GcLogOp( gtgcSetOwner, aOwner );
  {$ENDIF}
end;

{ removes a owner for item }
procedure TGtItem.RemoveOwner;
begin
  {$IFDEF GtGarbageCollector}
  if not GtGcAssigned(FOwner) then Exit;
  {$ELSE}
  if not Assigned(FOwner) then Exit;
  {$ENDIF}

  if FOwner is TGtUniList
    then TGtUniList(FOwner).RemoveItem( Self )
    else FOwner.RemoveReference( Self );

  FOwner := nil;

  {$IFDEF GtGarbageCollector}
  GcLogOp( gtgcRemoveOwner, FOwner );
  {$ENDIF}
end;

{----------------------------- Unified List Class -----------------------------}

{ class constructor }
constructor TGtUniList.Create(aOwner: TGtItem);
begin
  inherited Create(aOwner);

  FUseAsStringList := False;
end;

constructor TGtUniList.CreateAsNameList(aOwner: TGtItem);
begin
  inherited Create(aOwner);

  FUseAsStringList := True;
end;

{ class destructor, frees only items which are not connected to other lists }
destructor  TGtUniList.Destroy;
begin
  Clear;
  if Assigned(FObjectList) then FreeAndNil(FObjectList);
  if Assigned(FStringList) then FreeAndNil(FStringList);

  inherited Destroy;
end;

{ creates list }
procedure TGtUniList.CreateList;
begin
  if FUseAsStringList and not Assigned(FStringList) then begin
    FStringList := TStringList.Create;
    FStringList.Duplicates := dupError;
    FStringList.Sorted := True;
  end else
  if not FUseAsStringList and not Assigned(FObjectList) then begin
    FObjectList := TObjectList.Create(False);
  end;
end;

{ gets lists count }
function TGtUniList.GetCount: Integer;
begin
  Result := 0;
  if Assigned(FObjectList) then Result := FObjectList.Count else
  if Assigned(FStringList) then Result := FStringList.Count;
end;

{ gets list capacity }
function TGtUniList.GetCapacity: Integer;
begin
  Result := 0;
  if Assigned(FObjectList) then Result := FObjectList.Capacity else
  if Assigned(FStringList) then Result := FStringList.Capacity;
end;

{ sets capacity }
procedure TGtUniList.SetCapacity(aCapacity: Integer);
begin
  CreateList;
  if Assigned(FObjectList) then FObjectList.Capacity := aCapacity else
  if Assigned(FStringList) then FStringList.Capacity := aCapacity;
end;

{ removes all items from list }
procedure TGtUniList.Clear;
var i, lCount: Integer;
    Item: TGtItem;
begin
  i := Count;
  while i > 0 do begin
    Dec(i);
    Item := GetItem( i );

    lCount := Count;
    RemoveItem(Item);
    i := i - (lCount - Count - 1); { if removed more than one item }

    {$IFDEF GtUniListDebug}
    if lCount - Count > 1 then ShowMessage(ClassName + ' -- item added ' + IntToStr(lCount - Count) + ' times.');
    {$ENDIF}

    if Assigned(Item) then begin
      Item.RemoveReference( Self );

      if Item.Owner = Self then Item.Free else
      if Item.CanDestroy then Item.Free;
    end;
  end;
end;

{ adds item to list }
procedure TGtUniList.AddItem(aItem: TGtItem);
begin
  if not Assigned(aItem) then Exit;

  {$IFDEF GtUniListDebug}
  if Assigned(FObjectList) and (FObjectList.IndexOf(aItem) <> -1)
    then raise EUniListDuplicate.Create(ClassName + gtstrObjListDuplicate);

  if Assigned(FStringList) and (FStringList.IndexOfObject(aItem) <> -1)
    then raise EUniListDuplicate.Create(ClassName + gtstrStrListDuplicate);
  {$ENDIF}

  CreateList;

  if Assigned(FObjectList) then FObjectList.Add(aItem) else
  if Assigned(FStringList) then FStringList.AddObject(aItem.Name, aItem);

  aItem.AddReference( Self );

  {$IFDEF GtGarbageCollector}
  GcLogOp( gtgcAdd, aItem );
  {$ENDIF}
end;

{ adds item to list, before existing item }
procedure TGtUniList.AddItemBefore(aNewItem, aExistingItem: TGtItem);
var i: Integer;
begin
  if not Assigned(aNewItem) then Exit;
  if not Assigned(aExistingItem) then Exit;

  {$IFDEF GtUniListDebug}
  if Assigned(FObjectList) and (FObjectList.IndexOf(aItem) <> -1)
    then raise EUniListDuplicate.Create(ClassName + gtstrObjListDuplicate);

  if Assigned(FStringList) and (FStringList.IndexOfObject(aItem) <> -1)
    then raise EUniListDuplicate.Create(ClassName + gtstrStrListDuplicate);
  {$ENDIF}

  CreateList;

  if Assigned(FObjectList) then begin
    i := FObjectList.IndexOf(aExistingItem);
    if i <> -1 then begin
      FObjectList.Insert(i, aNewItem);
      aNewItem.AddReference( Self );
    end;
  end else
  if Assigned(FStringList) then begin
    i := FStringList.IndexOfObject(aExistingItem);
    if i <> -1 then begin
      FStringList.InsertObject(i, aNewItem.Name, aNewItem);
      aNewItem.AddReference( Self );
    end;
  end;

  {$IFDEF GtGarbageCollector}
  GcLogOp( gtgcAdd, aItem );
  {$ENDIF}
end;

{ adds item to list, after existing item }
procedure TGtUniList.AddItemAfter(aNewItem, aExistingItem: TGtItem);
var i: Integer;
begin
  if not Assigned(aNewItem) then Exit;
  if not Assigned(aExistingItem) then Exit;

  {$IFDEF GtUniListDebug}
  if Assigned(FObjectList) and (FObjectList.IndexOf(aItem) <> -1)
    then raise EUniListDuplicate.Create(ClassName + gtstrObjListDuplicate);

  if Assigned(FStringList) and (FStringList.IndexOfObject(aItem) <> -1)
    then raise EUniListDuplicate.Create(ClassName + gtstrStrListDuplicate);
  {$ENDIF}

  CreateList;

  if Assigned(FObjectList) then begin
    i := FObjectList.IndexOf(aExistingItem);
    if i <> -1 then begin
      FObjectList.Insert(i+1, aNewItem);
      aNewItem.AddReference( Self );
    end;
  end else
  if Assigned(FStringList) then begin
    i := FStringList.IndexOfObject(aExistingItem);
    if i <> -1 then begin
      FStringList.InsertObject(i+1, aNewItem.Name, aNewItem);
      aNewItem.AddReference( Self );
    end;
  end;

  {$IFDEF GtGarbageCollector}
  GcLogOp( gtgcAdd, aItem );
  {$ENDIF}
end;

{ removes item from list }
function TGtUniList.RemoveIndex(aIndex: Integer): Boolean;
begin
  Result := False;
  if (aIndex < 0) or (aIndex >= Count) then Exit;
  if(not Assigned(FObjectList) or (FObjectList.Count = 0)) and
    (not Assigned(FStringList) or (FStringList.Count = 0)) then Exit;

  inherited RemoveItem( Items[aIndex] );

  if Assigned(FObjectList) then begin
    FObjectList.Delete(aIndex);
    Result := True;
  end else
  if Assigned(FStringList) then begin
    FStringList.Delete(aIndex);
    Result := True;
  end;
end;

{ removes item from list }
function TGtUniList.RemoveItem(aItem: TGtItem): Boolean;
var i: Integer;
begin
  Result := False;
  if not Assigned(aItem) then Exit;
  if(not Assigned(FObjectList) or (FObjectList.Count = 0)) and
    (not Assigned(FStringList) or (FStringList.Count = 0)) then Exit;

  inherited RemoveItem(aItem);

  if Assigned(FObjectList) then begin
    i := FObjectList.IndexOf(aItem);
    if i > -1 then begin
      FObjectList.Delete(i);
      Result := True;
    end;
  end else
  if Assigned(FStringList) then begin
    i := FStringList.IndexOfObject(aItem);
    if i > -1 then begin
      FStringList.Delete(i);
      Result := True;
    end;
  end;
end;

{ gets item }
function TGtUniList.GetItem(aIndex: Integer): TGtItem;
begin
  Result := nil;
  if Assigned(FObjectList) then Result := FObjectList[aIndex] as TGtItem else
  if Assigned(FStringList) then Result := FStringList.Objects[aIndex] as TGtItem;
end;

{ gets item index }
function TGtUniList.GetIndex(aItem: TGtItem): Integer;
begin
  Result := -1;
  if Assigned(FObjectList) then Result := FObjectList.IndexOf(aItem) else
  if Assigned(FStringList) then Result := FStringList.IndexOfObject(aItem);
end;

{ checks if item is on list }
function TGtUniList.FindItem(aItem: TGtItem): Boolean;
begin
  Result := GetIndex (aItem) > -1;
end;

{ adds item to list }
procedure TGtUniList.AddName(aItem: TGtItem; aName: String='');
begin
  if not Assigned(aItem) then Exit;

  if aName='' then aName := aItem.FName;
  if aItem.FName='' then aItem.FName := aName;

  AddItem(aItem);
end;

{ changes item name on list }
procedure TGtUniList.ChangeItemName(aItem: TGtItem; aName: String);
begin
  RemoveItem(aItem);
  AddName(aItem, aName);
end;

end.

