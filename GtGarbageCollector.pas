(* $Header: /SQL Parser/pas/GtGarbageCollector.pas 5     17-12-13 18:09 Tomek $
   (c) Tomasz Gierka, github.com/SqlToys, 2011.01.06 - 2015.03.15 conditional *)
{--------------------------------------  --------------------------------------}
{$DEBUGINFO OFF}
{$LOCALSYMBOLS OFF}
unit GtGarbageCollector;

interface

uses Classes, Contnrs, SysUtils;

{------------------------------ Class Hierarchy -------------------------------}

{TObjectList                        EXT unit.Contnrs                           }
{  TGtGcList                            CleanUp, Destroy, Log                  }

{TGtGcItem                              adds & removes item from Garbage Coll. }

{----------------------------- Garbage Collector ------------------------------}
{ UWAGA: Garbage Collector nie ma funkcji kompaktowania listy                  }
{        lista stale siê rozrasta przy dodawaniu kolejnych elementow           }
{        i nie kurczy sie przy kasowaniu kolejnych elementow                   }

function GtGcAssigned(aItem: TObject): Boolean;

type
  TGtGcOper = ( gtgcCreate, gtgcDestroy, gtgcSetOwner, gtgcRemoveOwner,
                gtgcSetReference, gtgcRemoveReference,
                gtgcAdd, gtgcRemove, gtgcSetName );

{------------------------ Garbage Collector Item Class ------------------------}
type
  TGtGcList = class;

  TGtGcItem = class
  protected
    FGarbageColl: TGtGcList;

    function        ClassToLogStr( aClass: TGtGcItem=nil): String;
    procedure       GcLogOp( aOp: TGtGcOper; aClass: TGtGcItem=nil; aComment: String=''); virtual;
  public
    ClassNo: Integer;

    constructor     Create; virtual;
    destructor      Destroy; override;

    { basic item always could be destroyed }
    function        CanDestroy: Boolean; virtual;
  end;

{------------------------ Garbage Collector List Class ------------------------}

  TGtGcList = class (TObjectList)
  private
    FLogList: TStringList;
    FLastLogClass: TGtGcItem;

    CreateCounter, DestroyCounter,
    AddCounter,    RemoveCounter,
    SetRefCounter, RemoveRefCounter: Integer;
  public
    ClassCounter: Integer;
    SaveLogOnEachOperation: Boolean;
    DoNotLog: Boolean;

    constructor Create; virtual;
    destructor  Destroy; override;

    procedure   CleanUp;

    function    Add(aObject: TObject): Integer;
    function    Remove(aObject: TObject): Integer;

    procedure   StartLog;
    procedure   ClearLog;
    procedure   Log(aLogClass: TGtGcItem; aLog: String);
    procedure   LogCounters;
    procedure   SaveLog(aFileName: String='');
    procedure   SaveState(SL: TStrings);
  end;

{---------------------------------- General -----------------------------------}

var GarbageCollector: TGtGcList;

procedure GarbColl_SetClassCounter( aValue: Integer );

{------------------------------ Resource strings ------------------------------}

resourcestring
  gtstrGarbCollAddedClassNotATGtGcItem = 'Garbage Collector: Added class is not a TGtGcItem.';

  gtstrGarbCollAdd                     = 'ADD             : ';
  gtstrGarbCollCreate                  = 'CREATE          : ';
  gtstrGarbCollDestroy                 = 'DESTROY         : ';
  gtstrGarbCollRemove                  = 'REMOVE          : ';
  gtstrGarbCollRemoveOwner             = 'REMOVE OWNER    : ';
  gtstrGarbCollRemoveReference         = 'REMOVE REFERENCE: ';
  gtstrGarbCollSetName                 = 'SET NAME        : ';
  gtstrGarbCollSetOwner                = 'SET OWNER       : ';
  gtstrGarbCollSetReference            = 'SET REFERENCE   : ';

  gtstrGarbCollCountAdd                = 'ADD        COUNT: ';
  gtstrGarbCollCountCreate             = 'CREATE     COUNT: ';
  gtstrGarbCollCountDestroy            = 'DESTROY    COUNT: ';
  gtstrGarbCollCountRemove             = 'REMOVE     COUNT: ';
  gtstrGarbCollCountRemoveReference    = 'REMOVE REF COUNT: ';
  gtstrGarbCollCountSetReference       = 'SET REF    COUNT: ';

  gtstrGarbCollItem                    = 'ITEM: ';
  gtstrGarbCollReferencedBy            = 'REFERENCED BY: ';
  gtstrGarbCollOwner                   = 'OWNER: ';

  gtstrGarbCollCleanupStart            = 'CLEANUP START';
  gtstrGarbCollCleanupStop             = 'CLEANUP STOP';

  gtstrGarbCollDestructorStart         = 'GARBAGE COLLECTOR DESTRUCTOR START';
  gtstrGarbCollDestructorStop          = 'GARBAGE COLLECTOR DESTRUCTOR STOP';

implementation

{----------------------------- Garbage Collector ------------------------------}
// var GarbageCollector: TGtGcList;

{ checks if item is assigned (delphi mean) and is on Garbage Collector list }
function GtGcAssigned(aItem: TObject): Boolean;
begin
  Result := Assigned(aItem);
  if not Result or not Assigned(GarbageCollector) then Exit;

  Result := GarbageCollector.IndexOf(aItem) <> -1;
end;

{------------------------ Garbage Collector Item Class ------------------------}

{ class constructor }
constructor TGtGcItem.Create;
begin
  inherited Create;

  FGarbageColl := GarbageCollector;
  FGarbageColl.Add( Self );

  if Assigned(FGarbageColl) then begin
    Inc(FGarbageColl.ClassCounter);
    ClassNo := FGarbageColl.ClassCounter;
  end;

  GcLogOp( gtgcCreate );
end;

{ class destructor }
destructor  TGtGcItem.Destroy;
begin
  GcLogOp( gtgcDestroy );
  FGarbageColl.Remove( Self );

  inherited Destroy;
end;

{ checks if class could be destroyed }
function TGtGcItem.CanDestroy: Boolean;
begin
  Result := True;//False;
end;

{ gets log string for class }
function    TGtGcItem.ClassToLogStr( aClass: TGtGcItem=nil): String;
var lName, lCount, lOwner: String;
begin
  Result := '';
  if not Assigned(aClass) then Exit;
  if not GtGcAssigned(aClass) then Exit;

  Result := IntToStr( aClass.ClassNo ) + '=';
  if Result = '-1=' then Result := 'X=';

  Result := Result + aClass.ClassName;

  lName := '';
  lCount := '';
  lOwner := '';
//  if aClass is TGtItem then begin
//     lName := TGtItem(aClass).Name;
//     if GtGcAssigned(TGtItem(aClass).Owner) then lOwner := IntToStr( TGtItem(aClass).Owner.ClassNo );
//  end;
//  if aClass is TGtUniList then
//  try
//    lCount := IntToStr(TGtUniList(aClass).Count);
//  except
//    lCount := '';
//  end;

  if lName <> '' then lName := 'Name=' + lName;
  if lOwner <> '' then begin
    lOwner := 'Owner=' + lOwner;
    if lName <> '' then lName := lName + ',';
  end;
  if lCount <> '' then begin
    lCount := 'Count=' + lCount;
    if lOwner <> '' then lOwner := lOwner + ',' else
    if lName <> '' then lName := lName + ',';
  end;

  if lName <> '' then Result := Result + '(' + lName + lOwner + lCount + ')';
end;

{ logs garbage collector relevant operations }
procedure   TGtGcItem.GcLogOp( aOp: TGtGcOper; aClass: TGtGcItem=nil; aComment: String='');
var lOp, lClass, lExt2, lClass2: String;
begin
  if FGarbageColl.DoNotLog then Exit;

  case aOp of
    gtgcCreate         : Inc( FGarbageColl.CreateCounter );
    gtgcDestroy        : Inc( FGarbageColl.DestroyCounter );
    gtgcAdd            : Inc( FGarbageColl.AddCounter );
    gtgcRemove         : Inc( FGarbageColl.RemoveCounter );
    gtgcSetReference   : Inc( FGarbageColl.SetRefCounter );
    gtgcRemoveReference: Inc( FGarbageColl.RemoveRefCounter );
  end;

  case aOp of
    gtgcCreate         : lOp := gtstrGarbCollCreate;
    gtgcDestroy        : lOp := gtstrGarbCollDestroy;
    gtgcSetOwner       : lOp := gtstrGarbCollSetOwner;
    gtgcRemoveOwner    : lOp := gtstrGarbCollRemoveOwner;
    gtgcSetReference   : lOp := gtstrGarbCollSetReference;
    gtgcRemoveReference: lOp := gtstrGarbCollRemoveReference;
    gtgcAdd            : lOp := gtstrGarbCollAdd;
    gtgcRemove         : lOp := gtstrGarbCollRemove;
    gtgcSetName        : lOp := gtstrGarbCollSetName;
  else                   lOp := '';
  end;

  lClass := ClassToLogStr( Self );
  lExt2  := '';
  lClass2 := ClassToLogStr( aClass );

  if lClass2 <> '' then begin
    lExt2   := ' -- ';
    if aOp in [gtgcSetOwner, gtgcRemoveOwner] then lExt2 := lExt2 + gtstrGarbCollOwner else
    if aOp in [gtgcSetReference, gtgcRemoveReference] then lExt2 := lExt2 + gtstrGarbCollReferencedBy else
    if aOp in [gtgcAdd, gtgcRemove] then lExt2 := lExt2 + gtstrGarbCollItem;
  end;

  if aComment <> '' then aComment := ' -- ' + aComment;

  FGarbageColl.Log( Self, lOp + lClass + lExt2 + lClass2 + aComment );
end;

{------------------------ Garbage Collector List Class ------------------------}

{ class contructor }
constructor TGtGcList.Create;
begin
  inherited Create(False);

  FLastLogClass := nil;
  FLogList := nil;

  ClassCounter     := 0;
  SaveLogOnEachOperation := False;
  DoNotLog := False;

  CreateCounter    := 0;
  DestroyCounter   := 0;
  AddCounter       := 0;
  RemoveCounter    := 0;
  SetRefCounter    := 0;
  RemoveRefCounter := 0;
end;

{ class destructor }
destructor  TGtGcList.Destroy;
var i: Integer;
    Item: TGtGcItem;
begin
  Log( nil, gtstrGarbCollDestructorStart );

  i := Count -1;
  while i >= 0 do begin
    if Assigned(Items[i]) and (Items[i] is TGtGcItem) then begin
      Item := Items[i] as TGtGcItem;

      if GtGcAssigned(Item) then Item.Free;
      if SaveLogOnEachOperation then SaveLog;
    end;

    Dec(i);
    if i >= Count then i := Count -1;
  end;

  if SaveLogOnEachOperation then begin
    Log( nil, gtstrGarbCollDestructorStop );
    LogCounters;
    SaveLog;
  end;

  if Assigned(FLogList) then FLogList.Free;

  inherited Destroy;
end;

{ cleans up garbage collection }
procedure   TGtGcList.CleanUp;
var i: Integer;
    Item: TGtGcItem;
begin
  Log( nil, gtstrGarbCollCleanupStart );

  i := Count - 1;
  while i >= 0 do begin
    Item := Items[i] as TGtGcItem;
    if (Item is TGtGcItem) and Item.CanDestroy then begin
      Delete(i);
      Item.Free;
    end else begin
      Dec(i);
      if SaveLogOnEachOperation then SaveLog;
    end;
  end;

  Log( nil, gtstrGarbCollCleanupStop );
end;

{ adds item to a GarbageCollection list }
function    TGtGcList.Add(aObject: TObject): Integer;
begin
  if aObject is TGtGcItem
    then Result := inherited Add(aObject)
    else raise Exception.Create( gtstrGarbCollAddedClassNotATGtGcItem );
end;

{ removes item from a GarbageCollection list }
function    TGtGcList.Remove(aObject: TObject): Integer;
begin
  Result := IndexOf(aObject);
  if Result < 0 then Exit;
  Items[ Result ] := nil;
end;

{ start logging }
procedure   TGtGcList.StartLog;
begin
  if not Assigned(FLogList) then FLogList := TStringList.Create;
end;

{ clears log }
procedure   TGtGcList.ClearLog;
begin
  if not Assigned(FLogList) then Exit;

  FreeAndNil(FLogList);
  StartLog;
end;

{ logs }
procedure   TGtGcList.Log(aLogClass: TGtGcItem; aLog: String);
begin
  if DoNotLog then Exit;

  if FLastLogClass <> aLogClass then begin
    FLogList.Add('');
    FLastLogClass := aLogClass;
  end;

  if Assigned(FLogList) then FLogList.Add(aLog);
end;

{ logs counter numbers }
procedure TGtGcList.LogCounters;
begin
  FLogList.Add( '' );
  FLogList.Add( gtstrGarbCollCountCreate          + IntToStr( CreateCounter   ) );
  FLogList.Add( gtstrGarbCollCountDestroy         + IntToStr( DestroyCounter  ) );
  FLogList.Add( gtstrGarbCollCountAdd             + IntToStr( AddCounter      ) );
  FLogList.Add( gtstrGarbCollCountRemove          + IntToStr( RemoveCounter   ) );
  FLogList.Add( gtstrGarbCollCountSetReference    + IntToStr( SetRefCounter   ) );
  FLogList.Add( gtstrGarbCollCountRemoveReference + IntToStr( RemoveRefCounter) );
end;

{ saves Garbage Collector list }
procedure   TGtGcList.SaveLog(aFileName: String='');
begin
  if Assigned(FLogList) then begin
    if aFileName = '' then aFileName := 'TEST\Garbage Collector.log';
    FLogList.SaveToFile( aFileName );
  end;
end;

{ saves current Garbage Collector state }
procedure   TGtGcList.SaveState(SL: TStrings);
var i: Integer;
    s: String;
begin
  for i := 0 to Count - 1 do begin
    s := TGtGcItem(Items[i]).ClassToLogStr(TGtGcItem(Items[i]));
    if s <> '' then SL.Add( s );
  end;
end;

{---------------------------------- General -----------------------------------}

procedure GarbColl_SetClassCounter( aValue: Integer );
begin
  if Assigned(GarbageCollector) then GarbageCollector.ClassCounter := aValue;
end;

{---------------------------- Module Specific Code ----------------------------}

initialization
  GarbageCollector := TGtGcList.Create;
  GarbageCollector.StartLog;
finalization
  GarbageCollector.SaveLogOnEachOperation := False;//True;
  GarbageCollector.CleanUp;
  GarbageCollector.Free;
end.

