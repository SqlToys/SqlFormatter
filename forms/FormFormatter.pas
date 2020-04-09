(* $Header: /SQL Toys/forms/FormFormatter.pas 73    19-04-20 13:33 Tomek $
   (c) Tomasz Gierka, github.com/SqlToys, 2011.07.24                          *)
{--------------------------------------  --------------------------------------}
{$IFDEF RELEASE}
  {$DEBUGINFO OFF}
  {$LOCALSYMBOLS OFF}
{$ENDIF}
unit FormFormatter;

interface

uses Forms, ActnList, Menus, ComCtrls, Controls, ExtCtrls, Classes, Messages,
     System.Actions,
     Vcl.Ribbon, Vcl.ToolWin, Vcl.ImgList,
     Vcl.ActnMenus, Vcl.RibbonActnMenus, Vcl.StdActns,Vcl.ActnMan, Vcl.ActnCtrls,
     Vcl.RibbonLunaStyleActnCtrls, Vcl.PlatformDefaultStyleActnCtrls,
     SqlNode,
     FrameScriptEdit, Vcl.RibbonObsidianStyleActnCtrls, Vcl.RibbonSilverStyleActnCtrls;

type
  TMainForm = class(TForm)
    ActionList: TActionList;
    actViewFontSizeUp: TAction;
    actViewFontSizeDown: TAction;

    FrameScriptEdit: TFrameScriptEdit;
    StatusBar: TStatusBar;
    TimerMain: TTimer;
    ActionManager: TActionManager;
    actMenuMinimize: TAction;

    actFilesNew: TAction;
    actFilesOpen: TAction;
    actFilesSave: TAction;
    actFilesSaveAs: TAction;
    actFilesExit: TAction;

    actEditPaste: TAction;
    actEditCut: TAction;
    actEditCopy: TAction;

    actEditFindReplace: TAction;

    actToolsFormat: TAction;
    actToolsCompact: TAction;
    actToolsQuoteToDelphi: TAction;
    actToolsQuoteFromDelphi: TAction;

    actHighlightQuery: TAction;
    actHighlightPrevQuery: TAction;
    actHighlightNextQuery: TAction;

    actToolsSettings: TAction;
    actToolsColors: TAction;
    actToolsFont: TAction;
    actToolsFullScreen: TAction;

    actHelpAbout: TAction;
    actHelpTest: TAction;

    ImageList16: TImageList;
    ImageList32: TImageList;
    ImageList16disabled: TImageList;
    ImageList32disabled: TImageList;

    Ribbon: TRibbon;
    RibbonQuickAccessToolbar1: TRibbonQuickAccessToolbar;
    RibbonApplicationMenuBar1: TRibbonApplicationMenuBar;

    RibbonPageGeneral: TRibbonPage;
    RibbonGroupClipboard: TRibbonGroup;
    RibbonGroupFindReplace: TRibbonGroup;
    RibbonGroupFormat: TRibbonGroup;
    RibbonGroupHighlight: TRibbonGroup;
    RibbonGroupPreferences: TRibbonGroup;
    RibbonGroupAbout: TRibbonGroup;

    RibbonPageConverters: TRibbonPage;

    RibbonGroupConvertSortOrder: TRibbonGroup;
    actConvertSortOrderShortKeywords: TAction;
    actConvertSortOrderLongKeywords: TAction;
    actConvertSortOrderAddDefaultKeywords: TAction;
    actConvertSortOrderRemoveDefaultKeywords: TAction;

    RibbonGroupConvertJoins: TRibbonGroup;
    actConvertJoinsAddInner: TAction;
    actConvertJoinsRemoveInner: TAction;
    actConvertJoinsAddOuter: TAction;
    actConvertJoinsRemoveOuter: TAction;
    actConvertReset: TAction;
    RibbonPageCase: TRibbonPage;
    actFilesExportXML: TAction;
    actFilesImportXml: TAction;
    actConvertJoinCondRefToLeft: TAction;
    RibbonGroupCases: TRibbonGroup;
    actConvertCaseKeyword: TAction;
    actConvertCaseTableNames: TAction;
    actConvertCaseTableAliases: TAction;
    actConvertCaseColumnNames: TAction;
    actConvertCaseColumnAliases: TAction;
    actConvertCaseParameters: TAction;
    actConvertCaseFunctionNames: TAction;
    actConvertCaseViewNames: TAction;
    actConvertCaseConstraintNames: TAction;
    actConvertCaseSynonymNames: TAction;
    actConvertCaseTransactionNames: TAction;
    actConvertCaseFunParamNames: TAction;
    actConvertCaseExtQueryAliases: TAction;
    actConvertCaseIdentifierNames: TAction;
    RibbonGroupSpaces: TRibbonGroup;
    actConvertSpacesBeforeSemicolon: TAction;
    actConvertSpacesBeforeComma: TAction;
    actConvertSpacesAfterComma: TAction;
    actConvertSpacesAroundOperator: TAction;
    actConvertSpacesInsideBrackets: TAction;
    actConvertSpacesOutsideBrackets: TAction;
    RibbonGroupKeywords: TRibbonGroup;
    actConvertKeywordExprAs: TAction;
    actConvertKeywordTableAs: TAction;
    actConvertKeywordInner: TAction;
    actConvertKeywordOuter: TAction;

    { form & control events }
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);

    procedure WMDropFiles(var Msg: TWMDropFiles); message WM_DROPFILES;

    procedure actViewFontSizeUpExecute(Sender: TObject);
    procedure actViewFontSizeDownExecute(Sender: TObject);

    procedure TimerMainTimer(Sender: TObject);

    procedure ActionManagerUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure actMenuMinimizeExecute(Sender: TObject);

    procedure actToolsSettingsExecute(Sender: TObject);
    procedure actToolsColorsExecute(Sender: TObject);
    procedure actToolsFontExecute(Sender: TObject);
    procedure actViewFullScreenExecute(Sender: TObject);

    procedure actHelpAboutExecute(Sender: TObject);
    procedure actHelpTestExecute(Sender: TObject);

    procedure RibbonRecentItemClick(Sender: TObject; FileName: string; Index: Integer);

    procedure actToolsQuoteToDelphiExecute(Sender: TObject);
    procedure actToolsQuoteFromDelphiExecute(Sender: TObject);

    procedure actConvertExprAliasAddKeywordAsExecute(Sender: TObject);
    procedure actConvertSortOrderShortKeywordsExecute(Sender: TObject);
    procedure actConvertSortOrderLongKeywordsExecute(Sender: TObject);
    procedure actConvertSortOrderAddDefaultKeywordsExecute(Sender: TObject);
    procedure actConvertSortOrderRemoveDefaultKeywordsExecute(Sender: TObject);

    procedure actConvertJoinsAddInnerExecute(Sender: TObject);
    procedure actConvertJoinsRemoveInnerExecute(Sender: TObject);
    procedure actConvertJoinsAddOuterExecute(Sender: TObject);
    procedure actConvertJoinsRemoveOuterExecute(Sender: TObject);
    procedure actConvertResetExecute(Sender: TObject);
    procedure actConvertJoinCondRefToLeftExecute(Sender: TObject);
    procedure actConvertCaseKeywordExecute(Sender: TObject);
    procedure actConvertCaseTableNamesExecute(Sender: TObject);
    procedure actConvertCaseTableAliasesExecute(Sender: TObject);
    procedure actConvertCaseColumnNamesExecute(Sender: TObject);
    procedure actConvertCaseColumnAliasesExecute(Sender: TObject);
    procedure actConvertCaseParametersExecute(Sender: TObject);
    procedure actConvertCaseFunctionNamesExecute(Sender: TObject);
    procedure actConvertCaseViewNamesExecute(Sender: TObject);
    procedure actConvertCaseConstraintNamesExecute(Sender: TObject);
    procedure actConvertCaseSynonymNamesExecute(Sender: TObject);
    procedure actConvertCaseTransactionNamesExecute(Sender: TObject);
    procedure actConvertCaseFunParamNamesExecute(Sender: TObject);
    procedure actConvertCaseExtQueryAliasesExecute(Sender: TObject);
    procedure actConvertCaseIdentifierNamesExecute(Sender: TObject);
    procedure actConvertSpacesBeforeSemicolonExecute(Sender: TObject);
    procedure actConvertSpacesBeforeCommaExecute(Sender: TObject);
    procedure actConvertSpacesAfterCommaExecute(Sender: TObject);
    procedure actConvertSpacesAroundOperatorExecute(Sender: TObject);
    procedure actConvertSpacesInsideBracketsExecute(Sender: TObject);
    procedure actConvertSpacesOutsideBracketsExecute(Sender: TObject);
    procedure actConvertKeywordExprAsExecute(Sender: TObject);
    procedure actConvertKeywordTableAsExecute(Sender: TObject);
    procedure actConvertKeywordInnerExecute(Sender: TObject);
    procedure actConvertKeywordOuterExecute(Sender: TObject);
  public
    Before_FullScreen_State: TWindowState;
    Before_FullScreen_Top,   Before_FullScreen_Left,
    Before_FullScreen_Width, Before_FullScreen_Height: Integer;

    { internal methods }
    procedure SetVisibleControls;
    procedure Ribbon_RecentFiles_BuildMenu;
    procedure Ribbon_Minimize_Width;
    procedure Ribbon_Maximize_Width;
  end;

var
  MainForm: TMainForm;

implementation

uses Windows, ShellApi, Graphics, SysUtils, Dialogs,
     GtStandard, GtVisual, GtRegistry, GtTokenizers, GtExternals,
     {$IFDEF DEBUG} SqlTest, {$ENDIF}
     SqlCommon, SqlLister, SqlConverters, SqlVersion,
     FormSettings, FormColors, FormAbout;

{$R *.dfm}

const
  sb_position   = 0;
  sb_keystate   = 1;
  sb_memory     = 2;
  sb_connection = 3;
  sb_log        = 4;

{ Form Create Event }
procedure TMainForm.FormCreate(Sender: TObject);
var lModified: Boolean;
begin
  FrameScriptEdit.FrameCreate;

  Ribbon.Font.Size := 8;

  {$IFDEF RELEASE}
  actHelpTest.Visible := False;
  RibbonGroupAbout.Items.Delete(1);
  {$ELSE}
  actHelpTest.Visible := True;
  RibbonGroupAbout.Items[1].Visible := True;
  {$ENDIF}

  actFilesNew.OnExecute           := FrameScriptEdit.actFilesNewExecute;
  actFilesOpen.OnExecute          := FrameScriptEdit.actFilesOpenExecute;
  actFilesSave.OnExecute          := FrameScriptEdit.actFilesSaveExecute;
  actFilesSaveAs.OnExecute        := FrameScriptEdit.actFilesSaveAsExecute;
  actFilesExit.OnExecute          := FrameScriptEdit.actFilesExitExecute;
  actFilesExportXML.OnExecute     := FrameScriptEdit.actFilesExportXMLExecute;
  actFilesImportXML.OnExecute     := FrameScriptEdit.actFilesImportXMLExecute;

  actToolsFormat.OnExecute        := FrameScriptEdit.actToolsFormatExecute;
  actToolsCompact.OnExecute       := FrameScriptEdit.actToolsCompactExecute;

  actEditCopy.OnExecute           := FrameScriptEdit.actEditCopyExecute;
  actEditCut.OnExecute            := FrameScriptEdit.actEditCutExecute;
  actEditPaste.OnExecute          := FrameScriptEdit.actEditPasteExecute;

  actEditFindReplace.OnExecute    := FrameScriptEdit.actEditFindReplaceExecute;

  actHighlightQuery.OnExecute     := FrameScriptEdit.actEditHighlightQueryExecute;
  actHighlightPrevQuery.OnExecute := FrameScriptEdit.actEditPrevQueryExecute;
  actHighlightNextQuery.OnExecute := FrameScriptEdit.actEditNextQueryExecute;

  Caption := VER_CAPTION;
  Before_FullScreen_State := wsNormal;

  DragAcceptFiles(Handle, True);

  try
    FrameScriptEdit.ScriptEdit_LinesClear;
    if ParamCount > 0 then FrameScriptEdit.ScriptOpenFromFile( ParamStr(1) );
  finally
    FrameScriptEdit.ScriptTabToSpaces(FrameScriptEdit.ScriptEdit.Lines);
    FrameScriptEdit.ScriptEdit.Modified := False;
  end;

  Top    := YaRegistryGetInt(yastWindowTop,    Top);
  Left   := YaRegistryGetInt(yastWindowLeft,   Left);
  Height := YaRegistryGetInt(yastWindowHeight, Height);
  Width  := YaRegistryGetInt(yastWindowWidth,  Width);

  if YaRegistryGetBool(yastShowFullScreen)
    then actViewFullScreenExecute(Self)
    else SetVisibleControls;

  { restores font name and size }
  lModified := FrameScriptEdit.ScriptEdit.Modified;
  FrameScriptEdit.ScriptEdit.Font.Name      := YaRegistryGetStr(yastEditFontName);
  FrameScriptEdit.ScriptEdit.Font.Size      := YaRegistryGetInt(yastEditFontSize);
  FrameScriptEdit.ScriptEdit.Modified       := lModified;

  FrameScriptEdit.RecentFiles_ReadFromRegistry;
  FrameScriptEdit.OnRecentFiles_BuildMenu := Ribbon_RecentFiles_BuildMenu;
  Ribbon_RecentFiles_BuildMenu;

  { Merge Action List }
  ActionsMove(FrameScriptEdit.ActionList, ActionList);

  RichEditSetPosition(FrameScriptEdit.ScriptEdit, 0,0);
  Ribbon_Maximize_Width;

  Ribbon.Font.Height := -11;
  Ribbon.Font.Size   := 8;

  actConvertResetExecute(Sender);
end;

{ Form Destroy Event }
procedure TMainForm.FormDestroy(Sender: TObject);
begin
  DragAcceptFiles(Handle, False);
end;

{ Form Show Event }
procedure TMainForm.FormShow(Sender: TObject);
begin
  if FrameScriptEdit.ScriptEdit.Visible then FrameScriptEdit.ScriptEdit.SetFocus;
  Self.SetFocus;
end;

{ Form Close Event }
procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  { store file }
  if not FrameScriptEdit.ScriptCloseFile then begin
    Action := caNone;
    Exit;
  end;

  { stores recent files }
  FrameScriptEdit.RecentFiles_SaveToRegistry;

  { stores visible controls }
  YaRegistryPutBool(yastShowFullScreen, actToolsFullScreen.Checked, True);
  if actToolsFullScreen.Checked then begin
    YaRegistryPutInt(yastWindowState,   Ord(WindowState));
  end;

  { stores font name and size }
  YaRegistryPutInt(yastEditFontSize,  FrameScriptEdit.ScriptEdit.Font.Size);
  YaRegistryPutStr(yastEditFontName,  FrameScriptEdit.ScriptEdit.Font.Name);

  if WindowState = wsMinimized then Exit;

  YaRegistryPutInt(yastWindowState,   Ord(WindowState));

  if WindowState = wsNormal then begin
    YaRegistryPutInt(yastWindowTop,    Top);
    YaRegistryPutInt(yastWindowLeft,   Left);
    YaRegistryPutInt(yastWindowHeight, Height);
    YaRegistryPutInt(yastWindowWidth,  Width);
  end;
end;

{ Sets Visible Controls }
{ calls: FormShow ?? }
procedure TMainForm.SetVisibleControls;
begin
  { main window border and main menu }
  if actToolsFullScreen.Checked then begin
    BorderStyle := bsNone;
    Ribbon.Visible := False;
    GlassFrame.Enabled := False;
  end else begin
    BorderStyle := bsSizeable;
    Ribbon.Visible := True;
    GlassFrame.Enabled := True;
  end;
end;

{ Ribbon, Recent Files }
procedure TMainForm.RibbonRecentItemClick(Sender: TObject; FileName: string; Index: Integer);
begin
  if not FrameScriptEdit.ScriptCloseFile then Exit; { silent recent menu rebuild }
  FrameScriptEdit.ScriptOpenFromFile( FileName );
  Ribbon_RecentFiles_BuildMenu;

  if FrameScriptEdit.ScriptEdit.Visible then FrameScriptEdit.ScriptEdit.SetFocus;
end;

{ Ribbon, Recent Files }
procedure TMainForm.Ribbon_RecentFiles_BuildMenu;
var i: Integer;
begin
  Ribbon.ClearRecentItems;

  for i := FrameScriptEdit.RecentFiles.Count - 1 downto 0 do
    Ribbon.AddRecentItem(FrameScriptEdit.RecentFiles[i]);
end;

{ message WM_DROPFILES, otwiera przeciagniêty plik w aplikacji }
procedure TMainForm.WMDropFiles;
begin
  inherited;

  FrameScriptEdit.WMDropFiles(Msg);
end;

{ Timer Event - refresh used memory, memo position and current scope }
procedure TMainForm.TimerMainTimer(Sender: TObject);
var KeyState : TKeyboardState;
begin
  { disable/enable action Save }
  FrameScriptEdit.actFilesSave.Enabled := FrameScriptEdit.ScriptEdit.Modified;

  { Script Memo position }
  StatusBar.Panels[sb_position].Text := FrameScriptEdit.ScriptEdit_CurrPositionToStr;

  { Status bar - CAPS LOCK, INSERT }
  GetKeyboardState(KeyState);
  StatusBar.Panels[sb_keystate].Text := strif(KeyState[VK_CAPITAL] = 1, 'CAPS') +
                                        strif(FrameScriptEdit.ScriptEditInsertMode, ' INS', ' OVR');

  { Status bar - Mem used }
  StatusBar.Panels[sb_memory].Text := 'Script: ' + GtFileOrMemSizeToStr(Length(FrameScriptEdit.ScriptEdit.Lines.Text), 0) +
                                    ', Memory: ' + GtFileOrMemSizeToStr(GtExternals.AllocMemSize) +
                                    ', nodes: '  + IntToStr(GtSqlNodeCount);

  { Status bar = Log }
  {$IFDEF DEBUG}
  StatusBar.Panels[sb_log].Text := StatusLogText;
  {$ENDIF}
end;

{ Tools: Quote from Delphi or Pascal }
procedure TMainForm.actToolsQuoteFromDelphiExecute(Sender: TObject);
var lSL: TStringList;
    i: Integer;
begin
  lSL := TStringList.Create;
  try
    for i := 0 to FrameScriptEdit.ScriptEdit.Lines.Count -1 do begin
      lSL.Add(  strBreakOnLast1( '''',
                strBreakOnFirst2( '''',
                StringReplace(
                FrameScriptEdit.ScriptEdit.Lines[i]
                , gttkApostrophe.TokenText + gttkApostrophe.TokenText, gttkApostrophe.TokenText, [rfReplaceAll])
                ) ) );
    end;

    FrameScriptEdit.ScriptEdit.SelectAll;
    FrameScriptEdit.ScriptEdit.SelText := lSL.Text;
  finally
    lSL.Free;
  end;
end;

{ Tools: Quote to Delphi or Pascal }
procedure TMainForm.actToolsQuoteToDelphiExecute(Sender: TObject);
var lSL: TStringList;
    i: Integer;
begin
  lSL := TStringList.Create;
  try
    lSL.Text := FrameScriptEdit.ScriptEdit.Lines.Text;

    { normalnie }
    for i := 0 to lSL.Count -1 do
      lSL[i] := gttkApostrophe.TokenText +
                StringReplace(lSL[i], gttkApostrophe.TokenText, gttkApostrophe.TokenText + gttkApostrophe.TokenText, [rfReplaceAll]) +
                gttkApostrophe.TokenText + strif(i=lSL.Count-1, ';', '#13#10 +');

    FrameScriptEdit.ScriptEdit.SelectAll;
    FrameScriptEdit.ScriptEdit.SelText := lSL.Text;
  finally
    lSL.Free;
  end;
end;

{ action View, Full Screen }
procedure TMainForm.actViewFullScreenExecute(Sender: TObject);
begin
  if not actToolsFullScreen.Checked then begin
    Before_FullScreen_State := WindowState;
    Before_FullScreen_Top   := Top;
    Before_FullScreen_Left  := Left;
    Before_FullScreen_Width := Width;
    Before_FullScreen_Height:= Height;
  end else begin
    Hide;
  end;

  actToolsFullScreen.Checked := not actToolsFullScreen.Checked;

  Hide;

  SetVisibleControls;

  if actToolsFullScreen.Checked then begin
    WindowState             := wsMaximized;
  end else begin
    WindowState             := Before_FullScreen_State;

    Top    := Before_FullScreen_Top;
    Left   := Before_FullScreen_Left;
    Width  := Before_FullScreen_Width;
    Height := Before_FullScreen_Height;

    WindowState             := Before_FullScreen_State;
  end;

  // Hide i Show - zapobiegaja mruganiu formy przy przejsciu z/do FullScreen.
  Show;
end;

{ action View, Font Size Up }
procedure TMainForm.actViewFontSizeUpExecute(Sender: TObject);
var lModified: Boolean;
begin
  StatusLogStartTime;

  lModified := FrameScriptEdit.ScriptEdit.Modified;

  FontSizeUp(FrameScriptEdit.ScriptEdit.Font, YA_MAX_FONT_SIZE);

  FrameScriptEdit.actToolsListByTokensExecute(Sender);
  FrameScriptEdit.ScriptEdit.Modified := lModified;

  StatusLogStopTime('FontSizeUp');
end;

{ action View, Font Size Down }
procedure TMainForm.actViewFontSizeDownExecute(Sender: TObject);
var lModified: Boolean;
begin
  StatusLogStartTime;

  lModified := FrameScriptEdit.ScriptEdit.Modified;

  FontSizeDown(FrameScriptEdit.ScriptEdit.Font, YA_MIN_FONT_SIZE);

  FrameScriptEdit.actToolsListByTokensExecute(Sender);
  FrameScriptEdit.ScriptEdit.Modified := lModified;

  StatusLogStopTime('FontSizeDown');
end;

{ action Preferences, Colors }
procedure TMainForm.actToolsColorsExecute(Sender: TObject);
var lForm: TFormColors;
begin
  lForm := TFormColors.Create(Application);
  lForm.ScriptEditFont :=  FrameScriptEdit.ScriptEdit.Font;

  lForm.ShowModal;

  FrameScriptEdit.ScriptEdit.Font := lForm.ScriptEditFont;
  lForm.Free;

  FrameScriptEdit.ParseScript;
  FrameScriptEdit.ListScriptByToken;

  Self.SetFocus;
end;

{ action Preferences, Font }
procedure TMainForm.actToolsFontExecute(Sender: TObject);
begin
  FrameScriptEdit.ScriptEdit.Font.Assign( YaFontDialog( FrameScriptEdit.ScriptEdit.Font ) );
end;

{ action Preferences, Settings }
procedure TMainForm.actToolsSettingsExecute(Sender: TObject);
var lForm: TFormSettings;
begin
  lForm := TFormSettings.Create(Application);
  lForm.ScriptEditFont :=  FrameScriptEdit.ScriptEdit.Font;

  lForm.ShowModal;

  FrameScriptEdit.ScriptEdit.Font := lForm.ScriptEditFont;
  lForm.Free;

  FrameScriptEdit.ParseScript;
  FrameScriptEdit.ListScriptByToken;

  Self.SetFocus;
end;

{ action Convert: RESET converters }
procedure TMainForm.actConvertResetExecute(Sender: TObject);
begin
  actConvertSortOrderShortKeywords.Enabled            := True;
  actConvertSortOrderLongKeywords.Enabled             := True;

  actConvertSortOrderAddDefaultKeywords.Enabled       := True;
  actConvertSortOrderRemoveDefaultKeywords.Enabled    := True;

  actConvertJoinsAddInner.Enabled                     := True;
  actConvertJoinsRemoveInner.Enabled                  := True;

  actConvertJoinsAddOuter.Enabled                     := True;
  actConvertJoinsRemoveOuter.Enabled                  := True;
end;

procedure TMainForm.actConvertCaseColumnAliasesExecute(Sender: TObject);
begin
  case actConvertCaseColumnAliases.ImageIndex of
    35 : begin
           FrameScriptEdit.ScriptConvert(True, SQCG_CASES, SQCC_CASE_COLUMN_ALIAS, SQCV_UPPER );
           actConvertCaseColumnAliases.ImageIndex := 36;
         end;
    36 : begin
           FrameScriptEdit.ScriptConvert(True, SQCG_CASES, SQCC_CASE_COLUMN_ALIAS, SQCV_LOWER );
           actConvertCaseColumnAliases.ImageIndex := 35;
         end;
  end;
end;

procedure TMainForm.actConvertCaseColumnNamesExecute(Sender: TObject);
begin
  case actConvertCaseColumnNames.ImageIndex of
    35 : begin
           FrameScriptEdit.ScriptConvert(True, SQCG_CASES, SQCC_CASE_COLUMN, SQCV_UPPER );
           actConvertCaseColumnNames.ImageIndex := 36;
         end;
    36 : begin
           FrameScriptEdit.ScriptConvert(True, SQCG_CASES, SQCC_CASE_COLUMN, SQCV_LOWER );
           actConvertCaseColumnNames.ImageIndex := 35;
         end;
  end;
end;

procedure TMainForm.actConvertCaseConstraintNamesExecute(Sender: TObject);
begin
  case actConvertCaseConstraintNames.ImageIndex of
    35 : begin
           FrameScriptEdit.ScriptConvert(True, SQCG_CASES, SQCC_CASE_CONSTRAINT, SQCV_UPPER );
           actConvertCaseConstraintNames.ImageIndex := 36;
         end;
    36 : begin
           FrameScriptEdit.ScriptConvert(True, SQCG_CASES, SQCC_CASE_CONSTRAINT, SQCV_LOWER );
           actConvertCaseConstraintNames.ImageIndex := 35;
         end;
  end;
end;

procedure TMainForm.actConvertCaseExtQueryAliasesExecute(Sender: TObject);
begin
  case actConvertCaseExtQueryAliases.ImageIndex of
    35 : begin
           FrameScriptEdit.ScriptConvert(True, SQCG_CASES, SQCC_CASE_EXTQ_ALIAS, SQCV_UPPER );
           actConvertCaseExtQueryAliases.ImageIndex := 36;
         end;
    36 : begin
           FrameScriptEdit.ScriptConvert(True, SQCG_CASES, SQCC_CASE_EXTQ_ALIAS, SQCV_LOWER );
           actConvertCaseExtQueryAliases.ImageIndex := 35;
         end;
  end;
end;

procedure TMainForm.actConvertCaseFunctionNamesExecute(Sender: TObject);
begin
  case actConvertCaseFunctionNames.ImageIndex of
    35 : begin
           FrameScriptEdit.ScriptConvert(True, SQCG_CASES, SQCC_CASE_FUNC, SQCV_UPPER );
           actConvertCaseFunctionNames.ImageIndex := 36;
         end;
    36 : begin
           FrameScriptEdit.ScriptConvert(True, SQCG_CASES, SQCC_CASE_FUNC, SQCV_LOWER );
           actConvertCaseFunctionNames.ImageIndex := 35;
         end;
  end;
end;

procedure TMainForm.actConvertCaseFunParamNamesExecute(Sender: TObject);
begin
  case actConvertCaseFunParamNames.ImageIndex of
    35 : begin
           FrameScriptEdit.ScriptConvert(True, SQCG_CASES, SQCC_CASE_FUN_PARAM, SQCV_UPPER );
           actConvertCaseFunParamNames.ImageIndex := 36;
         end;
    36 : begin
           FrameScriptEdit.ScriptConvert(True, SQCG_CASES, SQCC_CASE_FUN_PARAM, SQCV_LOWER );
           actConvertCaseFunParamNames.ImageIndex := 35;
         end;
  end;
end;

procedure TMainForm.actConvertCaseIdentifierNamesExecute(Sender: TObject);
begin
  case actConvertCaseIdentifierNames.ImageIndex of
    35 : begin
           FrameScriptEdit.ScriptConvert(True, SQCG_CASES, SQCC_CASE_IDENTIFIER, SQCV_UPPER );
           actConvertCaseIdentifierNames.ImageIndex := 36;
         end;
    36 : begin
           FrameScriptEdit.ScriptConvert(True, SQCG_CASES, SQCC_CASE_IDENTIFIER, SQCV_LOWER );
           actConvertCaseIdentifierNames.ImageIndex := 35;
         end;
  end;
end;

{ executes token case converter for keywords }
procedure TMainForm.actConvertCaseKeywordExecute(Sender: TObject);
begin
  case actConvertCaseKeyword.ImageIndex of
    35 : begin
           FrameScriptEdit.ScriptConvert(True, SQCG_CASES, SQCC_CASE_KEYWORD, SQCV_UPPER );
           actConvertCaseKeyword.ImageIndex := 36;
         end;
    36 : begin
           FrameScriptEdit.ScriptConvert(True, SQCG_CASES, SQCC_CASE_KEYWORD, SQCV_LOWER );
           actConvertCaseKeyword.ImageIndex := 35;
         end;
  end;
end;

procedure TMainForm.actConvertCaseParametersExecute(Sender: TObject);
begin
  case actConvertCaseParameters.ImageIndex of
    35 : begin
           FrameScriptEdit.ScriptConvert(True, SQCG_CASES, SQCC_CASE_PARAM, SQCV_UPPER );
           actConvertCaseParameters.ImageIndex := 36;
         end;
    36 : begin
           FrameScriptEdit.ScriptConvert(True, SQCG_CASES, SQCC_CASE_PARAM, SQCV_LOWER );
           actConvertCaseParameters.ImageIndex := 35;
         end;
  end;
end;

procedure TMainForm.actConvertCaseSynonymNamesExecute(Sender: TObject);
begin
  case actConvertCaseSynonymNames.ImageIndex of
    35 : begin
           FrameScriptEdit.ScriptConvert(True, SQCG_CASES, SQCC_CASE_SYNONYM, SQCV_UPPER );
           actConvertCaseSynonymNames.ImageIndex := 36;
         end;
    36 : begin
           FrameScriptEdit.ScriptConvert(True, SQCG_CASES, SQCC_CASE_SYNONYM, SQCV_LOWER );
           actConvertCaseSynonymNames.ImageIndex := 35;
         end;
  end;
end;

procedure TMainForm.actConvertCaseTableAliasesExecute(Sender: TObject);
begin
  case actConvertCaseTableAliases.ImageIndex of
    35 : begin
           FrameScriptEdit.ScriptConvert(True, SQCG_CASES, SQCC_CASE_TABLE_ALIAS, SQCV_UPPER );
           actConvertCaseTableAliases.ImageIndex := 36;
         end;
    36 : begin
           FrameScriptEdit.ScriptConvert(True, SQCG_CASES, SQCC_CASE_TABLE_ALIAS, SQCV_LOWER );
           actConvertCaseTableAliases.ImageIndex := 35;
         end;
  end;
end;

procedure TMainForm.actConvertCaseTableNamesExecute(Sender: TObject);
begin
  case actConvertCaseTableNames.ImageIndex of
    35 : begin
           FrameScriptEdit.ScriptConvert(True, SQCG_CASES, SQCC_CASE_TABLE, SQCV_UPPER );
           actConvertCaseTableNames.ImageIndex := 36;
         end;
    36 : begin
           FrameScriptEdit.ScriptConvert(True, SQCG_CASES, SQCC_CASE_TABLE, SQCV_LOWER );
           actConvertCaseTableNames.ImageIndex := 35;
         end;
  end;
end;

procedure TMainForm.actConvertCaseTransactionNamesExecute(Sender: TObject);
begin
  case actConvertCaseTransactionNames.ImageIndex of
    35 : begin
           FrameScriptEdit.ScriptConvert(True, SQCG_CASES, SQCC_CASE_TRANSACTION, SQCV_UPPER );
           actConvertCaseTransactionNames.ImageIndex := 36;
         end;
    36 : begin
           FrameScriptEdit.ScriptConvert(True, SQCG_CASES, SQCC_CASE_TRANSACTION, SQCV_LOWER );
           actConvertCaseTransactionNames.ImageIndex := 35;
         end;
  end;
end;

procedure TMainForm.actConvertCaseViewNamesExecute(Sender: TObject);
begin
  case actConvertCaseViewNames.ImageIndex of
    35 : begin
           FrameScriptEdit.ScriptConvert(True, SQCG_CASES, SQCC_CASE_VIEW, SQCV_UPPER );
           actConvertCaseViewNames.ImageIndex := 36;
         end;
    36 : begin
           FrameScriptEdit.ScriptConvert(True, SQCG_CASES, SQCC_CASE_VIEW, SQCV_LOWER );
           actConvertCaseViewNames.ImageIndex := 35;
         end;
  end;
end;

{ action Convert }
procedure TMainForm.actConvertExprAliasAddKeywordAsExecute(Sender: TObject);
begin
  FrameScriptEdit.ScriptConvert(True, SQCG_KEYWORD, SQCC_KWD_AS_COLUMNS, SQCV_ADD );
end;

{ action Convert }
procedure TMainForm.actConvertJoinCondRefToLeftExecute(Sender: TObject);
begin
  FrameScriptEdit.ScriptConvert(True, SQCG_KEYWORD, SQCC_JOIN_ON_LEFT, SQCV_ADD );
end;

procedure TMainForm.actConvertJoinsAddInnerExecute(Sender: TObject);
begin
  FrameScriptEdit.ScriptConvert(True, SQCG_KEYWORD, SQCC_KWD_INNER, SQCV_ADD );

  actConvertJoinsAddInner.Enabled       := False;
  actConvertJoinsRemoveInner.Enabled    := True;
end;

{ action Convert }
procedure TMainForm.actConvertJoinsAddOuterExecute(Sender: TObject);
begin
  FrameScriptEdit.ScriptConvert(True, SQCG_KEYWORD, SQCC_KWD_OUTER, SQCV_ADD );

  actConvertJoinsAddOuter.Enabled       := False;
  actConvertJoinsRemoveOuter.Enabled    := True;
end;

{ action Convert }
procedure TMainForm.actConvertJoinsRemoveInnerExecute(Sender: TObject);
begin
  FrameScriptEdit.ScriptConvert(True, SQCG_KEYWORD, SQCC_KWD_INNER, SQCV_REMOVE );

  actConvertJoinsAddInner.Enabled       := True;
  actConvertJoinsRemoveInner.Enabled    := False;
end;

{ action Convert }
procedure TMainForm.actConvertJoinsRemoveOuterExecute(Sender: TObject);
begin
  FrameScriptEdit.ScriptConvert(True, SQCG_KEYWORD, SQCC_KWD_OUTER, SQCV_REMOVE );

  actConvertJoinsAddOuter.Enabled       := True;
  actConvertJoinsRemoveOuter.Enabled    := False;
end;

procedure TMainForm.actConvertKeywordExprAsExecute(Sender: TObject);
begin
  case actConvertKeywordExprAs.ImageIndex of
    37 : begin
           FrameScriptEdit.ScriptConvert(True, SQCG_KEYWORD, SQCC_KWD_AS_COLUMNS, SQCV_ADD );
           actConvertKeywordExprAs.ImageIndex := 38;
         end;
    38 : begin
           FrameScriptEdit.ScriptConvert(True, SQCG_KEYWORD, SQCC_KWD_AS_COLUMNS, SQCV_REMOVE );
           actConvertKeywordExprAs.ImageIndex := 37;
         end;
  end;
end;

procedure TMainForm.actConvertKeywordInnerExecute(Sender: TObject);
begin
  case actConvertKeywordInner.ImageIndex of
    37 : begin
           FrameScriptEdit.ScriptConvert(True, SQCG_KEYWORD, SQCC_KWD_INNER, SQCV_ADD );
           actConvertKeywordInner.ImageIndex := 38;
         end;
    38 : begin
           FrameScriptEdit.ScriptConvert(True, SQCG_KEYWORD, SQCC_KWD_INNER, SQCV_REMOVE );
           actConvertKeywordInner.ImageIndex := 37;
         end;
  end;
end;

procedure TMainForm.actConvertKeywordOuterExecute(Sender: TObject);
begin
  case actConvertKeywordOuter.ImageIndex of
    37 : begin
           FrameScriptEdit.ScriptConvert(True, SQCG_KEYWORD, SQCC_KWD_OUTER, SQCV_ADD );
           actConvertKeywordOuter.ImageIndex := 38;
         end;
    38 : begin
           FrameScriptEdit.ScriptConvert(True, SQCG_KEYWORD, SQCC_KWD_OUTER, SQCV_REMOVE );
           actConvertKeywordOuter.ImageIndex := 37;
         end;
  end;
end;

procedure TMainForm.actConvertKeywordTableAsExecute(Sender: TObject);
begin
  case actConvertKeywordTableAs.ImageIndex of
    37 : begin
           FrameScriptEdit.ScriptConvert(True, SQCG_KEYWORD, SQCC_KWD_AS_TABLES, SQCV_ADD );
           actConvertKeywordTableAs.ImageIndex := 38;
         end;
    38 : begin
           FrameScriptEdit.ScriptConvert(True, SQCG_KEYWORD, SQCC_KWD_AS_TABLES, SQCV_REMOVE );
           actConvertKeywordTableAs.ImageIndex := 37;
         end;
  end;
end;

{ action Convert }
procedure TMainForm.actConvertSortOrderShortKeywordsExecute(Sender: TObject);
begin
  FrameScriptEdit.ScriptConvert(True, SQCG_KEYWORD, SQCC_KWD_ORDER_LEN, SQCV_SHORT );

  actConvertSortOrderShortKeywords.Enabled     := False;
  actConvertSortOrderLongKeywords.Enabled      := True;
end;

procedure TMainForm.actConvertSpacesAfterCommaExecute(Sender: TObject);
begin
  case actConvertSpacesAfterComma.ImageIndex of
    37 : begin
           FrameScriptEdit.ScriptConvert(True, SQCG_SPACES, SQCC_SPACE_AFT_COMMA, SQCV_ADD );
           actConvertSpacesAfterComma.ImageIndex := 38;
         end;
    38 : begin
           FrameScriptEdit.ScriptConvert(True, SQCG_SPACES, SQCC_SPACE_AFT_COMMA, SQCV_REMOVE );
           actConvertSpacesAfterComma.ImageIndex := 37;
         end;
  end;
end;

procedure TMainForm.actConvertSpacesAroundOperatorExecute(Sender: TObject);
begin
  case actConvertSpacesAroundOperator.ImageIndex of
    37 : begin
           FrameScriptEdit.ScriptConvert(True, SQCG_SPACES, SQCC_SPACE_AROUND_OPER, SQCV_ADD );
           actConvertSpacesAroundOperator.ImageIndex := 38;
         end;
    38 : begin
           FrameScriptEdit.ScriptConvert(True, SQCG_SPACES, SQCC_SPACE_AROUND_OPER, SQCV_REMOVE );
           actConvertSpacesAroundOperator.ImageIndex := 37;
         end;
  end;
end;

procedure TMainForm.actConvertSpacesBeforeCommaExecute(Sender: TObject);
begin
  case actConvertSpacesBeforeComma.ImageIndex of
    37 : begin
           FrameScriptEdit.ScriptConvert(True, SQCG_SPACES, SQCC_SPACE_BEF_COMMA, SQCV_ADD );
           actConvertSpacesBeforeComma.ImageIndex := 38;
         end;
    38 : begin
           FrameScriptEdit.ScriptConvert(True, SQCG_SPACES, SQCC_SPACE_BEF_COMMA, SQCV_REMOVE );
           actConvertSpacesBeforeComma.ImageIndex := 37;
         end;
  end;
end;

procedure TMainForm.actConvertSpacesBeforeSemicolonExecute(Sender: TObject);
begin
  case actConvertSpacesBeforeSemicolon.ImageIndex of
    37 : begin
           FrameScriptEdit.ScriptConvert(True, SQCG_SPACES, SQCC_SPACE_BEF_SEMICOLON, SQCV_ADD );
           actConvertSpacesBeforeSemicolon.ImageIndex := 38;
         end;
    38 : begin
           FrameScriptEdit.ScriptConvert(True, SQCG_SPACES, SQCC_SPACE_BEF_SEMICOLON, SQCV_REMOVE );
           actConvertSpacesBeforeSemicolon.ImageIndex := 37;
         end;
  end;
end;

procedure TMainForm.actConvertSpacesInsideBracketsExecute(Sender: TObject);
begin
  case actConvertSpacesInsideBrackets.ImageIndex of
    37 : begin
           FrameScriptEdit.ScriptConvert(True, SQCG_SPACES, SQCC_SPACE_INSIDE_BRACKET, SQCV_ADD );
           actConvertSpacesInsideBrackets.ImageIndex := 38;
         end;
    38 : begin
           FrameScriptEdit.ScriptConvert(True, SQCG_SPACES, SQCC_SPACE_INSIDE_BRACKET, SQCV_REMOVE );
           actConvertSpacesInsideBrackets.ImageIndex := 37;
         end;
  end;
end;

procedure TMainForm.actConvertSpacesOutsideBracketsExecute(Sender: TObject);
begin
  case actConvertSpacesOutsideBrackets.ImageIndex of
    37 : begin
           FrameScriptEdit.ScriptConvert(True, SQCG_SPACES, SQCC_SPACE_OUTSIDE_BRACKET, SQCV_ADD );
           actConvertSpacesOutsideBrackets.ImageIndex := 38;
         end;
    38 : begin
           FrameScriptEdit.ScriptConvert(True, SQCG_SPACES, SQCC_SPACE_OUTSIDE_BRACKET, SQCV_REMOVE );
           actConvertSpacesOutsideBrackets.ImageIndex := 37;
         end;
  end;
end;

{ action Convert }
procedure TMainForm.actConvertSortOrderLongKeywordsExecute(Sender: TObject);
begin
  FrameScriptEdit.ScriptConvert(True, SQCG_KEYWORD, SQCC_KWD_ORDER_LEN, SQCV_LONG );

  actConvertSortOrderShortKeywords.Enabled     := True;
  actConvertSortOrderLongKeywords.Enabled      := False;
end;

{ action Convert }
procedure TMainForm.actConvertSortOrderAddDefaultKeywordsExecute(Sender: TObject);
begin
  FrameScriptEdit.ScriptConvert(True, SQCG_KEYWORD, SQCC_KWD_ORDER_DEF, SQCV_ADD );

  actConvertSortOrderAddDefaultKeywords.Enabled       := False;
  actConvertSortOrderRemoveDefaultKeywords.Enabled    := True;

  actConvertSortOrderShortKeywords.Enabled            := True;
  actConvertSortOrderLongKeywords.Enabled             := False;
end;

{ action Convert }
procedure TMainForm.actConvertSortOrderRemoveDefaultKeywordsExecute(Sender: TObject);
begin
  FrameScriptEdit.ScriptConvert(True, SQCG_KEYWORD, SQCC_KWD_ORDER_DEF, SQCV_REMOVE );

  actConvertSortOrderAddDefaultKeywords.Enabled       := True;
  actConvertSortOrderRemoveDefaultKeywords.Enabled    := False;
end;

{ action Help, About }
procedure TMainForm.actHelpAboutExecute(Sender: TObject);
var lForm: TForm;
begin
  lForm := TFormAboutYetAnother.Create(Application);
  lForm.Left := Left + (Width - lForm.Width)   div 2;
  lForm.Top  := Top  + (Height - lForm.Height) div 2;

  lForm.ShowModal;
  lForm.Free;

  Self.SetFocus;
end;

{ action Help, Test }
procedure TMainForm.actHelpTestExecute(Sender: TObject);
begin
  {$IFDEF DEBUG}
  TestQueryCount := 0;
  TestQueryErrors := 0;
  StatusLogStartTime;

  GtSqlTestRun;

  StatusLogStopTime('Test');
  ShowMessage('Finished tests of ' + IntToStr(TestQueryCount) +
                    ' queries in ' + StatusLogLastTimeStr +
  strif(TestQueryErrors>0,' with ' + IntToStr(TestQueryErrors) + ' errors' +
                           ' and ' + IntToStr(TestQueryCount - TestQueryErrors) + ' fines.', '.' ));
  {$ENDIF}
end;

{ action manager, update }
procedure TMainForm.ActionManagerUpdate(Action: TBasicAction;var Handled: Boolean);
begin
  Ribbon.DocumentName := FrameScriptEdit.ScriptFileName;

  actFilesSave.Enabled :=(FrameScriptEdit.ScriptFileName <> '') and FrameScriptEdit.ScriptEdit.Modified;

  actEditCopy.Enabled  := FrameScriptEdit.ScriptEdit.SelLength <> 0;
  actEditCut.Enabled   := FrameScriptEdit.ScriptEdit.SelLength <> 0;

  actEditFindReplace.Enabled := FrameScriptEdit.ScriptEdit.Lines.Count <> 0;
end;

{ action Ribbon, Minimize }
procedure TMainForm.actMenuMinimizeExecute(Sender: TObject);
begin
  Ribbon.Minimized := not Ribbon.Minimized;
end;

{ Ribbon, Minimize }
procedure TMainForm.Ribbon_Minimize_Width;
var i,j,k: Integer;
begin
  for i := 0 to Ribbon.Tabs.Count -1 do
    if Ribbon.Tabs[i].Page.GroupCount > 0 then
      for j := 0 to Ribbon.Tabs[i].Page.GroupCount -1 do
        if Assigned(Ribbon.Tabs[i].Page.Groups[j].Items) then
          for k := 0 to Ribbon.Tabs[i].Page.Groups[j].Items.Count-1 do begin
            Ribbon.Tabs[i].Page.Groups[j].Items[k].ShowCaption := False;
            TButtonProperties(Ribbon.Tabs[i].Page.Groups[j].Items[k].CommandProperties).ButtonSize := bsSmall;
            Ribbon.Tabs[i].Page.Groups[j].Items[k].Visible := Ribbon.Tabs[i].Page.Groups[j].Items[k].Action.Visible;
          end;
end;

{ Ribbon, Maximize }
procedure TMainForm.Ribbon_Maximize_Width;
var i,j,k: Integer;
begin
  for i := 0 to Ribbon.Tabs.Count -1 do
    if Ribbon.Tabs[i].Page.GroupCount > 0 then
      for j := 0 to Ribbon.Tabs[i].Page.GroupCount -1 do
        if Assigned(Ribbon.Tabs[i].Page.Groups[j].Items) then
          for k := 0 to Ribbon.Tabs[i].Page.Groups[j].Items.Count-1 do begin
            Ribbon.Tabs[i].Page.Groups[j].Items[k].ShowCaption := True;
            TButtonProperties(Ribbon.Tabs[i].Page.Groups[j].Items[k].CommandProperties).ButtonSize := bsLarge;
            Ribbon.Tabs[i].Page.Groups[j].Items[k].Visible := Ribbon.Tabs[i].Page.Groups[j].Items[k].Action.Visible;
          end;
end;

end.

