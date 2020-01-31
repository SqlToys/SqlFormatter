program SqlFormatter;

uses
  Forms,
  Vcl.Themes,
  Vcl.Styles,
  GtStandard in '..\units\GtStandard.pas',
  GtExternals in '..\units\GtExternals.pas',
  GtContainers in '..\units\GtContainers.pas',
  SqlNode in '..\units\SqlNode.pas',
  SqlCommon in '..\units\SqlCommon.pas',
  GtTokenizers in '..\units\GtTokenizers.pas',
  SqlParser in '..\units\SqlParser.pas',
  SqlLister in '..\units\SqlLister.pas',
  SqlConverters in '..\units\SqlConverters.pas',
  SqlTest in '..\units\SqlTest.pas',
  SqlXmlTree in '..\units\SqlXmlTree.pas',
  GtRegistry in '..\units\GtRegistry.pas',
  GtVisual in '..\units\GtVisual.pas',
  FormAbout in '..\forms\FormAbout.pas' {FormAboutYetAnother},
  FormFind in '..\forms\FormFind.pas' {FormFindReplace},
  FormSettings in '..\forms\FormSettings.pas' {FormSettings},
  FormColors in '..\forms\FormColors.pas' {FormColors},
  FrameScriptEdit in '..\forms\FrameScriptEdit.pas' {FrameScriptEdit: TFrame},
  FormFormatter in '..\forms\FormFormatter.pas' {MainForm},
  SqlVersion in '..\units\SqlVersion.pas';

{$R *.res}

begin
  VER_NAME := 'SQL Formatter';

  Application.Initialize;
  Application.Title := VER_NAME;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
