(* $Header: /SQL Toys/SqlFormat/SqlVersion.pas 333   18-03-21 21:17 Tomek $
   (c) Tomasz Gierka, github.com/SqlToys, 2012.09.16                          *)
{--------------------------------------  --------------------------------------}
unit SqlVersion;

interface

var VER_NAME: string;

  function VER_VERSION: string;

  function VER_CAPTION: string;

  function VER_NUMBER: string;
  function VER_DATE: string;
  function VER_BUILD: string;

function GetFileVersion_Major  (aFileName: String): Integer;
function GetFileVersion_Minor  (aFileName: String): Integer;
function GetFileVersion_Release(aFileName: String): Integer;
function GetFileVersion_ReleaseAsDateStr(aFileName: String): String;
function GetFileVersion_Build  (aFileName: String): Integer;

implementation

uses SysUtils, Windows;

{ gets file version major number }
function GetFileVersion_Major(aFileName: String): Integer;
begin
  Result := GetFileVersion(aFileName) shr 16;
end;

{ gets file version minor number }
function GetFileVersion_Minor(aFileName: String): Integer;
begin
  Result := GetFileVersion(aFileName) and $FFFF;
end;

{ gets file version release number }
function GetFileVersion_Release(aFileName: string): Integer;
var
  VerInfoSize, VerValueSize, Dummy: DWord;
  VerInfo: Pointer;
  VerValue: PVSFixedFileInfo;
begin
  Result := 0;
  VerInfoSize := GetFileVersionInfoSize(PChar(aFileName), Dummy);
  GetMem(VerInfo, VerInfoSize);
  GetFileVersionInfo(PChar(aFileName), 0, VerInfoSize, VerInfo);
  if Assigned(VerInfo) then begin
    VerQueryValue(VerInfo, '\', Pointer(VerValue), VerValueSize);
    // Major := dwFileVersionMS shr 16;
    // Minor := dwFileVersionMS and $FFFF;
    Result := VerValue^.dwFileVersionLS shr 16;
  end;
  FreeMem(VerInfo, VerInfoSize);
end;

{ gets file version release number as date in string }
function GetFileVersion_ReleaseAsDateStr(aFileName: String): String;
begin
  Result := DateToStr( GetFileVersion_Release(aFileName) + EncodeDate(2000,1,1) );
end;

{ gets file version build number }
function GetFileVersion_Build(aFileName: string): Integer;
var
  VerInfoSize, VerValueSize, Dummy: DWord;
  VerInfo: Pointer;
  VerValue: PVSFixedFileInfo;
begin
  Result := 0;
  VerInfoSize := GetFileVersionInfoSize(PChar(aFileName), Dummy);
  GetMem(VerInfo, VerInfoSize);
  GetFileVersionInfo(PChar(aFileName), 0, VerInfoSize, VerInfo);
  if Assigned(VerInfo) then begin
    VerQueryValue(VerInfo, '\', Pointer(VerValue), VerValueSize);
    Result := VerValue^.dwFileVersionLS and $FFFF;
  end;
  FreeMem(VerInfo, VerInfoSize);
end;

function VER_VERSION: string;
begin
  Result := 'ver: ' + VER_NUMBER + ', build: ' + VER_BUILD + ', date: ' + VER_DATE;
end;

function VER_CAPTION: string;
begin
  Result := VER_NAME + ' ' + VER_NUMBER;
end;

function VER_NUMBER: string;
begin
  Result := IntToStr(GetFileVersion_Major(ParamStr(0))) + '.' + IntToStr(GetFileVersion_Minor(ParamStr(0)));
// if GetFileVersion_Minor(ParamStr(0)) and 1 >0 then YA_VER := YA_VER + ' dev';
end;

function VER_DATE: string;
begin
  Result := GetFileVersion_ReleaseAsDateStr(ParamStr(0));
end;
//
// WORK TIME STATISTICS --------------------------------------------------------
//
// 2018   13 03 07
// 2017   -- -- --   -- -- --   -- -- --   -- 03 24     TOTAL:  27
// 2016   01 -- --   03 -- --   -- -- --   -- -- --     TOTAL:  04
// 2015   02 07 19   08 11 16   -- -- --   -- -- --     TOTAL:  63
// 2014   09 02 --   -- 07 05   10 09 07   12 -- 02     TOTAL:  63
// 2013   11 13 22   10 14 08   -- 06 06   05 06 05     TOTAL: 106
// 2012   13 22 18   16 17 15   03 03 13   17 11 12     TOTAL: 160
// 2011   15 10 13   22 36 21   28 30 06   12 13 10     TOTAL: 216
// 2010   -- -- 03   05 -- --   03 15 17   14 11 11     TOTAL:  79

function VER_BUILD: string;
begin
  Result := '741'; // current build
end;

// 2018-03   -------------------------------------------------------------------
//
// 0.81  741 21 Lister redesing concept => SyntaxtTree converters vs Script/TokenList converters
// 0.81  740 20 REMOVED: Space before semicolon, Space before comma, Space after comma.
//              Space around operator, Space outside brackets, Space inside brackets (except single param functions, except datatypes)
//              One expr on line, One cond on line, comma at new line, FormSettings with TreeView ONLY !!!
//              25.516 LINES.
// 0.81  739 19 REMOVED: ColumnConstraint, ClauseRightIntend, CREATE TABLE datatype indend,
//              TableAndAliasIntend, ON condtition intend, SET left expression intend, Expr alias intend.
// 0.81  738 11 Converter: Empty line before complex constraint. Split NewLines and EmptyLines converters.
//              Removed shortuqeries.
// 0.81  737 11 New line before column constraint, LinesNoAfterQuery removed.
//              REMOVED: MaxTableNameToIntend, MaxAliasNameToIntend, MaxIdentifierLen.
//              REMOVED: MaxClauseToIntend, MaxColumnNameToIntend, MaxDatatypeToIntend, MaxShortQueryLines.
// 0.81  736 11 Settings form rearranged from 4 to 2 groupbox columns.
//              Add/remove semicolon, Add/remove semicolon from single query.
// 0.81  735 11 CASE expr new lines.
//
// 2018-02   -------------------------------------------------------------------
//
// 0.81  734 11 ExprOp, CompOp, JoinOp => Operand
//              TEST: SELECT 1+2*4 FROM B JOIN C ON B.B = C.C WHERE 1=1 OR 2=2
// 0.81  733 11 Compact were using converters, removed SkipNextNewLine in List_DML for subqueries.
// 0.81  732 10 Empty line before clauese issues - removed last line script fixed.
//
// 2018-01   -------------------------------------------------------------------
//
// 0.81  731 28 New converters for empty lines before clause except subqueries/except short queries -- just started.
// 0.81  730 28 REMOVED: gtstExtQueryKeywordStyle and SELECT group from settings.
//              EmptyLines moved to TreeView settings. New converters for empty lines before clause and around unions.
// 0.81  729 28 ForEach methods moved from SqlConverters to SqlNode, joined both methods into one
//              Two projects instead of 4 => SqlFormat(command line) and SqlFormatter(visual).
// 0.81  728 24 Converters TreeVier data registry storage fixes, 25.002 LINES.
// 0,81  727 20 GitHub publication script, tool for removing comments from SqlVersion.pas
// 0.81  726 19 converters should not call SqlToysExec_ForEach_DeepInside directly, because SqlToysExec_ForEach_DeepInside it iterates by self.
// 0.81  725 14 Converters TreeView settings load and store.
// 0.81  724 14 Converters TreeView icons.
// 0.81  723 14 Converters TreeView - all current converters and icons, Settings Form reorganization.
// 0.81  722 13 Converters TreeView start, new procs rolled out - were hard to store different params,
//              Better way are single procedures which can be turned on or off (TreeView checkbox).
// 0.81  721 11 Case converters moved to new procs.
// 0.81  720 08 Converters instead of lister options: alias keyword, case converters.
// 0.81  719 08 Converters instead of lister options: JOIN converters, ON condition left side, sort order keywords.
//
// 2017-12   -------------------------------------------------------------------
//
// 0.81  718 28 version number for each tool, must be set 3 version numbers for all 3 build types
//              next converter
// 0.81  717 27 fixed xml problem in console app, https://stackoverflow.com/questions/27154319/use-ixml-in-a-delphi-console-application
// 0.81  716 26 XmlTree2Sql console project - Microsoft MSXML is not installed
// 0.81  715 26 xml drag&drop, ScriptOpenFromFile, ScriptSaveToFile.
// 0.81  714 26 xml import finished, skipped nodes removed only full list each time.
// 0.81  713 26 xml import TODO: skipped nodes, something wrong with lister when query list imported from XML.
// 0.81  712 25 xml import started.
// 0.80  711 24 xml output (not full) raised list index out of bonds.
//              xml output names quotation due to quoted alieses.
// 0.80  710 23 release fixes: sort order in ORDER BY function args, test procedure removes ASC and ASCENDING keywords.
//              FormAbout fixed to show licence info.
// 0.80  709 20 release fixes: export to XML error TokenAs is invalid number
// 0.80  708 17 GitHub release.
// 0.79  707 17 FormSettings, SqlParser and SqlLister merged with version 2015-04-28 (brings back options removed for converters).
// 0.79  706 16 SqlFormat: parameters -c and -f. Comments cleanup.
// 0.79  705 16 Sql2XmlTree project finished, 8900 LINES, 267kB (upx)
//              SqlFormat project finished. 13910 LINES, 289kB (upx)
//              SqlFormatter updated to reflect those changes.
// 0.79  704 15 SqlVersion, FormAbout - wersje specjalnie na GitHub.
//              GitHub project version, compile test -- ready to publish.
// 0.79  703 14 unit version enhancement, LIC variables moved to FormAbout.
//              GitHub: REMOVE tags inside FormAbout only, there will be two versions.
//              licence check removed from GtStdCommon to FormAbout.
// 0.79  702 13 About link to GitHub, vss headers. All units rename action.
// 0.79  701 12 Export to XML, XML clarify.
// 0.79  700 11 GitHub initial release plan. BEAUTIFUL VERSION NUMBER <<<<<<<<<<
//              test module runs 42s within Delphi IDE, 1.3s when standalone.
// 0.79  699 08 Export to XML, XML clarify.
// 0.79  698 07 Export to XML.
// 0.79  697 03 Column alias, parameter, function, quoted cols and identifier case converter.
//              Format tags research.
// 0.79  696 02 Column name, table alias and column alias case converter.
// 0.79  695 01 Project name back to "SQL Formatter", UseCustomFrame = True
//              Table name case converter.
//
// 2017-11   -------------------------------------------------------------------
//
// 0.79  694 29 Ribbon case actions: keywords - pojawi³ siê problem.
// 0.79  693 27 Ribbon names "Quote to Delphi" changed to "Quote", "Quote from Delphi" to "unQuote"
//              Ribbon names shortage "Prev query", "Mext query", "Full screen" and converters.
//              Application name "SQL Toys Formatter" exists in 4 places: Application, MainForm, Ribbon, YA_NAME.
// 0.79  692 24 Removed licence limit for script size, 2017 year to about form, NNICE & EASY COME BACK :)
//              return to project after e-mail from Tao Klerks, author of Poor Man SQL Formatter.
//              MainForm.StyleElements.seFont := FALSE -- CHECK ON BIG SCREEN !!
//              upper/lower case converters -- STARTED.
//              DEBUG = 22 819 LINES, RELEASE = 18 147 LINES.
//
// 2016-04   -------------------------------------------------------------------
//
// 0.79  691 09 Switching converter icons.
// 0.79  690 08 Switching converter icons -- not finished.
// 0.79  689 08 Converter icons with Image Magick.
//
// 2016-01 - 03  ---------------------------------------------------------------
//
// Amiga - KARMELIA 4 release, Tanks Furry game playing :)
//
// 2016-01   -------------------------------------------------------------------
//
// 0.79  688 20 Project return - EASY COMEBACK, INNER/OUTER keywords converter. 22 509 LINES - CORRECT THIS.
//
// 2015-07 - 12  ---------------------------------------------------------------
//
// Amiga - KARMELIA 3 fix, Boot Block, Audio Disk, BinTool, MakToolsLibrary, voice synthesis tries.
//
// 2015-06   -------------------------------------------------------------------
//
// 0.79  687 22 W£ADYS£AWOWO. Converters - INT -> INTEGER, correction of *. problem with modified strBreakOnLast.
// 0.79  686 16 Converters - add/remove AS keyword from aliased tables, SortOrder converters.
// 0.79  685 15 0.78 version published, some edition on www site, MainForm.Scaled = True - better effect for Ribbon, now FormShow font change to check.
// 0.79  684 14 Converters - start of, add/remove AS keyword from aliased expressions.
// 0.78  683 13 quote from delphi, icons for test and quote to delphi -- NEW RELEASE VERSION.
// 0.77  682 12 removed dialects and aTvParent args, units renamed to SqlToysXXX.
// 0.77  681 12 Test functions, 838 working test queries,  3 still commented, fixed condtree parse critical problem.
// 0.77  680 11 Test functions, 837 working test queries,  4 still commented, Try to fix condtree parse critical problem.
// 0.77  679 10 Test functions, 837 working test queries,  4 still commented, Try to fix condtree parse critical problem.
// 0.77  678 08 Test functions, 837 working test queries,  4 still commented, fix for column constraints, 1 critical test problem left.
// 0.77  677 06 Test functions, 837 working test queries,  5 still commented, sort order converters TODO.
// 0.77  676 06 Test functions, 835 working test queries,  9 still commented, TestQuery comments parsing problem.
// 0.77  675 05 Test functions, 830 working test queries, 13 still commented, with on clause cond tree and fun param name fix.
// 0.77  674 04 Test functions, 812 working test queries, 32 still commented, with transaction, alter table and exists fixes.
// 0.77  673 03 Test functions, 797 working test queries, with alter table column constraints fix.
// 0.77  672 02 Test functions, 791 working test queries, with lister alter table fix and old style joins fix.
// 0.77  671 01 Test functions, 732 working test queries, 17 029 or 21 910 lines, VSS new database due to its archive size.
//
// 2015-05   -------------------------------------------------------------------
//
// 0.77  670 27 Test functions, 625 working test queries with workaround for INTEGER test problem and QuoteToDelphi function.
// 0.77  669 26 Test functions, 565 working test queries.
// 0.77  668 25 Test functions, 550 working test queries with workaround for AS test problem.
// 0.77  667 23 Test functions, 500 working test queries.
// 0.77  666 21 Test functions, 2010 First Test.sql, MinQueries.sql, AdvQueries.sql, Subqueries.sql, Test cases (columns from aliased tables).sql
// 0.77  665 17 Test functions, 17 056 lines.
// 0.77  664 13 reduced triple to double flicker on Compact or Format by removement of ListByTokens from ParseScript
// 0.77  663 08 SqlNode Name-Value list for properties -- FINISHED.
// 0.77  662 07 SqlNode Name-Value list for properties -- IN PROGRESS.
// 0.77  661 02 standard find replace actions doesnt work better than mine, main form clean up.
// 0.77  660 01 ListByTokens preserves case, FindReplace Ctrl+C, Ctrl+V, Ctrl+X correction, any valid licence file will be used.
//
// 2015-04   -------------------------------------------------------------------
//
// 0.77  659 30 About form renamed to Formatter, SqlToysModuleTools excluded from project, Ribbon font change action.
// 0.77  658 28 Settings and color forms split, About - links to project site, settings available in demo version.
// 0.77  657 27 Project renamed to SqlFormatter (till I'll start sqltoys.pl), demo script size limit 8KB
//              Debug/Release Ribbon icons solved, no debugs problem solved.
// 0.77  656 24 mail to tao@klerks.biz, author of Poor Man's T-SQL Formatter (http://architectshack.com/PoorMansTSqlFormatter.ashx)
// 0.77  655 17 http://sites.google.com/site/sqlformatter/
// 0.77  654 16 http://sites.google.com/site/sqlformatter/
// 0.77  653 14 Form About app name fix, changed licence format - removed release information, demo - paste limit.
// 0.77  652 13 Script size in bytes, demo version for https://sites.google.com/site/sqlformatter.
//
// 2015-03   -------------------------------------------------------------------
//
// 0.77  651 31 Ribbon final touch, main menu removement.
// 0.77  650 30 Ribbon almost finished
// 0.77  649 27 Ribbon menu actions, no parent font, no main menu, my own actions instead of standart ones.
// 0.77  648 26 Ribbon menu actions, icons resized by IcoFX (Delphi sucks), removed Quick Settings panel.
// 0.77  647 26 Ribbon menu actions.
// 0.77  646 23 Ribbon first icons.
// 0.77  645 21 Ribbon first try.
//
// 0.75  644 21 overdozen comments removement, CompilerVersion removement, (c) Tomasz Gierka, www.sqltoys.pl,
//              GtLexerSpeed, GtLexerCounters, GtListerSpeed removement, GtGarbageCollector cleanup 17 604 -> 16 757 LINES.
// 0.75  643 20 overdoze comments removement, CompilerVersion removement, GtLexConverters removed from project, 19 809 -> 17 604 LINES.
// 0.75  642 17 general settings split, GtLexListersNavigator.
// 0.75  641 16 preserve semicolon, FLog disabled in LexTokenizer and Containers.
//
// 0.75  640 08 FindBySubKind removement - debug session
//
// 0.75  639 06 FindBySubKind, gtssClauseTables, gtssSetExpr removement -- FINISHED, DOESNT WORK - FREEZE ON FORMAT !!
// 0.75  638 06 FindBySubKind, gtssUnions, gtssDML, gtssDDL, gtssDCL, gtssTCL, gtssProgram, gtssClauseAlter removement -- IN PROGRESS.
// 0.75  637 06 FindBySubKind, gtssExpr, gtssConstraint removement -- IN PROGRESS.
// 0.75  636 06 FindBySubKind, gtssExprList, gtssExprTree, gtssCond removement -- IN PROGRESS.
// 0.75  635 06 FindByKind, FindByKindAndName, FindBySubKindAndName removed, gtsiExpressionsList to gtsiExprList.
//
// 0.75  634 05 gttk to tk, gtkw to kw abadoned, FindByKeywordAndName -- IN PROGRESS.
// 0.75  633 02 Ctrl+Shift+Arrow, left/right side selection stops on start/end of a word.
//
// 2015-02   -------------------------------------------------------------------
//
// 0.75  632 28 Ctrl+Left stops on start of a word, instead of end of white spaces.
// 0.75  631 27 Ctrl+Left stops on start of a word, instead of end of white spaces.
// 0.75  630 21 too much brackets -- FINISHED.
// 0.75  629 20 version date read from project, passport_v nodes takes 1.8 MB ca. 49 bytes/node.
// 0.75  628 19 version number read from project.
// 0.75  627 17 XE7 transition: polskie znaki w RichEdit -- FINISHED !!
// 0.75  626 14 richedit rtf codes comparision with simple project, 21 026 LINES.
//              polish char codes problem partially solved, doubled lines spacings to correct.
//
// 2015-01   -------------------------------------------------------------------
//
// 0.75  625 08 form about (c) 2010 - 2015, Project version number set to 0.75.0.625, Code page to 1250, Locale ID to Polska,
//              TSaveDialog some tries to center over MainForm.
// 0.75  624 06 open recent files then close file doesnt clear the edit, corrected cursor pointer for last column in line,
//              corrected chars counter for selected text.
//
// 2014-12   -------------------------------------------------------------------
//
// 0.75  623 przystosowanie projektu do ponownej kompilacji z D7 - bez QuickSettings dzia³a.
// 0.75  622 przystosowanie projektu do ponownej kompilacji z D7.
//
// ----  --- miesi¹c zu¿yty na przenoszenie obrazów moich starych dyskietek z Amigi
// ----  --- oraz na próby przygotowania dzia³aj¹cego Amiga BootBlock-u dla Karmelii #5.
//
// 2014-11   -------------------------------------------------------------------
//
// ----  --- miesi¹c zmarnowany przez nagrywanie i obróbkê filmów z FokusTV
// ----  --- oraz przez przenoszenie obrazów moich starych dyskietek z Amigi.
//
// 2014-10   -------------------------------------------------------------------
//
// 0.75  621 XE7 transition: polskie znaki w RichEdit -- NOT FINISHED.
// 0.75  620 ORACLE KEEP DENSE RANK, too much brackets -- IN PROGRESS.
// 0.75  619 ORACLE datatype xxx.yyy%TYPE, ORACLE sql%rowcount and similar expressions.
// 0.75  618 Ctrl+Shift+Right,Left selection without spaces at cursor position.
// 0.75  617 XE7 RichEdit current column number, Ctrl+Left,Right moves to start of next word, Windows 7 status bar height increased.
// 0.75  616 XE7 korekty form: color edits StyleElements bez fonta, rozmiary form i pozycje przycisków, zmiana formatu mem used.
//
// Smokey Quartz Kami (ciemna szara belka, panele jasko szare, pomarañczowe kontrolki)
// Sapphire Kamri     (ciemna granatowa belka, panele jasno niebieskie, pomarañczowe kontrolki)
// Slate Classico     (wszystko jasno szare, jasno pomarañczowe kontrolki)
// Light              (wszystko bia³e)
//
// 0.75  615 XE7 transition: RichEdit works with colors, About form transparency fixed.
// 0.75  614 XE7 startup: 28 warnings mostly CharInSet, 7 hints mostly uses, RichEdit doesnt work with colors.
//
// 0.73  613 nowe foldery bat, obsolete, Projects D7, Projects XE7.
// 0.73  612 du¿e ikony 96x96 dla Visty, effect Blur tworzy rzeczywisty alphachannel, ikona dodana do pliku res programem Melander Resource Editor.
// 0.73  611 nowe ikony dla SQL Toys Formatter, bez podpisu SQL Toys, konwersja programem IconFX.
// 0.73  610 nowe ikony dla SQL Toys Formatter, z podpisem SQL Toys, szukanie programu do konwersji ikon.
//
// 2014-09   -------------------------------------------------------------------
//
// 0.73  609 main form panels removed, main menu settings moved to Tools, checked all functions available from menu.
// 0.73  608 lincence check to mainform initialization, main menu restored.
// 0.73  607 frame script edit references removed, SQL Toys Formatter project started - 20 717 LINES.
// 0.73  606 status bar panel enums.
// 0.73  605 frames and forms dependencies cleanup.
// 0.73  604 frames and forms dependencies cleanup -- IN PROGRESS.
// 0.73  603 frames and forms dependencies cleanup -- IN PROGRESS.
//
// 2014-08   -------------------------------------------------------------------
//
// 0.73  602 cleanup.
// 0.73  601 SqlToysModuleMain removement.
// 0.73  600 Units and form names changed to SqlToys, SqlToysCommon, USES CLEAN UP - 600 SESJA, 21 650 LINES.
// 0.73  599 Project name changed to SqlToysYetAnother.
// 0.73  598 YaDataModuleTools, YaDataModuleMain -- NOT FINISHED.
// 0.73  597 YaDataModuleDbGrid -- REMOVED, YaDataModuleMain -- NOT FINISHED.
// 0.73  596 Frames -- STARTED.
// 0.73  595 YaDataModules -- NOT FINISHED, 21 500 LINES.
// 0.73  594 YaDataModules -- NOT FINISHED.
//
// 2014-07   -------------------------------------------------------------------
//
// 0.73  593 YaDataModules -- NOT FINISHED.
// 0.73  592 YaDataModules -- NOT FINISHED.
// 0.73  591 YaDataModules -- NOT FINISHED.
// 0.73  590 YaDataModules -- NOT FINISHED.
// 0.73  589 YaDataModules -- NOT FINISHED.
// 0.73  588 Licence about form.
// 0.73  587 Licence code module, 21 129 LINES
// 0.73  586 Recent files clear.
// 0.73  585 GtLexStructsNoDebug - GtLexStructs DEBUGINFO OFF, SpaceBeforeExpressionNeeded to CheckSpaceNeedBeforeExpression,
//           Parse_SelectQueryInBrackets.
// 0.72  584 STABLE VERSION
//
// 2014-06   -------------------------------------------------------------------
//
// 0.70  583 wrong format NOCYCLE.sql
// 0.70  582 hang FUNCTION nested calls - DONE, wrong DISTINCT star, wrong files cleanup.
// 0.70  581 hang format UNION misc - LONG - over 120 secs - DONE, format hang IN BRACKET, hang FUNCTION nested calls - NOT FINISHED.
// 0.70  580 hang format UNION misc - LONG - over 120 secs - NOT FINISHED.
// 0.70  579 wrong memory status bar, wrong no WHERE after format, wrong no WHERE in deepest query after format.
//
// 2014-05   -------------------------------------------------------------------
//
// 0.70  578 expr & cond with too much brackets - wrong parse COND escalation.sql, wrong parse EXPR escalation.sql, wrong WHERE 3.sql
// 0.70  577 expr & cond with too much brackets - SELECT subquery expression, expr escalation.
// 0.70  576 expr & cond with too much brackets - cond escalation, test scripts agregated 171 files into 142.
// 0.70  575 wrongs cleanup.
// 0.70  574 vector wrongs.
// 0.70  573 expr & cond with too much brackets, completely new expression tree inside brackets.
// 0.70  572 expr & cond with too much brackets, test cases -- PROBABLY DONE.
//
// 2014-04   -------------------------------------------------------------------
// 2014-03   -------------------------------------------------------------------
// 2014-02   -------------------------------------------------------------------
//
// 0.70  571 expr & cond with too much brackets, cond tree escalation with brackets -- IN PROGRESS,
//           AND escalation done, OR escalation DONE, missing brackets after Format -- partially fixed.
// 0.70  570 expr & cond with too much brackets, cond tree escalation with brackets -- IN PROGRESS,
//           AND escalation done, OR escalation DONE, missing brackets after Format -- partially fixed.
//
// 2014-01   9 SESJI -----------------------------------------------------------
//
// 0.70  569 expr & cond with too much brackets, cond tree escalation with brackets -- IN PROGRESS,
//           AND escalation done, OR escalation DONE, TODO: missing brackets after Format.
// 0.70  568 expr & cond with too much brackets, cond tree escalation with brackets -- IN PROGRESS,
//           AND escalation done, OR escalation TODO, TODO: missing brackets after Format.
// 0.70  567 expr & cond with too much brackets, cond tree escalation with brackets -- IN PROGRESS,
//           wrong too much brackets, wrong expr vs vector or cond list or tree.
// 0.70  566 expr & cond with too much brackets -- IN PROGRESS, 20 500 LINES.
// 0.70  565 REFACTORING. Parser: aItem -> aOwner, Lister: aItem, aExpr, aList -> aNode,
//           expr & cond with too much brackets -- IN PROGRESS.
// 0.70  564 REFACTORING. GtSqlItem -> GtSqlNode, GtSqlItem.Owner type TGtItem -> TGtSqlItem,
//           Class clones removed, gttoLogExpected -> gttoExpected, gttoDontGetNextToken -> gttoDontGetNext.
// 0.70  563 vectors for IN and SET ops, 20 500 LINES.
// 0.70  562 Parse___RightBracket then Exit removed, INSERT .. RETURNING .. INTO, RETURN.
// 0.70  561 About (c) 2014, version 0.70, Fun params new color, fun params and expr tree joined to single class.
//
// 2013-12   5 SESJI -----------------------------------------------------------
//
// 0.69  560 GtContainers DEBUGINFO and LOCALSYMBOLS to OFF, wrong no IN condition wo close bracket,
//           wrong no ALIAS after AS - terminate keyword, wrong second CONSTRAINT.sql
//           wrong two CONSTRAINTS on a column.sql, try column with their constraints.sql
//           wrong CREATE TABLE CHECK.sql, wrong ALTER TABLE MODIFY.sql, wrong AV or no column.sql
// 0.69  559 Fun params color fixed.
// 0.69  558 Settings width resized, script position on status bar corrected,
//           colors DELETE CC00CC, UPDATE CC4400, BEGIN END block.
// 0.69  557 VirtualBox 4.3.4 debug problems, revert to 4.2.20
// 0.69  556 comments for ML_ variables, UNION expr and alias intend.
//
// 2013-11   6 SESJI -----------------------------------------------------------
//
// 0.69  555 editor double click selection, view main menu.
// 0.69  554 editor selection length in brackets, CREATE GLOBAL TEMPORARY TABLE - break on 2,
//           Ext query alias color FF8800, expr xxx.[yyy], Oracle function params with => operator.
// 0.69  553 Ctrl+Left, Ctrl+Right wo. dots.
// 0.69  552 Shift+Left, Shift+Right, wrong JOIN QUERY cond not sorted.
// 0.69  551 Ctrl+Shift+Left, Ctrl+Shift+Right, GtStdRegistry key datatype mismatch fixed.
// 0.69  550 Ctrl+Tab no marked script delete, @ params, SELECT/INSERT/UPDATE/DELETE as query terminators,
//           CASE at new line, tabbed settings, Function upper/lower case opt,
//           INSERT close bracket intend with ClauseBodyIntend, Shift-Ctrl-Left/Right improvement,
//           Max Clause Intend + Clause Body Space divided by 2 for nice intends outside YetAnother.
//           20 000 LINES AGAIN.
//
// 2013-10   5 SESJI -----------------------------------------------------------
//
// 0.69  549 INSERT columns style fixed, no empty lines after last query,
//           About form center position inside main form, Ctrl-Left, Ctrl-Right.
// 0.69  548 empty lines before ORDER BY, empty lines after query, ListByToken after Ctrl+V,
//           wrong BETWEEN expr, workaround of RichEdit line insertion error.
// 0.69  547 percent parameter (for Slawek B), AddNewLine to AddCurrLine, AddEmptyLine, LastLineEmpty removed,
//           GetCurrToken - MISTAKE, ORDER BY empty line after UNION.
// 0.69  546 wrong AV ON condition broken, wrong CLAUSE FROM no empty line, wrong EXPR intend with no AS keyword.
// 0.69  545 AnsiUpperCase i AnsiLowerCase, ref columns style removed, column quoted alias case.
//
// 2013-09   6 SESJI -----------------------------------------------------------
//
// 0.69  544 wrong empty line before CLAUSE on short query to no linespaces, wrong no empty lines before clauses (short query set to 6 lines),
//           wrong DELETE FROM intend 2, wrong IN UNION empty lines, wrong ALIAS intend, wrong INSERT INTO after format
// 0.69  543 UNION ALL listed as separate tokens, UNION/MINUS empty lines depends on empty lines before clauses,
//           dialect None renamed to General (SQL 92 or none of above), number of empty lines after query (optional).
//           some cleanup with empty line before clause, Tab intend to nearest column.
// 0.69  542 View gutter action removed, QuickSettings - Show Navigator button, Empty line before claues.
// 0.69  541 too long identifiers as errors only chars abowe limit, ANSI to OldStyle removed,
//           START WITH no empty after CONNECT BY, QuickSettings just over editor, Tab in search text form fixed.
//           wrong ORDER BY too much empty lines, wrong WHERE AV no conditions.
// 0.69  540 grid font settings, too long identifiers as errors, no MINUS/UNION wo following query.
// 0.69  539 some default settings changed, SaveAs - file overwrite confirmation,
//           QuickSettings visible by default, Expr alias intend to QuickSettings.
//
// 2013-08   6 SESJI -----------------------------------------------------------
//
// 0.69  538 Mem used format, removed Parse command, constraint name case, ext query keyword style fixed.
// 0.69  537 full version date, Ctrl+R show/hide grid, first char upper case, case checkbox-es changed to combos.
// 0.69  536 ORACLE nested query tables identifiers are limited to 1 level
//
// 0.68  535 STABLE VERSION.
//
// 0.67  535 Quick Settings Panel: Right justify clauses, No semicolon on single query, Ext query keyword style.
// 0.67  534 Shift-Ctrl-U inserted uppercase text before selected, no space before DISTINCT inside COUNT function,
//           intend at end of line doesnt work, DELETE FROM as one clause when no expr-list.
// 0.67  533 missing colon on params parsing (ie. :NEW.ID)
//
// 2013-07   -------------------------------------------------------------------
//
// 2013-06   8 SESJI -----------------------------------------------------------
//
// 0.67  532 SELECT INTO - MS, ORCL
// 0.67  531 DELETE exprlist, remove comments 19 913 -> 19 607 LINES.
// 0.67  530 MaxClauseKeywordIntend (settings), DDL CREATE VIEW and AGGREGATE FUNCTION STYLES,
//           DELETE exprlist -- NOT FINISHED.
// 0.67  529 Ext query keyword style (opt).
// 0.67  528 ON CASCADE RESTRICT, ALTER TABLE DEALLOCATE UNUSED, ENABLE ROW MOVEMENT, SHRINK SPACE,
//           ALTER INDEX, ANALYZE INDEX.
// 0.67  527 DROP TABLE PURGE, PURGE RECYCLEBIN, CREATE MATERIALIZED VIEW, INSERT INTO wrong keyword.
// 0.67  526 TODO CLEANUP & ORGANIZATION, Boolean datatype.
// 0.67  525 New line before ORDER BY when UNIONS used, Editor
//
// 2013-05   14 SESJI ----------------------------------------------------------
//
// 0.67  524 DROP TABLE CASCADE CONSTRAINTS, TOP, LIMIT, INSERT OR REPLACE, TGtSqlItem memory save concept.
// 0.67  523 multi identifiers GRANT, DENY and REVOKE, CREATE USER and CREATE LOGIN.
// 0.67  522 upper,lower case options, numeric datatype with star, VARCHAR(N CHAR).
// 0.67  521 wrong TRUNCATE TABLE, wrong CREATE TABLE AS, wrong subquery intend, wrong ON COND intend.
// 0.67  520 gtssConsReferenceKey some cleanup, SaveAs shows new filename on bar and adds to RecentFiles.
// 0.67  519 wrong params wno colon after format.sql, wrong FOR UPDATE column name.sql
//           wrong FOR UPDATE intend.sql
// 0.67  518 Parse_NameAndDots, GetTableName removed (gtssOtherTableRef cleanup)
// 0.67  517 TGtSqlItem properties usage (XLS).
// 0.67  516 TGtSqlItem properties usage (XLS).
//
// 0.66  515 wrong PRIOR in IN.sql, TGtSqlItem references usage cleanup.
// 0.66  514 dont parse on clipboard actions, dont maximize on start, wrong PRIOR in SELECT and WHERE.sql
//           GtLexStruct service functions.
// 0.66  513 GtLexStruct service functions.
// 0.66  512 ON references first - FINISHED.
// 0.66  511 ON references first - WORKS WITH PROBLEMS.
//
// 2013-04   10 SESJI ----------------------------------------------------------
//
// 0.66  510 wrong table alias B2 vs AS keyword hash.sql, wrong DELETE FROM intend.sql
//           wrong IN PRIOR.sql, wrong ON current query alias color.sql,
//           wrong UPDATE aliased column.sql -> ExprValue replaced with Name -- CHECKS NEEDED.
// 0.65  509 TODO 0.65 topics checked then removed, CREATE TABLE empty line before closing bracket removed.
//           TODO 0.65 & 0.70
// 0.65  508 ON COMMIT DELETE ROWS, Keyword Body Space ONLY for opend and close brackets.
//           wrong alias intend for subquery, empty line before SELECT clause in INSERT query.
//           ORDER BY NULLS FIRST or LAST, MaxClauseToIntend.
// 0.65  507 ON cond intend only first cond, ClauseKeywordSpace -> ML_ClauseKewyord,
//           some functions moved from Lister to Struct, some wrongs done.
// 0.65  506 wrong subquery empty line before clause, ON cond rigth side const value intend,
//           gtloColumnConstraint removed, SELECT subquery intend.
// 0.65  505 wrong IN intend, AV on Ctrl+X. DIRTY WORK: INTERTELECOM.
// 0.65  504 CheckThenParse function, ON condition intendations.
// 0.65  503 List_Clause_Name finished and tested, fun ClauseAppend, 19 691 -> 19 002 LINES.
// 0.65  502 List_Clause_Name contination. DIRTY WORK: ZAK.
// 0.65  501 Star expr alias, ExprList alias intend - GREAT !!
//
// 2013-03   22 SESJE ----------------------------------------------------------
//
// 0.65  500 TODO CLEANUP - 500 SESJA.
// 0.64  499 Font size on statur bar after change, intend (tab), unintend (shift tab) + preserve selection. 19 500 LINES.
// 0.64  498 editor upper/lower case, PRIOR color, new line before clause - for long queries, form About with no barcode.
// 0.64  497 right side ON condition intend, no spaces inside brackets for datatypes.
// 0.64  496 More contrast on brackets colors, non existed aliases colored as errors,
//           double space between table name and its alias when no AS keyword is used,
//           left side ON condition intend.
// 0.64  495 SELECT expression alias intend to static right margin.
// 0.64  494 Table alias uppercase. Settings form intendation.
// 0.64  493 ExprTreeOwner. ON cond intend. DIRTY WORK: POZMAN.
// 0.63  492 ALTER TRIGGER, ALTER TABLE column REFERENCES, wrong alias color in expr for JOIN subquery, wrong scripts cleanup.
// 0.63  491 SELECT INTO, CAST and brackets for single param.
// 0.63  490 Empty lines before clauses - except subquery, CREATE TABLE empty line before complex constraints
// 0.63  489 CREATE TABLE separate options. 19 000 LINES.
// 0.63  488 CREATE TABLE columns and datatypes justify.
// 0.63  487 Join condition left side order, empty line after column constraint in CREATE TABLE.
// 0.63  486 Parse and List most common functions - IN PROGRESS.
// 0.63  485 Space inside brackets skip on one param function.
// 0.63  484 Column prefix colors as alias or table if it is an alias or table name.
// 0.63  483 AddClauseBack FINISHED, INNER keyword option.
// 0.63  482 AddClauseBack - NOT FINISHED. DIRTY WORK: POZMAN.
// 0.63  481 ListerOpt.
// 0.63  480 GLOBAL TEMP TABLE, MATERIALIZED VIEW, comments with CR-LF.
// 0.63  479 PrevComplexToken, NULL color, DIRTY WORK: POZMAN.
//
// 2013-02   13 SESJI ----------------------------------------------------------
//
// 0.63  478 Dynamic clause intend, status bar messeges cut to 127 chars. 18 500 LINES.
// 0.63  477 CAST and CONVERT functions and FLOAT datatype.
// 0.62  476 CREATE TABLE single column constraint at new line, no semicolon on single query,
//           NULL color.
// 0.62  475 left side expression alignment in SET clause.
// 0.62  474 tables and aliases alignment in FROM clause.
// 0.62  473 three spaces intend.
// 0.62  472 statusbar time log, time format for query exec, ALTER TABLE clauses colors.
// 0.62  471 script cut on paste, action time measures, RichMemo takes too much time.
// 0.62  470 TODO for next versions. List Navigator only when visible.
// 0.62  469 TODO edits, ListByToken vs Format diffs check.
// 0.62  468 NewItem method. Parser and Colorizer work actions split.
// 0.61  467 parser procedures options, and check token procedure options.
// 0.61  466 parser procedures options.
//
// 2013-01   11 SESJI ----------------------------------------------------------
//
// 0.61  465 Item names removed.
// 0.61  464 Dialect settings, outermarks optional parse. Item names removing -- NOT FINISHED.
// 0.61  463 Multi-token function name. Navigator. Lister option parameter.
// 0.61  462 Navigator. Column list for PK,FK,UQ,IX, single FROM clause one level up,
//           SetExpressions, clause VALUES, and CHECK rebuilded. 18 000 LINES.
// 0.61  461 Navigator. TESTY, function args.
// 0.61  460 Navigator. IN condition, WHERE keyword changes to OR, ORDER BY with UNION,
//           QueryItemHighlight by TreeNode.
// 0.60  459 REFACTORING. Other tasks, cleanup.
// 0.60  458 REFACTORING. Parse_ExprTree REBUILD ABADONED. TESTY.
// 0.60  457 REFACTORING. Expressions, expr priorities -- Parse_ExprTree REBUILD NEEDED.
// 0.60  456 REFACTORING. Expressions, expr priorities -- IN PROGRESS.
// 0.60  455 REFACTORING. Expressions, ExprReverseOp. ZAAWANSOWANE TESTY EXPRESSIONS.
//
// 2012-12   12 SESJI ----------------------------------------------------------
//
// 0.60  455 REFACTORING. Expressions. ExprOp -> Expression.
// 0.60  454 REFACTORING. TESTY.
// 0.60  453 REFACTORING. TESTY.
// 0.60  452 REFACTORING. loops with FindBy... changed to for loops. TESTY.
// 0.60  451 REFACTORING. TGtSqlItem Token based properties, added separators to multiple word tokens, DOWN TO 17 797 LINES.
// 0.60  450 REFACTORING. TGtSqlLogicalOperator to Token, etc, DOWN TO 17 828 LINES.
// 0.60  449 REFACTORING. TGtSqlItem properties renamed, List method case.
// 0.60  448 REFACTORING. compact TGtSqlItem subclasses, TGtConditions and TGtExpressions.
// 0.60  447 REFACTORING. compact TGtSqlItem subclasses, 21 500 -> 18 387 LINES.
// 0.60  446 REFACTORING. compact TGtSqlItem subclasses
// 0.60  445 REFACTORING. compact TGtSqlItem subclasses.
// 0.60  444 REFACTORING. compact TGtSqlItem subclasses, removed TGtSqlQueryItem and TGtSqlQueryOrStructItem, TGtIndex to TGtUnique.
//
// 2012-11   11 SESJI ----------------------------------------------------------
//
// 0.60  443 REFACTORING. Struct Lister, Structs and Linker (Materializer) removed.
// 0.60  442 REFACTORING. Expression sub functions. StdCommon - substring functions and binary constants.
// 0.60  441 New About image with photo and QR code (shaded for this moment due to lack of the internet domain)
// 0.60  440 REFACTORING. Expression list code clean-up.
// 0.60  439 REFACTORING. NavigatorLister code check-out, TreeView fullcolor and AddCount function.
//           Navigator scrollbars (too wide) resolved.
// 0.60  438 REFACTORING. Another AddTreeNode shortage function, NavigatorLister code cleanup.
// 0.60  437 REFACTORING. AddTreeNode shortage function, NavigatorLister code cleanup.
// 0.60  436 REFACTORING. TGtSqlNavigatorLister (Syntax Tree -> Navigator) new class, 21 000 LINES.
// 0.59  435 filename missing on application name bar after file open dialog close,
//           subquery intendation in expression, in IN & EXISTS conditions.
// 0.59  434 empty script on start, strings with subsequent apostrophes, UNION quoting inside FROM clause subquery.
//           format - additional right bracket after subquery, wrong bracket colors when clause.
// 0.59  433 AV on function whitout params in brackets, CONNECT BY adds PRIOR to expression
//           ADD COLUMN with constraint.
//
// 2012-10   17 SESJI ----------------------------------------------------------
//
// 0.59  432 FOR UPDATE clause, Format and Compact preserves Modified flag,
//           SkipOneExprOnLine repaired after nested functions, wrong apostrophe color.
// 0.59  431 version renum, 2 numbers missed, version 0.60 requirements.
// 0.59  430 UNION fourth query quoted as subquery (if to while), dont read from and store to registry ShowGrid.
//           negative numbers joined with minus operator, open file - was storing not opened filed in recent files list.
// 0.59  429 common registry functions for styles.
// 0.59  428 wrong subquery split on union, expression terminate tokens function, CONNECT BY.
// 0.59  427 UNION third query was quoted as subquery, function name uppers,
//           recent files code and registry cleanup, ignores subsequent semicolons after query,
//           form about yase version info.
// 0.59  426 Case colors, parameters with dots inside, column from aliased tables - NOT FINISHED,
//           Empty lines around UNION, INSERT without column list, 20 000 LINES AGAIN.
// 0.59  425 Case colors, settings - IN PROGRESS.
// 0.59  424 Wawa, Case intendation, recent files limit raised from 5 to 10.
// 0.59  423 Full Screen with no circullar form repaintings, CommaAtNewLine replaces !CommaAfterExpr,
//           SkipOneExprOnLine fix for functions, SkipOneCondOnLine introduced,
//           destroy not parsed class for Expr, ExprTree, Cond, CondTree.
// 0.59  422 Identifiers with #$ chars, skip one expression on line inside function parameters list,
//           cursor at top after file open, expr list comma at new line fixed,
//           space inside/outside brackets, space around operator, space after comma.
// 0.59  421 Serock, functions for all registry settings, ESCAPE parse and list.
// 0.59  420 Serock, internal registry settings wrapper functions and lister settings, 19850 -> 19 648.
// 0.59  419 Serock, Format Lines groupbox, New line before clause, parse and color parametersm
//           toogle ext. keyword colors (Shift+F8), toggle ext. identifier colors (Shift+F7)
//           main menu accelerators, INSERT INTO columns list on one column fixed, 20 001 -> 19 850.
// 0.59  418 Parser and Lister - Brackets and spaces problems with IN condition,
//           drag & drop open file, single expr/cond on line, HTML file removed,
//           skip ASCENDING, switch ASC/ASCENDING.
// 0.59  417 Arithmetic expressions levels error, space before operator, expression
//           and bracket, do not store default values to registry.
// 0.59  416 Fixed IS NOT NULL condition parse error, Expressions Color removed,
//           One condition per line, TRUNCATE TABLE.
//
// 2012-09   13 SESJI ----------------------------------------------------------
//
// 0.58  415 Buchal Query - IN PROGRESS.
// 0.58  414 Ref. column color - IN PROGRESS.
// 0.58  413 Settings cleanup, 20 500 -> 19 750 LINES.
// 0.58  412 AddKeyword removed, Settings cleanup - IN PROGRESS.
// 0.58  411 Column def constraints, Inner/Outer Joins.
// 0.58  410 Identifier names, Column def constraints - IN PROGRESS.
// 0.57  409 KeywordStyle for Parser, Transaction Color
// 0.57  408 DML Colors, KeywordStyle for Parser & Lister (cleanup task) - IN PROGRESS
// 0.57  407 DDL TCL Colors - DONE, Default Colors, Ext Colors Switches, 20 000 LINES
// 0.57  406 DDL Colors - IN PROGRESS
// 0.57  405 Union & Constraint Colors - WORKS
// 0.57  404 DML DDL DCL Colors - not finished, 19 500 LINES
// 0.57  403 Quoted Identifiers, Column Aliases Color
//
// 2012-08   3 SESJE -----------------------------------------------------------
//
// 0.56  402 Font dialog
// 0.55  401 Color dialogs
// 0.54  400 About form
//
// 2012-07   3 SESJE -----------------------------------------------------------
//
// 0.53  399 Synonyms, grants, 19 000 LINES
// 0.52  398 Views, not finished
// 0.51  397 TCL Queries, 18 500 LINES.
//
// 2012-06   15 SESJI ----------------------------------------------------------
//
// 0.51  396 TCL Queries, not finished
// 0.50  395 DASHBOARD QUERY PARSED, function color
// 0.50  394 function expressions, CASE errors
// 0.50  393 CASE expression, COUNT expression
// 0.50  392 CASE expression, 390 SESJA
// 0.50  391 CASE Expression, not finished, 18 000 LINES
// 0.49  390 Recent files done), SELECT DISTINCT, expr list problem when brackets used
// 0.49  389 Brackets 6 colors, Recent files not finished
// 0.49  388 Recent files, not finished, backup file extensions
// 0.48  387 XDBGrid optimal column width finished
// 0.48  386 XDBGrid colum auto width and other small improvements
// 0.48  385 XDBGrid, columns visibility and filtering, 17 500 LINII
// 0.48  384 XDBGrid, 382 SESJA
// 0.47  383 full screen wo window bar and menu
// 0.47  382 disabled color, tree view subqueries and condition simplify
//
// 2012-05   17 SESJI ----------------------------------------------------------
//
// 0.46  381 tokens lister, tabs to spaces, store edit font size
// 0.46  380 tokens lister on file open, works with all current scripts
// 0.46  379 tokens lister on font size up and down, rich text r&d - slow and no proper backward selection
// 0.46  378 tokens lister, parse & list col ref, not recognized complex tokens on token list error
// 0.46  377 tokens lister, parser sets token style, token subtype removed
// 0.46  376 tokens lister, parser sets tokens style
// 0.46  375 tokens lister class, 17 000 LINES
// 0.46  374 tokens lister, proto lister class extracton
// 0.45  373 lister column style, backup files, 371 SESJA
// 0.45  372 save unformatted script, current timming, lister bracket style
// 0.45  371 lister add functions replacement
// 0.45  370 CAPS and INS-OVR states, SaveAs on Close repaired
// 0.44  369 find & replace - finished
// 0.44  368 find & replace - case sensitive
// 0.44  367 find & replace - back to work
// 0.44  366 settings - styles and defaults
// 0.44  365 settings - colors - not finished
//
// 2012-04   16 SESJI ----------------------------------------------------------
//
// 0.43  364 lister - common colors - done
// 0.42  363 rich edit & tree view lister colors
// 0.42  362 rich edit lister, 360 SESJA
// 0.41  361 plus memo replaced with rich edit, 16 000 LINES
// 0.41  360 plus memo replaced with rich edit - not finished
// 0.4   359 RICH EDIT R&D <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
// 0.4   358 font size up and down, tree view token list, 16 000 LINES
// 0.4   357 find and replace done
// 0.4   356 find and replace almost finished
// 0.4   355 find and replace form
// 0.4   354 smart view options w. Full Screen, Tree View Custom Draw refinement
// 0.4   353 actions cleanup - finished
// 0.4   352 actions cleanup, 350 SESJA
// 0.4   351 actions cleanup - not finished
// 0.4   350 connection string editor, treeview blank on Win7
// 0.4   349 connection manager
//
// 2012-03   18 SESJI ----------------------------------------------------------
//
// 0.4   348 settings form, 346 SESJA, 2 LATA PROJEKTU <<<<<<<<<<<<<<<<<<<<<<<<<
// 0.4   347 lister consolidation abbaddoned
// 0.4   346 treeview colors - conditions - 15 000 LINES
// 0.4   345 treeview colors - DDLs
// 0.4   344 treeview colors - clauses
// 0.4   343 treeview colors - start
// 0.4   342 FKMG modules joined, 340 SESJA
//       341 removed class checks, 12 500 LINES
//       340 mem leaks
//       339 conditionals, clean up
//       338 tokenizer speed up - clean up
//       337 tokenizer speed up - 140 ms
//       336 tokenizer speed up - 426 ms
//       335 tokenizer speed up - 1229 ms
//       334 tokenizer speed up - 2880 ms
//       333 tokenizer speed up removed script position class
//       332 tokenizer speed up function simplification, 330 SESJA
//       331 tokenizer speed up function simplification
//
// 2012-02   22 SESJE ----------------------------------------------------------
//
//       330 tokenizer speed up time measures
//       329 tokenizer speed up counters
//       328 test script, default value expr tree
//       327 test script - column defs
//       326 token list - create table
//       325 parse log out
//       324 parse log out - NOT FINISHED
//       323 AddItem cleanup
//       322 AddItem R&D, 320 SESJA
//       321 unions
//       320 speedup by disabling Garb Collector
//       319 constraints three forms, float values fix
//       318 foreign key shortest form, ddl table names
//       317 tree view DDL
//       316 identity, unique
//       315 sort order, constraints long and short form
//       314 lister - subsequent alter clauses
//       313 code cleanup
//       312 lister - works again, 310 SESJA, STILL 13 000 LINES
//       311 lister - index, add column
//       310 lister - subqueries
//       309 lister - create table column, pf, fk
//
// 2012-01   13 SESJI ----------------------------------------------------------
//       308 lister - create table constraints - not finished
//       307 parser and listers works again with issues
//       306 lister - create table
//       305
//       304 removing general parse function - not compiling
//       303 removing general parse function
//       302 removing general parse function, 300 SESJA
//       301 parser to materializer
//       300 parser to materializer
//       299 parser to materializer - DDL parsers done
//       298 parser to materializer - alter table
//       297 parser to materializer - alter table
//       296 parser to materializer - create table and constraint finished
//
// 2011-12   10 SESJI ----------------------------------------------------------
//
//       295 parser to materializer - some improvements, CHECK to materialize later
//       294 parser to materializer - unique and check to materialize
//       293 parser to materializer - foreign key materializer to finish, reference key concept do finish]
//       292 parser to materializer - foreign key still in rework, 290 SESJA
//       291 parser to materializer - primary key and foreign key
//       290 parser to materializer - column def done and constraint to start
//       289 parser to materializer - column def
//       288 parser to materializer - materialize table reference and create table
//       287 parser to materializer - around column def cleanup
//       286 parser to materializer - create table
//
// 2011-11   13 SESJI ----------------------------------------------------------
//
//       285 parser to materializer - table reference done
//       284 parser to materializer - tab ref check names
//       283 class unification - constructors loop solved
//       282 class unification - lists and item, doesnt work, 280 SESJA
//       281 class unification - lists
//       280 class unification - lists
//       279 lists cleanup
//       278 lists cleanup
//       277 structs - renamed to Qi Si, problem with Si collection in Qi]
//       276 parser - garbage collectors AVs removed
//       275 parser - alter table wrong reference of CHECK CONSTRAINT
//       274 parser - alter table, problems with complex keywords ie ADD CONTRAINT
//       273 yet another - working back, 271 SESJA
//
// 2011-10   12 SESJI ----------------------------------------------------------
//
//       272 parser - find nearest token, token owners not finished, 270 SESJII
//       271 parser - token script position
//       270 lexers - token script position
//       269 lexers - clean up
//       268 lexers - works, parser prev token to prev relevant token
//       267 lexers - parser relevant tokens
//       266 lexers - expression relevant tokens text - TODO
//       265 lexers - looks like it will be working, DZIEN WOLNY
//       264 lexers - parser switch to new tokenizer, DZIEN WOLNY
//       263 lexers - parser switch to new tokenizer
//       262 lexers - complex tokens, 260 SESJII
//       261 lexers - comments
//
// 2011-09   6 SESJI -----------------------------------------------------------
//
//       260 lexers - complex tokens
//       259 lexers - token defs
//       258 lexers - token defs
//       257 lexers - sql tokenizer and tokens
//       256 lexers - start working
//       255 lexers - new tokenizer
//
// 2011-08   30 SESJI ----------------------------------------------------------
//
//       254 lexers - new tokenizer
//       253 lexers - new tokenizer
//       252 lexers - new tokenizer first steps, 250 SESJI, W SAMOCHODZIE
//       251 lexers - classes concept, W SAMOCHODZIE
//       250 lexers - classes concept
//       249 lexers - new tokenizer - concept
// 0.3   248 script text reference lister
// 0.3   247 query select and run - improved
// 0.3   246 query run smart
// 0.3   245 database connect, query results
// 0.2   244 query syntax tree, big icons rework
// 0.2   243 query syntax tree, 13 000 LINES
// 0.2   242 F5 F6 F7 shortcuts, query syntax tree, icons
// 0.2   241 Y icon, Select All, Undo, Redo, Shift + F5
// 0.2   240 main form cleanup, query to tree view lister
// 0.2   239 main form cleanup
// 0.2   238 parser tree view double click
// 0.2   237 scope and treeviews - starts
// 0.2   236 plus memo
// 0.1   235 save buttons
// 0.1   234 memory leak, other left
// 0.1   233 FList.Count - AV removed
// 0.1   232 fields in new line for insert into & create table
// 0.1   231 query type highlight
// 0.1   230 separate add aliases and add prefixes & add space before semicolon
// 0.1   229 formatting - space before comma
// 0.1   228 smart aliases finished
// 0.1   227 smart aliases, 12 000 LINES
// 0.1   226 format options
// 0.1   225 formatting options
//
// 2011-07   28 SESJI ----------------------------------------------------------
//
// 0.1   224 garbage collector leaks in progress
// 0.1   223 compact, intend query formats, show parse log, struct and some errors repaired
// 0.1   222 converters - works ok, garbage collector destructor disabled
// 0.1   221 converters - works with troubles
// 0.1   220 converters - get back to work, clones new owner
// 0.1   219 query assistant - clone problems
// 0.1   218 query assistant - NEW PROJECT
//       217 subqueries - save script rework - DONE
//       216 subqueries - save script rework
//       215 subqueries - save script rework
//       214 subqueries - save script rework
//       213 subqueries - save script rework
//       212 subqueries - save script rework
//       211 subqueries - save script rework
//       210 subqueries - save script rework
//       209 subqueries - save script rework
//       208 save parse log - broken intends
//       207 save parse log - broken intends
//       206 Garbage Collector - Class No
//       205 subqueries - invalid logs removed
//       204 subqueries - script lister - from and colors
//       203 subqueries - script lister - select, exists, in
//       202 subqueries - parsers and parse log listers, 200 SESJII]
//       201 subqueries - small changes and GtQuery refactoring
//       200 subqueries - GtQueryItem.GetQuery, GtQuery.IsSingleRowsetQuery
//       199 subqueries - work map
//       198 subqueries - select clause, expression linked to upper query
//       197 subqueries - concept
//
// 2011-06   21 SESJI ----------------------------------------------------------
//
//       196 functional refactoring - AV cleanup, store table name for failed ALTER TABLE's
//       195 functional refactoring - units movement, ddl test without warn, AccessViolation
//       194 functional refactoring - TGtSqlStatement to TGtSqlQueryItem, some Logs cleanups
//       193 functional refactoring - units movement and rename
//       192 functional refactoring - units movement and rename
//       191 functional refactoring - Script param removed
//       190 functional refactoring - Script Text reference
//       189 functional refactoring - Tokenizer to Script changed
//       188 functional refactoring - Parser
//       187 functional refactoring - Parser
//       186 functional refactoring - Parser
//       185 functional refactoring - Parser
//       184 functional refactoring - Parser
//       183 functional refactoring - Parser
//       182 functional refactoring - SrciptLister
//       181 functional refactoring - ScriptLister
//       180 functional refactoring - ScriptLister
//       179 functional refactoring - StatementLister
//       178 functional refactoring - StatementLister
//       177 functional refactoring - StatementLister
//       176 functional refactoring - StatementLister
//
// 2011-05   36 SESJI ----------------------------------------------------------
//
//       175 functional refactoring - StatementLister
//       174 functional refactoring - save parse log - statement lister
//       173 references with TempItemList always allowed
//       172 CHECK brackets, references
//       171 references, obsoletes. clones etc.
//       170 check reference class
//       169 DML converter idea rework - almost finished
//       168 DML converter idea rework - not finished
//       167 coverter ToDeleteFirst from INSERT - conditions IN & EXISTS & EQUAL to do
//       166 converter ToDeleteFirst from INSERT with SELECT not finished
//       165 converter ToDeleteFirst from SELECT and UPDATE finished
//       164 converter ToDeleteFirst, AV problem
//       163 statement converters, begin
//       162 CheckTokens unification, table and column name changes
//       161 SaveSQL, parse DDL and comments cleanup
//       160 alter column with constraints, remove GtContainers changes
//       159 foreing key destroy, AV removed
//       158 foreign key destroy, not finished
//       157 item iterator rework
//       156 item iterator, struct lister
//       155 struct items removed, not finished
//       154 unrecognized statement, on delete on update clauses
//       153 struct renames, not finished
//       152 struct drops cleanup
//       151 alter table drop, modify
//       150 alter table syntax cleanup
//       149 SaveHTML, alter table unique, 10 000 LINES
//       148 SaveHTML, alter table check completed
//       147 SaveHTML, drop index in progress
//       146 SaveHTML, create table in progress
//       145 SaveHTML, cleanup
//       144 SaveHTML, tab aliases in one color
//       143 SaveHTML, colors ok
//       142 SaveHTML, almost finished
//       141 Remove Unnseccesary Aliases, Insert Set Expression Converter
//       140 resource strings, Remove Unneccessary Aliases - not finished
//
// 2011-04   22 SESJE ----------------------------------------------------------
//
//       139 SaveSQL, SaveParseLog cleanup
//       138 SaveSQL, shortest name, add aliases - finished
//       137 SaveSQL, shortest name, add aliases - not finished
//       136 SaveSQL, shortest name not finished
//       135 SaveSQL, smart item spacing
//       134 SaveSQL, DML almost done
//       133 SaveSQL early stage
//       132 Column Reference
//       131 SET Clause for INSERT, StatementToken removed
//       130 SaveParseLog cleanup
//       129 SaveParseLog cleanup, not finished
//       128 UPDATE statement, UPDATE FROM left
//       127 UPDATE statement, not finished
//       126 INSERT statement
//       125 DELETE statement, TODO cleanup
//       124 convert ANSI joins to old style, finished
//       123 convert ANSI joins to old style, not finished
//       122 Converter class cleanup, completed
//       121 Converter class cleanup, almost completed
//       120 Converter class cleanup, not finished
//       119 ABBADONED, so many RemoveItems
//       118 convert old style joins - completed
//
// 2011-03   13 SESJI ----------------------------------------------------------
//
//       117 convert old style joins - works, cleanup & tests needed
//       116 convert old style joins - almost completed conditions
//       115 convert old style joins - Garb Coll cleanup
//       114 convert old style joins - ClassType problem
//       113 convert old style joins - almost done - 1 ROCZNICA PROJEKTU
//       112 convert old style joins - simple CROSS JOINS
//       111 clone ok, convert old style joins
//       110 clone new functions
//       109 clone come back, little cleanup, works ok
//       108 old todos, functional table name parsing
//       107 old todos cleanup
//       106 resource strings units
//       105 resource strings units
//
// 2011-02   10 SESJI ----------------------------------------------------------
//
//       104 Statement References test
//       103 Statement DML - structural cleanup
//       102 SELECT expressions references
//       101 Expression and Table Aliases, Clones removed
//       100 Table aliases improvements
//        99 CheckTokens enum params
//        98 separated tokens ie. &lt &sp &eq, log events cleanup
//        97 missing log entries, logs to exact statement
//        96 tables references by star expressions
//        95 star expression - column list
//
// 2011-01   15 SESJI ----------------------------------------------------------
//
//        94 Statement References test - not finished
//        93 Garbage Collector events order against statements
//        92 Garbage Collector class events log, log symetry clean up
//        91 Garbage Collector class events log
//        90 structural cleanup - expressions and conditions, GtList enhancements
//        89 Garbage Collector without Parser OK, structural cleanup
//        88 Garbage Collector still with problems, structural cleanup
//        87 Garbage Collector still with problems, structural cleanup
//        86 Garbage Collector cleanup
//        85 TGtItemList replaces TObjectList
//        84 garbage collector destructor
//        83 garbage collector, item relations, dropping not finished
//        82 save DDL statements parse log
//        81 podzial klas DDL
//        80 testy DDL
//
// 2010-12   11 SESJI ----------------------------------------------------------
//
//        79 zmiana hierachii klas struktury bazy
//        78 problemy z zale¾no˜ciami moduˆ¢w
//        77 testy DDL
//        76 porz¥dki w «r¢dˆach
//        75 drop table + drop index - not finished
//        74 alter table + create index
//        73 struct common items + table multi id names
//        72 struct common items - not finished
//        71 alter table wo drops
//        70 wrong expected tokens logs
//        69 create table fixes
//
// 2010-11   11 SESJI ----------------------------------------------------------
//
//        68 db struct AV
//        67 powr¢t do projektu
//        66 create table
//        65 data types
//        64 create columns
//        63 check tokens
//        62 check token, parse endposition
//        61 create table and constraints
//        60 name list and elems
//        59 garbage collector - zaniechany
//        58 constraints - pk, fk
//
// 2010-10   14 SESJI ----------------------------------------------------------
//
//        57 db struct
//        56 db struct
//        55 db struct
//        54 db struct
//        53 old style joins
//        52 3 ms - text buffer
//        51 16 ms
//        50
//        49 20 ms
//        48
//        47 «le cytuje skrypt «r¢dˆowy
//        46 bˆ©dy w keywords
//        45 hash tokens w toku
//        44 tokenizer w toku
//
// 2010-09   17 SESJI ----------------------------------------------------------
//
//        43 tokenizer w toku
//        42 tokenizer w toku
//        41
//        40
//        39
//        38
//        37 GetChar zepsuty
//        36
//        35
//        34
//        33
//        32
//        31
//        30
//        29
//        28
//        27
//
// 2010-08   15 SESJI ----------------------------------------------------------
//
//        26
//        25
//        24
//        23
//        22
//        21
//        20
//        19
//        18
//        17
//        16
//        15
//        14
//        13
//        12
//
// 2010-07   3 SESJE -----------------------------------------------------------
//
//        11
//        10
//         9
//
// 2010-06   -------------------------------------------------------------------
// 2010-05   -------------------------------------------------------------------
// 2010-04   5 SESJI -----------------------------------------------------------
//
//         8
//         7
//         6 gets skips comments
//         5
//         4
//
// 2010-03   3 SESJE -----------------------------------------------------------
//
//         3
//         2
//         1
//
end.
