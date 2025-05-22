unit Notepad;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ExtCtrls, StdCtrls;

type
  TForm1 = class(TForm)
    mainMenu: TMainMenu;
    fileTab: TMenuItem;
    open: TMenuItem;
    save: TMenuItem;
    formatTab: TMenuItem;
    autoLinebreak: TMenuItem;
    font: TMenuItem;
    openDialog: TOpenDialog;
    saveDialog: TSaveDialog;
    fontDialog: TFontDialog;
    findDialog: TFindDialog;
    replaceDialog: TReplaceDialog;
    editTab: TMenuItem;
    find: TMenuItem;
    replace: TMenuItem;
    textArea: TMemo;
    copy: TMenuItem;
    undo: TMenuItem;
    cut: TMenuItem;
    paste: TMenuItem;
    selectAll: TMenuItem;
    RightNow1: TMenuItem;

    procedure FormCreate(Sender: TObject);
    procedure fontClick(Sender: TObject);
    procedure openClick(Sender: TObject);
    procedure saveClick(Sender: TObject);
    procedure findClick(Sender: TObject);
    procedure findDialogFind(Sender: TObject);
    procedure replaceClick(Sender: TObject);
    procedure replaceDialogReplace(Sender: TObject);
    procedure replaceDialogFind(Sender: TObject);
    procedure textAreaKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure autoLinebreakClick(Sender: TObject);
    procedure undoClick(Sender: TObject);
    procedure copyClick(Sender: TObject);
    procedure cutClick(Sender: TObject);
    procedure pasteClick(Sender: TObject);
    procedure selectAllClick(Sender: TObject);
    procedure RightNow1Click(Sender: TObject);
  private
    procedure CopyAction;
    procedure CutAction;
    procedure PasteAction;
    procedure SelectAllAction;
    procedure UndoAction;
    procedure SaveAction;
    procedure OpenAction;
    procedure FindAction;
    procedure ReplaceAction;
    procedure ChangeLinebreakAction;
    procedure ChangeFontAction;
    procedure RightNowAction;
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  textArea.Align := alClient;
  textArea.WordWrap := False;
  textArea.ScrollBars := ssBoth;

  openDialog.Filter := 'Text Files (*.txt)|*.txt|All Files (*.*)|*.*';
  openDialog.DefaultExt := 'txt';

  saveDialog.Filter := 'Text Files (*.txt)|*.txt|All Files (*.*)|*.*';
  saveDialog.DefaultExt := 'txt';
end;

procedure TForm1.OpenAction;
begin
  if openDialog.Execute then
  begin
    textArea.Lines.LoadFromFile(openDialog.FileName);
  end;
end;

procedure TForm1.SaveAction;
begin
  if saveDialog.Execute then
    textArea.Lines.SaveToFile(saveDialog.FileName);
end;

procedure TForm1.UndoAction;
begin
  if textArea.CanUndo then
      textArea.Undo;
end;

procedure TForm1.CopyAction;
begin
  textArea.CopyToClipboard;
end;

procedure TForm1.CutAction;
begin
  textArea.CopyToClipboard;
  textArea.ClearSelection;
end;

procedure TForm1.PasteAction;
begin
  textArea.PasteFromClipboard;
end;

procedure TForm1.FindAction;
begin
  findDialog.Execute;
end;

procedure TForm1.ReplaceAction;
begin
  replaceDialog.Execute;
end;

procedure TForm1.SelectAllAction;
begin
  textArea.SelectAll;
end;

procedure TForm1.RightNowAction;
begin
  textArea.Lines.Add(DateTimeToStr(Now));
end;

procedure TForm1.ChangeLinebreakAction;
begin
  textArea.WordWrap := not textArea.WordWrap;
  autoLinebreak.Checked := textArea.WordWrap;

  if textArea.WordWrap then
    textArea.ScrollBars := ssVertical
  else
    textArea.ScrollBars := ssBoth;
end;

procedure TForm1.ChangeFontAction;
begin
  fontDialog.Font := textArea.Font;
  if fontDialog.Execute then
    textArea.Font := fontDialog.Font;
end;

procedure TForm1.openClick(Sender: TObject);
begin
  OpenAction;
end;

procedure TForm1.saveClick(Sender: TObject);
begin
  SaveAction;
end;

procedure TForm1.undoClick(Sender: TObject);
begin
  UndoAction;
end;

procedure TForm1.copyClick(Sender: TObject);
begin
  CopyAction;
end;

procedure TForm1.cutClick(Sender: TObject);
begin
  CutAction;
end;

procedure TForm1.pasteClick(Sender: TObject);
begin
  PasteAction;
end;

procedure TForm1.findClick(Sender: TObject);
begin
  FindAction;
end;

procedure TForm1.findDialogFind(Sender: TObject);
var
  SelPos: Integer;
begin
  SelPos := Pos(findDialog.FindText, textArea.Lines.Text);
  if SelPos > 0 then
  begin
    textArea.SelStart := SelPos - 1;
    textArea.SelLength := Length(findDialog.FindText);
    findDialog.CloseDialog;
  end
  else
    MessageDlg('Could not find "' + findDialog.FindText + '" in Notepad.', mtError, [mbOk], 0);
end;

procedure TForm1.replaceClick(Sender: TObject);
begin
  ReplaceAction;
end;

procedure TForm1.replaceDialogReplace(Sender: TObject);
var
  SelPos: Integer;
begin
  SelPos := Pos(replaceDialog.FindText, textArea.Lines.Text);
  if SelPos > 0 then
  begin
    textArea.SelStart := SelPos - 1;
    textArea.SelLength := Length(replaceDialog.FindText);
    textArea.SelText := replaceDialog.ReplaceText;
  end
  else
    MessageDlg('Could not find "' + replaceDialog.FindText + '" in Notepad.', mtError, [mbOk], 0);
end;

procedure TForm1.replaceDialogFind(Sender: TObject);
var
  SelPos: Integer;
begin
  SelPos := Pos(replaceDialog.FindText, textArea.Lines.Text);
  if SelPos > 0 then
  begin
    textArea.SelStart := SelPos - 1;
    textArea.SelLength := Length(replaceDialog.FindText);
    replaceDialog.CloseDialog;
  end
  else
    MessageDlg('Could not find "' + replaceDialog.FindText + '" in Notepad.', mtError, [mbOk], 0);
end;

procedure TForm1.selectAllClick(Sender: TObject);
begin
  SelectAllAction;
end;

procedure TForm1.RightNow1Click(Sender: TObject);
begin
  RightNowAction;
end;

procedure TForm1.autoLinebreakClick(Sender: TObject);
begin
  ChangeLinebreakAction;
end;

procedure TForm1.fontClick(Sender: TObject);
begin
  ChangeFontAction;
end;

procedure TForm1.textAreaKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = Ord('O')) and (ssCtrl in Shift) then
  begin
    OpenAction;
    Key := 0;
  end
  else if (Key = Ord('S')) and (ssCtrl in Shift) then
  begin
    SaveAction;
    Key := 0;
  end;

  if (Key = Ord('Z')) and (ssCtrl in Shift) then
  begin
    UndoAction;
    Key := 0;
  end
  else if (Key = Ord('C')) and (ssCtrl in Shift) then
  begin
    CopyAction;
    Key := 0;
  end
  else if (Key = Ord('X')) and (ssCtrl in Shift) then
  begin
    CutAction;
    Key := 0;
  end
  else if (Key = Ord('V')) and (ssCtrl in Shift) then
  begin
    Key := 0;
  end;

  if (Key = Ord('F')) and (ssCtrl in Shift) then
  begin
    FindAction;
    Key := 0;
  end
  else if (Key = Ord('H')) and (ssCtrl in Shift) then
  begin
    ReplaceAction;
    Key := 0;
  end;

  if (Key = Ord('A')) and (ssCtrl in Shift) then
  begin
    SelectAllAction;
    Key := 0;
  end;

  if (Key = VK_F5) then
  begin
    RightNowAction;
    Key := 0;
  end;

  if (Key = Ord('L')) and (ssCtrl in Shift) then
  begin
    ChangeLinebreakAction;
    Key := 0;
  end
  else if (Key = Ord('F')) and (ssCtrl in Shift) and (ssShift in Shift) then
  begin
    ChangeFontAction;
    Key := 0;
  end;
end;

end.

