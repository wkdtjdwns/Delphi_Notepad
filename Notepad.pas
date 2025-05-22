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

{ 프로그램 시작 }
procedure TForm1.FormCreate(Sender: TObject);
begin
  // textArea를 화면 크기에 맞춰 초기화
  textArea.Align := alClient;
  textArea.WordWrap := False;
  textArea.ScrollBars := ssBoth;

  // 열기 및 저장 확장자 초기화
  openDialog.Filter := 'Text Files (*.txt)|*.txt|All Files (*.*)|*.*';
  openDialog.DefaultExt := 'txt';

  saveDialog.Filter := 'Text Files (*.txt)|*.txt|All Files (*.*)|*.*';
  saveDialog.DefaultExt := 'txt';
end;

{ 파일 열기 (Open) }
procedure TForm1.OpenAction;
begin
  if openDialog.Execute then
  begin
    textArea.Lines.LoadFromFile(openDialog.FileName);
  end;
end;

{ 파일 저장 (Save) }
procedure TForm1.SaveAction;
begin
  if saveDialog.Execute then
    textArea.Lines.SaveToFile(saveDialog.FileName);
end;

{ 되돌리기 (Undo) }
procedure TForm1.UndoAction;
begin
  // TMemo 요소에 이미 Undo가 구현되어 있음.
  if textArea.CanUndo then
      textArea.Undo;
end;

{ 복사 (Copy) }
procedure TForm1.CopyAction;
begin                       
  // TMemo 요소에 이미 Copy가 구현되어 있음.
  textArea.CopyToClipboard;
end;

{ 잘라내기 (Cut) }
procedure TForm1.CutAction;
begin                   
  // Copy 후 선택 영역 지우기
  textArea.CopyToClipboard;
  textArea.ClearSelection;
end;

{ 붙여넣기 (Paste) }
procedure TForm1.PasteAction;
begin
  // TMemo 요소에 이미 Paste가 구현되어 있음.
  textArea.PasteFromClipboard;
end;

{ 찾기 (Find) }
procedure TForm1.FindAction;
begin
  // FindDialog 요소 실행
  findDialog.Execute;
end;

{ 바꾸기 {Replace) }
procedure TForm1.ReplaceAction;
begin
  // ReplaceDialog 요소 실행
  replaceDialog.Execute;
end;

{ 전체 선택 (Select All) }
procedure TForm1.SelectAllAction;
begin
  // TMemo 요소에 이미 SelectAll이 구현되어 있음.
  textArea.SelectAll;
end;

{ 현재 시각 }
procedure TForm1.RightNowAction;
begin
  // 현재 시각을 구해서 TMemo 요소의 Lines에 추가
  textArea.Lines.Add(DateTimeToStr(Now));
end;

{ 자동 줄바꿈 토글 }
procedure TForm1.ChangeLinebreakAction;
begin
  // 자동 줄바꿈 및 요소 Checked 토글
  textArea.WordWrap := not textArea.WordWrap;
  autoLinebreak.Checked := textArea.WordWrap;

  // 자동 줄바꿈을 위해 ScrollBar 설정 (자동 줄바꿈 -> 세로 스크롤바 | 자동 줄바꿈 X -> 가로 및 세로 스크롤바)
  if textArea.WordWrap then
    textArea.ScrollBars := ssVertical
  else
    textArea.ScrollBars := ssBoth;
end;

{ 폰트 바꾸기 }
procedure TForm1.ChangeFontAction;
begin
  // FontDialong의 폰트를 현재 폰트로 초기화
  fontDialog.Font := textArea.Font;

  // FontDialog를 열고 바뀐 폰트 저장`
  if fontDialog.Execute then
    textArea.Font := fontDialog.Font;
end;

{ 클릭 이벤트들 }
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

{ 실질적인 찾기 로직 }
procedure TForm1.findDialogFind(Sender: TObject);
var
  selectPos: Integer;
begin            
  // 입력한 검색어(findDialog.FindText)를 textArea 전체 텍스트에서 찾음
  selectPos := Pos(findDialog.FindText, textArea.Lines.Text);

  // 검색어가 발견되면
  if selectPos > 0 then
  begin                         
    // 검색어가 발견되면 해당 위치를 선택함
    textArea.SelStart := selectPos - 1;
    textArea.SelLength := Length(findDialog.FindText);
    
    // 찾기 대화상자를 닫음
    findDialog.CloseDialog;
  end

  // 예외 처리
  else
    MessageDlg('Could not find "' + findDialog.FindText + '" in Notepad.', mtError, [mbOk], 0);
end;

procedure TForm1.replaceClick(Sender: TObject);
begin
  ReplaceAction;
end;

{ 실질적인 바꾸기 로직 }
procedure TForm1.replaceDialogReplace(Sender: TObject);
var
  selectPos: Integer;
begin
  // 입력한 검색어(findDialog.FindText)를 textArea 전체 텍스트에서 찾음
  selectPos := Pos(replaceDialog.FindText, textArea.Lines.Text);

  // 검색어가 발견되면
  if selectPos > 0 then
  begin
    // 검색어가 발견되면 해당 위치를 선택함
    textArea.SelStart := selectPos - 1;
    textArea.SelLength := Length(replaceDialog.FindText);

    // 선택한 위치의 텍스트를 바꿀 텍스트로 변경
    textArea.SelText := replaceDialog.ReplaceText;
  end

  // 예외 처리
  else
    MessageDlg('Could not find "' + replaceDialog.FindText + '" in Notepad.', mtError, [mbOk], 0);
end;

{ 바꾸기 대화상자에서 실행하는 바꾸기 }
procedure TForm1.replaceDialogFind(Sender: TObject);
var
  SelPos: Integer;
begin
  SelPos := Pos(replaceDialog.FindText, textArea.Lines.Text);
  if SelPos > 0 then
  begin
    textArea.SelStart := SelPos - 1;
    textArea.SelLength := Length(replaceDialog.FindText);

    // 다른 로직은 동일하고 찾기 대화상자 대신 바꾸기 대화상자를 닫음
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
{ 클릭 이벤트들 }

{ 델파이에서 지원해주긴 하지만 혹시 몰라서 구현해둔 단축키들 }
procedure TForm1.textAreaKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  // Ctrl + O
  if (Key = Ord('O')) and (ssCtrl in Shift) then
  begin
    OpenAction;
    Key := 0;
  end

  // Ctrl + S
  else if (Key = Ord('S')) and (ssCtrl in Shift) then
  begin
    SaveAction;
    Key := 0;
  end;

  // Ctrl + Z
  if (Key = Ord('Z')) and (ssCtrl in Shift) then
  begin
    UndoAction;
    Key := 0;
  end

  // Ctrl + C
  else if (Key = Ord('C')) and (ssCtrl in Shift) then
  begin
    CopyAction;
    Key := 0;
  end

  // Ctrl + X
  else if (Key = Ord('X')) and (ssCtrl in Shift) then
  begin
    CutAction;
    Key := 0;
  end

  // Ctrl + V
  else if (Key = Ord('V')) and (ssCtrl in Shift) then
  begin
    Key := 0;
  end;

  // Ctrl + F
  if (Key = Ord('F')) and (ssCtrl in Shift) then
  begin
    FindAction;
    Key := 0;
  end

  // Ctrl + H
  else if (Key = Ord('H')) and (ssCtrl in Shift) then
  begin
    ReplaceAction;
    Key := 0;
  end;

  // Ctrl + A
  if (Key = Ord('A')) and (ssCtrl in Shift) then
  begin
    SelectAllAction;
    Key := 0;
  end;

  // F5
  if (Key = VK_F5) then
  begin
    RightNowAction;
    Key := 0;
  end;

  // Ctrl + L
  if (Key = Ord('L')) and (ssCtrl in Shift) then
  begin
    ChangeLinebreakAction;
    Key := 0;
  end
  // Ctrl + Shift + F
  else if (Key = Ord('F')) and (ssCtrl in Shift) and (ssShift in Shift) then
  begin
    ChangeFontAction;
    Key := 0;
  end;
end;

end.

