object Form1: TForm1
  Left = 188
  Top = 137
  Width = 1300
  Height = 674
  Caption = 'Notepad'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = mainMenu
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object textArea: TMemo
    Left = 0
    Top = 0
    Width = 1284
    Height = 615
    Align = alClient
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 0
    OnKeyDown = textAreaKeyDown
  end
  object mainMenu: TMainMenu
    Left = 8
    Top = 584
    object fileTab: TMenuItem
      Caption = '&File'
      object open: TMenuItem
        Caption = 'Open'
        Hint = 'Open the file'
        ShortCut = 16463
        OnClick = openClick
      end
      object save: TMenuItem
        Caption = 'Save'
        ShortCut = 16467
        OnClick = saveClick
      end
    end
    object editTab: TMenuItem
      Caption = 'Edit'
      object undo: TMenuItem
        Caption = 'Undo'
        ShortCut = 16474
        OnClick = undoClick
      end
      object copy: TMenuItem
        Caption = 'Copy'
        ShortCut = 16451
        OnClick = copyClick
      end
      object cut: TMenuItem
        Caption = 'Cut'
        ShortCut = 16472
        OnClick = cutClick
      end
      object paste: TMenuItem
        Caption = 'Paste'
        ShortCut = 16470
        OnClick = pasteClick
      end
      object find: TMenuItem
        Caption = 'Find'
        ShortCut = 16454
        OnClick = findClick
      end
      object replace: TMenuItem
        Caption = 'Replace'
        ShortCut = 16456
        OnClick = replaceClick
      end
      object selectAll: TMenuItem
        Caption = 'Select All'
        ShortCut = 16449
        OnClick = selectAllClick
      end
      object RightNow1: TMenuItem
        Caption = 'Right Now'
        ShortCut = 116
        OnClick = RightNow1Click
      end
    end
    object formatTab: TMenuItem
      Caption = '&Format'
      object autoLinebreak: TMenuItem
        Caption = 'Auto Line break'
        ShortCut = 16460
        OnClick = autoLinebreakClick
      end
      object font: TMenuItem
        Caption = 'Font'
        ShortCut = 24646
        OnClick = fontClick
      end
    end
  end
  object openDialog: TOpenDialog
    Left = 40
    Top = 584
  end
  object saveDialog: TSaveDialog
    Left = 72
    Top = 584
  end
  object fontDialog: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Left = 168
    Top = 584
  end
  object findDialog: TFindDialog
    OnFind = findDialogFind
    Left = 104
    Top = 584
  end
  object replaceDialog: TReplaceDialog
    OnFind = replaceDialogFind
    OnReplace = replaceDialogReplace
    Left = 136
    Top = 584
  end
end
