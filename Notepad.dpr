program Notepand;

uses
  Forms,
  Notepad in 'Notepad.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
