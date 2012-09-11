program Server2;

uses
  Forms,
  uServer2 in 'uServer2.pas' {ServerForm},
  uJQ in 'uJQ.pas',
  uWebSocket in 'uWebSocket.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'JQM Component Demo';
  Application.CreateForm(TServerForm, ServerForm);
  Application.Run;
end.
