program TestBaseBlockPacketTyped;

uses
  Forms,
  FmTestBaseBlockPacketTyped in 'FmTestBaseBlockPacketTyped.pas' {FormTestPacketTyped};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormTestPacketTyped, FormTestPacketTyped);
  Application.Run;
end.
