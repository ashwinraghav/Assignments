program TestBaseContainerSpeed;

{$INCLUDE Defines.inc}

uses
  Forms,
  FmTestBaseContainerSpeed in 'FmTestBaseContainerSpeed.pas' {FormTestContainerSpeed};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormTestContainerSpeed, FormTestContainerSpeed);
  Application.Run;
end.
