program TestOsThreadMultiRunnable;

uses
  Forms,
  FmTestOsThreadMultiRunnable in 'FmTestOsThreadMultiRunnable.pas' {FormTestOsThreadMultiRunnable};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormTestOsThreadMultiRunnable, FormTestOsThreadMultiRunnable);
  Application.Run;
end.
