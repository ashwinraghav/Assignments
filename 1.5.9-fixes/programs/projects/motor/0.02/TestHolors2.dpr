program TestHolors2;

uses
  Forms,
  FmTestHolor2 in 'FmTestHolor2.pas' {FormHolors},
  SilSiHolors in 'SilSiHolors.pas',
  SilSmHolors in 'SilSmHolors.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormHolors, FormHolors);
  Application.Run;
end.
