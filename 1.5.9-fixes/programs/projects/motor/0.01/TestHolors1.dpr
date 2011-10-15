program TestHolors1;

uses
  Forms,
  SilSeHolors in 'SilSeHolors.pas',
  FmTestHolor1 in 'FmTestHolor1.pas' {FormHolors};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormHolors, FormHolors);
  Application.Run;
end.
