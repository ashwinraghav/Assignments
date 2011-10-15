program TestBaseFrameworkConversion;

uses
  Forms,
  FmTestBaseFrameworkConversion in 'FmTestBaseFrameworkConversion.pas' {FormTestConvert};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormTestConvert, FormTestConvert);
  Application.Run;
end.
