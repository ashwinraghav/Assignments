program TestBaseFrameworkAggregation;

uses
  Forms,
  FmTestBaseFrameworkAggregation in 'FmTestBaseFrameworkAggregation.pas' {FormTestAggregation};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormTestAggregation, FormTestAggregation);
  Application.Run;
end.
