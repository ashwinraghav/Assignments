program TestBaseListPerformance;

uses
  Forms,
  FmTestBaseListPerformance in 'FmTestBaseListPerformance.pas' {FormTestPerformance};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormTestPerformance, FormTestPerformance);
  Application.Run;
end.
