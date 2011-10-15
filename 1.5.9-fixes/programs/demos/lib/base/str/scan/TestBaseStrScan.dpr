program TestBaseStrScan;

uses
  Forms,
  FmTestBaseStrScan in 'FmTestBaseStrScan.pas' {FormTestScan1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormTestScan1, FormTestScan1);
  Application.Run;
end.
