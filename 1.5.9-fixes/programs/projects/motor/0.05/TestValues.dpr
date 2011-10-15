program TestValues;

uses
  Forms,
  FmTestValues in 'FmTestValues.pas' {FormTestValues},
  SilSfValues in 'SilSfValues.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormTestValues, FormTestValues);
  Application.Run;
end.
