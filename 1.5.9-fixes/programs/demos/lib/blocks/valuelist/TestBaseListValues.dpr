program TestBaseListValues;

uses
  Forms,
  FmTestBaseListValues in 'FmTestBaseListValues.pas' {FormTestValuelist};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormTestValuelist, FormTestValuelist);
  Application.Run;
end.
