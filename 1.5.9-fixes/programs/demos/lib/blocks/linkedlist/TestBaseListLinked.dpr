program TestBaseListLinked;

uses
  Forms,
  FmTestBaseListLinked in 'FmTestBaseListLinked.pas' {FormTestLinked};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormTestLinked, FormTestLinked);
  Application.Run;
end.
