program TestBaseToolsTypeInfo;



uses
  Forms,
  FmTestBaseToolsTypeinfo in 'FmTestBaseToolsTypeinfo.pas' {FormTestTypeinfo};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormTestTypeinfo, FormTestTypeinfo);
  Application.Run;
end.
