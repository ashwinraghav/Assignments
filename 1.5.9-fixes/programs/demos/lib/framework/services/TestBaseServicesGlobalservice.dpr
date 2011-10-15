program TestBaseServicesGlobalservice;

{%File 'UfDemoService.pas'}

uses
  SilExport,
  Forms,
  FmTestBaseServicesGlobalservice in 'FmTestBaseServicesGlobalservice.pas' {FormTestGlobalservices};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormTestGlobalservices, FormTestGlobalservices);
  Application.Run;
end.
