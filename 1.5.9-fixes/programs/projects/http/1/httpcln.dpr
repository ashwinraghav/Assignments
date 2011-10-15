program httpcln;

uses
  Forms,
  FmClnMain in 'FmClnMain.pas' {FormHttpClient};

{$R *.res}                                      
                               
begin
  Application.Initialize;
  Application.CreateForm(TFormHttpClient, FormHttpClient);
  Application.Run;
end.
