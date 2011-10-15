program TestOsFileTextReader;

uses
  Forms,
  FmTestOsFileTextReader in 'FmTestOsFileTextReader.pas' {FormTestTextReader};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormTestTextReader, FormTestTextReader);
  Application.Run;
end.
