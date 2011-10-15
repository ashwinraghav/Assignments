program ntlmpx;



{%File '..\..\..\..\..\..\..\..\ouch\received\Diego Degese\ntlmaps086\ntlmaps086\doc\NTLM Authentication Scheme for HTTP.htm'}

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  UtNtlm in 'UtNtlm.pas',
  Hash in 'Hash.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
