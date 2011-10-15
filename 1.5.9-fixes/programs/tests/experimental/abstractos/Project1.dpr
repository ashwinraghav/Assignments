program Project1;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  SilOhInterface in 'SilOhInterface.pas',
  SilOxSharedLibrary in 'SilOxSharedLibrary.pas',
  SilOxThread in 'SilOxThread.pas',
  SilOmDispatchRunnable in 'SilOmDispatchRunnable.pas',
  SilOmAdoptRunnable in 'SilOmAdoptRunnable.pas',
  SilLfTraits in 'SilLfTraits.pas',
  SilOsInterface in 'SilOsInterface.pas',
  SilOgInterface in 'SilOgInterface.pas',
  SilOgThread in 'SilOgThread.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
