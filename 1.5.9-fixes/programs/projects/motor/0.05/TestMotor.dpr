program TestMotor;

{$INCLUDE Defines.inc}

uses
  Forms,
  FmTester in 'FmTester.pas' {FormMotor},
  SilSiHolors in 'SilSiHolors.pas',
  SilSiModel in 'SilSiModel.pas',
  SilSiLists in 'SilSiLists.pas',
  SilSiBlocks in 'SilSiBlocks.pas',
  SilSmHolors in 'SilSmHolors.pas',
  SilSmTags in 'SilSmTags.pas',
  SilSmModel in 'SilSmModel.pas',
  SilSmEntities in 'SilSmEntities.pas',
  SilSmRules in 'SilSmRules.pas',
  SilSiMotor in 'SilSiMotor.pas',
  SilSmMotor in 'SilSmMotor.pas',
  SilSfValues in 'SilSfValues.pas',
  SilSeValues in 'SilSeValues.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMotor, FormMotor);
  Application.Run;
end.
