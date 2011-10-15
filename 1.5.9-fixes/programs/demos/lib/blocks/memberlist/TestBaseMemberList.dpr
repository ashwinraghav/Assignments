program TestBaseMemberList;

uses
  Forms,
  FmTestBaseMemberList in 'FmTestBaseMemberList.pas' {FormTestBaseMemberList},
  SilLmMemberList in '..\..\..\..\..\source\lib\SilLmMemberList.pas',
  SilLiMemberList in '..\..\..\..\..\source\lib\SilLiMemberList.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormTestBaseMemberList, FormTestBaseMemberList);
  Application.Run;
end.
