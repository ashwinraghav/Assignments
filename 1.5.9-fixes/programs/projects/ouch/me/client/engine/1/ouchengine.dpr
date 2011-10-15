library ouchengine;

{%File '..\..\..\doc\protocol.txt'}
{%File '..\..\..\common\1\src\UiOuchEngine.pas'}
{%File '..\..\..\common\1\src\UiOuchProtocol.pas'}
{%File '..\..\..\common\1\src\UmOuchProtocol.pas'}
{%File '..\..\..\common\1\src\UkOuchChannel.pas'}
{%ToDo 'ouchengine.todo'}

uses
  ShareMem,
  UiEngine in 'src\UiEngine.pas',
  UfEngine in 'src\UfEngine.pas',
  UmEngine in 'src\UmEngine.pas',
  UmEngineConnection in 'src\UmEngineConnection.pas',
  UmEngineListener in 'src\UmEngineListener.pas',
  UmEngineSession in 'src\UmEngineSession.pas',
  UkEngineObject in 'src\UkEngineObject.pas',
  UkEngineChannel in 'src\UkEngineChannel.pas',
  UmEngineClient in 'src\UmEngineClient.pas',
  UmEngineUserData in 'src\UmEngineUserData.pas',
  UmEngineGroupData in 'src\UmEngineGroupData.pas',
  UmEngineGroupList in 'src\UmEngineGroupList.pas',
  UdEngineGroupList in 'src\UdEngineGroupList.pas',
  UmEngineUserList in 'src\UmEngineUserList.pas',
  UmEngineGroup in 'src\UmEngineGroup.pas',
  UmEngineMessage in 'src\UmEngineMessage.pas',
  UeOuchData in '..\..\..\common\1\src\UeOuchData.pas',
  UtOuchData in '..\..\..\common\1\src\UtOuchData.pas',
  Ouch in '..\..\..\common\1\src\Ouch.pas';

exports
  UfEngine.Engine;
  
{$R *.RES}

begin
end.
 