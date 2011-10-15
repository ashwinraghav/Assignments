unit UtLog;

interface

uses
  Sil,
  SilTool,
  SilLayer,
  SilLog;

type
  GlobalLog = class (Tool)
    class procedure Initialize;
    class procedure Finalize;
  end;

implementation

uses
  UcOuch;

class procedure GlobalLog.Initialize;
begin
  SilLog.Logger.Initialize(CRootKey, SilLog.LogService, SilLog.StackFmt);
  Sil.Os.Environment.Load(CRootKey);
  SilTool.Sv.SharedObject.Initialize(CRootKey);
  Sil.Debug.Level := dlInOut;
end;

class procedure GlobalLog.Finalize;
begin
  SilTool.Sv.SharedObject.Finalize;
  SilLog.Logger.Finalize;
end;

end.
 