unit UrEngine;

interface

uses
  UiClient,
  SilTool;

procedure Register;

implementation

uses
  UmData, UmClient;

procedure Register;
begin
  SilTool.Sv.SharedObject.Register(COuchClient, TClient, SOuchClient);
  SilTool.Sv.SharedObject.Register(COuchData, TData, SOuchData);
end;

end.
