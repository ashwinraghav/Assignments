unit UmServer;

interface

{$include Defines.inc}

uses
  Sil,
  SilLayer,

  UtLog,
  UiClient,
  UiServer,
  UiUpdater;

type
  TServer = class (
    TSilObject,
    IServer,
    ILayerChainSource)
  private
    FChain: ILayerChain;
    FClients: IClientList;
    FData: IData;
    FUpdater: IUpdater;
  private
    procedure DoInitLayers;
  protected // IServer
    function GetOnlines: IClients;
    procedure Start;
    procedure Stop;
    procedure SendMessage(const MsgId: TGUID; const UserId: TGuid; const Recipients: TGuidArray; const Text: String; const Time: TDateTime; Bc: Boolean);
    procedure SendInfo(const UserId: TGuid; const Recipients: TGuidArray; const Data: IParameters);
    procedure SendResponse(const MsgId: TGUID; const FromId, ToId: TGuid; const Time: TDateTime);
    function Data: IData;
    property Onlines: IClients read GetOnlines;
  protected // ILayerChainSource
    procedure CreateChain(const Chain: ILayerChain; const Context: IUnknown = nil);
    procedure DestroyChain(const Chain: ILayerChain; const Context: IUnknown = nil);
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  UmClientList,
  UmClient,
  UmData,
  UmUpdater;

{ TServer }

constructor TServer.Create;
begin
  GlobalLog.Initialize;
  inherited Create;
end;

destructor TServer.Destroy;
begin
  inherited;
  GlobalLog.Finalize;
end;

procedure TServer.Start;
var
  Path: String;
begin
  Sil.Trace.Enter('TServer.Start');

  Path := Sil.OS.Module.Current.Info.Path;

  FUpdater := TUpdater.Create(Path + 'pub\updates', 'update.xml');
  FUpdater.Start;

  FData := TData.Create(Path);
  FData.Initialize;

  DoInitLayers;

  FClients := TClientList.Create;
  FChain.Control.Activate;

  Sil.Trace.Leave;
end;

procedure TServer.Stop;
begin
  Sil.Trace.Enter('TServer.Stop');

  if Assigned(FUpdater) then
  begin
    FUpdater.Stop;
    FUpdater := nil;
  end;

  if Assigned(FChain) then
  begin
    FChain.Clear(true);
    FChain := nil;
  end;

  if Assigned(FClients) then
  begin
    FClients.Clear;
    FClients := nil;
  end;

  if Assigned(FData) then
  begin
    FData.Finalize;
    FData := nil;
  end;

  Sil.Trace.Leave;
end;

procedure TServer.DoInitLayers;
var
  Params: IParameterList;
begin
  Params := Sil.List.Parameters;
  Params['Port'] := 23361;

  FChain := SilLayer.Layer.Chain;
  FChain.Add(SilLayer.Device.SocketServer(Params));
  FChain.Add(Self);
end;

procedure TServer.CreateChain(const Chain: ILayerChain; const Context: IInterface);
var
  Client: IClient;
begin
  Sil.Trace.Enter('TServer.CreateChain');

  Client := TClient.Create(Self, Chain);
  FClients.Add(Client);

  Sil.Trace.Leave;
end;

procedure TServer.DestroyChain(const Chain: ILayerChain; const Context: IInterface);
var
  Enum: IEnumerator;
  Client: IClient;
begin
  Sil.Trace.Enter('TServer.DestroyChain');

  while FClients.Enumerate(Enum, Client) do
    if Client.Chain.Id = Chain.Id then
    begin
      try
        Client.Logoff;

        if Guid.NotEmpty(Client.Id) then
          FData.LogoffAccount(Client.Id);

        FClients.Delete(Enum.Iteration);
        Sil.Trace.Log('cliente desloggeado %s', [Guid.ToStr(Client.Id)]);
      except
        Sil.Trace.Exception;
      end;

      Break;
    end;

  Sil.Trace.Leave;
end;

function TServer.GetOnlines: IClients;
begin
  Result := FClients;
end;

function TServer.Data: IData;
begin
  Result := FData;
end;

procedure TServer.SendInfo(const UserId: TGuid; const Recipients: TGuidArray; const Data: IParameters);
var
  Enum: IEnumerator;
  Item: IClient;
begin
  try
    while FClients.Enumerate(Enum, Item) do
      if not Guid.IsEqual(UserId, Item.Id) and (not Assigned(Recipients) or (Guid.ArrayFind(Recipients, Item.Id) >= 0)) then
        if Item.IsLogged then
          Item.ServerSide.SendInfo(UserId, Data);
  except
    Sil.Trace.Exception('TServer.SendInfo');
  end;
end;

procedure TServer.SendMessage(const MsgId: TGUID; const UserId: TGuid; const Recipients: TGuidArray; const Text: String; const Time: TDateTime; Bc: Boolean);
var
  Enum: IEnumerator;
  Item: IClient;
  i: Integer;
begin
  try
    FClients.Locked;

    while FClients.Enumerate(Enum, Item) do
    begin
      i := Guid.ArrayFind(Recipients, Item.Id);

      if (i >= 0) and Item.IsLogged then
        try
          if Bc then
            Item.ServerSide.SendMessage(MsgId, UserId, Text, Time, Recipients) else
            Item.ServerSide.SendMessage(MsgId, UserId, Text, Time, nil);

          Recipients[i] := Guid.Null;
        except
          Sil.Trace.Exception('TServer.SendMessage: send');
        end;
    end;

    for i := 0 to Length(Recipients) - 1 do
      if not Guid.IsEmpty(Recipients[i]) then
        FData.StoreMessage(MsgId, Recipients[i], UserId, Recipients, Text, Time, Bc);
  except
    Sil.Trace.Exception('TServer.SendMessage');
  end;
end;

procedure TServer.SendResponse(const MsgId: TGUID; const FromId, ToId: TGuid; const Time: TDateTime);
var
  Enum: IEnumerator;
  Item: IClient;
begin
  try
    while FClients.Enumerate(Enum, Item) do
      if Guid.IsEqual(ToId, Item.Id) then
      begin
        if Item.IsLogged then
          Item.ServerSide.SendResponse(MsgId, FromId, Time) else
          FData.StoreResponse(MsgId, FromId, ToId, Time);

        Break;
      end;
  except
    Sil.Trace.Exception('TServer.SendResponse');
  end;
end;

end.
