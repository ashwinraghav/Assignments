unit UmClient;

interface

{$include Defines.inc}

uses
  Sil,
  SilLayer,
  SilCoder,

  UiProtocol,
  UiServer,
  UiClient;

type
  TClient = class (
    TSilObject,
    IClient,
    IBlindProtocolHook,
    IEblisProtocolHook,
    IServerSideEvents,
    IServerSideFileProtocolHook)
  private
    FUserId: TGuid;
    FServer: IServer;
    FData: IData;
    FChain: ILayerChain;
    FSlot: ILayerSlot;
    FBlind: IBlindProtocol;
    FServerSide: IServerSide;
    FEblis: IEblisProtocol;
    FFileProtocol: IServerSideFileProtocol;
    FIsLogged: Boolean;
    FIPAddress: LongWord;
    FCipherKey: String;
  private
    procedure DoInitLayers;
    procedure DoAddProtocol(Id: LongWord; const Protocol: IInterface; const CipherKey: String = '');
    procedure DoSendUserStatus(Status: TUserStatus);
  protected // IClient
    function GetId: TGuid;
    function GetIsLogged: Boolean;
    function GetServerSide: IServerSide;
    function Chain: ILayerChain;
    procedure Logoff;
    property Id: TGuid read GetId;
    property ServerSide: IServerSide read GetServerSide;
    property IsLogged: Boolean read GetIsLogged;
  protected // IBlindProtocolHook
    procedure OnText(var Event: RBlindTextEvent);
    procedure OnRequest(var Event: RBlindRequestEvent);
  protected // IEblisProtocolHook
    procedure OnNegotiate(var Event: REblisNegotiateEvent);
  protected // IServerSideEvents
    procedure OnCreateAccount(var Event: RServerSideCreateAccountEvent);
    procedure OnGetAccount(var Event: RServerSideGetAccountEvent);
    procedure OnLogon(var Event: RServerSideLogonEvent);
    procedure OnSendMessage(var Event: RServerSideMessageEvent);
    procedure OnSendResponse(var Event: RServerSideResponseEvent);
    procedure OnSendInfo(var Event: RServerSideInfoEvent);
    procedure OnQueryInfo(var Event: RServerSideQueryInfoEvent);
    procedure OnQueryUsers(var Event: RServerSideQueryUsersEvent);
    procedure OnQueryOfflines(var Event: RServerSideQueryOfflinesEvent);
    procedure OnConnectionRequest(var Event: RServerSideConnectionEvent);
    procedure OnConnectionAccept(var Event: RServerSideConnectionEvent);
  protected // IServerSideFileProtocolHook
    procedure OnOpenFile(var Event: RFPOpenFileEvent);
    procedure OnCreateFile(var Event: RFPCreateFileEvent);
    procedure OnReadFile(var Event: RFFileDataEvent);
    procedure OnWriteFile(var Event: RFFileDataEvent);
    procedure OnSeekFile(var Event: RFSeekFileEvent);
    procedure OnFlushFile(var Event: RFFlushFileEvent);
    procedure OnCloseFile(var Event: RFCloseFileEvent);
    procedure OnGetFileSize(var Event: RFPFileSizeEvent);
    procedure OnCreateDirectory(var Event: RFPCreateDirectoryEvent);
    procedure OnCreateDirectoryReader(var Event: RFCreateDirectoryReaderEvent);
    procedure OnDirectoryRead(var Event: RFDirectoryReaderEvent);
    procedure OnDestroyDirectoryReader(var Event: RFDirectoryReaderEvent);
    procedure OnMove(var Event: RFPMoveEvent);
    procedure OnDelete(var Event: RFPDeleteEvent);
    procedure OnGetInfo(var Event: RFPInfoEvent);
    procedure OnSetFileSize(var Event: RFPFileSizeEvent);
    procedure OnSetFileAttributes(var Event: RFPFileAttributeEvent);
    procedure OnSetFileTime(var Event: RFPFileTimeEvent);
  public
    constructor Create(const Server: IServer; const Chain: ILayerChain);
    destructor Destroy; override;
  end;

implementation

uses
  UtProtocol,
  UmProtocolServer, SilLtList, SilOtGuid;

procedure DoCheckEnabled(Condition: Boolean);
begin
  if not Condition then
    raise Error.Create('operacion no valida');
end;

{ TClient }

constructor TClient.Create(const Server: IServer; const Chain: ILayerChain);
begin
  inherited Create;

  FServer := Server;
  FData := Server.Data;
  FChain := Chain;

  DoInitLayers;
end;

destructor TClient.Destroy;
begin
  FChain := nil;
  FSlot := nil;
  FBlind := nil;
  FServerSide := nil;
  FFileProtocol := nil;
  FEblis := nil;

  inherited;
end;

function TClient.Chain: ILayerChain;
begin
  Result := FChain;
end;

procedure TClient.DoInitLayers;
var
  Link: ILayerLink;
  Socket: ISocketClient;
begin
  if FChain.GetFirst(ILayerLink, Link) and Vart.ToInterface(Link.Operation.Parameters['Device'], ISocketClient, Socket) then
    FIPAddress := Socket.Info.Remote.Address else
    FIPAddress := 0;

  FSlot := SilLayer.Layer.Slot;
  FBlind := SilLayer.Protocol.Blind;

  FChain.Add(SilLayer.Packer.Imate);
  FChain.Add(FSlot);

  FBlind := SilLayer.Protocol.Blind;
  DoAddProtocol(0, FBlind);
end;

procedure TClient.OnRequest(var Event: RBlindRequestEvent);
var
  Params: IParameterList;
begin
  Event.Result := false;

  if Guid.Compare(Event.Protocol, IEblisProtocol) = 0 then
  begin
    if not Assigned(FEblis) then
    begin
      FEblis := SilLayer.Protocol.Eblis;
      DoAddProtocol(Event.Id, FEblis);
    end;

    Event.Result := true;
  end;

  if Guid.Compare(Event.Protocol, IServerSide) = 0 then
  begin
    if not Assigned(FServerSide) then
    begin
      FServerSide := TServerSide.Create;
      DoAddProtocol(Event.Id, FServerSide, FCipherKey);
    end;

    Event.Result := true;
  end;

  if Guid.Compare(Event.Protocol, IServerSideFileProtocol) = 0 then
  begin
    if not Assigned(FFileProtocol) then
    begin
      Params := Sil.List.Parameters;
      Params['root'] := FData.Path + 'pub\';
      Sil.OS.FileSystem.ForceDirectories(Params['root']);

      FFileProtocol := SilLayer.Protocol.FileServer(Params);
      DoAddProtocol(Event.Id, FFileProtocol, FCipherKey);
    end;

    Event.Result := true;
  end;
end;

procedure TClient.DoAddProtocol(Id: LongWord; const Protocol: IUnknown; const CipherKey: String);
var
  Chain: ILayerChain;
  Params: IParameterList;
begin
  Params := Sil.List.Parameters;
  Chain := SilLayer.Layer.Chain;
  FSlot.Add(Chain);

  if Str.NotEmpty(CipherKey) then
  begin
    Params['Cipher'] := SilCoder.Cipher.SimpleMix;
    Params['Key'] := CipherKey;
    Chain.Add(SilLayer.Layer.Cipher(Params));
    Params.Clear;
  end;

  Params['Id'] := Id;
  Chain.Add(SilLayer.Protocol.Imate(Params));
  Chain.Add(Protocol);
  Chain.Add(Self);

  if FSlot.Control.IsActive then Chain.Control.Activate;
end;

procedure TClient.OnText(var Event: RBlindTextEvent);
begin
  // none
end;

procedure TClient.OnCreateAccount(var Event: RServerSideCreateAccountEvent);
begin
  DoCheckEnabled(FIsLogged = false);
  FData.SetupAccount(Event.Nick, Event.Password, Event.UserId, true);
end;

procedure TClient.OnGetAccount(var Event: RServerSideGetAccountEvent);
begin
  DoCheckEnabled(FIsLogged = false);
  FData.SetupAccount(Event.Nick, Event.Password, Event.UserId, false);
end;

procedure TClient.OnLogon(var Event: RServerSideLogonEvent);
begin
  DoCheckEnabled(FIsLogged = false);

  FData.LogonAccount(Event.UserId, Event.Password, FIPAddress);
  FUserId := Event.UserId;
  FIsLogged := true;

  DoSendUserStatus(usOnline);
end;

procedure TClient.Logoff;
begin
  DoSendUserStatus(usOffline);
end;

procedure TClient.DoSendUserStatus(Status: TUserStatus);
var
  Data: IParameterList;
begin
  if FIsLogged then
  begin
    Data := Sil.List.Parameters;
    Data['status'] := Status;
    FServer.SendInfo(FUserId, nil, Data);
  end;
end;

procedure TClient.OnQueryInfo(var Event: RServerSideQueryInfoEvent);
begin
  DoCheckEnabled(FIsLogged = true);
  FData.QueryInfo(Event.UserId, Event.Data);
end;

procedure TClient.OnQueryUsers(var Event: RServerSideQueryUsersEvent);
begin
  DoCheckEnabled(FIsLogged = true);
  FData.QueryUsers(FUserId, Event.Status, Event.Users);
end;

procedure TClient.OnQueryOfflines(var Event: RServerSideQueryOfflinesEvent);
var
  RecList, Text, Item, Kind: String;
  Data: IParameters;
  User: TGuid;
  Recipients: TGuidArray;
  i: Integer;
begin
  while FData.GetMessage(FUserId, Data) do
  begin
    Kind := Data['kind'];

    if Str.TextCompare(Kind, 'ime') = 0 then
    begin
      RecList := Data['recipients'];
      Text := Data['text'];
      User := Guid.FromStr(Data['from']);

      i := 0;

      while Str.Enumerate(RecList, ',', Item, i) do
        Guid.ArrayAdd(Recipients, Guid.FromStr(Item));

      FServer.SendMessage(Guid.FromStr(Data['msgid']), User, Recipients, Text, Data['time'], Length(Recipients) = 0);
    end else
    if Str.TextCompare(Kind, 'res') = 0 then
      FServer.SendResponse(Guid.FromStr(Data['msgid']), User, FUserId, Data['time']);

    FData.DeleteOffline(Data['id']);
  end;
end;

procedure TClient.OnSendInfo(var Event: RServerSideInfoEvent);
var
  Users: TUserArray;
begin
  DoCheckEnabled(FIsLogged = true);

  FData.QueryUsers(Guid.Null, usOnline, Users);
  FServer.SendInfo(FUserId, Prot.ToGuidArray(Users), Event.Data);
end;

procedure TClient.OnSendMessage(var Event: RServerSideMessageEvent);
begin
  DoCheckEnabled(FIsLogged = true);
  FServer.SendMessage(Event.MsgId, FUserId, Event.Recipients, Event.Text, Event.Time, Event.Bc);
end;

procedure TClient.OnSendResponse(var Event: RServerSideResponseEvent);
begin
  DoCheckEnabled(FIsLogged = true);
  FServer.SendResponse(Event.MsgId, FUserId, Event.Recipient, Event.Time);
end;

function TClient.GetId: TGuid;
begin
  Result := FUserId;
end;

function TClient.GetServerSide: IServerSide;
begin
  Result := FServerSide;
end;

function TClient.GetIsLogged: Boolean;
begin
  Result := FIsLogged;
end;

procedure TClient.OnGetFileSize(var Event: RFPFileSizeEvent);
begin
  Event.Size := Event.Source.Info.Size;
end;

procedure TClient.OnGetInfo(var Event: RFPInfoEvent);
begin
  Event.Info := Sil.OS.FileSystem.GetInfo(Event.FileName);
end;

procedure TClient.OnConnectionAccept(var Event: RServerSideConnectionEvent);
var
  UserId: TGuid;
  Client: IClient;
  Params: IParameterList;
begin
  DoCheckEnabled(FIsLogged = true);
  UserId := Guid.FromStr(Event.Params['user.id']);

  if not Guid.IsEmpty(UserId) and FServer.Onlines.Find(UserId, Client) then
  begin
    Params := Sil.List.Parameters;
    Params.Merge(Event.Params);

    Params['user.id'] := Guid.ToStr(FUserId);
    Params['address.ip'] := FIPAddress;

    Client.ServerSide.AcceptConnection(Params);
  end else
    DoCheckEnabled(false);
end;

procedure TClient.OnConnectionRequest(var Event: RServerSideConnectionEvent);
var
  UserId: TGuid;
  Client: IClient;
  Params: IParameterList;
begin
  DoCheckEnabled(FIsLogged = true);
  UserId := Guid.FromStr(Event.Params['user.id']);

  if not Guid.IsEmpty(UserId) and FServer.Onlines.Find(UserId, Client) then
  begin
    Params := Sil.List.Parameters;
    Params.Merge(Event.Params);
    
    Params['user.id'] := Guid.ToStr(FUserId);
    Params['address.ip'] := FIPAddress;

    Client.ServerSide.RequestConnection(Params);
  end else
    DoCheckEnabled(false);
end;

procedure TClient.OnOpenFile(var Event: RFPOpenFileEvent);
begin
  Event.Result := Sil.OS.FileSystem.OpenFile(Event.FileName, fmAccessRead, fmShareReadWrite, true);
end;

procedure TClient.OnReadFile(var Event: RFFileDataEvent);
begin
  Event.Count := Event.Source.Stream.Read(Event.Buffer^, Event.Count);
end;

procedure TClient.OnSeekFile(var Event: RFSeekFileEvent);
begin
  Event.Source.Stream.Seek(Event.Offset, Event.Origin);
end;

procedure TClient.OnCloseFile(var Event: RFCloseFileEvent);
begin
  // none
end;

procedure TClient.OnCreateDirectory(var Event: RFPCreateDirectoryEvent);
begin
  // none
end;

procedure TClient.OnCreateDirectoryReader(var Event: RFCreateDirectoryReaderEvent);
begin
  // none
end;

procedure TClient.OnCreateFile(var Event: RFPCreateFileEvent);
begin
  // none
end;

procedure TClient.OnDelete(var Event: RFPDeleteEvent);
begin
  // none
end;

procedure TClient.OnDestroyDirectoryReader(var Event: RFDirectoryReaderEvent);
begin
  // none
end;

procedure TClient.OnDirectoryRead(var Event: RFDirectoryReaderEvent);
begin
  // none
end;

procedure TClient.OnFlushFile(var Event: RFFlushFileEvent);
begin
  // none
end;

procedure TClient.OnMove(var Event: RFPMoveEvent);
begin
  // none
end;

procedure TClient.OnSetFileAttributes(var Event: RFPFileAttributeEvent);
begin
  // none
end;

procedure TClient.OnSetFileSize(var Event: RFPFileSizeEvent);
begin
  // none
end;

procedure TClient.OnSetFileTime(var Event: RFPFileTimeEvent);
begin
  // none
end;

procedure TClient.OnWriteFile(var Event: RFFileDataEvent);
begin
  // none
end;

procedure TClient.OnNegotiate(var Event: REblisNegotiateEvent);
begin
  FCipherKey := Event.Key;
end;

end.
