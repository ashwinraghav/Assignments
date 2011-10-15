unit UmFileServer;

interface

uses
  Sil,
  SilLayer,
  
  UiFileServer;

type
  TFileServer = class (
    TSilObject,
    IFileServer,
    ILayerConnectionManager)
  private
    FParams: IParameters;
    FConnection: ILayerConnectionServer;
  protected // IFileServer
    procedure Start(const Params: IParameters);
    procedure Stop;
  protected // ILayerConnectionManager
    procedure Initialize(const Connection: ILayerConnection; const Chain: ILayerChain);
    procedure Connected(const Connection: ILayerConnection);
    procedure Disconnected(const Connection: ILayerConnection);
    procedure ConnectFailed(const Connection: ILayerConnection);
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TFilePeer = class (
    TSilLayerImateConnectionPeer,
    IServerSideFileProtocolHook)
  private
    FHomeDir: String;
    FFile: IServerSideFileProtocol;
  protected
    procedure ProtocolRequest(var Event: RBlindRequestEvent); override;
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
    constructor Create(const HomeDir: String);
    destructor Destroy; override;
  end;

implementation

{ TFileServer }

constructor TFileServer.Create;
begin
  inherited Create;
end;

destructor TFileServer.Destroy;
begin
  inherited;
end;

procedure TFileServer.Start(const Params: IParameters);
begin
  FParams := Params;
  FConnection := SilLayer.Connection.Server;
  FConnection.Configure(Self);
  FConnection.Start;
end;

procedure TFileServer.Stop;
begin
  if Assigned(FConnection) then
  begin
    FConnection.Stop;
    FConnection := nil;
  end;
end;

procedure TFileServer.Initialize(const Connection: ILayerConnection; const Chain: ILayerChain);
var
  Params: IParameterList;
begin
  Params := Sil.List.Parameters;
  Params['Port'] := FParams['server.port'];

  Chain.Add(SilLayer.Device.SocketServer(Params));
  Chain.Add(FConnection);
  Chain.Control.Activate;
end;

procedure TFileServer.Connected(const Connection: ILayerConnection);
begin
  Connection.Context := TFilePeer.Create(FParams['share.path']);
end;

procedure TFileServer.Disconnected(const Connection: ILayerConnection);
begin
end;

procedure TFileServer.ConnectFailed(const Connection: ILayerConnection);
begin
end;

{ TFilePeer }

constructor TFilePeer.Create(const HomeDir: String);
begin
  inherited Create(nil);
  FHomeDir := Sil.OS.FSys.DeleteSlash(HomeDir);
end;

destructor TFilePeer.Destroy;
begin
  inherited;
end;

procedure TFilePeer.ProtocolRequest(var Event: RBlindRequestEvent);
var
  Params: IParameterList;
begin
  if Guid.Compare(Event.Protocol, IServerSideFileProtocol) = 0 then
  begin
    Params := Sil.List.Parameters;
    Params['Root'] := FHomeDir;
    FFile := SilLayer.Protocol.FileServer(Params);
    InsertProtocol(FFile, Self, Event.Id, false);
    Event.Result := true;
  end;
end;

procedure TFilePeer.OnCreateFile(var Event: RFPCreateFileEvent);
begin
  Event.Result := Sil.OS.FSys.OpenFile(Event.FileName, fmAccessRead, fmShareReadWrite, true);
end;

procedure TFilePeer.OnDelete(var Event: RFPDeleteEvent);
begin
  Event.Result := Sil.OS.FSys.DeleteFile(Event.FileName);
end;

procedure TFilePeer.OnOpenFile(var Event: RFPOpenFileEvent);
begin
  Event.Result := Sil.OS.FSys.OpenFile(Event.FileName, fmAccessRead, fmShareReadWrite, true);
end;

procedure TFilePeer.OnCloseFile(var Event: RFCloseFileEvent);
begin
end;

procedure TFilePeer.OnFlushFile(var Event: RFFlushFileEvent);
begin
  Event.Result := Event.Source.FlushBuffer;
end;

procedure TFilePeer.OnReadFile(var Event: RFFileDataEvent);
begin
  Event.Result := Event.Source.Stream.Read(Event.Buffer^, Event.Count);
end;

procedure TFilePeer.OnSeekFile(var Event: RFSeekFileEvent);
begin
  Event.Source.Stream.Seek(Event.Offset, Event.Origin);
end;

procedure TFilePeer.OnWriteFile(var Event: RFFileDataEvent);
begin
  Event.Result := Event.Source.Stream.Write(Event.Buffer^, Event.Count);
end;

procedure TFilePeer.OnGetFileSize(var Event: RFPFileSizeEvent);
begin
  Event.Size := Event.Source.Info.Size;
end;

procedure TFilePeer.OnGetInfo(var Event: RFPInfoEvent);
begin
  Event.Info := Sil.OS.FSys.GetInfo(Event.FileName);
end;

procedure TFilePeer.OnMove(var Event: RFPMoveEvent);
begin
  Event.Result := Sil.OS.FSys.MoveFile(Event.OldName, Event.NewName);
end;

procedure TFilePeer.OnSetFileAttributes(var Event: RFPFileAttributeEvent);
begin
  Event.Source.Info.Attributes := Event.Attributes;
end;

procedure TFilePeer.OnSetFileSize(var Event: RFPFileSizeEvent);
begin
  Event.Source.Stream.Size := Event.Size;
end;

procedure TFilePeer.OnSetFileTime(var Event: RFPFileTimeEvent);
begin
  Event.Source.Info.Time := Event.Time;
end;

procedure TFilePeer.OnCreateDirectory(var Event: RFPCreateDirectoryEvent);
begin
  Event.Result := Sil.OS.FSys.CreateDirectory(Event.PathName);
end;

procedure TFilePeer.OnCreateDirectoryReader(var Event: RFCreateDirectoryReaderEvent);
begin
  Event.Result := Sil.OS.FSys.ReadDirectory(Event.Path, Event.Include, Event.Exclude);
end;

procedure TFilePeer.OnDestroyDirectoryReader(var Event: RFDirectoryReaderEvent);
begin
end;

procedure TFilePeer.OnDirectoryRead(var Event: RFDirectoryReaderEvent);
begin
  Event.Reader.BufferSize := Event.BufferSize;
  Event.Result := Event.Reader.Read;
end;

end.
