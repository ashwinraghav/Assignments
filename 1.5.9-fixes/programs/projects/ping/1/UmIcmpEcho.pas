unit UmIcmpEcho;

interface

uses
  Sil,
  UiIcmp,
  UiIcmpEcho;

type
  TSilIcmpEcho = class (TSilObject, IIcmpEcho)
  private
    FSeq: Integer;
  protected // IIcmpEcho
    function Ping(const Host: String; Timeout: LongWord = 2000; DataSize: LongWord = 20): Integer; overload;
    function Ping(const Socket: ISocketClient; var Dest: ISocketAddress; const Query: REcho): REcho; overload;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  Windows,
  UmIcmp;

{ TSilIcmpEcho }

constructor TSilIcmpEcho.Create;
begin
  inherited Create;
  FSeq := 0;
end;

destructor TSilIcmpEcho.Destroy;
begin
  inherited;
end;

function TSilIcmpEcho.Ping(const Socket: ISocketClient; var Dest: ISocketAddress; const Query: REcho): REcho;
var
  Packet: IPacket;
  Icmp: IIcmpProtocol;
  Elap: TDateTime;
  Size: LongWord;
begin
  Icmp := TIcmpProtocol.Create;

  Packet := Icmp.Echo.Pack(Query);
  Socket.Stream.WriteTo(Packet.Buffer.Memory^, Packet.Buffer.Size, Dest);
  Elap := DateTime.Now + DateTime.FromMilisecs(Socket.Parameters.ReadTimeout);

  while true do
  begin
    Packet.Buffer.Size := 1024;
    Sil.Mem.Clear(Packet.Buffer.Memory^, Packet.Buffer.Size);
    Size := Socket.Stream.ReadFrom(Packet.Buffer.Memory^, Packet.Buffer.Size, Dest);

    if Size <> 0 then
    begin
      Result := Icmp.Echo.Unpack(Packet);
      if Query.Id = Result.Id then Break;
    end;

    if (DateTime.Now > Elap) or (Size = 0) then
    begin
      Dest := nil;
      Break;
    end;
  end;
end;

function TSilIcmpEcho.Ping(const Host: String; Timeout, DataSize: LongWord): Integer;
var
  Socket: ISocketClient;
  Dest: ISocketAddress;
  Query, Reply: REcho;
  Timestamp: LongWord;
begin
  Socket := Sil.Os.Socket.CreateClient(stRaw, spICMP);
  Socket.Parameters.ReadTimeout := Timeout;

  Dest := Sil.Os.Socket.IP.Create(Host, 0);

  Query.Id := GetCurrentProcessId();
  Query.Sequence := Sil.Os.Locked.Increment(FSeq);
  Query.Data := Sil.Str.Replicate('X', DataSize);

  Timestamp := GetTickCount;

  Reply := Ping(Socket, Dest, Query);
  Result := GetTickCount() - TimeStamp;

  if not Assigned(Dest) then
    Result := Result * -1;
end;

end.
