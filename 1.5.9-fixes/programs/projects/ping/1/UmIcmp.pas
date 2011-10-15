unit UmIcmp;

interface

uses
  Sil, UiIcmp;

type
  PIP = ^RIP;
  RIP = packed record
    length_and_version: Byte;
    tos: Byte;
    total_len: Word;
    ident: Word;
    frag_and_flags: Word;
    ttl: Byte;
    proto: Byte;
    checksum: Word;
    sourceIP: LongWord;
    destIP: LongWord;
  end;

type
  PICMP = ^RICMP;
  RICMP = packed record
    i_type: BYTE;
    i_code: BYTE; (* type sub code *)
    i_cksum: Word;
    i_id: Word;
    i_seq: Word;
    timestamp: longword;
  end;

type
  TIcmpProtocol = class(
    TSilInterfacedObject,
    IIcmpProtocol,
    IIcmpEcho )
  private
    function DoChecksum(Buffer: Pointer; Size: Integer): Word;
    function DoIpUnpack(const Packet: IPacket; out Inner: IPacket): RIP;
  protected // IIcmpProtocol
    function GetEcho: IIcmpEcho;
  protected // IIcmpEcho
    function Pack(const Query: REcho): IPacket;
    function Unpack(const Buffer: IPacket): REcho;
  end;

implementation

uses
  Windows;

{ TIcmpProtocol }

function TIcmpProtocol.GetEcho: IIcmpEcho;
begin
  Result := Self;
end;

function TIcmpProtocol.Pack(const Query: REcho): IPacket;
begin
  Result := Sil.Stream.Raw.Packet();
  Result.Writer.WriteByte(ICMP_ECHO);      // i_type
  Result.Writer.WriteByte(0);              // i_code
  Result.Writer.WriteWord(0);              // i_cksum
  Result.Writer.WriteWord(Query.Id);       // i_id
  Result.Writer.WriteWord(Query.Sequence); // i_seq
  Result.Writer.WriteString(Query.Data);   // icmp data
  PIcmp(Result.Buffer.Memory).i_cksum := DoCheckSum(Result.Buffer.Memory, Result.Buffer.Size);
end;

function TIcmpProtocol.Unpack(const Buffer: IPacket): REcho;
var
  IP: RIP;
  Inner: IPacket;
begin
  IP := DoIpUnpack(Buffer, Inner);

  if Inner.Buffer.Size < 8 then
    raise Sil.Error.Create('Too few bytes');

(*)  if DoChecksum(Inner.Buffer.Memory, Inner.Buffer.Size) <> 0 then
    raise Sil.Error.Create('invalid checksum');(*)

  if Inner.Reader.ReadByte() <> ICMP_ECHOREPLY then
  begin
    FillChar(Result, SizeOf(Result), 0);
    Exit;
  end;
    //raise Sil.Error.Create('non-echo type');

  Inner.Reader.ReadByte(); // i_code: ignorado
  Inner.Reader.ReadWord(); // checksum

  Result.Id := Inner.Reader.ReadWord;
  Result.Sequence := Inner.Reader.ReadWord;
  Result.Data := Inner.Reader.ReadString();
end;

function TIcmpProtocol.DoChecksum(Buffer: Pointer; Size: Integer): Word;
var
  CkSum: LongWord;
  Ptr: PWord absolute Buffer;
begin
  CkSum := 0;

  while Size > 1 do
  begin
    Inc(CkSum, Ptr^);
    Inc(Ptr);
    Dec(Size, SizeOf(Word));
  end;

  if Size <> 0 then
     Inc(CkSum, PByte(Ptr)^);

  CkSum := (CkSum shr 16) + (CkSum and $FFFF);
  Result := CkSum + CkSum shr 16;

  Result := not Result;
end;

function TIcmpProtocol.DoIpUnpack(const Packet: IPacket; out Inner: IPacket): RIP;
begin
  Packet.Buffer.Position := 0;
  Packet.Reader.Read(Result, SizeOf(Result));
  Inner := Sil.Stream.Raw.Packet();
  Inner.Buffer.Size := (Result.length_and_version and $0F) shl 2;
  if Inner.Buffer.Size > 0 then
  begin
    Packet.Reader.Read(Inner.Buffer.Memory^, Inner.Buffer.Size);
    Inner.Buffer.Position := 0;
  end;
end;

end.
