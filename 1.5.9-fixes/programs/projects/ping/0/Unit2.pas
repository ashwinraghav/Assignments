unit Unit2;

interface

uses
  Windows, WinSock, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Sil, StdCtrls, ExtCtrls;

const
  ICMP_ECHO = 8;
  ICMP_ECHOREPLY = 0;

type
  PIP = ^RIP;
  RIP = packed record
    length: Byte;
    tos: Byte;
    total_len: Word;
    ident: Word;
    frag_and_flags: Word;
    ttl: Word;
    proto: Word;
    checksum: Word;
    sourceIP: LongWord;
    destIP: LongWord;
  end;

  PICMP = ^RICMP;
  RICMP = packed record
    i_type: BYTE;
    i_code: BYTE; (* type sub code *)
    i_cksum: Word;
    i_id: Word;
    i_seq: Word;
    (* This is not the std header, but we reserve space for time *)
    timestamp: LongWord;
  end;


  REchoHeader = packed record
    Id: Word;
    Sequence: Word;
  end;

  REchoQuery = packed record
    Header: REchoHeader;
    Data: String;
  end;

  REchoReply = packed record
    Header: REchoHeader;
    Delay: LongWord;
  end;

  IIcmpProtocol = interface
    ['{7E937181-0DA6-41BA-A34A-99B362D196A3}']
    function Echo(const Query: REchoQuery): IPacket; overload;
    function Echo(const Buffer: IPacket; var Reply: REchoReply): String; overload;
  end;

  TForm2 = class(TForm)
    Button1: TButton;
    Edit1: TEdit;
    Display: TMemo;
    Timer: TTimer;
    procedure Button1Click(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    FSocket: ISocketClient;
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

var
  Seq: Integer = 0;

type
  TIcmpProtocol = class(
    TSilInterfacedObject,
    IIcmpProtocol )
  private
    function DoChecksum(Buffer: PWord; Size: Integer): Word;
  protected // IIcmpProtocol
    function Echo(const Query: REchoQuery): IPacket; overload;
    function Echo(const Buffer: IPacket; var Reply: REchoReply): String; overload;
  end;

{ TIcmpProtocol }

function TIcmpProtocol.Echo(const Query: REchoQuery): IPacket;
begin
  Result := Sil.
end;

function TIcmpProtocol.Echo(const Buffer: IPacket; var Reply: REchoReply): String;
begin

end;

function TIcmpProtocol.DoChecksum(Buffer: PWord; Size: Integer): Word;
var
  CkSum: LongWord;
begin
  CkSum := 0;

  while Size > 1 do
  begin
    Inc(CkSum, Buffer^);
    Inc(Buffer);
    Dec(Size, SizeOf(Word));
  end;

  if Size <> 0 then
    Inc(CkSum, PByte(Buffer)^);

  CkSum := (CkSum shr 16) + (CkSum and $FFFF);
  Result := CkSum + CkSum shr 16;

  Result := not Result;
end;

procedure TForm2.Button1Click(Sender: TObject);
begin
  Timer.Enabled := not Timer.Enabled; 
end;

procedure TForm2.TimerTimer(Sender: TObject);
var
  Address: ISocketAddress;
  Data: Pointer;
  Recv: array[0..1023] of Byte;
  ICMP: PICMP;
  IP: PIP;
  Text: PChar;
  Size: Integer;
begin
  FSocket := Sil.Os.Socket.CreateClient(stRaw, spICMP);
  
//@@  FSocket.Parameters.Flags[sfReceiveTimeout] := 1000;
//@@  FSocket.Parameters.Flags[sfSendTimeout] := 1000;

  Address := Sil.Os.Socket.IP.Create(Edit1.Text, 0);

  Data := Sil.Mem.Alloc(64 + SizeOf(RICMP));

  ICMP := Data;

  icmp.i_type := ICMP_ECHO;
  icmp.i_code := 0;
  icmp.i_id := GetCurrentProcessId();
  icmp.i_cksum := 0;
  icmp.i_seq := Sil.Os.Locked.Increment(Seq);

  Text := PChar(ICMP) + sizeof(RICMP);

  StrCopy(Text, 'HOLA VISUAL PING');

  icmp.timestamp := GetTickCount;
  icmp.i_cksum := checksum(Data, 64 + SizeOf(RICMP));


  FSocket.Stream.WriteTo(Data^, 64 + SizeOf(RICMP), Address);

  if FSocket.Stream.ReadFrom(Recv, SizeOf(Recv), Address) > 0 then
  begin
    IP := @Recv;

    Size := (IP.length and $0F) shl 2;
    if Size < 8 then
      raise Sil.Error.Create('Too few bytes');

    ICMP := Pointer(LongWord(IP) + Size);

    if ICMP.i_type <> ICMP_ECHOREPLY then
      raise Sil.Error.Create('non-echo type');

    if ICMP.i_id <> GetCurrentProcessId() then
      raise Sil.Error.Create('someone else''s packet!');

    Display.Lines.Add( Format('%d bytes from %s: sequence = %d, time %d', [Size, Sil.Os.Socket.IP.ToStr(Address.Address), ICMP.i_seq, GetTickCount()-ICMP.timestamp]));

  end;
end;

end.
