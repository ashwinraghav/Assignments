unit UiIcmp;

interface

uses
  Sil;

const
  ICMP_ECHO = 8;
  ICMP_ECHOREPLY = 0;

type
  REcho = packed record
    Id: Word;
    Sequence: Word;
    Data: String;
  end;

  IIcmpEcho = interface
    ['{C2804FCF-D206-4535-9C1E-04943D3E5673}']
    function Pack(const Query: REcho): IPacket;
    function Unpack(const Buffer: IPacket): REcho;
  end;

  IIcmpProtocol = interface
    ['{7E937181-0DA6-41BA-A34A-99B362D196A3}']
    function GetEcho: IIcmpEcho;
    property Echo: IIcmpEcho read GetEcho;
  end;

implementation
end.
