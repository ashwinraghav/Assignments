unit UiLogan;

interface

uses
  Sil;

type
  ILogan = interface
    ['{D5AD204C-A85B-489E-A29A-0B063B0B0F1C}']
    procedure Start;
    procedure Stop;
  end;

  ILoganClient = interface
    ['{A0EF2BAC-1EAC-4CCB-A5A4-5B1BF30D2BFD}']
    function GetModuleName: String;
    function IsOnline: Boolean;
    procedure Write(const Packet: IPacket);
    property ModuleName: String read GetModuleName;
  end;

implementation

end.

