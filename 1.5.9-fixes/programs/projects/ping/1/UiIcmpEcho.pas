unit UiIcmpEcho;

interface

uses
  Sil,
  UiIcmp;

type
  IIcmpEcho = interface
    ['{281ADD6C-04EF-494A-B801-C19A211C2B07}']
    function Ping(const Host: String; Timeout: LongWord = 2000; DataSize: LongWord = 20): Integer; overload;
    function Ping(const Socket: ISocketClient; var Dest: ISocketAddress; const Query: REcho): REcho; overload;
  end;

implementation

end.
 