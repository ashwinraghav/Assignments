unit UiServer;

interface

uses
  Sil;
  
type
  IOuchServer = interface
    ['{8ED8098B-9743-4CE9-B536-49DDD8CC52B9}']
    function GetClients: IInterfaceList;
    procedure Shutdown;
    property Clients: IInterfaceList read GetClients;
  end;

implementation

end.
 