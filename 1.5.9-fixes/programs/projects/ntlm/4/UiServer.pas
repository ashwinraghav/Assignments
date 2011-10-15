unit UiServer;

interface

uses
  Sil;

type
  IProxyInfo = interface
    ['{3E790D80-05E0-4842-ACAF-4C5E89E33867}']
    function GetServer: String;
    function GetPort: Word;
    function GetHost: String;
    function GetDomain: String;
    function GetPassword: String;
    function GetUser: String;
    procedure SetServer(const Value: String);
    procedure SetPort(Value: Word);
    procedure SetHost(const Value: String);
    procedure SetDomain(const Value: String);
    procedure SetPassword(const Value: String);
    procedure SetUser(const Value: String);
    Property Server: String read GetServer write SetServer;
    Property Port: Word read GetPort write SetPort;
    Property Host: String read GetHost write SetHost;
    Property Domain: String read GetDomain write SetDomain;
    Property Password: String read GetPassword write SetPassword;
    Property User: String read GetUser write SetUser;
  end;

  IServer = interface
    ['{C330578C-690B-44B7-BCD7-C4D26A521EF1}']
    function GetProxyInfo: IProxyInfo;
    function GetClients: IInterfaceList;
    procedure Start;
    procedure Stop;
    property ProxyInfo: IProxyInfo read GetProxyInfo;
    property Clients: IInterfaceList read GetClients;
  end;

implementation

end.
 