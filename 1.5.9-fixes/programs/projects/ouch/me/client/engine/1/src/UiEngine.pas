unit UiEngine;

interface

uses
  Sil,
  UiOuchProtocol,
  UiOuchEngine;

type
  IEngineConnection = interface;
  IEngineClient = interface;
  IEngineListener = interface;
  IEngineSender = interface;
  IEngineChannel = interface;

  IEngineConnection = interface
    ['{9A69AD3D-CEB9-470B-ABCA-3760EF4CC032}']
    function GetEngine: IOuchEngine;
    function GetListener: IEngineListener;
    procedure Activate;
    property Engine: IOuchEngine read GetEngine;
    property Listener: IEngineListener read GetListener;
  end;

  IEngineListener = interface
    ['{38DE3264-E17D-4089-96B6-AA582B67D62A}']
    function GetConnection: IEngineConnection;
    function GetServer: IServerSocketConnection;
    procedure Startup;
    procedure Shutdown;
    property Connection: IEngineConnection read GetConnection;
    property Server: IServerSocketConnection read GetServer;
  end;

  IEngineClient = interface
    ['{70AD4AFE-BE12-45C9-9A77-2E3F5329A48C}']
    function GetListener: IEngineListener;
    function GetChannel: IEngineChannel;
    property Listener: IEngineListener read GetListener;
    property Channel: IEngineChannel read GetChannel;
  end;

  IEngineSender = interface
    ['{487943BD-0471-4991-AB64-029CE31CF6D7}']
    function GetConnection: IEngineConnection;
    function GetChannel: IEngineChannel;
    procedure Startup;
    procedure Shutdown;
    property Connection: IEngineConnection read GetConnection;
    property Channel: IEngineChannel read GetChannel;
  end;

  IEngineChannel = interface
    ['{F42B875C-C5AC-4067-BAF2-FBFF76326923}']
    function GetConnection: IClientSocketConnection;
    function GetRequest: IOuchRequest;
    function GetReply: IOuchReply;
    property Connection: IClientSocketConnection read GetConnection;
    property Request: IOuchRequest read GetRequest;
    property Reply: IOuchReply read GetReply;
  end;

  IEngineSession = interface
    ['{622C99FB-977B-427E-8EAB-256C71D57C89}']
    function GetSender: IEngineSender;
    function GetOuch: IOuchSession;
    procedure Request;
    property Sender: IEngineSender read GetSender;
    property Ouch: IOuchSession read GetOuch;
  end;

  IEngineGroup = interface
    ['{CA419238-94F8-4E3D-BB3F-02EE2455AB09}']
    function GetOuch: IOuchGroup;
    function GetSession: IEngineSession;
    function SetUsers(const Users: IOuchUsers): IEngineGroup;
    property Ouch: IOuchGroup read GetOuch;
    property Session: IEngineSession read GetSession;
  end;

  IEnginePending = interface
    ['{57EBA545-E188-4ACD-AEF7-5195D44DFD86}']
    function GetID: TGUID;
    function GetItem: IUnknown;
    function Get(const IID: TGUID; out Obj): Boolean;
    property ID: TGUID read GetID; 
    property Item: IUnknown read GetItem; 
  end;
  
  IEnginePendings = interface(IInterfaceList)
    ['{B3A734B8-C05D-43EA-A832-6B78DED18005}']
  end;

implementation
end.
