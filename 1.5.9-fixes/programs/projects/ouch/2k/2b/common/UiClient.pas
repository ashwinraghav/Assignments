unit UiClient;

interface

uses
  Sil,
  SilData,
  SilLayer,

  UiProtocol;

const
  COuchClient: TGUID = '{AEFA8B62-76C6-40A5-8E40-7244A5839BB2}';
  SOuchClient: PChar = 'ouch2.client.engine.1';
  COuchData: TGUID = '{FD223EB2-0206-4A76-8554-C1CD01284037}';
  SOuchData: PChar = 'ouch2.client.data.1';
  COuchContact: TGUID = '{FA40F5DF-B36A-4A77-B9C2-D838DFA90F29}';
  SOuchContact: PChar = 'ouch2.client.contact.1';

const
  hsRecibido    = 'r';
  hsConfirmado  = 'c';
  hsEsperando   = 'w';
  hsRecibiendo  = 'rp';
  hsEnviando    = 'sp';


type
  IData = interface;
  IClient = interface;
  IClientHook = interface;

  IData = interface
    ['{00CC2B5C-AC65-4728-9187-5E6D48C86964}']
    procedure Initialize(const Path: String);
    procedure Finalize;
    function FindAccount(const Id: TGuid; out Info: IParameterList): Boolean;
    function QueryUser(const Id: TGuid; out Info: IParameterList): Boolean;
    function AccountExists(const Id: TGuid; const Password: String): Boolean;
    procedure CreateLocalAccount(const Id: TGuid; const Nick, Password: String);
    procedure UpdateUser(const Id: TGuid; const Info: IParameters);
    procedure AppendHistory(const MsgId: TGUID; const SndTime, RcvTime: TDateTime; const FromId, ToId: TGuid; const Kind, Status, Text: String);
    function OpenHistory(const User: TGuid; out Table: IDataRowset): Boolean;
  end;

  IClient = interface
    ['{C60898B4-BE05-462D-8A6B-FDD859AC150A}']
    function GetFileProtocol: IClientSideFileProtocol;
    function GetIsOnline: Boolean;
    function GetNick: String;
    function GetId: TGuid;
    function GetRootPath: string;
    function GetBasePath: string;
    procedure Start;
    procedure Stop;
    procedure Finalize;
    procedure Initialize;
    procedure Configure(const RootPath: string = ''; const ConfigName: string = '');
    procedure CreateLocalAccount(const Nick, Password: String);
    procedure GetAccount(const Nick, Password: String);
    procedure Logon;
    procedure SendMessage(const MsgId: TGUID; const Recipients: TGuidArray; const Text: String; Bc: Boolean);
    procedure SendInfo(const Data: IParameters);
    procedure QueryUsers(Status: TUserStatus; out Users: TUserArray);
    function QueryUser(const Id: TGuid; out Info: IParameterList; Update: Boolean = false): Boolean;
    procedure QueryOfflines;
    function OpenHistory(const User: TGuid; out Table: IDataRowset): Boolean;
    procedure RequestFileTransfer(const User: TGuid; const List: IFileInfoList);
    property IsOnline: Boolean read GetIsOnline;
    property Nick: String read GetNick;
    property Id: TGuid read GetId;
    property RootPath: string read GetRootPath;
    property BasePath: string read GetBasePath;
    property FileProtocol: IClientSideFileProtocol read GetFileProtocol;
  end;

  IClientHook = interface
    ['{7E3CFE6C-E849-4848-A0DA-A68D0C2FC4FD}']
    procedure OnMessage(const Id: TGuid; const Text: String; const Time: TDateTime; const Recipients: TGuidArray);
    procedure OnInfo(const Id: TGuid; const Data: IParameters);
    procedure OnConnected;
    procedure OnDisconnected;
    procedure OnConnectionFailed;
    procedure OnConnectionRequest(const Params: IParameters);
    procedure OnConnectionAccept(const Params: IParameters);
  end;

  IOuchPending = interface
    ['{32950EEF-ACC7-4BF9-B6E7-142F055D94E5}']
  end;

implementation

end.
