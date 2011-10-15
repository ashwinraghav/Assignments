unit UiOuch;

interface

uses
  Sil, SilData, Classes, Db, TypInfo,
  UiProtocol, UiClient;

type
  TUserStatus         = UiProtocol.TUserStatus;

const
  usAny               = TUserStatus(UiProtocol.usAny    );
  usOnline            = TUserStatus(UiProtocol.usOnline );
  usOffline           = TUserStatus(UiProtocol.usOffline);

const
  EV_OUCHBASE = EV_FIRST + $1785;
  
type
  TOuchEngineState = (
      esUnknown,
      esDisconnected,
      esConnected,
      esLoggedOn
    );

const
  EV_OUCH_UNKNOWN         = EV_OUCHBASE + Ord(esUnknown);
  EV_OUCH_DISCONNECTED    = EV_OUCHBASE + Ord(esDisconnected);
  EV_OUCH_CONNECTED       = EV_OUCHBASE + Ord(esConnected);
  EV_OUCH_LOGGEDON        = EV_OUCHBASE + Ord(esLoggedOn);

type
  IOuchEngine = interface;
  IOuchFactory = interface;
  IOuchApplication = interface;
  IOuchWindows = interface;
  IOuchWindowList = interface;
  IOuchWindow = interface;
  IOuchView = interface;
  IOuchEventView = interface;
  IOuchAccount = interface;
  IOuchLocalAccount = interface;
  IOuchContact = interface;
  IOuchAccounts = interface;
  IOuchAccountList = interface;
  IOuchMessage = interface;
  IOuchProfile = interface;

  IOuchEngine = interface
    ['{857D9018-5414-4631-879A-05A61D77F294}']
    function GetFactory: IOuchFactory;
    function GetClient: IClient;
    function GetState: TOuchEngineState;
    function GetIsOnline: Boolean;
    function GetAccount: IOuchLocalAccount;
    function GetProfile: IOuchProfile;
    procedure Reset;
    procedure Setup(const Nick, Password: string; MustExist: Boolean = True);
    procedure Logon;
    procedure Send(const Msg: IOuchMessage);
    procedure Update;
    property Factory: IOuchFactory read GetFactory;
    property Client: IClient read GetClient;
    property State: TOuchEngineState read GetState;
    property IsOnline: Boolean read GetIsOnline;
    property Account: IOuchLocalAccount read GetAccount;
    property Profile: IOuchProfile read GetProfile;
  end;

  IOuchEngineConnectedEvent = interface
    ['{E9B8E540-EA81-4042-84B5-816F46BD2578}']
    procedure OnEngineConnected(const Sender: IOuchEngine);
  end;

  IOuchEngineDisconnectedEvent = interface
    ['{ED0C0EBC-3163-4E47-B265-F57005A01425}']
    procedure OnEngineDisconnected(const Sender: IOuchEngine);
  end;

  TOuchUpdateStage = (
      usStart,
      usDownload,
      usChange,
      usEnd,
      usError
    );

  POuchUpdateStatus = ^ROuchUpdateStatus;
  ROuchUpdateStatus = record
    Message: string;
    Stage: TOuchUpdateStage;
  end;

  ROuchEvUpdateStarted = record
    Sender: IOuchEngine;
  end;

  ROuchEvUpdateComplete = record
    Sender: IOuchEngine;
    Status: ROuchUpdateStatus;
  end;

  ROuchEvUpdateNotification = record
    Sender: IOuchEngine;
    Status: ROuchUpdateStatus;
  end;

  IOuchEngineUpdateEvent = interface
    ['{E6CA927A-93EE-4D56-BC30-5D003C0B32D8}']
    procedure OnUpdateNotification(const Event: ROuchEvUpdateNotification);
  end;

  IOuchEngineStatusEvent = interface
    ['{0E91ECF8-2130-4FF7-9654-DA5ABDC2E7FC}']
    procedure OnStatusChanged(const Sender: IOuchEngine);
  end;

  IOuchEngineMessageEvent = interface
    ['{BFDBC9B7-3B79-4728-9A47-F5E0A5DC0DCF}']
    procedure OnMessage(const Msg: IOuchMessage);
  end;

  IOuchFactory = interface
    ['{CC30396C-F558-423B-85D4-DB1EECF89320}']
    function AccountList: IOuchAccountList; overload;
    function AccountList(const Contacts: IOuchAccounts): IOuchAccountList; overload;
    function AccountList(const Users: TGuidArray): IOuchAccountList; overload;
    function Message(const From: IOuchAccount; const Text: String; const Time: TDateTime; const Recipients: IOuchAccounts): IOuchMessage; overload;
    function Message(const From: IOuchAccount; const Recipients: IOuchAccounts): IOuchMessage; overload;
    function Message: IOuchMessage; overload;
    function Profile(const FileName: string): IOuchProfile; 
  end;

  IOuchApplication = interface (IReferenceable)
    ['{857BFD30-A706-41FC-AB7A-F3931008F4BF}']
    function GetView: IOuchView;
    function GetLog: IOuchEventView;
    function GetEngine: IOuchEngine;
    function GetWindows: IOuchWindows;
    procedure Compose(const Contacts: IOuchAccounts);
    procedure FileTransfer(const Contacts: IOuchAccounts);
    property View: IOuchView read GetView;
    property Log: IOuchEventView read GetLog;
    property Engine: IOuchEngine read GetEngine;
    property Windows: IOuchWindows read GetWindows;
  end;

  IOuchApplicationEvents = interface
    ['{C57AA0F6-6DCB-46C1-A5C0-B8EC279C176A}']
  end;

  IOuchWindows = interface
    ['{E74E70A8-90AA-4977-99B1-B19E56E4FFB9}']
    function GetCount: Integer;
    function Create(WindowClass: TComponentClass; const IID: TGUID; out Instance; Lookup: Boolean = False): Boolean;
    function Find(const IID: TGUID; out Item): Boolean;  
    function Enumerate(var Enum: IEnumerator; out Item: IOuchWindow): Boolean; overload;
    function Enumerate(var Enum: IEnumerator; const IID: TGUID; out Item): Boolean; overload;
    procedure Clear;
    property Count: Integer read GetCount;
  end;

  IOuchWindowList = interface (IOuchWindows)
    ['{45C22F51-8E35-4CEB-AF43-5A117BB2A041}']
    function Add(const Item: IOuchWindow): Integer;
    function Remove(const Item: IOuchWindow): Integer;
  end;

  IOuchWindow = interface (IDispatchable)
    ['{5B3806F4-B786-4BEA-B029-40B86FE625D1}']
    function GetInstance: TComponent;
    procedure Show;
    procedure Hide;
    procedure Close;
    property Component: TComponent read GetInstance;
  end;

  IOuchView = interface
    ['{A63608E9-FA79-45C3-ABF5-7E0F6A7B6DB8}']
    procedure BeginUpdate;
    procedure EndUpdate;
  end;

  IOuchTraybar = interface
    ['{A616FFB7-FD92-4AA1-8ABF-B131FF420F6D}']
    procedure OnReceived(const Msg: IOuchMessage); 
  end;
  
  IOuchAccount = interface
    ['{7C95BF86-CD53-4448-B3E9-3A531527856B}']
    function GetID: TGuid;
    function GetNick: String;
    function GetMail: String;
    function GetState: TUserStatus;
    procedure SetState(Value: TUserStatus);
    function GetInfo: IParameters;
    function GetData: Pointer;
    procedure SetData(Value: Pointer);
    property ID: TGuid read GetID;
    property Nick: String read GetNick;
    property Mail: String read GetMail;
    property Info: IParameters read GetInfo;
    property State: TUserStatus read GetState write SetState;
    property Data: Pointer read GetData write SetData;
  end;

  IOuchContact = interface (IOuchAccount)
    ['{FF7E9C3C-82FF-45B5-8E68-E29CFF74454C}']
    procedure SetInfo(const Value: IParameters);
    property ID: TGuid read GetID;
    property Nick: String read GetNick;
    property Mail: String read GetMail;
    property Info: IParameters read GetInfo write SetInfo;
    property Data: Pointer read GetData write SetData;
  end;

  IOuchLocalAccount = interface(IOuchAccount)
    ['{AA43E887-15AF-4259-A82D-C266D0034B00}']
  end;

  IOuchAccounts = interface
    ['{77BF3045-B546-4B13-AF5D-6E7DEF0C4B7B}']
    function GetCount: Integer;
    function Find(const ID: TGUID; out Item: IOuchAccount): Boolean;
    function Enumerate(var Enum: IEnumerator; out Item: IOuchAccount): Boolean;
    function ToGuidList: TGuidArray;
    property Count: Integer read GetCount;
  end;
    
  IOuchContactsEvents = interface
    ['{54D29A2F-8B01-4BA6-997E-2C7ED4ED18AA}']
    procedure OnUserAdd(const User: IOuchAccount);
    procedure OnUserRemove(const User: IOuchAccount);
    procedure OnUserChanged(const User: IOuchAccount);
  end;

  IOuchAccountList = interface (IOuchAccounts)
    ['{5C4C1317-BF90-4A8F-A017-4333AC4208D4}']
    procedure Clear;
    function Add(const Item: IOuchAccount): Integer; overload;
    function Remove(const Item: IOuchAccount): Integer; overload;
    procedure Delete(Index: Integer);
    procedure Changed(const Item: IOuchAccount);
  end;

  IOuchMessage = interface
    ['{9764EBC5-B41A-41EE-9562-7149ACFBBF6D}']
    function GetFrom: IOuchAccount;
    procedure SetFrom(const Value: IOuchAccount);
    function GetTime: TDateTime;
    procedure SetTime(const Value: TDateTime);
    function GetText: string;
    procedure SetText(const Value: string);
    function GetRecipients: IOuchAccountList;
    procedure SetRecipients(const Value: IOuchAccountList);
    function GetBlindCopy: Boolean;
    procedure SetBlindCopy(Value: Boolean);
    function GetReplyTo: IOuchMessage;
    procedure SetReplyTo(const Value: IOuchMessage);
    property From: IOuchAccount read GetFrom write SetFrom;
    property Time: TDateTime read GetTime write SetTime;
    property Text: string read GetText write SetText;
    property BlindCopy: Boolean read GetBlindCopy write SetBlindCopy;
    property Recipients: IOuchAccountList read GetRecipients write SetRecipients;
    property ReplyTo: IOuchMessage read GetReplyTo write SetReplyTo;
  end;

  IOuchMessageView = interface (IOuchView)
    ['{161956AE-BD41-4800-8409-47E009040C4A}']
    procedure Show(const Msg: IOuchMessage);
    procedure Compose(const Msg: IOuchMessage);
  end;

  IOuchEventView = interface (IOuchView)
    ['{07B3E9D3-7C8E-4504-94A3-611227C43CA5}']
    procedure Add(const Message: string; const Operation: string = ''); overload;
    procedure Add(const Message: string; const Args: array of const; const Operation: string = ''); overload;
  end;

  IOuchHistoryWindow = interface (IOuchWindow)
    ['{DB5D20F3-7BED-494D-836D-1414EC021E18}']
    function GetAccount: IOuchAccount;
    procedure SetAccount(const Value: IOuchAccount);
    property Account: IOuchAccount read GetAccount write SetAccount;
  end;

  IOuchLogonData = interface
    ['{E53C8D45-E867-43FB-B171-0E480D9D9F4C}']
    function GetNick: String;
    function GetPassword: String;
    property Nick: String read GetNick;
    property Password: String read GetPassword;
  end;

  IOuchLogonWindow = interface (IOuchWindow)
    ['{46797289-13E9-4DAC-9ABB-6A0DBAE1B8E7}']
    function GetData: IOuchLogonData;
    function GetLogonOnly: Boolean;
    procedure SetLogonOnly(Value: Boolean);
    property Data: IOuchLogonData read GetData;
    property LogonOnly: Boolean read GetLogonOnly write SetLogonOnly; 
  end;

  IOuchProfile = interface
    ['{EE41D020-19FD-4DC1-84E3-FE9F691F9EC7}']
    procedure Load(const Instance: IOuchWindow);
    procedure Store(const Instance: IOuchWindow);
    procedure Save(const FileName: string);
  end;

  IOuchProfileReader = interface
    ['{8A7A8DDD-1E73-492B-A00A-D088F95EE27C}']
    function ReadProperty(Instance: TObject; const Prop: string): Boolean; overload;
  end;

  IOuchProfileWriter = interface
    ['{2B1F61A5-5B14-49DD-9353-BB3EB491A932}']
    procedure WriteProperty(Instance: TObject; const Prop: string); overload;
  end;

  IOuchStreamable = interface
    ['{6B4B9F68-CB38-422C-8502-06EA68B22784}']
    procedure Load(const Reader: IOuchProfileReader);
    procedure Store(const Writer: IOuchProfileWriter);
  end;

  IOuchDataset = interface
    ['{DCAAF4D9-46E6-40A6-B7A6-EC12D57D78C0}']
    function GetDataSet: TDataSet;
    property DataSet: TDataSet read GetDataSet;
  end;
  
  IOuchDbHistory = interface (IOuchDataset)
    ['{A352C323-C606-4593-9323-B3972930D161}']
  end;
  
implementation
end.
