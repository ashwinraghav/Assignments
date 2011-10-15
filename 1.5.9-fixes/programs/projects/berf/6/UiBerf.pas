unit UiBerf;

interface

uses
  Sil;

const
  TM_RUNCLIENT = $400;
  TM_RUNSERVER = $401;

type
  IAuthInfo = interface
    ['{3E790D80-05E0-4842-ACAF-4C5E89E33867}']
    function GetName: String;
    function GetMethod: String;
    function GetPort: Word;
    function GetDomain: String;
    function GetPassword: String;
    function GetUser: String;
    procedure SetPort(Value: Word);
    procedure SetDomain(const Value: String);
    procedure SetPassword(const Value: String);
    procedure SetUser(const Value: String);
    procedure SetMethod(const Value: String);
    property Name: String read GetName;
    property Port: Word read GetPort write SetPort;
    property Domain: String read GetDomain write SetDomain;
    property Password: String read GetPassword write SetPassword;
    property User: String read GetUser write SetUser;
    property Method: String read GetMethod write SetMethod;
  end;

  IAclInfo = interface
    ['{7006C065-E8A4-4042-8887-27DAA75F230F}']
    function GetName: String;
    function GetAllowNames: IStringList;
    function GetDenyNames: IStringList;
    function GetIsDefaultAllow: Boolean;
    function IsValid(const Address: ISocketAddress): Boolean;
    procedure ResolveNames;
    procedure SetIsDefaultAllow(Value: Boolean);
    property Name: String read GetName;
    property AllowNames: IStringList read GetAllowNames;
    property DenyNames: IStringList read GetDenyNames;
    property IsDefaultAllow: Boolean read GetIsDefaultAllow write SetIsDefaultAllow;
  end;

  IServer = interface
    ['{C5B54E9A-01C1-11D5-98B5-00104B0FA1EF}']
    function GetLocal: String;
    function GetRemote: String;
    function GetAuth: IAuthInfo;
    function GetAcl: IAclInfo;
    procedure SetLocal(const Value: String);
    procedure SetRemote(const Value: String);
    procedure SetAuth(const Value: IAuthInfo);
    procedure SetAcl(const Value: IAclInfo);
    property Local: String read GetLocal write SetLocal;
    property Remote: String read GetRemote write SetRemote;
    property Auth: IAuthInfo read GetAuth write SetAuth;
    property Acl: IAclInfo read GetAcl write SetAcl;
    procedure Start;
    procedure Stop;
  end;

  IBerfList = interface
    ['{EF0EC2E4-2E3B-4FB5-BB0A-6ED417AE5971}']
    function Configure(const FileName: String): Boolean;
    function Start: Boolean;
    procedure Stop;
  end;

  IClient = interface
    ['{16A30887-8B36-43D1-93F9-BD9C5A3AD093}']
    function GetIsFinished: Boolean;
    procedure Start;
    procedure Stop;
    property IsFinished: Boolean read GetIsFinished;
  end;

implementation

end.
 