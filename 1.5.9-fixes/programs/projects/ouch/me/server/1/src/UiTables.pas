unit UiTables;

interface

uses
  Sil,
  SilData;

type
  ITable = interface
    ['{51C19685-9F7C-49A9-BE53-7703030CF054}']
    function GetRowset: IDataRowset;
    function Open: Boolean;
    property Rowset: IDataRowset read GetRowset;
  end;

type
  TUserIndex = (uiUser, uiNick, uiMail);

const
  CUserTable = 'user.d';
  AUserIndex: array[TUserIndex] of String = ('ixUser', 'ixNick', 'ixMail');

type
  IUserTable = interface (ITable)
    ['{3DE9F1FB-1100-4BB8-9F02-6E69AF18E515}']
    function GetUser: TGuid;
    procedure SetUser(const Value: TGuid);
    function GetNick: String;
    procedure SetNick(const Value: String);
    function GetMail: String;
    procedure SetMail(const Value: String);
    function GetPassword: String;
    procedure SetPassword(const Value: String);
    procedure ActivateIndex(const Index: TUserIndex);
    property User: TGuid read GetUser write SetUser;
    property Nick: String read GetNick write SetNick;
    property Mail: String read GetMail write SetMail;
    property Password: String read GetPassword write SetPassword;
  end;

type
  TOfflineIndex = (oiOfflineId, oiOfflineIdTo, oiOfflineTo, oiOfflineTimeout);

const
  COfflineTable = 'offline.d';
  AOfflineIndex: array[TOfflineIndex] of String = ('ixId', 'ixIdTo', 'ixTo', 'ixTimeout');

type
  IOfflineTable = interface (ITable)
    ['{FDDAAEA0-C1F5-4B25-B606-126F0594CEE3}']
    function GetId: TGuid;
    procedure SetId(const Value: TGuid);
    function GetToUser: TGuid;
    procedure SetToUser(const Value: TGuid);
    function GetFromUser: TGuid;
    procedure SetFromUser(const Value: TGuid);
    function GetTime: TDateTime;
    procedure SetTime(const Value: TDateTime);
    function GetKind: Byte;
    procedure SetKind(const Value: Byte);
    function GetData: String;
    procedure SetData(const Value: String);
    function GetTimeout: TDateTime;
    procedure SetTimeout(const Value: TDateTime);
    property Id: TGuid read GetId write SetId;
    property ToUser: TGuid read GetToUser write SetToUser;
    property FromUser: TGuid read GetFromUser write SetFromUser;
    property Time: TDateTime read GetTime write SetTime;
    property Kind: Byte read GetKind write SetKind;
    property Data: String read GetData write SetData;
    property Timeout: TDateTime read GetTimeout write SetTimeout;
  end;

implementation

end.
 