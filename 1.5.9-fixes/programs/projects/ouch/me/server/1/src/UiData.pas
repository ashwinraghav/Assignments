unit UiData;

interface

uses
  Sil,
  SilData;

type
  TOfflineKind = (okMessage, okMessageReply, okUserStatus, okUserInfo);

  ROfflineInfo = record
    Id: TGuid;
    FromUser: TGuid;
    ToUser: TGuid;
    Text: String;
    Time: TDateTime;
    Kind: TOfflineKind;
  end;

  TOfflineInfoArray = array of ROfflineInfo;

  IDataMgr = interface
    ['{82E4E188-3168-40C9-9D50-C260B627882A}']
    function GetPath: String;
    function GetUser: IDataRowset;
    procedure SetUser(const Value: IDataRowset);
    function GetData: IDataRowset;
    procedure SetData(const Value: IDataRowset);
    function GetContact: IDataRowset;
    procedure SetContact(const Value: IDataRowset);
    function GetOffline: IDataRowset;
    procedure SetOffline(const Value: IDataRowset);
    function GetGroup: IDataRowset;
    procedure SetGroup(const Value: IDataRowset);
    function GetGroupUser: IDataRowset;
    procedure SetGroupUser(const Value: IDataRowset);
    function GetSession: IDataRowset;
    procedure SetSession(const Value: IDataRowset);
    function GetValidateTime: TDateTime;
    property Path: String read GetPath;
    property User: IDataRowset read GetUser write SetUser;
    property Data: IDataRowset read GetData write SetData;
    property Contact: IDataRowset read GetContact write SetContact;
    property Offline: IDataRowset read GetOffline write SetOffline;
    property Group: IDataRowset read GetGroup write SetGroup;
    property GroupUser: IDataRowset read GetGroupUser write SetGroupUser;
    property Session: IDataRowset read GetSession write SetSession;
    property ValidateTime: TDateTime read GetValidateTime;
  end;


implementation

end.
