unit UiOuchProtocol;

interface

uses
  Sil,
  SilCoder;

{
  secuencia:
    c -> s via IOuchRequest
    s <- c via IOuchRequest

    s -> c via IOuchReply
    c <- s via IOuchReply


  User
  Name
  Value
  Visibility
}

type
  ROuchGroup = record
    Parent: TGuid;
    Id: TGuid;
    Name: String;
  end;

  TOuchGroupArray = array of ROuchGroup;

  ROuchUser = record
    User: TGuid;
    Data: IParameters;
  end;

  TOuchUserArray = array of ROuchUser;

  TOuchUserStatus = (usUnknown, usAsk, usValidate, usShutdown, usOffline, usOnline, usBusy);
  TOuchMessageKind = (mkInstantMessage, mkClipboard);
  TOuchMessageStatus = (msReceived, msPending, msOffline);

  ROuchReply = record
    ID: TGuid;
    Result: Integer;
    Comment: String;
  end;

  ROuchDefineUserData = record
    IdUser: TGuid;
  end;

  ROuchDefineUserReply = record
    Reply: ROuchReply;
    DefineUser: ROuchDefineUserData;
  end;

  ROuchConnectData = record
    IdUser: TGuid;
    IdSession: TGuid;
    ExpirationTime: TDateTime;
  end;

  ROuchConnectReply = record
    Reply: ROuchReply;
    Connect: ROuchConnectData;
  end;

  ROuchQueryOfflinesData = record
  end;

  ROuchQueryOfflinesReply = record
    Reply: ROuchReply;
    QueryOfflines: ROuchQueryOfflinesData;
  end;

  ROuchNotifyStatusData = record
    ExpirationTime: TDateTime;
  end;

  ROuchNotifyStatusReply = record
    Reply: ROuchReply;
    NotifyStatus: ROuchNotifyStatusData;
  end;

  ROuchQueryGroupsData = record
    Groups: TOuchGroupArray;
  end;

  ROuchQueryGroupsReply = record
    Reply: ROuchReply;
    QueryGroups: ROuchQueryGroupsData;
  end;

  ROuchGroupLogonData = record
    Users: TOuchUserArray;
  end;

  ROuchGroupLogonReply = record
    Reply: ROuchReply;
    GroupLogon: ROuchGroupLogonData;
  end;

  ROuchSendMessageData = record
    Status: TOuchMessageStatus;
  end;

  ROuchSendMessageReply = record
    Reply: ROuchReply;
    SendMessage: ROuchSendMessageData;
  end;

  IOuchProtocol = interface
    ['{6CCF150A-B950-4F11-9BC3-CFEBC8E8EA32}']
    function GetCipher: ICipher;
    procedure SetCipher(const Value: ICipher);
    property Cipher: ICipher read GetCipher write SetCipher;
  end;

  IOuchRequestPacker = interface
    ['{EF3A74E4-5A7F-4AE1-B298-8328962A8AFF}']
    function DefineUser(const RequestId, User: TGuid; const Password: String; const Data: IParameters = nil): IProtocolPacket;
    function Connect(const User: TGuid; const Password: string): IProtocolPacket;
    function QueryOfflines(const RequestId, Session: TGuid): IProtocolPacket;
    function NotifyStatus(const RequestId, Session: TGuid; UserStatus: TOuchUserStatus): IProtocolPacket;
    function QueryGroups(const RequestId, Session: TGuid): IProtocolPacket;
    function GroupLogon(const RequestId, Session: TGuid; const Group: TGuid; const Password: String): IProtocolPacket;
    function SendMessage(const RequestId, Session, User: TGuid; const Text: String; Priority: Word; Kind: TOuchMessageKind; const Time: TDateTime; const ReplyTo: TGuidArray): IProtocolPacket;
    function DefineUserReply(const Data: ROuchDefineUserReply): IProtocolPacket;
    function ConnectReply(const Data: ROuchConnectReply): IProtocolPacket;
    function QueryOfflinesReply(const Data: ROuchQueryOfflinesReply): IProtocolPacket;
    function NotifyStatusReply(const Data: ROuchNotifyStatusReply): IProtocolPacket;
    function QueryGroupsReply(const Data: ROuchQueryGroupsReply): IProtocolPacket;
    function GroupLogonReply(const Data: ROuchGroupLogonReply): IProtocolPacket;
    function SendMessageReply(const Data: ROuchSendMessageReply): IProtocolPacket;
  end;

  IOuchRequest = interface
    ['{10105B33-A914-406B-8097-93D903504BE9}']
    procedure DefineUser(const RequestId, User: TGuid; const Password: String; const Data: IParameters = nil);
    procedure Connect(const User: TGuid; const Password: string);
    procedure QueryOfflines(const RequestId, Session: TGuid);
    procedure NotifyStatus(const RequestId, Session: TGuid; UserStatus: TOuchUserStatus);
    procedure QueryGroups(const RequestId, Session: TGuid);
    procedure GroupLogon(const RequestId, Session: TGuid; const Group: TGuid; const Password: String);
    procedure SendMessage(const RequestId, Session, User: TGuid; const Text: String; Priority: Word; Kind: TOuchMessageKind; const Time: TDateTime; const ReplyTo: TGuidArray);
  end;

  IOuchReply = interface
    ['{382F2B3E-9D25-45AB-ACC9-A63999AA6B12}']
    procedure DefineUserReply(const Data: ROuchDefineUserReply);
    procedure ConnectReply(const Data: ROuchConnectReply);
    procedure QueryOfflinesReply(const Data: ROuchQueryOfflinesReply);
    procedure NotifyStatusReply(const Data: ROuchNotifyStatusReply);
    procedure QueryGroupsReply(const Data: ROuchQueryGroupsReply);
    procedure GroupLogonReply(const Data: ROuchGroupLogonReply);
    procedure SendMessageReply(const Data: ROuchSendMessageReply);
  end;

implementation
end.
