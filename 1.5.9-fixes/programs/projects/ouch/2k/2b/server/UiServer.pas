unit UiServer;

interface

uses
  Sil,
  UiProtocol,
  UiClient;

type
  IData = interface
    ['{A1A2C324-7ECB-4A55-88CA-CBC0D9178631}']
    function GetPath: String;
    procedure SetupAccount(const Nick, Password: String; var UserId: TGuid; CreateAccount: Boolean);
    procedure LogonAccount(const UserId: TGuid; const Password: String; IPAddress: LongWord);
    procedure LogoffAccount(const UserId: TGuid);
    procedure QueryInfo(const UserId: TGuid; out Data: IParameters);
    procedure QueryUsers(const UserId: TGuid; Status: TUserStatus; out Users: TUserArray);
    procedure StoreMessage(const MsgId: TGUID; const UserId, FromId: TGuid; const Recipients: TGuidArray; const Text: String; const Time: TDateTime; Bc: Boolean);
    procedure StoreResponse(const MsgId: TGUID; const FromId, ToId: TGuid; const Time: TDateTime);
    function GetMessage(const Id: TGuid; out Data: IParameters): Boolean;
    procedure DeleteOffline(Id: LongWord);
    procedure Initialize;
    procedure Finalize;
    property Path: String read GetPath;
  end;

  IServer = interface
    ['{F0BC4FAB-E531-43CC-8304-80F28B5A18A0}']
    function GetOnlines: IClients;
    procedure Start;
    procedure Stop;
    procedure SendMessage(const MsgId: TGUID; const UserId: TGuid; const Recipients: TGuidArray; const Text: String; const Time: TDateTime; Bc: Boolean);
    procedure SendResponse(const MsgId: TGUID; const FromId, ToId: TGuid; const Time: TDateTime);
    procedure SendInfo(const UserId: TGuid; const Recipients: TGuidArray; const Data: IParameters);
    function Data: IData;
    property Onlines: IClients read GetOnlines;
  end;

implementation

end.
 