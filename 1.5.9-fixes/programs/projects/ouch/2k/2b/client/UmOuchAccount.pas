unit UmOuchAccount;

interface

uses
  Sil,
  UiProtocol,
  UiOuch;

type
  TOuchAccount = class(
    TSilObject,
    IOuchAccount )
  private  
    FID: TGUID;
    FInfo: IParameterList;
    FState: TUserStatus;
    FData: Pointer;
  protected // IOuchContact
    function GetID: TGuid;
    function GetNick: String;
    function GetMail: String;
    function GetState: TUserStatus;
    procedure SetState(Value: TUserStatus); virtual; 
    function GetInfo: IParameters;
    function GetData: Pointer;
    procedure SetData(Value: Pointer);
  protected  
    property ID: TGuid read FID write FID;
    property Info: IParameterList read FInfo write FInfo;
    property Data: Pointer read FData write FData;
    property State: TUserStatus read FState write FState;
  public
    constructor Create(const ID: TGUID; const Info: IParameters = nil); overload;
    destructor Destroy; override;
  end;

  TOuchLocalAccount = class(
    TOuchAccount,
    IOuchLocalAccount )
  protected // IOuchLocalAccount
  
  end;

  
implementation

uses
  UcOuch;

{ TOuchAccount }

constructor TOuchAccount.Create(const ID: TGUID; const Info: IParameters);
begin
  inherited Create;
  FID := ID;
  FInfo := Sil.List.Parameters(True);
  FState := usOffline;
end;

destructor TOuchAccount.Destroy;
begin
  FInfo := Info;
  inherited;
end;

function TOuchAccount.GetID: TGuid;
begin
  Result := FID;
end;

function TOuchAccount.GetInfo: IParameters;
begin
  Result := FInfo;
end;

function TOuchAccount.GetMail: String;
begin
  Result := FInfo[CUserMail];
end;

function TOuchAccount.GetNick: String;
begin
  Result := FInfo[CUserNick];
end;

function TOuchAccount.GetState: TUserStatus;
begin
  Result := FState;
end;

procedure TOuchAccount.SetState(Value: TUserStatus);
begin
  FState := Value;
end;

function TOuchAccount.GetData: Pointer;
begin
  Result := FData;
end;

procedure TOuchAccount.SetData(Value: Pointer);
begin
  FData := Value;
end;

end.
