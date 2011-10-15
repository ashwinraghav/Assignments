unit UmEngineUserData;

interface

uses
  Sil,
  UiOuchEngine;

type
  TOuchEngineUserData = class(
    TInterfacedObject,
    IConnectable,
    IParameterListEvents,
    IOuchUserData,
    IOuchUserDef )
  private
    FUserID: TGUID;
    FData: IParameters;
    FPtr: Pointer;
    FEvents: IEventList;
    FLocks: Integer;
    FModified: Boolean;
  protected // IConnectable
    procedure AddListener(const Listener: IUnknown);
    procedure RemoveListener(const Listener: IUnknown);
  protected // IParameterListEvents
    procedure OnItemAdded(const Event: REvParametersItemEvent);
    procedure OnItemChanged(const Event: REvParametersItemEvent);
  protected // IOuchUserData
    function GetID: TGUID;
    function GetData: IParameters;
    function GetPtr: Pointer;
    procedure SetPtr(const Value: Pointer);
  protected // IOuchUserDef
    procedure SetID(const Value: TGUID);
    function GetList: IParameterList;
  protected
    procedure DoFireChanged;
    procedure DoChanged;
    procedure DoModified;
    procedure DoReset;
    property IsModified: Boolean read FModified;
  public
    constructor Create(const UserID: TGUID; const Data: IParameters);
    destructor Destroy; override;
    function BeginUpdate: ILock;
    function EndUpdate: ILock;
  end;

  TOuchEngineLocalUser = class(
    TOuchEngineUserData,
    IOuchLocalUser )
  private
    FPassword: string;
  protected // IOuchLocalUser
    function GetPassword: string;
    procedure SetPassword(const Value: string);
    function Edit: Boolean;
    procedure Apply;
    procedure Cancel;
  public
    constructor Create(const UserID: TGUID; const Password: string; const Data: IParameters);
  end;

implementation

{ TOuchEngineUserData }

constructor TOuchEngineUserData.Create(const UserID: TGUID; const Data: IParameters);
begin
  inherited Create;
  FUserID := UserID;
  FData := Data;
  if FData = nil then
    FData := Sil.List.Parameters(True);
  Sil.Sink.Connect(FData, Self);
end;

destructor TOuchEngineUserData.Destroy;
begin
  Sil.Sink.Disconnect(FData, Self);
  FData := nil;
  inherited;
end;

procedure TOuchEngineUserData.AddListener(const Listener: IInterface);
begin
  Sil.Sv.EventCaster.Add(FEvents, Listener);
end;

procedure TOuchEngineUserData.RemoveListener(const Listener: IInterface);
begin
  Sil.Sv.EventCaster.Remove(FEvents, Listener);
end;

function TOuchEngineUserData.GetID: TGUID;
begin
  Result := FUserID;
end;

function TOuchEngineUserData.GetPtr: Pointer;
begin
  Result := FPtr;
end;

procedure TOuchEngineUserData.SetPtr(const Value: Pointer);
begin
  FPtr := Value;
end;

procedure TOuchEngineUserData.SetID(const Value: TGUID);
begin
  FUserID := Value;
  DoChanged();
end;

procedure TOuchEngineUserData.DoChanged;
begin
  DoModified();
  if FLocks = 0 then
    DoFireChanged();
end;

procedure TOuchEngineUserData.DoFireChanged;
var
  Enum: IEnumerator;
  Item: IOuchUserDataEvents;
  Event: ROuchUserDataEvent;
begin
  Event.Sender := Self;
  Event.Thread := Sil.Os.Thread.Current;
  if FEvents <> nil then
    while FEvents.Enumerate(Enum, Item, IOuchUserDataEvents) do
      Item.OnUserDataChanged(Event);
end;

function TOuchEngineUserData.BeginUpdate: ILock;
begin
  Result := Sil.Lock.Take(Lockable);
  Inc(FLocks);
end;

function TOuchEngineUserData.EndUpdate: ILock;
begin
  Result := Sil.Lock.Take(Lockable);
  Dec(FLocks);
end;

procedure TOuchEngineUserData.DoModified;
begin
  FModified := True;
end;

procedure TOuchEngineUserData.DoReset;
begin
  FModified := False;
end;

function TOuchEngineUserData.GetData: IParameters;
begin
  Result := FData;
end;

function TOuchEngineUserData.GetList: IParameterList;
begin
  Result := FData as IParameterList;
end;

procedure TOuchEngineUserData.OnItemAdded(const Event: REvParametersItemEvent);
begin
  DoChanged;
end;

procedure TOuchEngineUserData.OnItemChanged(const Event: REvParametersItemEvent);
begin
  DoChanged;
end;

{ TOuchEngineLocalUser }

constructor TOuchEngineLocalUser.Create(const UserID: TGUID; const Password: string; const Data: IParameters);
begin
  inherited Create(UserID, Data);
  FPassword := Password;
end;

function TOuchEngineLocalUser.Edit: Boolean;
begin
  BeginUpdate();
  Result := True;
end;

procedure TOuchEngineLocalUser.Apply;
begin
  EndUpdate();
  if (FLocks = 0) and IsModified then
  begin
    DoFireChanged();
    DoReset();
  end;
end;

procedure TOuchEngineLocalUser.Cancel;
begin
  DoReset();
  EndUpdate();
end;

function TOuchEngineLocalUser.GetPassword: string;
begin
  Result := FPassword;
end;

procedure TOuchEngineLocalUser.SetPassword(const Value: string);
begin
  FPassword := Value;
  DoChanged;
end;

end.
