unit UmEngine;

interface

uses
  Sil,
  UiOuchProtocol,
  UiOuchEngine,
  UiEngine;

type
  TOuchEngine = class(
    TInterfacedObject,
    IOuchEngine,
    IOuchToolkit )
  private

  protected // IOuchEngine
    function GetToolkit: IOuchToolkit;
    procedure Connect(const Parameters: IParameters; const Listener: IUnknown);

  protected // IOuchToolkit

    function NewMessage(
                 const Timestamp: TDateTime;
                 const MessageID: TGUID;
                 const Recipients: IOuchUsers;
                 const Senders: IOuchUsers;
                 const Text: string;
                 const Priority: Word = 0;
                 const Kind: TOuchMessageKind = mkInstantMessage
               ): IOuchMessage;

    function NewUserList(
                const Users: IOuchUsers = nil
              ): IOuchUserList; overload;

    function NewUserList(
                const Users: TOuchUserArray
              ): IOuchUserList; overload;

    function NewGroupList(
                const Groups: IOuchGroups = nil
              ): IOuchGroupList; overload;

    function NewGroupList(
                const Groups: TOuchGroupArray
              ): IOuchGroupList; overload;

    function NewUser(
                const UserID: TGUID;
                const Data: IParameters
              ): IOuchUserDef; overload;

    function NewUser(
                const Rec: ROuchUser
              ): IOuchUserDef; overload;

    function NewLocalUser(
                const UserID: TGUID;
                const Password: string;
                const Data: IParameters
              ): IOuchLocalUser; overload;

    function NewLocalUser(
                const Rec: ROuchUser;
                const Password: string
              ): IOuchLocalUser; overload;

    function NewGroup(
                const GroupID: TGUID;
                const Parent: TGUID;
                const Name: string
              ): IOuchGroupDef; overload;

    function NewGroup(
                const Rec: ROuchGroup
              ): IOuchGroupDef; overload;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  UmEngineConnection,
  UmEngineMessage,
  UmEngineUserData,
  UmEngineUserList,
  UmEngineGroupData,
  UmEngineGroupList;

{ TOuchEngine }

constructor TOuchEngine.Create;
begin
  inherited Create;
end;

destructor TOuchEngine.Destroy;
begin
  inherited;
end;

procedure TOuchEngine.Connect(const Parameters: IParameters; const Listener: IUnknown);
var
  Connection: IEngineConnection;
begin
  Connection := TOuchEngineConnection.Create(Self, Parameters);
  Sil.Sink.Connect(Connection, Listener);
  Connection.Activate(); // Este se queda con una ref hasta que se resuelva si se pudo conectar o falle
end;

function TOuchEngine.GetToolkit: IOuchToolkit;
begin
  Result := Self;
end;

function TOuchEngine.NewMessage(
  const Timestamp: TDateTime;
  const MessageID: TGUID;
  const Recipients, Senders: IOuchUsers;
  const Text: string;
  const Priority: Word;
  const Kind: TOuchMessageKind): IOuchMessage;
begin
  Result := TEngineMessage.Create(Self, Timestamp, MessageID, Recipients, Senders, Text, Priority, Kind);
end;

function TOuchEngine.NewGroupList(const Groups: IOuchGroups): IOuchGroupList;
begin
  Result := TEngineGroupList.Create(Groups);
end;

function TOuchEngine.NewGroupList(const Groups: TOuchGroupArray): IOuchGroupList;
var
  Index: Integer;
begin
  Result := NewGroupList();
  for Index := Low(Groups) to High(Groups) do
    Result.Add(NewGroup(Groups[Index]));
end;

function TOuchEngine.NewUserList(const Users: IOuchUsers): IOuchUserList;
begin
  Result := TEngineUserList.Create(Users);
end;

function TOuchEngine.NewUserList(const Users: TOuchUserArray): IOuchUserList;
var
  Index: Integer;
begin
  Result := NewUserList();
  for Index := Low(Users) to High(Users) do
    Result.Add(NewUser(Users[Index]));
end;

function TOuchEngine.NewGroup(const Rec: ROuchGroup): IOuchGroupDef;
begin
  Result := NewGroup(Rec.Id, Rec.Parent, Rec.Name);
end;

function TOuchEngine.NewGroup(const GroupID: TGUID; const Parent: TGUID; const Name: string): IOuchGroupDef;
begin
  Result := TOuchEngineGroupData.Create(GroupID, Parent, Name);
end;

function TOuchEngine.NewUser(const UserID: TGUID; const Data: IParameters): IOuchUserDef;
begin
  Result := TOuchEngineUserData.Create(UserID, Data);
end;

function TOuchEngine.NewUser(const Rec: ROuchUser): IOuchUserDef;
begin
  Result := Self.NewUser(Rec.User, Rec.Data);
end;

function TOuchEngine.NewLocalUser(const UserID: TGUID; const Password: string; const Data: IParameters): IOuchLocalUser;
begin
  Result := TOuchEngineLocalUser.Create(UserID, Password, Data);
end;

function TOuchEngine.NewLocalUser(const Rec: ROuchUser; const Password: string): IOuchLocalUser;
begin
  Result := Self.NewLocalUser(Rec.User, Password, Rec.Data);
end;

end.
