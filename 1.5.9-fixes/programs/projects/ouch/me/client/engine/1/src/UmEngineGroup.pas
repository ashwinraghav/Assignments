unit UmEngineGroup;

interface

uses
  Sil,
  UiOuchProtocol,
  UiOuchEngine,
  UiEngine;

type
  RGroup = record
    Data: IOuchGroupData;
    Password: string;
  end;

  TEngineGroup = class(
    TInterfacedObject,
    IOuchGroup,
    IEngineGroup )
  private
    FSession: IEngineSession;
    FGroup: RGroup;
    FUsers: IOuchUsers;
  protected // IOuchGroup
    function GetData: IOuchGroupData;
    function GetUsers: IOuchUsers;
    function Talk(const User: TGUID): IOuchUser;
  protected // IEngineGroup
    function GetOuch: IOuchGroup;
    function GetSession: IEngineSession;
    function SetUsers(const Users: IOuchUsers): IEngineGroup;
  public
    constructor Create(const Session: IEngineSession; const Group: IOuchGroupData; const Password: string);
    destructor Destroy; override;
  end;

implementation

{ TEngineGroup }

constructor TEngineGroup.Create(const Session: IEngineSession; const Group: IOuchGroupData; const Password: string);
begin
  inherited Create;
  FSession := Session;
  FGroup.Data := Group;
  FGroup.Password := Password;
end;

destructor TEngineGroup.Destroy;
begin
  FGroup.Data := nil;
  FSession := nil;
  inherited;
end;

function TEngineGroup.GetData: IOuchGroupData;
begin
  Result := FGroup.Data;
end;

function TEngineGroup.GetUsers: IOuchUsers;
begin
  Result := FUsers;
end;

function TEngineGroup.Talk(const User: TGUID): IOuchUser;
begin

end;

function TEngineGroup.GetOuch: IOuchGroup;
begin
  Result := Self;
end;

function TEngineGroup.GetSession: IEngineSession;
begin
  Result := FSession;
end;

function TEngineGroup.SetUsers(const Users: IOuchUsers): IEngineGroup;
begin
  FUsers := Users;
  Result := Self;
end;

end.
