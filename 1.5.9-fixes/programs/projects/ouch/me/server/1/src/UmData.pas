unit UmData;

interface

uses
  Sil,
  SilData,

  UiData;

type
  TDataMgr = class(
    // extends
    TInterfacedObject,
    // implements
    IDataMgr)
  private
    FPath: String;
    FUser: IDataRowset;
    FData: IDataRowset;
    FContact: IDataRowset;
    FOffline: IDataRowset;
    FGroup: IDataRowset;
    FGroupUser: IDataRowset;
    FSession: IDataRowset;
    FValidateTime: TDateTime;
  protected // IDataMgr
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
  public
    constructor Create;
    destructor Destroy; override;
  end;
         
implementation

uses
  UtData;

{ TDataMgr }

constructor TDataMgr.Create;
begin
  inherited Create;

  FPath := Sil.OS.Process.Current.Info.Path;
  FValidateTime := Time.EncodeParts(0, 5, 0, 0);
  Db.OpenTables(Self);
end;

destructor TDataMgr.Destroy;
begin
  FUser := nil;
  FData := nil;
  FContact := nil;
  FOffline := nil;
  FGroup := nil;
  FGroupUser := nil;
  FSession := nil;
  
  inherited;
end;

function TDataMgr.GetContact: IDataRowset;
begin
  Result := FContact;
end;

function TDataMgr.GetData: IDataRowset;
begin
  Result := FData;
end;

function TDataMgr.GetGroup: IDataRowset;
begin
  Result := FGroup;
end;

function TDataMgr.GetGroupUser: IDataRowset;
begin
  Result := FGroupUser;
end;

function TDataMgr.GetOffline: IDataRowset;
begin
  Result := FOffline;
end;

function TDataMgr.GetPath: String;
begin
  Result := FPath;
end;

function TDataMgr.GetSession: IDataRowset;
begin
  Result := FSession;
end;

function TDataMgr.GetUser: IDataRowset;
begin
  Result := FUser;
end;

function TDataMgr.GetValidateTime: TDateTime;
begin
  Result := FValidateTime;
end;

procedure TDataMgr.SetContact(const Value: IDataRowset);
begin
  FContact := Value;
end;

procedure TDataMgr.SetData(const Value: IDataRowset);
begin
  FData := Value;
end;

procedure TDataMgr.SetGroup(const Value: IDataRowset);
begin
  FGroup := Value;
end;

procedure TDataMgr.SetGroupUser(const Value: IDataRowset);
begin
  FGroupUser := Value;
end;

procedure TDataMgr.SetOffline(const Value: IDataRowset);
begin
  FOffline := Value;
end;

procedure TDataMgr.SetSession(const Value: IDataRowset);
begin
  FSession := Value;
end;

procedure TDataMgr.SetUser(const Value: IDataRowset);
begin
  FUser := Value;
end;

end.
 