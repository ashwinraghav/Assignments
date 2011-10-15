unit UmOuchContact;

interface

uses
  Sil,
  UiProtocol,
  UiOuch,
  UmOuchAccount;

type
  TOuchContact = class(
    TOuchAccount,
    IOuchContact )
  private
    FList: Pointer;
  private
    function DoGetList: IOuchAccountList;
    procedure DoSetInfo(const Value: IParameters);
  protected // IOuchContact
    procedure SetState(Value: TUserStatus); override; 
    procedure SetInfo(const Value: IParameters);
  protected  
    property List: IOuchAccountList read DoGetList;
  public
    constructor Create(const List: IOuchAccountList; const ID: TGUID; const Info: IParameters = nil);
    destructor Destroy; override;
  end;

implementation

uses
  UcOuch;

{ TOuchContact }

constructor TOuchContact.Create(const List: IOuchAccountList; const ID: TGUID; const Info: IParameters = nil);
begin
  inherited Create(ID, Info);
  FList := Pointer(List);
  DoSetInfo(Info);
  List.Add(Self)
end;

destructor TOuchContact.Destroy;
begin
  inherited;
end;

procedure TOuchContact.SetInfo(const Value: IParameters);
begin
  DoSetInfo(Value);
  List.Changed(Self);
end;

procedure TOuchContact.SetState(Value: TUserStatus);
begin
  inherited;
  List.Changed(Self);
end;

function TOuchContact.DoGetList: IOuchAccountList;
begin
  Result := IOuchAccountList(FList);
end;

procedure TOuchContact.DoSetInfo(const Value: IParameters);
begin
  if Assigned(Value) then
  begin
    Self.Info.Merge(Value);
    Self.State := Self.Info.Get(CUserState, Self.State);
  end;
end;

end.
