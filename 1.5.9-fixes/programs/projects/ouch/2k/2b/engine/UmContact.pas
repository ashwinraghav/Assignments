unit UmContact;

interface

uses
  Sil,
  SilClasses,

  UiClient;

type
  IContact = interface;
  IContactList = interface;

  IContactList = interface
    ['{87FCC6DA-571C-419A-8809-3445C15FAC3D}']
    procedure Add(const Contact: IContact);
    function CreateContact(const Id: TGuid; const Nick: String): IContact;
    procedure Remove(const Contact: IContact);
    procedure Clear;
    function Enumerate(var Enum: IEnumerator; out Contact: IContact): Boolean;
  end;

  IContact = interface
    ['{17DD373F-6337-4B25-897C-6CB8BDA3CB52}']
    function GetId: TGuid;
    function GetNick: String;
    property Id: TGuid read GetId;
    property Nick: String read GetNick;
  end;

  TContact = class (
    TSilObject,
    IContact)
  private
    FId: TGuid;
    FNick: String;
  protected // IContact
    function GetId: TGuid;
    function GetNick: String;
    property Id: TGuid read GetId;
    property Nick: String read GetNick;
  public
    constructor Create(const Id: TGuid; const Nick: String);
  end;

  TContactList = class (
    TSilInterfaceList,
    IContactList)
  protected
    procedure Add(const Contact: IContact); reintroduce;
    function CreateContact(const Id: TGuid; const Nick: String): IContact;
    procedure Remove(const Contact: IContact); reintroduce;
    procedure Clear; reintroduce;
    function Enumerate(var Enum: IEnumerator; out Contact: IContact): Boolean; reintroduce;
  public
    constructor Create;
  end;

implementation

{ TContact }

constructor TContact.Create(const Id: TGuid; const Nick: String);
begin
  inherited Create;

  FId := Id;
  FNick := Nick;
end;

function TContact.GetId: TGuid;
begin
  Result := FId;
end;

function TContact.GetNick: String;
begin
  Result := FNick;
end;

{ TContactList }

constructor TContactList.Create;
begin
  inherited Create(true);
end;

procedure TContactList.Add(const Contact: IContact);
begin
  inherited Add(Contact);
end;

function TContactList.Enumerate(var Enum: IEnumerator; out Contact: IContact): Boolean;
begin
  Result := inherited Enumerate(Enum, Contact);
end;

procedure TContactList.Remove(const Contact: IContact);
begin
  inherited Remove(Contact);
end;

procedure TContactList.Clear;
begin
  inherited Clear;
end;

function TContactList.CreateContact(const Id: TGuid; const Nick: String): IContact;
begin
  Result := TContact.Create(Id, Nick);
  Add(Result);
end;

end.
 