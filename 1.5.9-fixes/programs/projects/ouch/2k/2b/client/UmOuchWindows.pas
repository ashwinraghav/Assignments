unit UmOuchWindows;

interface

uses
  Sil, SilClasses, Classes,
  UiOuch;

type
  TOuchWindowList = class(
    TSilInterfaceList,
    IOuchWindows,
    IOuchWindowList )
  private
    FApplication: IOuchApplication;
  protected // IOuchWindows
    function IOuchWindows.Create = CreateWindow;
    function IOuchWindowList.Create = CreateWindow;
    function Find(const IID: TGUID; out Item): Boolean;
    function Enumerate(var Enum: IEnumerator; out Item: IOuchWindow): Boolean; reintroduce; overload;
    function Enumerate(var Enum: IEnumerator; const IID: TGUID; out Item): Boolean; reintroduce; overload;
    function CreateWindow(WindowClass: TComponentClass; const IID: TGUID; out Instance; Lookup: Boolean): Boolean;
    procedure Clear; override;
  protected // IOuchWindowList
    function Add(const Item: IOuchWindow): Integer; reintroduce;
    function Remove(const Item: IOuchWindow): Integer; reintroduce;
  public
    constructor Create(const Application: IOuchApplication); overload;
    destructor Destroy; override;
  end;

implementation

uses
  Forms, FkOuchBase;

{ TOuchWindowList }

constructor TOuchWindowList.Create(const Application: IOuchApplication);
begin
  inherited Create;
  FApplication := Application;
end;

destructor TOuchWindowList.Destroy;
begin
  FApplication := nil;
  inherited;
end;

function TOuchWindowList.Find(const IID: TGUID; out Item): Boolean;
var
  Enum: IEnumerator;
begin
  Result := Enumerate(Enum, IID, Item);
end;

function TOuchWindowList.Enumerate(var Enum: IEnumerator; out Item: IOuchWindow): Boolean;
begin
  Result := Enumerate(Enum, IOuchWindow, Item);
end;

function TOuchWindowList.Enumerate(var Enum: IEnumerator; const IID: TGUID; out Item): Boolean;
var
  Unknown: IUnknown;
begin
  repeat
    Result := inherited Enumerate(Enum, Unknown);
  until not Result or (Unknown.QueryInterface(IID, Item) = 0);
end;

function TOuchWindowList.CreateWindow(WindowClass: TComponentClass; const IID: TGUID; out Instance; Lookup: Boolean): Boolean;
var
  Form: TComponent;
begin
  Result := Lookup and Find(IID, Instance);
  if not Result then
  begin

    if WindowClass.InheritsFrom(TFormOuchBase) then
      Form := TFormOuchBaseClass(WindowClass).Create(TComponent(FApplication.Instance), Self) else
      Form := WindowClass.Create(FApplication.Instance);

    Result := Form.GetInterface(IID, Instance);

    if not Result and Assigned(Form) then
      if Form.InheritsFrom(TCustomForm) then
        TCustomForm(Form).Release else
        Form.Free;
  end;
end;

function TOuchWindowList.Add(const Item: IOuchWindow): Integer;
begin
  Result := inherited Add(Item);
end;

function TOuchWindowList.Remove(const Item: IOuchWindow): Integer;
begin
  Result := inherited Remove(Item);
end;

procedure TOuchWindowList.Clear;
var
  Enum: IEnumerator;
  Item: IOuchWindow;
begin
  while Enumerate(Enum, Item) do
  try
    Delete(Enum.Iteration);
    Item.Close;
  finally
    Item := nil;
  end;
end;

end.
