unit UmGroupInfo;

interface

{$include Defines.inc}

uses
  Sil,
  UiBerf;

type
  TAclInfo = class (TSilObject, IAclInfo)
  private
    FName: String;
    FAllowNames: IStringList;
    FDenyNames: IStringList;
    FIsDefaultAllow: Boolean;
    FAllow: TIntegerArray;
    FDeny: TIntegerArray;
  protected
    function GetName: String;
    function GetAllowNames: IStringList;
    function GetDenyNames: IStringList;
    function GetIsDefaultAllow: Boolean;
    function IsValid(const Address: ISocketAddress): Boolean;
    procedure ResolveNames;
    procedure SetIsDefaultAllow(Value: Boolean);
  public
    constructor Create(const Name: String);
    destructor Destroy; override;
  end;

implementation

uses SilBtStr;

{ TAclInfo }

constructor TAclInfo.Create(const Name: String);
begin
  inherited Create;

  FName := Name;
  FAllowNames := Sil.List.StringList(true);
  FDenyNames := Sil.List.StringList(true);
end;

destructor TAclInfo.Destroy;
begin
  FAllowNames := nil;
  FDenyNames := nil;

  inherited;
end;

function TAclInfo.GetAllowNames: IStringList;
begin
  Result := FAllowNames;
end;

function TAclInfo.GetDenyNames: IStringList;
begin
  Result := FDenyNames;
end;

function TAclInfo.GetIsDefaultAllow: Boolean;
begin
  Result := FIsDefaultAllow;
end;

function TAclInfo.GetName: String;
begin
  Result := FName;
end;

procedure TAclInfo.SetIsDefaultAllow(Value: Boolean);
begin
  FIsDefaultAllow := Value;
end;

procedure TAclInfo.ResolveNames;
var
  Enum: IEnumerator;
  Item: String;
begin
  SetLength(FAllow, 0);
  SetLength(FDeny, 0);

  while FAllowNames.Enumerate(Enum, Item) do
    if Str.DelimiterPos('*?', Item) = 0 then
      try
        Int.ArrayAdd(FAllow, Sil.OS.Socket.IP.FromHostName(Item));
      except end;

  while FDenyNames.Enumerate(Enum, Item) do
    if Str.DelimiterPos('*?', Item) = 0 then
      try
        Int.ArrayAdd(FDeny, Sil.OS.Socket.IP.FromHostName(Item));
      except end;
end;

function TAclInfo.IsValid(const Address: ISocketAddress): Boolean;
var
  HostName: String;

  function DoFind(const List: IStringList): Boolean;
  var
    Enum: IEnumerator;
    Item: String;
  begin
    Result := false;

    while not Result and List.Enumerate(Enum, Item) do
      if Str.DelimiterPos('*?', Item) = 0 then
        Result := Sil.Str.WildCard(HostName, Item, true)
      else
        Result := Sil.Text.IsEqual(HostName, Item);
  end;

var
  IPAddr: LongWord;
begin
  IPAddr := Address.Address;

  if Int.ArrayFind(FDeny, IPAddr) >= 0 then
  begin
    Result := false;
    Exit;
  end;

  if Int.ArrayFind(FAllow, IPAddr) >= 0 then
  begin
    Result := true;
    Exit;
  end;

  try
    HostName := Address.Host;
  except
    Result := false;
    Exit;
  end;

  if DoFind(FDenyNames) then
  begin
    Int.ArrayAdd(FDeny, IPAddr);
    Result := false;
    Exit;
  end;

  if FIsDefaultAllow or DoFind(FAllowNames) then
  begin
    Int.ArrayAdd(FAllow, IPAddr);
    Result := true;
    Exit;
  end;

  Int.ArrayAdd(FDeny, IPAddr);
  Result := false;
end;

end.
