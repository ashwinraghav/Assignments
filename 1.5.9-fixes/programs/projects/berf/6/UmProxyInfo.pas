unit UmProxyInfo;

interface

{$include Defines.inc}

uses
  Sil,
  UiBerf;

type
  TAuthInfo = class (TSilObject, IAuthInfo)
  private
    FName: String;
    FMethod: String;
    FPort: Word;
    FDomain: String;
    FPassword: String;
    FUser: String;
  protected
    function GetName: String;
    function GetMethod: String;
    function GetPort: Word;
    function GetDomain: String;
    function GetPassword: String;
    function GetUser: String;
    procedure SetPort(Value: Word);
    procedure SetDomain(const Value: String);
    procedure SetPassword(const Value: String);
    procedure SetUser(const Value: String);
    procedure SetMethod(const Value: String);
  public
    constructor Create(const Name: String);
  end;

implementation

{ TAuthInfo }

constructor TAuthInfo.Create(const Name: String);
begin
  inherited Create;
  FName := Name;
end;

function TAuthInfo.GetMethod: String;
begin
  Result := FMethod;
end;

function TAuthInfo.GetDomain: String;
begin
  Result := FDomain;
end;

function TAuthInfo.GetName: String;
begin
  Result := FName;
end;

function TAuthInfo.GetPassword: String;
begin
  Result := FPassword;
end;

function TAuthInfo.GetPort: Word;
begin
  Result := FPort;
end;

function TAuthInfo.GetUser: String;
begin
  Result := FUser;
end;

procedure TAuthInfo.SetMethod(const Value: String);
begin
  FMethod := Value;
end;

procedure TAuthInfo.SetDomain(const Value: String);
begin
  FDomain := Value;
end;

procedure TAuthInfo.SetPassword(const Value: String);
begin
  FPassword := Value;
end;

procedure TAuthInfo.SetPort(Value: Word);
begin
  FPort := Value;
end;

procedure TAuthInfo.SetUser(const Value: String);
begin
  FUser := Value;
end;

end.
 