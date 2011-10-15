unit UmTest;

interface

uses
  Sil, UiTest;

type
  TTest = class(
    TSilObject,
    ITest )
  private
    FPosition: Extended;
  protected
    procedure SetBoolean(const Value: Boolean); stdcall;
    procedure SetDouble(const Value: Double); stdcall;
    procedure SetExtended(const Value: Extended); stdcall;
    procedure SetSingle(const Value: Single); stdcall;
    procedure SetString(const Value: string); stdcall;
    procedure SetInteger(const Value: Integer); stdcall;
    function GetBoolean: Boolean; stdcall;
    function GetDouble: Double; stdcall;
    function GetExtended: Extended; stdcall;
    function GetSingle: Single; stdcall;
    function GetString: string; stdcall;
    function GetInteger: Integer; stdcall;
    procedure VoidResult(const Value: string); stdcall;
    function StringResult(const Value: string): string; stdcall;
    function VariantResult(const Value: string): Variant; stdcall;
    function InterfaceResult(const Value: string): IUnknown; stdcall;
    procedure StringOut(const Value: string; out Result: string); stdcall;
    procedure VariantOut(const Value: string; out Result: Variant); stdcall;
    procedure InterfaceOut(const Value: string; out Result: IUnknown); stdcall;
  public
    constructor Create(const Value: Extended);
  end;

implementation

constructor TTest.Create(const Value: Extended);
begin
  inherited Create;
  FPosition := Value;
end;

function TTest.GetDouble: Double;
begin
  Result := FPosition;
end;

function TTest.GetExtended: Extended;
begin
  Result := FPosition;
end;

function TTest.GetSingle: Single;
begin
  Result := FPosition;
end;

procedure TTest.SetDouble(const Value: Double);
begin
  FPosition := Value;
end;

procedure TTest.SetExtended(const Value: Extended);
begin
  FPosition := Value;
end;

procedure TTest.SetSingle(const Value: Single);
begin
  FPosition := Value;
end;

procedure TTest.VoidResult(const Value: string);
begin
end;

function TTest.StringResult(const Value: string): string;
begin
  Result := 'StringResult: ' + Value;
end;

function TTest.VariantResult(const Value: string): Variant;
begin
  Result := 'VariantResult: ' + Value;
end;

function TTest.InterfaceResult(const Value: string): IUnknown;
begin
  Result := Self;
end;

procedure TTest.InterfaceOut(const Value: string; out Result: IInterface);
begin
  Result := Self;
end;

procedure TTest.StringOut(const Value: string; out Result: string);
begin
  Result := 'StringOut: ' + Value;
end;

procedure TTest.VariantOut(const Value: string; out Result: Variant);
begin
  Result := 'VariantOut: ' + Value;
end;

function TTest.GetString: string;
begin
  result := Float.ToStr(FPosition);
end;

procedure TTest.SetString(const Value: string);
begin
  FPosition := Str.ToFloat(Value);
end;

function TTest.GetBoolean: Boolean;
begin
  result := FPosition <> 0;
end;

procedure TTest.SetBoolean(const Value: Boolean);
begin
  FPosition := Int.IIf(Value, 1, 0);
end;

function TTest.GetInteger: Integer;
begin
  result := Trunc(FPosition);
end;

procedure TTest.SetInteger(const Value: Integer);
begin
  FPosition := Value;
end;

end.
