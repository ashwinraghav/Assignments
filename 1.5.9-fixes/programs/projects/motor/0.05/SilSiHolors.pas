unit SilSiHolors;

{$INCLUDE Defines.inc}
{$O+,W-}

interface

uses
  Sil;

type
  TMerate      = Extended;
  PMerate      = ^TMerate;

const
  CMerateSize  = SizeOf(TMerate);
  CMerateLimit = MaxInt div CMerateSize;

type
  TMerates = array[0..CMerateLimit-1] of TMerate;
  PMerates = ^TMerates;

  PIHolor = ^IHolor;

  RHolorLimits = record
    Ptr: PMerate;
    Last: PMerate;
  end;

  IHolor = interface
    ['{C27524F8-321A-47F5-BA45-5DD283FCF8FF}']
    function GetSize: Integer;
    function GetData: PMerates;
    function GetFirst: PMerate;
    function GetLast: PMerate;
    function GetLimits: RHolorLimits;
    property Size: Integer read GetSize;
    property Data: PMerates read GetData;
    property First: PMerate read GetFirst;
    property Last: PMerate read GetLast;
    property Limits: RHolorLimits read GetLimits;
  end;

  Vector = class
    class function Create(const Data: array of TMerate): IHolor; overload;
    class function Create(const Vector: IHolor): IHolor; overload;
  end;

procedure Asg(This: RHolorLimits; Source: PMerate);
procedure Add(This: RHolorLimits; Source: PMerate); overload;
procedure Add(This: RHolorLimits; Source1, Source2: PMerate); overload;
procedure Sub(This: RHolorLimits; Source: PMerate); overload;
procedure Sub(This: RHolorLimits; Source1, Source2: PMerate); overload;
function Dot(This: RHolorLimits; Source: PMerate): Double; overload;
function Norm(This: RHolorLimits): Double; overload;
procedure Mul(This: RHolorLimits; const Value: Double); overload;
procedure Mul(This: RHolorLimits; Source: PMerate; const Value: Double); overload;
procedure MulAdd(This: RHolorLimits; Source: PMerate; const Value: Double); overload;
function Shw(This: RHolorLimits): string;

implementation

uses
  Math, SilSmHolors;

{W-,O+}

procedure AsgM(First, Last, Source: PMerate);
begin
  System.Move(Source^, First^, Integer(Last) - Integer(First));
end;

procedure Asg(This: RHolorLimits; Source: PMerate);
begin
  with This do while Ptr <> Last do
    begin
      Ptr^ := Source^;
      Inc(Ptr);
      Inc(Source);
    end;
end;

procedure Add(This: RHolorLimits; Source: PMerate);
begin
  with This do while Ptr <> Last do
    begin
      Ptr^ := Ptr^ + Source^;
      Inc(Ptr);
      Inc(Source);
    end;
end;

procedure Add(This: RHolorLimits; Source1, Source2: PMerate);
begin
  with This do while Ptr <> Last do
    begin
      Ptr^ := Source1^ + Source2^;
      Inc(Ptr);
      Inc(Source1);
      Inc(Source2);
    end;
end;

procedure Sub(This: RHolorLimits; Source: PMerate); overload;
begin
  with This do while Ptr <> Last do
    begin
      Ptr^ := Ptr^ - Source^;
      Inc(Ptr);
      Inc(Source);
    end;
end;

procedure Sub(This: RHolorLimits; Source1, Source2: PMerate); overload;
begin
  with This do while Ptr <> Last do
    begin
      Ptr^ := Source1^ - Source2^;
      Inc(Ptr);
      Inc(Source1);
      Inc(Source2);
    end;
end;

function Dot(This: RHolorLimits; Source: PMerate): Double; overload;
begin
  Result := 0;
  with This do while Ptr <> Last do
    begin
      Result := Result + Ptr^ * Source^;
      Inc(Ptr);
      Inc(Source);
    end;
end;

function Norm(This: RHolorLimits): Double; overload;
begin
  Result := 0;
  with This do while Ptr <> Last do
    begin
      Result := Result + Ptr^ * Ptr^;
      Inc(Ptr);
    end;
end;

procedure Mul(This: RHolorLimits; const Value: Double); overload;
begin
  with This do while Ptr <> Last do
    begin
      Ptr^ := Ptr^ * Value;
      Inc(Ptr);
    end;
end;

procedure Mul(This: RHolorLimits; Source: PMerate; const Value: Double); overload;
begin
  with This do while Ptr <> Last do
    begin
      Ptr^ := Source^ * Value;
      Inc(Ptr);
      Inc(Source);
    end;
end;

procedure MulAdd(This: RHolorLimits; Source: PMerate; const Value: Double); overload;
begin
  with This do while Ptr <> Last do
    begin
      Ptr^ := Ptr^ + Source^ * Value;
      Inc(Ptr);
      Inc(Source);
    end;
end;

function Shw(This: RHolorLimits): string;
begin
  Result := '';
  with This do while Ptr <> Last do
    begin
      Str.Add(Result, Float.ToStr(Ptr^), ' ');
      Inc(Ptr);
    end;
end;

{ Vector }

class function Vector.Create(const Data: array of TMerate): IHolor;
begin
  Result := THolor.Create(Length(Data));
  Asg(Result.Limits, @Data[0]);
end;

class function Vector.Create(const Vector: IHolor): IHolor;
begin
  Result := THolor.Create(Vector.Size);
  Asg(Result.Limits, Vector.First);
end;

end.
