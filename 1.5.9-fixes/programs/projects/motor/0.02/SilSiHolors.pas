unit SilSiHolors;

{$INCLUDE Defines.inc}
{$O+,W-}

interface

uses
  Sil;

type
  TMerate      = Double;
  PMerate      = ^TMerate;

const
  CMerateSize  = SizeOf(TMerate);
  CMerateLimit = MaxInt div CMerateSize;

type
  TMerateArray = array[0..CMerateLimit-1] of TMerate;
  PMerateArray = ^TMerateArray;

  PIHolor = ^IHolor;

  RHolorLimits = record
    Ptr: PMerate;
    Last: PMerate;
  end;

  IHolor = interface
    ['{C27524F8-321A-47F5-BA45-5DD283FCF8FF}']
    function GetSize: Integer;
    function GetData: PMerateArray;
    function GetFirst: PMerate;
    function GetLast: PMerate;
    function GetLimits: RHolorLimits;
    property Size: Integer read GetSize;
    property Data: PMerateArray read GetData;
    property First: PMerate read GetFirst;
    property Last: PMerate read GetLast;
    property Limits: RHolorLimits read GetLimits;
  end;

  Vector = class
    class function Create(const Data: array of TMerate): IHolor; overload;
    class function Create(const Vector: IHolor): IHolor; overload;
  end;

procedure AsgM(First, Last, Source: PMerate);
procedure Asg0(First, Last, Source: PMerate);
procedure Asg1(First, Last, Source: PMerate);
procedure Asg2(First, Last, Source: PMerate);
procedure Sum(First, Last, Source: PMerate);
procedure Mul(First, Last: PMerate; const Value: Double); overload;
function Mul(First, Last, Source: PMerate): Double; overload;
 
procedure Asg(This: RHolorLimits; Source: PMerate);
function Mul(This: RHolorLimits; Source: PMerate): Double; overload;
procedure Mul(This: RHolorLimits; const Value: Double); overload;
procedure Mul(This: RHolorLimits; Source: PMerate; const Value: Double); overload;


implementation

uses
  SilSmHolors;

{W-,O+}

procedure AsgM(First, Last, Source: PMerate);
begin
  System.Move(Source^, First^, Integer(Last) - Integer(First));
end;

procedure Asg(This: RHolorLimits; Source: PMerate);
begin
  with This do
    while Ptr <> Last do
    begin
      Ptr^ := Source^;
      Inc(Ptr);
      Inc(Source);
    end;
end;

function Mul(This: RHolorLimits; Source: PMerate): Double; overload;
begin
  Result := 0;
  with This do
    while Ptr <> Last do
    begin
      Result := Result + Ptr^ * Source^;
      Inc(Ptr);
      Inc(Source);
    end;
end;

procedure Mul(This: RHolorLimits; const Value: Double); overload;
begin
  with This do
    while Ptr <> Last do
    begin
      Ptr^ := Ptr^ * Value;
      Inc(Ptr);
    end;
end;

procedure Mul(This: RHolorLimits; Source: PMerate; const Value: Double); overload;
begin
  with This do
    while Ptr <> Last do
    begin
      Ptr^ := Source^ * Value;
      Inc(Ptr);
      Inc(Source);
    end;
end;

procedure Asg0(First, Last, Source: PMerate);
begin
  while First <> Last do
  begin
    First^ := Source^;
    Inc(First);
    Inc(Source);
  end;
end;

procedure Asg1(First, Last, Source: PMerate);
  //    First  == EAX
  //    Last   == EDX
  //    Source == ECX
asm
        push  ebx
@@loop1:cmp   edx, eax
        jz    @@done
@@loop2:mov   ebx, [ecx]
        mov   [eax], ebx
        mov   ebx, [ecx+4]
        mov   [eax+4], ebx
        add   eax, 8
        add   ecx, 8
        cmp   edx, eax
        jne   @@loop2
@@done: pop   ebx
end;

procedure Asg2(First, Last, Source: PMerate);
asm
        xchg  edi, eax
        xchg  edx, ecx
        xchg  esi, edx
        sub   ecx, edi
        shr   ecx, 2
        cld
        rep   movsd
        xchg  edi, eax
        xchg  esi, edx
end;

procedure Sum(First, Last, Source: PMerate);
begin
  while First <> Last do
  begin
    First^ := First^ + Source^;
    Inc(First);
    Inc(Source);
  end;
end;

procedure Mul(First, Last: PMerate; const Value: Double);
begin
  while First <> Last do
  begin
    First^ := First^ * Value;
    Inc(First);
  end;
end;

function Mul(First, Last, Source: PMerate): Double;
begin
  Result := 0;
  while First <> Last do
  begin
    Result := Result + First^ * Source^;
    Inc(First);
    Inc(Source);
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
