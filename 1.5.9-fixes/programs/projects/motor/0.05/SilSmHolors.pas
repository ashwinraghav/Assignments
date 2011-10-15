unit SilSmHolors;

{$INCLUDE Defines.inc}

interface

uses
  Sil, SilSiHolors;

type
  THolor = class(
    TSilObject,
    IHolor)
  private
    FData: PMerates;
    FSize: Integer;
  protected // IHolor
    function GetSize: Integer;
    function GetData: PMerates;
    function GetFirst: PMerate;
    function GetLast: PMerate;
    function GetLimits: RHolorLimits;
  public
    constructor Create(Size: Integer);
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils;
  
{$W-,O+}

{ THolor }

constructor THolor.Create(Size: Integer);
begin
  inherited Create;
  FData := AllocMem(Size * CMerateSize);
  FSize := Size;
end;

destructor THolor.Destroy;
begin
  FreeMem(FData);
  FData := nil;
  inherited;
end;

function THolor.GetData: PMerates;
begin
  Result := FData;
end;

function THolor.GetFirst: PMerate;
begin
  Result := Pointer(FData);
end;

function THolor.GetLast: PMerate;
begin
  Result := Pointer(Integer(FData) + FSize * CMerateSize);
end;

function THolor.GetLimits: RHolorLimits;
begin
  Result.Ptr  := Pointer(FData);
  Result.Last := Pointer(Integer(FData) + FSize * CMerateSize);
end;

function THolor.GetSize: Integer;
begin
  Result := FSize;
end;

end.
