unit SilLmInvokeParam;

interface

uses
  SilBeTypeInfo,
  SilLiTypeInfo,
  SilLkInvokeParam;

type
  TSilParamUnknown = packed class(TSilParamObject)
  end;

  TSilParamPointer = packed class(TSilParamObject)
  protected
    FPtr: Pointer;
    FData: record end;
  public 
    class function DoGetResultKind(const Info: ITypeParam): TSilResultKind; override;
  public
    constructor CreateNew(const Info: ITypeParam; const Value: PVarRec; Previous: TSilParamObject = nil); override;
    destructor Destroy; override;
  end;

  TSilOrdinalKind = class of TSilParamOrdinal;
  
  TSilParamOrdinal = packed class(TSilParamObject)
  protected
    FData: LongWord;
  protected
    procedure Assign(Return0, Return1: LongWord); override; 
  public
    class function DoGetParamKind(const Info: ITypeParam): TSilParamKind; override;
    class function DoGetResultKind(const Info: ITypeParam): TSilResultKind; override; 
  end;

  TSilParamSignedByte = packed class(TSilParamOrdinal)
  protected
    procedure FromInteger(const Value: PVarRec); override;
    function GetValue: Variant; override;
  end;

  TSilParamUnsignedByte = packed class(TSilParamOrdinal)
  protected
    procedure FromInteger(const Value: PVarRec); override;
    function GetValue: Variant; override;      
  end;

  TSilParamSignedWord = packed class(TSilParamOrdinal)
  protected
    procedure FromInteger(const Value: PVarRec); override;
    function GetValue: Variant; override;      
  end;

  TSilParamUnsignedWord = packed class(TSilParamOrdinal)
  protected
    procedure FromInteger(const Value: PVarRec); override;
    function GetValue: Variant; override;      
  end;

  TSilParamSignedLong = packed class(TSilParamOrdinal)
  protected
    procedure FromInteger(const Value: PVarRec); override;
    function GetValue: Variant; override;      
  end;

  TSilParamUnsignedLong = packed class(TSilParamOrdinal)
  protected
    procedure FromInteger(const Value: PVarRec); override;
    function GetValue: Variant; override;      
  end;

  TSilParamAnsiChar = packed class(TSilParamUnsignedByte)
    function GetValue: Variant; override;      
  end;

  TSilParamWideChar = packed class(TSilParamUnsignedWord)
    function GetValue: Variant; override;      
  end;

  TSilParamEnumeration = packed class(TSilParamOrdinal)
  protected
    procedure FromInteger(const Value: PVarRec); override;
    function GetValue: Variant; override;      
  end;

  TSilParamSet = packed class(TSilParamOrdinal)
  protected
    procedure FromInteger(const Value: PVarRec); override;
    function GetValue: Variant; override;      
  end;

  TSilParamFloat = packed class(TSilParamObject)
  public
    class function DoGetResultKind(const Info: ITypeParam): TSilResultKind; override;
    class function DoGetParamKind(const Info: ITypeParam): TSilParamKind; override;
  end;

  TSilParamFloatSingle = packed class(TSilParamFloat)
  protected 
    FData: Single;
  protected
    procedure Assign(Return0, Return1: LongWord); override; 
    function GetValue: Variant; override;      
  protected
    procedure FromInteger(const Value: PVarRec); override;
    procedure FromString(const Value: PVarRec); override;
    procedure FromPWideChar(const Value: PVarRec); override;
    procedure FromAnsiString(const Value: PVarRec); override;
    procedure FromPChar(const Value: PVarRec); override;
    procedure FromExtended(const Value: PVarRec); override;
    procedure FromInt64(const Value: PVarRec); override;
    procedure FromVariant(const Value: PVarRec); override;
    procedure FromCurrency(const Value: PVarRec); override;
  end;

  TSilParamFloatDouble = packed class(TSilParamFloat)
  protected
    FData: Double;
  protected
    procedure Assign(Return0, Return1: LongWord); override;
    function GetValue: Variant; override;      
  end;

  TSilParamFloatExtended = packed class(TSilParamFloat)
  protected
    FData: Extended;
  protected
    procedure Assign(Return0, Return1: LongWord); override;
    function GetValue: Variant; override;      
  end;

  TSilParamFloatComp = packed class(TSilParamFloat)
  protected
    FData: Comp;
  protected
    procedure Assign(Return0, Return1: LongWord); override;
    function GetValue: Variant; override;      
  end;

  TSilParamFloatCurr = packed class(TSilParamFloat)
  protected
    FData: Currency;
  protected
    procedure Assign(Return0, Return1: LongWord); override;
    function GetValue: Variant; override;      
  end;

  TSilParamString = packed class(TSilParamPointer)
  protected
    FData: ShortString;
  protected 
    function GetValue: Variant; override;
  end;

  TSilParamClass = packed class(TSilParamObject)
  end;

  TSilParamMethod = packed class(TSilParamObject)
  end;

  TSilParamAnsiString = packed class(TSilParamObject)
  protected
    FData: AnsiString;
  protected
    function GetValue: Variant; override;
  protected
    procedure FromInteger(const Value: PVarRec); override;
    procedure FromString(const Value: PVarRec); override;
    procedure FromPWideChar(const Value: PVarRec); override;
    procedure FromAnsiString(const Value: PVarRec); override;
    procedure FromPChar(const Value: PVarRec); override;
    procedure FromExtended(const Value: PVarRec); override;
    procedure FromInt64(const Value: PVarRec); override;
    procedure FromVariant(const Value: PVarRec); override;
    procedure FromCurrency(const Value: PVarRec); override;
  public
    constructor CreateNew(const Info: ITypeParam; const Value: PVarRec; Previous: TSilParamObject = nil); override;
    class function DoGetResultKind(const Info: ITypeParam): TSilResultKind; override;
  end;

  TSilParamWideString = packed class(TSilParamObject)
  protected 
    FData: WideString;
  protected 
    function GetValue: Variant; override;
  public
    class function DoGetResultKind(const Info: ITypeParam): TSilResultKind; override;
  end;

  TSilParamVariant = packed class(TSilParamPointer)
  protected
    FData: Variant;
  protected
    procedure FromVariant(const Value: PVarRec); override;
  protected 
    function GetValue: Variant; override;
  public
    class function DoGetResultKind(const Info: ITypeParam): TSilResultKind; override;
  end;

  TSilParamArray = packed class(TSilParamObject)
  end;

  TSilParamRecord = packed class(TSilParamPointer)
  end;

  TSilParamInterface = packed class(TSilParamObject)
  protected
    FData: IUnknown;
  protected
    procedure FromVariant(const Value: PVarRec); override;
    procedure FromInterface(const Value: PVarRec); override;
    procedure FromObject(const Value: PVarRec); override;
  protected 
    function GetValue: Variant; override;
  public
    class function DoGetResultKind(const Info: ITypeParam): TSilResultKind; override;
  end;

  TSilParamInt64 = packed class(TSilParamPointer)
  protected
    FData: Int64;
  protected 
    function GetValue: Variant; override;
  public
    class function DoGetResultKind(const Info: ITypeParam): TSilResultKind; override;
  end;

  TSilParamDynArray = packed class(TSilParamObject)
  end;

implementation

uses
  SysUtils,
  SilBtError,
  SilBtVart;

{ TSilParamPointer }

constructor TSilParamPointer.CreateNew(const Info: ITypeParam; const Value: PVarRec; Previous: TSilParamObject);
begin
  inherited;
  FPtr := @FData;
end;

destructor TSilParamPointer.Destroy;
begin
  FPtr := nil;
  inherited;
end;

class function TSilParamPointer.DoGetResultKind(const Info: ITypeParam): TSilResultKind;
begin
  Result := TSilResultRegister;
end;

{ TSilParamOrdinal }

const
  GOrdinalKind: array[TOrdType] of TSilOrdinalKind = (
      TSilParamSignedByte,    //  otSByte,
      TSilParamUnsignedByte,  //  otUByte,
      TSilParamSignedWord,    //  otSWord,
      TSilParamUnsignedWord,  //  otUWord,
      TSilParamSignedLong,    //  otSLong,
      TSilParamUnsignedLong   //  otULong
    );

procedure TSilParamOrdinal.Assign(Return0, Return1: LongWord);
begin
  FData := Return0;
end;

class function TSilParamOrdinal.DoGetParamKind(const Info: ITypeParam): TSilParamKind;
begin
  Result := GOrdinalKind[Info.BaseType.Data.AsOrdinal.OrdinalKind];
end;

class function TSilParamOrdinal.DoGetResultKind(const Info: ITypeParam): TSilResultKind;
begin
  Result := TSilResultRegister;
end;

{ TSilParamSignedByte }

procedure TSilParamSignedByte.FromInteger(const Value: PVarRec);
begin
  FData := ShortInt(Value.VInteger);
end;

function TSilParamSignedByte.GetValue: Variant;
begin
  Result := ShortInt(FData);
end;

{ TSilParamUnsignedByte }

procedure TSilParamUnsignedByte.FromInteger(const Value: PVarRec);
begin
  FData := Byte(Value.VInteger);
end;

function TSilParamUnsignedByte.GetValue: Variant;
begin
  Result := Byte(FData);
end;

{ TSilParamSignedWord }

procedure TSilParamSignedWord.FromInteger(const Value: PVarRec);
begin
  FData := Smallint(Value.VInteger);
end;

function TSilParamSignedWord.GetValue: Variant;
begin
  Result := Smallint(FData);
end;

{ TSilParamUnsignedWord }

procedure TSilParamUnsignedWord.FromInteger(const Value: PVarRec);
begin
  FData := Word(Value.VInteger);
end;

function TSilParamUnsignedWord.GetValue: Variant;
begin
  Result := Word(FData);
end;

{ TSilParamSignedLong }

procedure TSilParamSignedLong.FromInteger(const Value: PVarRec);
begin
  FData := LongInt(Value.VInteger);
end;

function TSilParamSignedLong.GetValue: Variant;
begin
  Result := LongInt(FData);
end;

{ TSilParamUnsignedLong }

procedure TSilParamUnsignedLong.FromInteger(const Value: PVarRec);
begin
  FData := LongWord(Value.VInteger);
end;

function TSilParamUnsignedLong.GetValue: Variant;
begin
  Result := LongWord(FData);
end;

{ TSilParamAnsiChar }

function TSilParamAnsiChar.GetValue: Variant;
begin
  Result := AnsiChar(FData);
end;

{ TSilParamWideChar }

function TSilParamWideChar.GetValue: Variant;
begin
  Result := WideChar(FData);
end;

{ TSilParamEnumeration }

procedure TSilParamEnumeration.FromInteger(const Value: PVarRec);
begin
  FData := Value.VInteger;
end;

function TSilParamEnumeration.GetValue: Variant;
begin
  Result := FData;
end;

{ TSilParamSet }

procedure TSilParamSet.FromInteger(const Value: PVarRec);
begin
  FData := Value.VInteger;
end;

function TSilParamSet.GetValue: Variant;
begin
  Result := FData;
end;

{ TSilParamFloat }

const
  GFloatKind: array[TFloatType] of TSilParamKind =
    (
      TSilParamFloatSingle,
      TSilParamFloatDouble,
      TSilParamFloatExtended,
      TSilParamFloatComp,
      TSilParamFloatCurr
    );

class function TSilParamFloat.DoGetParamKind(const Info: ITypeParam): TSilParamKind;
begin
  Result := GFloatKind[Info.BaseType.Data.AsFloat.FloatType]
end;

class function TSilParamFloat.DoGetResultKind(const Info: ITypeParam): TSilResultKind;
begin
  Result := TSilResultNumeric;
end;

{ TSilParamInterface }

class function TSilParamInterface.DoGetResultKind(const Info: ITypeParam): TSilResultKind;
begin
  Result := TSilResultReference;
end;

procedure TSilParamInterface.FromInterface(const Value: PVarRec);
var
  Info: ITypeInterface;
begin
  Info := FInfo.BaseType.Data.AsInterface;
  if not Assigned(Value.VInterface) or (IUnknown(Value.VInterface).QueryInterface(Info.GUID, FData) <> 0) then
    raise Error.Create('parametro %s: el valor no soporta la interface %s', [FInfo.ParamName, Info.Name], ESilParamConversion);
end;

procedure TSilParamInterface.FromObject(const Value: PVarRec);
var
  Info: ITypeInterface;
begin
  Info := FInfo.BaseType.Data.AsInterface;
  if Assigned(Value.VObject) and not Value.VObject.GetInterface(Info.GUID, FData) then
    raise Error.Create('parametro %s: la clase %s no soporta la interface %s', [FInfo.ParamName, Value.VObject.ClassName, Info.Name], ESilParamConversion);
end;

procedure TSilParamInterface.FromVariant(const Value: PVarRec);
var
  Info: ITypeInterface;
begin
  Info := FInfo.BaseType.Data.AsInterface;
  if Vart.IsOK(Value.VVariant^) and not Vart.ToInterface(Value.VVariant^, Info.GUID, FData) then
    raise Error.Create('parametro %s: no fue posible convertir el argumento Variant a la interface %s', [FInfo.ParamName, Value.VObject.ClassName, Info.Name], ESilParamConversion);
end;

function TSilParamInterface.GetValue: Variant;
begin
  Result := FData;
end;

{ TSilParamFloatSingle }

procedure TSilParamFloatSingle.Assign(Return0, Return1: LongWord);
asm
  mov   eax, Self
  fstp  FData[eax]
  wait
end;

procedure TSilParamFloatSingle.FromAnsiString(const Value: PVarRec);
begin
  FData := StrToFloat(string(Value.VAnsiString));
end;

procedure TSilParamFloatSingle.FromCurrency(const Value: PVarRec);
begin
  FData := Value.VCurrency^;
end;

procedure TSilParamFloatSingle.FromExtended(const Value: PVarRec);
begin
  FData := Value.VExtended^
end;

procedure TSilParamFloatSingle.FromInt64(const Value: PVarRec);
begin
  FData := Value.VInt64^;
end;

procedure TSilParamFloatSingle.FromInteger(const Value: PVarRec);
begin
  FData := Value.VInteger;
end;

procedure TSilParamFloatSingle.FromPChar(const Value: PVarRec);
begin
  FData := StrToFloat(Value.VPChar);
end;

procedure TSilParamFloatSingle.FromPWideChar(const Value: PVarRec);
begin
  FData := StrToFloat(Value.VPWideChar);
end;

procedure TSilParamFloatSingle.FromString(const Value: PVarRec);
begin
  FData := StrToFloat(Value.VString^);
end;

procedure TSilParamFloatSingle.FromVariant(const Value: PVarRec);
begin
  FData := Value.VVariant^;
end;

function TSilParamFloatSingle.GetValue: Variant;
begin
  Result := FData;
end;

{ TSilParamAnsiString }

constructor TSilParamAnsiString.CreateNew(const Info: ITypeParam; const Value: PVarRec; Previous: TSilParamObject);
begin
  inherited;
end;

class function TSilParamAnsiString.DoGetResultKind(const Info: ITypeParam): TSilResultKind;
begin
  Result := TSilResultReference;
end;

procedure TSilParamAnsiString.FromInteger(const Value: PVarRec);
begin
  FData := IntToStr(Value.VInteger);
end;

procedure TSilParamAnsiString.FromString(const Value: PVarRec);
begin
  FData := Value.VString^;
end;

procedure TSilParamAnsiString.FromPWideChar(const Value: PVarRec);
begin
  FData := Value.VWideChar;
end;

procedure TSilParamAnsiString.FromAnsiString(const Value: PVarRec);
begin
  FData := AnsiString(Value.VAnsiString);
end;

procedure TSilParamAnsiString.FromPChar(const Value: PVarRec);
begin
  FData := Value.VPChar;
end;

procedure TSilParamAnsiString.FromExtended(const Value: PVarRec);
begin
  FData := FloatToStr(Value.VExtended^);
end;

procedure TSilParamAnsiString.FromInt64(const Value: PVarRec);
begin
  FData := IntToStr(Value.VInt64^);
end;

procedure TSilParamAnsiString.FromVariant(const Value: PVarRec);
begin
  FData := Vart.ToStr(Value.VVariant^);
end;

procedure TSilParamAnsiString.FromCurrency(const Value: PVarRec);
begin
  FData := CurrToStr(Value.VCurrency^);
end;

function TSilParamAnsiString.GetValue: Variant;
begin
  Result := FData;
end;

{ TSilParamWideString }

class function TSilParamWideString.DoGetResultKind(const Info: ITypeParam): TSilResultKind;
begin
  Result := TSilResultReference;
end;

function TSilParamWideString.GetValue: Variant;
begin
  Result := FData;
end;

{ TSilParamVariant }

class function TSilParamVariant.DoGetResultKind(const Info: ITypeParam): TSilResultKind;
begin
  Result := TSilResultReference;
end;

procedure TSilParamVariant.FromVariant(const Value: PVarRec);
begin
  FData := Value.VVariant^
end;

function TSilParamVariant.GetValue: Variant;
begin
  Result := FData;
end;

{ TSilParamInt64 }

class function TSilParamInt64.DoGetResultKind(const Info: ITypeParam): TSilResultKind;
begin
 Result := TSilResultReference;
end;

function TSilParamInt64.GetValue: Variant;
begin
  Result := FData;
end;

{ TSilParamFloatDouble }

procedure TSilParamFloatDouble.Assign(Return0, Return1: LongWord);
asm
  mov   eax, Self
  fstp  FData[eax]
  wait
end;

function TSilParamFloatDouble.GetValue: Variant;
begin
  Result := FData;
end;

{ TSilParamFloatExtended }

procedure TSilParamFloatExtended.Assign(Return0, Return1: LongWord);
asm
  mov   eax, Self
  fstp  FData[eax]
  wait
end;

function TSilParamFloatExtended.GetValue: Variant;
begin
  Result := FData;
end;

{ TSilParamFloatComp }

procedure TSilParamFloatComp.Assign(Return0, Return1: LongWord);
asm
  mov   eax, Self
  fstp  FData[eax]
  wait
end;

function TSilParamFloatComp.GetValue: Variant;
begin
  Result := FData;
end;

{ TSilParamFloatCurr }

procedure TSilParamFloatCurr.Assign(Return0, Return1: LongWord);
asm
  mov   eax, Self
  fstp  FData[eax]
  wait
end;

function TSilParamFloatCurr.GetValue: Variant;
begin
  Result := FData;
end;

{ TSilParamString }

function TSilParamString.GetValue: Variant;
begin
  Result := FData;
end;

end.
