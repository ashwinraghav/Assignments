unit SilLkInvokeParam;

interface

uses
  SilBeError,
  SilBeTypeInfo,
  SilLiDataType,
  SilLiTypeInfo;

type
  ESilParamConversion = class(Exception);

type
  TSilParamKind = class of TSilParamObject;
  TSilResultKind = class of TSilResultObject;

  PReturnValue = ^RReturnValue;
  RReturnValue = packed record
    Reg: array[0 .. 2] of LongWord;
  end;

  TSilParamObject = packed class(TObject)
  protected
    FSize: Integer;
    FPrev: TSilParamObject;
    FInfo: ITypeParam;
    FType: IDataHandler;
    FCount: Integer;
    FBuffer: Pointer;
    FData: record end;
  private
    procedure DoAssign(const Value: PVarRec); stdcall;
    function DoGetNext: TSilParamObject;
  protected
    class function DoGetParamKind(const Info: ITypeParam): TSilParamKind; virtual;
    class function DoGetResultKind(const Info: ITypeParam): TSilResultKind; virtual;
    class function DoGetSize(const Info: ITypeParam): Integer; virtual;
    class function DoGetBaseSize(const Info: ITypeParam): Integer; virtual;
  protected
    procedure DoAssignError(const Value: PVarRec); virtual; stdcall;
  protected // Tabla de conversion
    procedure FromInteger(const Value: PVarRec); virtual; stdcall; abstract;
    procedure FromBoolean(const Value: PVarRec); virtual; stdcall; abstract;
    procedure FromChar(const Value: PVarRec); virtual; stdcall; abstract;
    procedure FromExtended(const Value: PVarRec); virtual; stdcall; abstract;
    procedure FromString(const Value: PVarRec); virtual; stdcall; abstract;
    procedure FromPointer(const Value: PVarRec); virtual; stdcall; abstract;
    procedure FromPChar(const Value: PVarRec); virtual; stdcall; abstract;
    procedure FromObject(const Value: PVarRec); virtual; stdcall; abstract;
    procedure FromClass(const Value: PVarRec); virtual; stdcall; abstract;
    procedure FromWideChar(const Value: PVarRec); virtual; stdcall; abstract;
    procedure FromPWideChar(const Value: PVarRec); virtual; stdcall; abstract;
    procedure FromAnsiString(const Value: PVarRec); virtual; stdcall; abstract;
    procedure FromCurrency(const Value: PVarRec); virtual; stdcall; abstract;
    procedure FromVariant(const Value: PVarRec); virtual; stdcall; abstract;
    procedure FromInterface(const Value: PVarRec); virtual; stdcall; abstract;
    procedure FromWideString(const Value: PVarRec); virtual; stdcall; abstract;
    procedure FromInt64(const Value: PVarRec); virtual; stdcall; abstract;
  protected
    procedure Assign(Return0, Return1: LongWord); virtual; stdcall;
    function GetValue: Variant; virtual; abstract;
  protected
    constructor CreateNew(const Info: ITypeParam; const Value: PVarRec = nil; Previous: TSilParamObject = nil); virtual;
  public
    destructor Destroy; override;
    procedure FreeInstance; override;
    procedure Free;
  public
    class function Create(Instance: Pointer; const Info: ITypeParam; const Value: PVarRec = nil; Previous: TSilParamObject = nil): Pointer;
    class function GetSize(const Info: ITypeParam): Integer;
    class function GetKind(const Info: ITypeParam): TSilParamKind; virtual;
  public
    property Size: Integer read FSize;
    property Buffer: Pointer read FBuffer;
    property Prev: TSilParamObject read FPrev;
    property Next: TSilParamObject read DoGetNext;
    property Typ: IDataHandler read FType;
    property Info: ITypeParam read FInfo;
  end;

  TSilParamByReference = packed class(TSilParamObject)
  protected
    FReference: PPointer;
  protected
    procedure FromPointer(const Value: PVarRec); override;
  protected
    constructor CreateNew(const Info: ITypeParam; const Value: PVarRec; Previous: TSilParamObject = nil); override;
  public
    destructor Destroy; override;
  end;

  TSilResultObject = packed class(TSilParamObject)
  private
    FReturn: PVariant;
    FParam: TSilParamObject;
    FData: record end;
  protected
    class function DoGetSize(const Info: ITypeParam): Integer; override;
    class function DoGetBaseSize(const Info: ITypeParam): Integer; override;
  protected
    procedure FromVariant(const Value: PVarRec); override;
  protected
    procedure Assign(Return0, Return1: LongWord); override;
  public
    constructor CreateNew(const Info: ITypeParam; const Value: PVarRec; Previous: TSilParamObject = nil); override;
    destructor Destroy; override;
  public
    class function GetKind(const Info: ITypeParam): TSilParamKind; override;
  end;

  TSilResultVoid  = packed class(TSilResultObject);

  TSilResultRegister = packed class(TSilResultVoid);

  TSilResultNumeric = packed class(TSilResultVoid);

  TSilResultReference = packed class(TSilResultObject)
  protected
    FAddress: PPointer;
  public
    constructor CreateNew(const Info: ITypeParam; const Value: PVarRec; Previous: TSilParamObject = nil); override;
    destructor Destroy; override;
  end;

implementation

uses
  SilLtTypeInfo,
  SilAfCallVirtual,
  SilBtError,
  SilLmInvokeParam;

const
  GParamKind: array[TTypeKind] of TSilParamKind =
    (
      TSilParamUnknown,
      TSilParamOrdinal,
      TSilParamAnsiChar,
      TSilParamEnumeration,
      TSilParamFloat,
      TSilParamString,
      TSilParamSet,
      TSilParamClass,
      TSilParamMethod,
      TSilParamWideChar,
      TSilParamAnsiString,
      TSilParamWideString,
      TSilParamVariant,
      TSilParamArray,
      TSilParamRecord,
      TSilParamInterface,
      TSilParamInt64,
      TSilParamDynArray
    );

{ TSilParamObject }

//{$W-,R-,Q-,O+}

class function TSilParamObject.Create(Instance: Pointer; const Info: ITypeParam; const Value: PVarRec; Previous: TSilParamObject): Pointer;
begin
  Pointer(Instance^) := GetKind(Info);
  Result := TSilParamObject(Instance).CreateNew(Info, Value, Previous);
end;

class function TSilParamObject.GetKind(const Info: ITypeParam): TSilParamKind;
begin
  if (Info.Flags * [paConst, paResult] <> []) or  (Info.Flags * [paVar, paOut] = []) then
    Result := GParamKind[Info.BaseType.TypeKind].DoGetParamKind(Info) else
    Result := TSilParamByReference;
end;

class function TSilParamObject.GetSize(const Info: ITypeParam): Integer;
begin
  Result := GetKind(Info).DoGetSize(Info);
end;

constructor TSilParamObject.CreateNew(const Info: ITypeParam; const Value: PVarRec; Previous: TSilParamObject);
begin
  ASSERT(Assigned(Info));
  inherited Create;
  FInfo := Info;
  FPrev := Previous;
  FType := SilLtTypeInfo.Typ.List.Get(FInfo.BaseType);
  FSize := DoGetSize(Info);
  FCount := (FSize - DoGetBaseSize(Info)) div SizeOf(Pointer);
  FBuffer := @FData;
  if Assigned(Value) then DoAssign(Value);
end;

destructor TSilParamObject.Destroy;
begin
  FType := nil;
  FInfo := nil;
  inherited;
end;

procedure TSilParamObject.Free;
begin
  if Assigned(Self) then
  try
    Destroy;
  except
  end;
end;

procedure TSilParamObject.FreeInstance;
begin
  CleanupInstance;
end;

class function TSilParamObject.DoGetParamKind(const Info: ITypeParam): TSilParamKind;
begin
  Result := Self;
end;

class function TSilParamObject.DoGetResultKind(const Info: ITypeParam): TSilResultKind;
begin
  Result := TSilResultVoid;
end;

class function TSilParamObject.DoGetSize(const Info: ITypeParam): Integer;
begin
  Result := InstanceSize;
end;

class function TSilParamObject.DoGetBaseSize(const Info: ITypeParam): Integer;
begin
  Result := TSilParamObject.InstanceSize;
end;

procedure TSilParamObject.DoAssign(const Value: PVarRec);
  // ejecuta la rutina FromXXXX en funcion al VType que trae el Value
asm
      MOV       EAX, Self                   // EAX: Self
      MOV       ECX, Value
      PUSH      ECX
      PUSH      EAX
      MOVZX     ECX, TVarRec[ECX].VType     // ECX: Metodo a ejecutar
      MOV       EDX, VMTOFFSET FromInteger  // EDX: Indice base
      CALL      DoVirtualCall
end;

function TSilParamObject.DoGetNext: TSilParamObject;
begin
  Result := Pointer(PChar(Self) + InstanceSize);
end;

procedure TSilParamObject.DoAssignError(const Value: PVarRec);
begin
  raise Error.Create('No es posible convertir un argumento de tipo (%s) recibido al tipo (%s) requerido por el parametro (%s)', ['x', 'y', 'z'], ESilParamConversion);
end;

procedure TSilParamObject.Assign(Return0, Return1: LongWord);
begin
end;

{ TSilParamByReference }

constructor TSilParamByReference.CreateNew(const Info: ITypeParam; const Value: PVarRec; Previous: TSilParamObject);
begin
  inherited;
end;

destructor TSilParamByReference.Destroy;
begin
  inherited;
end;

procedure TSilParamByReference.FromPointer(const Value: PVarRec);
begin
  FReference := Value.VPointer;
  if paOut in FInfo.Flags then FReference^ := nil;
end;

{ TSilResultObject }

constructor TSilResultObject.CreateNew(const Info: ITypeParam; const Value: PVarRec; Previous: TSilParamObject);
begin
  ASSERT(Assigned(Value));
  inherited;
  FParam := TSilParamObject.Create(Next, Info);
  FBuffer := @FData;
end;

destructor TSilResultObject.Destroy;
begin
  FBuffer := nil;
  FParam.Free;
  FParam := nil;
  inherited;
end;

class function TSilResultObject.GetKind(const Info: ITypeParam): TSilParamKind;
begin
  Result := GParamKind[Info.BaseType.TypeKind].DoGetResultKind(Info);
end;

class function TSilResultObject.DoGetSize(const Info: ITypeParam): Integer;
begin
  Result := inherited DoGetSize(Info) + TSilParamObject.GetSize(Info);
end;

class function TSilResultObject.DoGetBaseSize(const Info: ITypeParam): Integer;
begin
  Result := TSilResultObject.InstanceSize + TSilParamObject.GetSize(Info);
end;

procedure TSilResultObject.FromVariant(const Value: PVarRec);
begin
  FReturn := Value.VVariant;
end;

procedure TSilResultObject.Assign(Return0, Return1: LongWord);
begin
  FParam.Assign(Return0, Return1);
  FReturn^ := FParam.GetValue;
end;

{ TSilResultReference }

constructor TSilResultReference.CreateNew(const Info: ITypeParam; const Value: PVarRec; Previous: TSilParamObject);
begin
  inherited;
  FAddress := FParam.Buffer;
end;

destructor TSilResultReference.Destroy;
begin
  FAddress := nil;
  inherited;
end;

end.
