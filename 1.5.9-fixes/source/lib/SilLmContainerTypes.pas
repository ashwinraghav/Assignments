{********************************************************************************
 *                  Standard Interface Library (SIL)                            *
 *                                                                              *
 *       General purpose library whose design is based in STRONG                *
 *   use of interfaces.                                                         *
 *                                                                              *
 *                                                                              *
 *     Copyright (C) 2000 Mariano Podestá    antiriad@gmail.com                 *
 *     Copyright (C) 2000 Leandro Conde      lconde@str.com.ar                  *
 *     Copyright (C) 2000 Lisandro Podestá   lisandrop@movi.com.ar              *
 *                                                                              *
 *     See License.txt for details.                                             *
 *                                                                              *
 *   This library is free software; you can redistribute it and/or              *
 *   modify it under the terms of the GNU Lesser General Public                 *
 *   License as published by the Free Software Foundation; either               *
 *   version 2.1 of the License, or (at your option) any later version.         *
 *                                                                              *
 *   This library is distributed in the hope that it will be useful,            *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of             *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU          *
 *   Lesser General Public License for more details.                            *
 *                                                                              *
 *   You should have received a copy of the GNU Lesser General Public           *
 *   License along with this library; if not, write to the Free Software        *
 *   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA  *
 *                                                                              *
 ********************************************************************************}

unit SilLmContainerTypes;

{$INCLUDE Defines.inc}

interface

uses
  Sil,
  SilBeTypes,
  SilBeTypeInfo,
  SilLiTraits,
  SilLiContainerTypes,
  SilLkNestedProcedure;

type
  TSilTypeHandler = class(
    TSilObject,
    IAdoptionTrait,
    ITypeComparator,
    ITypeHandler )
  private
    FComparator: RAdoptedRef;
    FHook: ITypeHandler;
    FSize: Integer;
  protected // ITypeComparator
    function ITypeComparator.Compare = DoCompare;
    function ITypeHandler.Compare = DoCompare;
    function DoCompare(Data1, Data2: HData; Param: Pointer): Integer; 
  protected // ITypeHandler
    procedure ITypeHandler.Initialize = DoInitialize;
    procedure ITypeHandler.Finalize = DoFinalize;
    procedure ITypeHandler.Copy = DoCopy;
    function ITypeHandler.GetSize = DoGetSize;
    procedure DoInitialize(Data: HData; Count: Integer);
    procedure DoFinalize(Data: HData; Count: Integer);
    procedure DoCopy(Data: HData; Source: HData; Count, Step: Integer);
    function DoGetSize: Integer;  
  protected
    procedure Initialize(Data: HData; Count: Integer); virtual;
    procedure Finalize(Data: HData; Count: Integer); virtual;
    procedure Copy(Data: HData; Source: HData; Count, Step: Integer); virtual; 
    function Compare(Data1, Data2: HData; Param: Pointer): Integer; virtual;
    function GetSize: Integer; virtual;
  public
    constructor Create(const Compare: ITypeComparator = nil; Size: Integer = 0; const Hook: ITypeHandler = nil);
    destructor Destroy; override;
  public
    property Size: Integer read FSize;
  end;

  TSilListHandler = class(TSilTypeHandler);

  TSilTypeinfoHandlerClass = class of TSilTypeinfoHandler;
   
  TSilTypeinfoHandler = class(
    TSilTypeHandler,
    ITypeinfoHandler )
  private
    FTypeInfo: PTypeInfo;
  protected
    class function SizeOf(TypeInfo: PTypeInfo): Integer; overload; register;  
  protected // ITypeinfoHandler
    procedure ITypeinfoHandler.Initialize = DoInitialize;
    procedure ITypeinfoHandler.Finalize = DoFinalize;
    procedure ITypeinfoHandler.Copy = DoCopy;
    function ITypeinfoHandler.GetSize = DoGetSize;
    function GetTypeInfo: PTypeInfo;
  protected
    property TypeInfo: PTypeInfo read GetTypeInfo;
  protected
    constructor Create(TypeInfo: PTypeInfo; const Compare: ITypeComparator = nil; Size: Integer = 0; const Hook: ITypeHandler = nil); reintroduce; virtual;
  public
    class function CreateHandler(TypeInfo: PTypeInfo; const Compare: ITypeComparator = nil; Size: Integer = 0; const Hook: ITypeHandler = nil): ITypeinfoHandler;
    destructor Destroy; override;
  end;

  TSilTypeinfoDynamicHandler = class(TSilTypeinfoHandler)
  protected // ITypeHandler
    procedure Initialize(Data: HData; Count: Integer); override;
    procedure Finalize(Data: HData; Count: Integer); override;
    procedure Copy(Data: HData; Source: HData; Count, Step: Integer); override;
  end;

  TSilTypeCompositeHandler = class(TSilTypeHandler)
  private
    FHandlers: TTypeHandlerList;
  protected // ITypeHandler
    procedure Initialize(Data: HData; Count: Integer); override;
    procedure Finalize(Data: HData; Count: Integer); override;
    procedure Copy(Data: HData; Source: HData; Count, Step: Integer); override;
    function DoGetSize(const Handlers: array of ITypeHandler): Integer;
  protected
    function Compare(Data1, Data2: HData; Param: Pointer): Integer; override; 
  public
    constructor Create(const Handlers: array of ITypeHandler; const Compare: ITypeComparator = nil; Size: Integer = 0; const Hook: ITypeHandler = nil);
    destructor Destroy; override;
  end;

  TSilTypeinfoStaticHandler = class(TSilTypeinfoHandler);

  TSilTypeAllocator = class(
    TSilObject,
    ITypeAllocator )
  private
    FHandler: ITypeHandler;
    FOffset: Integer;
  protected // ITypeAllocator
    function GetHandler: ITypeHandler;
    function Get(Count: Integer = 1): HData;
    procedure Release(var Data: HData; Count: Integer = 1); reintroduce;
  public
    constructor Create(const Handler: ITypeHandler; Offset: Integer = 0);
    destructor Destroy; override;
  end;

  TSilTypeMethodCompare = class(
    TSilObject,
    IAdoptionTrait,
    ITypeComparator )
  private
    FCompare: TTypeCompare;
  protected // IComparator
    function Compare(Data1, Data2: HData; Param: Pointer = nil): Integer;
  public
    constructor Create(Compare: TTypeCompare);
    destructor Destroy; override;
  end;

  TSilTypeNestedCompare = class(
    TNestedProcedure,
    IAdoptionTrait,
    ITypeComparator )
  protected // IComparator
    function Compare(Data1, Data2: HData; Param: Pointer = nil): Integer;
  public
    constructor Create(Compare: Pointer);
    destructor Destroy; override;
  end;

type
  TSilInterfaceComparator = class (
    TSilObject,
    IAdoptionTrait,
    ITypeComparator )
  private
    FComparer: ITypeComparator;
  protected // ITypeComparator
    function Compare(Data1, Data2: HData; Param: Pointer = nil): Integer;
  public
    constructor Create(const Comparer: ITypeComparator = nil);
    destructor Destroy; override; 
  end;

type
  TSilPointerComparator = class (
    TSilObject,
    IAdoptionTrait,
    ITypeComparator )
  private
    FComparer: ITypeComparator;
  protected // ITypeComparator
    function Compare(Data1, Data2: HData; Param: Pointer = nil): Integer;
  public
    constructor Create(const Comparer: ITypeComparator = nil);
    destructor Destroy; override; 
  end;

implementation

uses
  TypInfo,
  SilLkAbstractProcedure,
  SilLfTraits;

{ TSilTypeHandler }

constructor TSilTypeHandler.Create(const Compare: ITypeComparator; Size: Integer; const Hook: ITypeHandler);
begin
  inherited Create;
  FSize := Size + GetSize();
  Adopt(FComparator, Compare);
  FHook := Hook;
end;

destructor TSilTypeHandler.Destroy;
begin
  FHook := nil;
  Adopt(FComparator, nil);
  inherited;
end;

function TSilTypeHandler.DoCompare(Data1, Data2: HData; Param: Pointer): Integer;
begin
  Result := Compare(Data1, Data2, Param);
  if Assigned(FHook) and (Result = 0) then Result := FHook.Compare(Data1, Data2, Param);   
end;

procedure TSilTypeHandler.DoInitialize(Data: HData; Count: Integer);
begin
  Initialize(Data, Count);
  if Assigned(FHook) then FHook.Initialize(Data, Count);
end;

procedure TSilTypeHandler.DoFinalize(Data: HData; Count: Integer);
begin
  if Assigned(FHook) then FHook.Finalize(Data, Count);   
  Finalize(Data, Count);
end;

procedure TSilTypeHandler.DoCopy(Data, Source: HData; Count, Step: Integer);
begin
  Copy(Data, Source, Count, Step);
  if Assigned(FHook) then FHook.Copy(Data, Source, Count, Step)   
end;

function TSilTypeHandler.DoGetSize: Integer;
begin
  Result := FSize;
end;

function TSilTypeHandler.Compare(Data1, Data2: HData; Param: Pointer): Integer;
begin
  if Assigned(FComparator.Instance) then
    Result := ITypeComparator(FComparator.Instance).Compare(Data1, Data2, Param) else
    Result := Integer(Data1) - Integer(Data2);
end;

procedure TSilTypeHandler.Initialize(Data: HData; Count: Integer);
begin
  if FSize > 0 then System.FillChar(Data^, FSize * Count, 0);
end;

procedure TSilTypeHandler.Finalize(Data: HData; Count: Integer);
begin
  if FSize > 0 then System.FillChar(Data^, FSize * Count, 0);
end;

procedure TSilTypeHandler.Copy(Data, Source: HData; Count, Step: Integer);
begin
  if FSize > 0 then System.Move(Source^, Data^, FSize * Count)
end;

function TSilTypeHandler.GetSize: Integer;
begin
  Result := 0;
  if Assigned(FHook) then Inc(Result, FHook.Size);
end;

{ TSilTypeinfoHandler }

type
  TFieldInfo = packed record
    TypeInfo: PPTypeInfo;
    Offset: Cardinal;
  end;

  PFieldTable = ^TFieldTable;
  TFieldTable = packed record
    //X: Word;
    Size: Cardinal;
    Count: Cardinal;
    Fields: array [0..0] of TFieldInfo;
  end;

procedure   _Copy(dest, source, typeInfo: Pointer; cnt: Integer); register;
asm
    PUSH    cnt
    CALL    System.@CopyArray
end;

const
  tkNonInitializable = [
      tkInteger,
      tkChar,
      tkEnumeration,
      tkMethod,
      tkClass,
      tkFloat,
      tkString,
      tkWChar,
      tkInt64,
      tkSet
    ];

const
  tkInitializable = [
      tkLString,
      tkWString,
      tkVariant,
      tkArray,
      tkRecord,
      tkInterface,
      tkDynArray
    ];

const
  HandlerClass: array[Boolean] of TSilTypeinfoHandlerClass = (TSilTypeinfoStaticHandler, TSilTypeinfoDynamicHandler);

class function TSilTypeinfoHandler.CreateHandler(TypeInfo: PTypeInfo; const Compare: ITypeComparator; Size: Integer; const Hook: ITypeHandler): ITypeinfoHandler;
begin
  Result := HandlerClass[TypeInfo.Kind in tkInitializable].Create(TypeInfo, Compare, Size, Hook);
end;

constructor TSilTypeinfoHandler.Create(TypeInfo: PTypeInfo; const Compare: ITypeComparator; Size: Integer; const Hook: ITypeHandler);
begin
  inherited Create(Compare, SizeOf(TypeInfo) + Size, Hook);
  FTypeInfo := TypeInfo;
end;

destructor TSilTypeinfoHandler.Destroy;
begin
  FTypeInfo := nil;
  inherited;
end;

function TSilTypeinfoHandler.GetTypeInfo: PTypeInfo;
begin
  Result := FTypeInfo;
end;

class function TSilTypeinfoHandler.SizeOf(TypeInfo: PTypeInfo): Integer;
begin
  if Assigned(TypeInfo) then
    case TypeInfo.Kind of
      tkInteger, tkChar, tkEnumeration, tkSet, tkWChar:
        case  GetTypeData(TypeInfo).OrdType of
          otSByte:    Result := System.SizeOf(Shortint);
          otUByte:    Result := System.SizeOf(Byte);
          otSWord:    Result := System.SizeOf(Smallint);
          otUWord:    Result := System.SizeOf(Word);
          otSLong:    Result := System.SizeOf(Integer);
          else        Result := System.SizeOf(LongWord);
        end;
      tkLString:      Result := System.SizeOf(string);
      tkInt64:        Result := System.SizeOf(Int64);
      tkFloat:
        case GetTypeData(TypeInfo).FloatType of
          ftSingle:   Result := System.SizeOf(Single);
          ftDouble:   Result := System.SizeOf(Double);
          ftExtended: Result := System.SizeOf(Extended);
          ftComp:     Result := System.SizeOf(Comp);
          else        Result := System.SizeOf(Currency);
        end;
      tkRecord:       Result := PFieldTable(GetTypeData(TypeInfo)).Size;
      tkInterface:    Result := System.SizeOf(IUnknown);
      tkWString:      Result := System.SizeOf(WideString);
      tkVariant:      Result := System.SizeOf(Variant);
      else            raise Sil.Error.Create('Unsupported TypeKind: %s', [Sil.Enum.Name(System.TypeInfo(TTypeKind), Ord(TypeInfo.Kind), 'tk')]);
    end
  else
    raise Sil.Error.Create('Unsupported Type: TypeInfo = nil');
end;

{ TSilTypeinfoDynamicHandler }

procedure TSilTypeinfoDynamicHandler.Initialize(Data: HData; Count: Integer);
asm
    MOV     EAX, TSilTypeinfoHandler[EAX].FTypeInfo
    XCHG    EDX, EAX
    CALL    System.@InitializeArray
end;

procedure TSilTypeinfoDynamicHandler.Finalize(Data: HData; Count: Integer);
asm
    MOV     EAX, TSilTypeinfoHandler[EAX].FTypeInfo
    XCHG    EDX, EAX
    CALL    System.@FinalizeArray
end;

procedure TSilTypeinfoDynamicHandler.Copy(Data, Source: HData; Count, Step: Integer);
begin
  Step := FSize * Step;
  for Count := Count - 1 downto 0 do
  begin
    _Copy(Data, Source, FTypeInfo, 1);
    if Count = 0 then Break;
    Inc(PByte(Data), Step);
    Inc(PByte(Source), Step);
  end;
end;

{ TSilTypeCompositeHandler }

constructor TSilTypeCompositeHandler.Create(const Handlers: array of ITypeHandler; const Compare: ITypeComparator; Size: Integer; const Hook: ITypeHandler);
var
  I: Integer;
begin
  inherited Create(Compare, Size + DoGetSize(Handlers), Hook);
  SetLength(FHandlers, Length(Handlers));
  for I := Low(Handlers) to High(Handlers) do
    FHandlers[I] := Handlers[I];
end;

destructor TSilTypeCompositeHandler.Destroy;
begin
  FHandlers := nil;
  inherited;
end;

procedure TSilTypeCompositeHandler.Initialize(Data: HData; Count: Integer);
var
  I, Offset: Integer;
begin
  for Count := Count - 1 downto 0 do
  begin
    Offset := 0;
    for I := Low(FHandlers) to High(FHandlers) do
      with FHandlers[I] do
      begin
        Initialize(HData(Integer(Data) + Offset));
        Inc(Offset, Size);
      end;
    Inc(PByte(Data), Size)
  end;
end;

procedure TSilTypeCompositeHandler.Finalize(Data: HData; Count: Integer);
var
  I, Offset: Integer;
begin
  for Count := Count - 1 downto 0 do
  begin
    Offset := Size;
    for I := High(FHandlers) downto Low(FHandlers) do
      with FHandlers[I] do
      begin
        Dec(Offset, Size);
        if Size < 0 then Break;
        Finalize(HData(Integer(Data) + Offset));
      end;
    Inc(PByte(Data), Size)
  end;
end;

procedure TSilTypeCompositeHandler.Copy(Data, Source: HData; Count, Step: Integer);
var
  I, Offset: Integer;
begin
  Step := Size * Step;
  for Count := Count - 1 downto 0 do
  begin
    Offset := 0;
    for I := Low(FHandlers) to High(FHandlers) do
      with FHandlers[I] do
      begin
        Copy(HData(Integer(Data) + Offset), HData(Integer(Source) + Offset));
        Inc(Offset, Size);
      end;
    if Count = 0 then Break;
    Inc(PByte(Data), Step);
    Inc(PByte(Source), Step);
  end;
end;

function TSilTypeCompositeHandler.DoGetSize(const Handlers: array of ITypeHandler): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := Low(Handlers) to High(Handlers) do
    with Handlers[I] do
      Inc(Result, Size);
end;

function TSilTypeCompositeHandler.Compare(Data1, Data2: HData; Param: Pointer): Integer;
var
  Handler: ITypeHandler;
  Size: Integer;
  Index: Integer;
begin
  Result := -1;

  for Index := Low(FHandlers) to High(FHandlers) do
  begin
    Handler := FHandlers[Index];

    Result := Handler.Compare(Data1, Data2, Param);

    if Result <> 0 then
      Break;

    Size := Handler.Size;
    
    Inc(PByte(Data1), Size);
    Inc(PByte(Data2), Size);
  end;
end;

{ TSilTypeAllocator }

constructor TSilTypeAllocator.Create(const Handler: ITypeHandler; Offset: Integer);
begin
  inherited Create;
  FHandler := Handler;
  FOffset := Offset;
end;

destructor TSilTypeAllocator.Destroy;
begin
  FHandler := nil;
  inherited;
end;

function TSilTypeAllocator.Get(Count: Integer): HData;
var
  Size: LongWord;
begin
  Size := FOffset + Count * FHandler.Size;
  GetMem(Result, Size);
  FillChar(Result^, Size, 0);
  FHandler.Initialize(HData(Integer(Result) + FOffset), Count);
end;

procedure TSilTypeAllocator.Release(var Data: HData; Count: Integer);
var
  Size: LongWord;
begin
  Size := FOffset + Count * FHandler.Size;
  FHandler.Finalize(HData(Integer(Data) + FOffset), Count);
  if FOffset > 0 then FillChar(Data^, Size, 0);
  FreeMem(Data, Size);
  Data := nil;
end;

function TSilTypeAllocator.GetHandler: ITypeHandler;
begin
  Result := FHandler;
end;

{ TSilTypeMethodCompare }

constructor TSilTypeMethodCompare.Create(Compare: TTypeCompare);
begin
  inherited Create;
  FCompare := Compare;
end;

destructor TSilTypeMethodCompare.Destroy;
begin
  FCompare := nil;
  inherited;
end;

function TSilTypeMethodCompare.Compare(Data1, Data2: HData; Param: Pointer): Integer;
begin
  if Assigned(FCompare) then
    Result := FCompare(Data1, Data2, Param) else
    Result := Integer(Data1) - Integer(Data2);
end;

{ TSilTypeNestedCompare }

constructor TSilTypeNestedCompare.Create(Compare: Pointer);
begin
  inherited Create(Compare, GetCallerFrame());
end;

destructor TSilTypeNestedCompare.Destroy;
begin
  inherited;
end;

function TSilTypeNestedCompare.Compare(Data1, Data2: HData; Param: Pointer): Integer;
begin
  Result := Invoke(Data1, Data2, Param)
end;

{ TSilInterfaceComparator }

constructor TSilInterfaceComparator.Create(const Comparer: ITypeComparator);
begin
  inherited Create;
  FComparer := Comparer;
end;

destructor TSilInterfaceComparator.Destroy;
begin
  FComparer := nil;
  inherited;
end;

function TSilInterfaceComparator.Compare(Data1, Data2: HData; Param: Pointer): Integer;
begin
  if not Assigned(FComparer) then
    Result := Ref.Compare(PUnknown(Data1)^, PUnknown(Data2)^, Param) else
    Result := FComparer.Compare(Data1, Data2, Param);
end;

{ TSilPointerComparator }

constructor TSilPointerComparator.Create(const Comparer: ITypeComparator);
begin
  inherited Create;
  FComparer := Comparer;
end;

destructor TSilPointerComparator.Destroy;
begin
  FComparer := nil;
  inherited;
end;

function TSilPointerComparator.Compare(Data1, Data2: HData; Param: Pointer): Integer;
begin
  if not Assigned(FComparer) then
    Result := Integer(LongWord(Data1^) - LongWord(Data2^)) else
    Result := FComparer.Compare(Data1, Data2, Param);
end;

end.
