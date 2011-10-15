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

unit SilLmDelphiTypeInfo;

{$I Defines.inc}

interface

uses
  SilBeTypes,
  SilBeTypeInfo,

  SilLeDelphiTypeInfo,

  SilLiEnumerator,
  SilLiStringList,
  SilLiTypeInfo,

  //SilLiDelphiTypeInfo,

  SilLkTypeInfo,
  SilLmInterfaceList;

type
  TSilDelphiTypeDef = class(
    TSilTypeDef{,
    IDelphiTypeInfo} )
  private
    FInfo: PTypeInfo;
  protected // ITypeDef
    function GetName: string; override;
    function GetTypeKind: TTypeKind; override;
    function GetData: ITypeData; override;
  protected // IDelphiTypeInfo
    function GetInfo: PTypeInfo;
  protected
    class function DoLookup(const Data: Pointer): TSilTypeInfoClass; override;
  public
    constructor CreateNew(const Data: Pointer); override;
    destructor Destroy; override;
  end;

  TSilDelphiTypeUnknown = class(TSilDelphiTypeDef);

  TSilDelphiTypeData = class(
    TSilDelphiTypeDef,
    ITypeData )
  private
    FData: PTypeData;
  protected // ITypeData
    function GetAsOrdinal: ITypeOrdinal;
    function GetAsEnum: ITypeEnum;
    function GetAsSet: ITypeSet;
    function GetAsFloat: ITypeFloat;
    function GetAsString: ITypeString;
    function GetAsClass: ITypeClass;
    function GetAsMethod: ITypeMethod;
    function GetAsInterface: ITypeInterface;
    function GetAsLarge: ITypeLarge;
    function GetAsArray: ITypeStaticArray;
    function GetAsDynarray: ITypeDynarray;
  public
    constructor CreateNew(const Data: Pointer); override;
    destructor Destroy; override;
  end;

  TSilDelphiTypeVariant = class(TSilDelphiTypeData);

  TSilDelphiTypeOrdinal = class(
    TSilDelphiTypeData,
    ITypeOrdinal )
  protected // ITypeOrdinal
    function GetOrdinalKind: TOrdType;
    function GetMinValue: Integer;
    function GetMaxValue: Integer;
  end;

  TSilDelphiTypeEnum = class(
    TSilDelphiTypeOrdinal,
    ITypeEnum )
  private
    FBaseType: ITypeOrdinal;
    FNames: IStrings;
    function DoBuildNames: IStringList;
  protected // ITypeEnum
    function GetBaseType: ITypeOrdinal;
    function GetNames: IStrings;
  end;

  TSilDelphiTypeSet = class(
    TSilDelphiTypeData,
    ITypeSet )
  private
    FBaseType: ITypeEnum;
  protected // ITypeSet
    function GetEnumType: ITypeEnum;
  end;

  TSilDelphiTypeFloat = class(
    TSilDelphiTypeData,
    ITypeFloat )
  protected // ITypeFloat
    function GetFloatType: TFloatType;
  end;

  TSilDelphiTypeString = class(
    TSilDelphiTypeData,
    ITypeString )
  private
    FCharType: ITypeInfo;
    procedure DoBuildCharType;
  protected // ITypeString
    function GetCharType: ITypeInfo;
    function GetMaxLength: Integer; virtual;
  end;

  TSilDelphiTypeShortString = class(
    TSilDelphiTypeString )
  protected // ITypeString
    function GetMaxLength: Integer; override;
  end;

  TSilDelphiTypeLarge = class(
    TSilDelphiTypeData,
    ITypeLarge )
  protected // ITypeLarge
    function GetMinValue: LargeInt;
    function GetMaxValue: LargeInt;
  end;

  TSilDelphiTypeInterface = class(
    TSilDelphiTypeData,
    ITypeInterface )
  private
    FPropData: PPropData;
    FParentType: ITypeInterface;
    FMethods: ITypeMethods;
    procedure DoBuildMethods;
  protected // ITypeInterface
    function GetParentType: ITypeInterface;
    function GetFlags: TIntfFlagsBase;
    function GetGUID: TGUID;
    function GetMethodCount: Integer;
    function GetMethods: ITypeMethods;
  public
    constructor CreateNew(const Data: Pointer); override;
  end;

  TSilDelphiTypeMethods = class(
    TSilInterfaceList,
    ITypeMethods )
  protected
    function Add(const Item: IUnknown): Integer; override;
  protected // ITypeMethods
    function GetItem(Index: Integer): ITypeMethod;
    function Enumerate(var Enum: IEnumerator; out Item: ITypeMethod): Boolean; reintroduce;
    function Find(const Name: string; out Item: ITypeMethod): Boolean;
    function Get(const Name: string): ITypeMethod;
  end;

  TSilDelphiInterfaceMethod = class(
    TSilTypeInfo,
    ITypeMethod,
    ITypeInterfaceMethod )
  private
    FOwner: Pointer;
    FIndex: Integer;
    FData: TMethodData;
    FResult: ITypeParam;
    FParams: ITypeParams;
    procedure DoBuildParams(const ParamList: array of TParamRec);
  protected // ITypeInfo
    function GetName: string; override;
    function GetTypeKind: TTypeKind; override;
  protected // ITypeMethod
    function GetMethodKind: TMethodKind;
    function GetCallingKind: TCallingKind;
    function GetParams: ITypeParams;
    function GetResult: ITypeParam;
  protected // ITypeInterfaceMethod
    function GetOwner: ITypeInterface;
    function GetIndex: Integer;
    function GetPosition: Integer;
  public
    constructor Create(const Owner: ITypeInterface; const Index: Integer; const Method: PMethodRec);
    property Owner: ITypeInterface read GetOwner;
  end;

  TSilDelphiParams = class(
    TSilInterfaceList,
    ITypeParams )
  protected
    function Add(const Item: IUnknown): Integer; override;
  protected
    function GetItem(Index: Integer): ITypeParam;
    function Enumerate(var Enum: IEnumerator; out Item: ITypeParam): Boolean; reintroduce;
  end;

  TSilDelphiBaseParam = class(
    TInterfacedObject,
    ITypeBaseParam )
  private
    FMethod: Pointer;
    FData: TParamData;
    FBaseType: ITypeInfo;
  protected // ITypeBaseParam
    function GetMethod: ITypeMethod;
    function GetTypeName: string;
    function GetBaseType: ITypeInfo;
  public
    constructor Create(const Method: ITypeMethod; const Param: PParamData);
    destructor Destroy; override;
  end;

  TSilDelphiParam = class(
    TSilDelphiBaseParam,
    ITypeParam )
  private
    FInfo: TParamInfo;
  protected // ITypeParam
    function GetFlags: TParamAttributes;
    function GetParamName: string;
  public
    constructor Create(const Method: ITypeMethod; const Param: PParamRec);
    destructor Destroy; override;
  end;

  TSilDelphiResult = class(
    TSilDelphiBaseParam,
    ITypeParam )
  protected // ITypeParam
    function GetFlags: TParamAttributes;
    function GetParamName: string;
  end;

  TSilDelphiTypeStaticArray = class(
    TSilDelphiTypeData,
    ITypeArray,
    ITypeStaticArray )
  protected // ITypeArray
    function GetBaseType: ITypeInfo;
    function GetElemType: ITypeInfo;
  protected // ITypeStaticArray
    function GetElemCount: Integer;
  end;

  TSilDelphiTypeDynarray = class(
    TSilDelphiTypeData,
    ITypeArray,
    ITypeDynarray )
  private
    FBaseType: ITypeInfo;
    FElemType: ITypeInfo;
  protected // ITypeArray
    function GetBaseType: ITypeInfo;
    function GetElemType: ITypeInfo;
  protected // ITypeDynarray
    function GetElemSize: Integer;
    function GetVarType: Integer;
  end;

implementation

uses
  TypInfo,
  SilLtList;

const
  MTypeClasses: array[TTypeKind] of TSilTypeDefClass =
    (
      TSilDelphiTypeUnknown,        // tkUnknown,
      TSilDelphiTypeOrdinal,        // tkInteger,
      TSilDelphiTypeOrdinal,        // tkChar,
      TSilDelphiTypeEnum,           // tkEnumeration,
      TSilDelphiTypeFloat,          // tkFloat,
      TSilDelphiTypeSet,            // tkSet,
      TSilDelphiTypeDef,            // tkMethod,
      TSilDelphiTypeShortString,    // tkSString,
      TSilDelphiTypeString,         // tkLString,
      TSilDelphiTypeString,         // tkAString,
      TSilDelphiTypeString,         // tkWString,
      TSilDelphiTypeVariant,        // tkVariant,
      TSilDelphiTypeStaticArray,    // tkArray,
      TSilDelphiTypeDef,            // tkRecord,
      TSilDelphiTypeInterface,      // tkInterface,
      TSilDelphiTypeDef,            // tkClass,
      nil,                          // tkObject,
      TSilDelphiTypeOrdinal,        // tkWChar,
      nil,                          // tkBool,
      TSilDelphiTypeLarge,          // tkInt64,
      nil,                          // tkQWord,
      TSilDelphiTypeDynarray,       // tkDynArray
      nil                           // tkInterfaceRaw
    );

{ TSilDelphiTypeDef }

constructor TSilDelphiTypeDef.CreateNew(const Data: Pointer);
begin
  inherited;
  FInfo := Data;
end;

destructor TSilDelphiTypeDef.Destroy;
begin
  FInfo := nil;
  inherited;
end;

class function TSilDelphiTypeDef.DoLookup(const Data: Pointer): TSilTypeInfoClass;
begin
  Result := MTypeClasses[PtypeInfo(Data).Kind];
end;

function TSilDelphiTypeDef.GetData: ITypeData;
begin
  Result := Self as TSilDelphiTypeData;
end;

function TSilDelphiTypeDef.GetTypeKind: TTypeKind;
begin
  Result := FInfo.Kind;
end;

function TSilDelphiTypeDef.GetName: string;
begin
  Result := FInfo.Name;
end;

function TSilDelphiTypeDef.GetInfo: PTypeInfo;
begin
  Result := FInfo;
end;

{ TSilDelphiTypeData }

constructor TSilDelphiTypeData.CreateNew(const Data: Pointer);
begin
  inherited;
  FData := GetTypeData(Data);
end;

destructor TSilDelphiTypeData.Destroy;
begin
  FData := nil;
  inherited;
end;

function TSilDelphiTypeData.GetAsOrdinal: ITypeOrdinal;
begin
  Result := Self as TSilDelphiTypeOrdinal;
end;

function TSilDelphiTypeData.GetAsSet: ITypeSet;
begin
  Result := Self as TSilDelphiTypeSet;
end;

function TSilDelphiTypeData.GetAsEnum: ITypeEnum;
begin
  Result := Self as TSilDelphiTypeEnum;
end;

function TSilDelphiTypeData.GetAsFloat: ITypeFloat;
begin
  Result := Self as TSilDelphiTypeFloat;
end;

function TSilDelphiTypeData.GetAsString: ITypeString;
begin
  Result := Self as TSilDelphiTypeString;
end;

function TSilDelphiTypeData.GetAsClass: ITypeClass;
begin
end;

function TSilDelphiTypeData.GetAsDynarray: ITypeDynarray;
begin
  Result := Self as TSilDelphiTypeDynarray;
end;

function TSilDelphiTypeData.GetAsInterface: ITypeInterface;
begin
  Result := Self as TSilDelphiTypeInterface;
end;

function TSilDelphiTypeData.GetAsLarge: ITypeLarge;
begin
  Result := Self as TSilDelphiTypeLarge;
end;

function TSilDelphiTypeData.GetAsMethod: ITypeMethod;
begin
end;

function TSilDelphiTypeData.GetAsArray: ITypeStaticArray;
begin

end;

{ TSilDelphiTypeOrdinal }

function TSilDelphiTypeOrdinal.GetMaxValue: Integer;
begin
  Result := FData.MaxValue;
end;

function TSilDelphiTypeOrdinal.GetMinValue: Integer;
begin
  Result := FData.MinValue
end;

function TSilDelphiTypeOrdinal.GetOrdinalKind: TOrdType;
begin
  //Result := FData.OrdType;
end;

{ TSilDelphiTypeEnum }

function TSilDelphiTypeEnum.GetBaseType: ITypeOrdinal;
begin
  if FBaseType = nil then
    FBaseType := TSilDelphiTypeOrdinal.CreateNew(FData.BaseType{^});
  Result := FBaseType;
end;

function TSilDelphiTypeEnum.GetNames: IStrings;
begin
  if FNames = nil then
    FNames := DoBuildNames();
  Result := FNames;
end;

function TSilDelphiTypeEnum.DoBuildNames: IStringList;
var
  I: Integer;
  P: ^ShortString;
begin
  Result := ListTool.StringList();
  P := @FData.NameList;
  for I := FData.MinValue to FData.MaxValue do
  begin
    Result.Add(P^);
    Inc(Integer(P), Length(P^) + 1);
  end;
end;

{ TSilDelphiTypeSet }

function TSilDelphiTypeSet.GetEnumType: ITypeEnum;
begin
  if FBaseType = nil then
    FBaseType := TSilDelphiTypeEnum.CreateNew(FData.CompType{^});
  Result := FBaseType;
end;

{ TSilDelphiTypeFloat }

function TSilDelphiTypeFloat.GetFloatType: TFloatType;
begin
  Result := FData.FloatType;
end;

{ TSilDelphiTypeLarge }

function TSilDelphiTypeLarge.GetMinValue: LargeInt;
begin
  Result := FData.MinInt64Value;
end;

function TSilDelphiTypeLarge.GetMaxValue: LargeInt;
begin
  Result := FData.MaxInt64Value;
end;

{ TSilDelphiTypeString }

function TSilDelphiTypeString.GetCharType: ITypeInfo;
begin
  if FCharType = nil then
    DoBuildCharType();
  Result := FCharType;
end;

function TSilDelphiTypeString.GetMaxLength: Integer;
begin
  Result := MaxInt;
end;

procedure TSilDelphiTypeString.DoBuildCharType;
begin
  case FInfo.Kind of
    tkString, tkLString:
      FCharType := TSilDelphiTypeOrdinal.CreateNew(TypeInfo(Char));
    else
      FCharType := TSilDelphiTypeOrdinal.CreateNew(TypeInfo(WideChar));
  end;
end;

{ TSilDelphiTypeShortString }

function TSilDelphiTypeShortString.GetMaxLength: Integer;
begin
  Result := FData.MaxLength;
end;

{ TSilDelphiTypeStaticArray }

function TSilDelphiTypeStaticArray.GetBaseType: ITypeInfo;
begin
end;

function TSilDelphiTypeStaticArray.GetElemCount: Integer;
begin
  Result := 0;
end;

function TSilDelphiTypeStaticArray.GetElemType: ITypeInfo;
begin
end;

{ TSilDelphiTypeDynarray }

function TSilDelphiTypeDynarray.GetBaseType: ITypeInfo;
begin
  if FBaseType = nil then
    FBaseType := TSilDelphiTypeDef.Create(FData.elType^);
  Result := FBaseType;
end;

function TSilDelphiTypeDynarray.GetElemType: ITypeInfo;
begin
  if FElemType = nil then
    FElemType := TSilDelphiTypeDef.Create(FData.elType2^);
  Result := FElemType;
end;

function TSilDelphiTypeDynarray.GetElemSize: Integer;
begin
  Result := FData.elSize;
end;

function TSilDelphiTypeDynarray.GetVarType: Integer;
begin
  Result := FData.varType;
end;

{ TSilDelphiTypeInterface }

constructor TSilDelphiTypeInterface.CreateNew(const Data: Pointer);
begin
  inherited;
  FPropData := Pointer(Integer(@FData.IntfUnit) + Length(FData.IntfUnit) + 1);
end;

function TSilDelphiTypeInterface.GetFlags: TIntfFlagsBase;
begin
  Result := FData.IntfFlags;
end;

function TSilDelphiTypeInterface.GetGUID: TGUID;
begin
  Result := FData.Guid;
end;

function TSilDelphiTypeInterface.GetMethodCount: Integer;
begin
  Result := FPropData.PropCount;
end;

function TSilDelphiTypeInterface.GetMethods: ITypeMethods;
begin
  if FMethods = nil then
    DoBuildMethods;
  Result := FMethods;
end;

function TSilDelphiTypeInterface.GetParentType: ITypeInterface;
begin
  if (FParentType = nil) and (FData.IntfParent <> nil) then
    FParentType := TSilDelphiTypeInterface.CreateNew(FData.IntfParent^);
  Result := FParentType;
end;

procedure TSilDelphiTypeInterface.DoBuildMethods;
var
  List: IInterfaceList;
  Methods: ^TIntfMethods;
  Method: TMethodRec;
  Param: PParamRec;
  P: PByte;
  MethodIndex, ParamIndex: Integer;
begin
  List := TSilDelphiTypeMethods.Create();

  Methods := @FPropData.PropList;

  if Methods.Count <> $FFFF then
  begin
    P := @Methods.Data;
    for MethodIndex := 0 to Methods.Count - 1 do
    begin
      Method.Data.Name := PShortString(P)^;
      Inc(P, P^ + 1);
      Method.Data.Kind := TMethodKind(P^);
      Inc(P, SizeOf(Byte));
      Method.Data.Call := TCallingKind(P^);
      Inc(P, SizeOf(Byte));
      SetLength(Method.Params.List, P^);
      Inc(P, SizeOf(Byte));
      for ParamIndex := Low(Method.Params.List) to High(Method.Params.List) do
      begin
        Param := @Method.Params.List[ParamIndex];
        Inc(P, SizeOf(Byte));
        Param.Info.Flags := TParamAttributes(P^);
        Param.Info.Name := PShortString(P)^;
        Inc(P, P^ + 1);
        Param.Data.DataType := PShortString(P)^;
        Inc(P, P^ + 1);
        Param.Data.Info := PPointer(P)^;
        Inc(P, SizeOf(PPTypeInfo));
      end;

      if Method.Data.Kind = mkFunction then
      begin
        Method.Params.Result.DataType := PShortString(P)^;
        Inc(P, P^ + 1);
        Method.Params.Result.Info := PPointer(P)^;
        Inc(P, SizeOf(PPTypeInfo));
      end else
      begin
        Method.Params.Result.DataType := '';
        Method.Params.Result.Info := nil;
      end;

      List.Add(TSilDelphiInterfaceMethod.Create(Self, MethodIndex, @Method));
    end;
  end;

  FMethods := List as ITypeMethods;

end;

{ TSilDelphiTypeMethods }

function TSilDelphiTypeMethods.Add(const Item: IUnknown): Integer;
begin
  Result := inherited Add(Item as ITypeMethod);
end;

function TSilDelphiTypeMethods.Enumerate(var Enum: IEnumerator; out Item: ITypeMethod): Boolean;
begin
  Result := inherited Enumerate(Enum, Item);
end;

function TSilDelphiTypeMethods.Find(const Name: string; out Item: ITypeMethod): Boolean;
var
  Enum: IEnumerator;
begin
  while inherited Enumerate(Enum, Item) do
    if Sil.Str.CompareText(Item.Name, Name, True) = 0 then
    begin
      Result := True;
      Exit;
    end;
  Result := False;
end;

function TSilDelphiTypeMethods.Get(const Name: string): ITypeMethod;
begin
  if not Find(Name, Result) then
    raise Sil.Error.Create('Method not found'); // localize!!!!!!!!
end;

function TSilDelphiTypeMethods.GetItem(Index: Integer): ITypeMethod;
begin
  Result := inherited GetItem(Index) as ITypeMethod
end;

{ TSilDelphiInterfaceMethod }

constructor TSilDelphiInterfaceMethod.Create(const Owner: ITypeInterface; const Index: Integer; const Method: PMethodRec);
begin
  inherited Create;
  FOwner := Pointer(Owner);
  FData := Method.Data;
  FIndex := Index;
  FResult := TSilDelphiResult.Create(Self, @Method.Params.Result);
  DoBuildParams(Method.Params.List);
end;

function TSilDelphiInterfaceMethod.GetName: string;
begin
  Result := FData.Name;
end;

function TSilDelphiInterfaceMethod.GetTypeKind: TTypeKind;
begin
  Result := tkMethod;
end;

function TSilDelphiInterfaceMethod.GetMethodKind: TMethodKind;
begin
  Result := FData.Kind;
end;

function TSilDelphiInterfaceMethod.GetCallingKind: TCallingKind;
begin
  Result := FData.Call;
end;

function TSilDelphiInterfaceMethod.GetParams: ITypeParams;
begin
  Result := FParams;
end;

function TSilDelphiInterfaceMethod.GetResult: ITypeParam;
begin
  Result := FResult;
end;

procedure TSilDelphiInterfaceMethod.DoBuildParams(const ParamList: array of TParamRec);
var
  List: IInterfaceList;
  I: Integer;
begin
  List := TSilDelphiParams.Create();
  for I := Low(ParamList) to High(ParamList) do
    List.Add(TSilDelphiParam.Create(Self, @ParamList[I]));
  FParams := List as ITypeParams;
end;

function TSilDelphiInterfaceMethod.GetOwner: ITypeInterface;
begin
  Result := ITypeInterface(FOwner);
end;

function TSilDelphiInterfaceMethod.GetIndex: Integer;
begin
  Result := FIndex;
end;

function TSilDelphiInterfaceMethod.GetPosition: Integer;
  function CountMethods(const Intf: ITypeInterface): Integer;
  begin
    if Intf <> nil then
      Result := CountMethods(Intf.ParentType) + Intf.MethodCount else
      Result := 0;
  end;
begin
  Result := FIndex + CountMethods(Owner.ParentType);
end;

{ TSilDelphiBaseParam }

constructor TSilDelphiBaseParam.Create(const Method: ITypeMethod; const Param: PParamData);
begin
  inherited Create;
  FMethod := Pointer(Method);
  FData := Param^;
end;

destructor TSilDelphiBaseParam.Destroy;
begin
  FBaseType := nil;
  inherited;
end;

function TSilDelphiBaseParam.GetBaseType: ITypeInfo;
begin
  if FBaseType = nil then
    FBaseType := TSilDelphiTypeDef.Create(FData.Info^);
  Result := FBaseType;
end;

function TSilDelphiBaseParam.GetMethod: ITypeMethod;
begin
  Result := ITypeMethod(FMethod);
end;

function TSilDelphiBaseParam.GetTypeName: string;
begin
  Result := FData.DataType;
end;

{ TSilDelphiParam }

constructor TSilDelphiParam.Create(const Method: ITypeMethod; const Param: PParamRec);
begin
  inherited Create(Method, @Param.Data);
  FInfo := Param.Info;
end;

destructor TSilDelphiParam.Destroy;
begin
  inherited;
end;

function TSilDelphiParam.GetFlags: TParamAttributes;
begin
  Result := FInfo.Flags;
end;

function TSilDelphiParam.GetParamName: string;
begin
  Result := FInfo.Name;
end;

{ TSilDelphiParams }

function TSilDelphiParams.Add(const Item: IInterface): Integer;
begin
  Result := inherited Add(Item as ITypeParam);
end;

function TSilDelphiParams.Enumerate(var Enum: IEnumerator; out Item: ITypeParam): Boolean;
begin
  Result := inherited Enumerate(Enum, Item);
end;

function TSilDelphiParams.GetItem(Index: Integer): ITypeParam;
begin
  Result := inherited GetItem(Index) as ITypeParam;
end;

{ TSilDelphiResult }

function TSilDelphiResult.GetFlags: TParamAttributes;
begin(*)
  Result := [paOut, paVar, pfResult];
(*)end;

function TSilDelphiResult.GetParamName: string;
begin
  Result := 'Result';
end;

end.
