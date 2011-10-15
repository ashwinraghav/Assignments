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

unit SilLiTypeInfo;

{$I Defines.inc}

interface

uses
  SilBeTypes,
  SilBeTypeInfo,
  SilLiEnumerator,
  SilLiList,
  SilLiStringList;

type
  ITypeInfo = interface;
  ITypeData = interface;
  ITypeProperties = interface;
  ITypeProperty = interface;
  ITypeParams = interface;
  ITypeParam = interface;
  ITypeMethods = interface;
  ITypeOrdinal = interface;
  ITypeEnum = interface;
  ITypeSet = interface;
  ITypeFloat = interface;
  ITypeString = interface;
  ITypeClass = interface;
  ITypeMethod = interface;
  ITypeInterface = interface;
  ITypeLarge = interface;
  ITypeStaticArray = interface;
  ITypeDynarray = interface;

  ITypeInfo = interface
    ['{E6B1F013-764C-4B36-8264-952C6481A50C}']
    function GetName: string;
    function GetTypeKind: TTypeKind;
    function GetData: ITypeData;
    function GetTypeInfo: PTypeInfo;
    property Name: string read GetName;
    property TypeKind: TTypeKind read GetTypeKind;
    property Data: ITypeData read GetData;
    property Ptr: PTypeInfo read GetTypeInfo;
  end;

  ITypeData = interface (ITypeInfo)
    ['{9A3F74B7-E35A-4906-9486-DCA8241A4B22}']
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
    property AsOrdinal: ITypeOrdinal read GetAsOrdinal;
    property AsEnum: ITypeEnum read GetAsEnum;
    property AsSet: ITypeSet read GetAsSet;
    property AsFloat: ITypeFloat read GetAsFloat;
    property AsString: ITypeString read GetAsString;
    property AsClass: ITypeClass read GetAsClass;
    property AsMethod: ITypeMethod read GetAsMethod;
    property AsInterface: ITypeInterface read GetAsInterface;
    property AsLarge: ITypeLarge read GetAsLarge;
    property AsArray: ITypeStaticArray read GetAsArray;
    property AsDynarray: ITypeDynarray read GetAsDynarray;
  end;

  ITypeItems = interface
    ['{B2D7A41C-4571-4B53-82A6-F46706109A0A}']
    function GetCount: Integer;
    property Count: Integer read GetCount;
  end;

  ITypeProperties = interface (ITypeItems)
    ['{C9E5600C-F79E-404A-96B5-D2116DC89B97}']
    function GetItem(Index: Integer): ITypeProperty;
    function Enumerate(var Enum: IEnumerator; out Item: ITypeProperty): Boolean;
    property Item[Indx: Integer]: ITypeProperty read GetItem; default;
  end;

  ITypeProperty = interface
    ['{B77347DD-CA62-4F9F-A4C7-6402D4A4570F}']
    function GetGetterProc: Pointer;
    function GetSetterProc: Pointer;
    function GetStoredProc: Pointer;
    function GetIndex: Integer;
    function GetDefault: Integer;
    function GetNameIndex: SmallInt;
    function GetName: string;
    property GetterProc: Pointer read GetGetterProc;
    property SetterProc: Pointer read GetSetterProc;
    property StoredProc: Pointer read GetStoredProc;
    property Index: Integer read GetIndex;
    property Default: Integer read GetDefault;
    property NameIndex: SmallInt read GetNameIndex;
    property Name: string read GetName;
  end;

  ITypeParams = interface (ITypeItems)
    ['{7882D45D-4407-478C-AEB6-BB13435AC27E}']
    function GetItem(Index: Integer): ITypeParam;
    function Enumerate(var Enum: IEnumerator; out Item: ITypeParam): Boolean;
    property Item[Indx: Integer]: ITypeParam read GetItem; default;
  end;

  ITypeBaseParam = interface
    ['{651EF944-D49A-471C-B405-704B0D8A2DA1}']
    function GetMethod: ITypeMethod;
    function GetTypeName: string;
    function GetBaseType: ITypeInfo;
    property Method: ITypeMethod read GetMethod;
    property TypeName: string read GetTypeName;
    property BaseType: ITypeInfo read GetBaseType;
  end;

  ITypeParam = interface (ITypeBaseParam)
    ['{17640C95-18AF-4E91-96E5-48CFD0B4512E}']
    function GetFlags: TParamAttributes;
    function GetParamName: string;
    property Flags: TParamAttributes read GetFlags;
    property ParamName: string read GetParamName;
  end;

  ITypeMethods = interface (ITypeItems)
    ['{9B57B3B7-99C4-4772-9295-56527BD5B1D2}']
    function GetItem(Index: Integer): ITypeMethod;
    function Enumerate(var Enum: IEnumerator; out Item: ITypeMethod): Boolean;
    function Find(const Name: string; out Item: ITypeMethod): Boolean;
    function Get(const Name: string): ITypeMethod;
    property Item[Indx: Integer]: ITypeMethod read GetItem; default;
  end;

  ITypeOrdinal = interface(ITypeInfo)
    ['{D10130EC-9949-4677-8CCB-FDFD47477974}']
    function GetOrdinalKind: TOrdType;
    function GetMinValue: Integer;
    function GetMaxValue: Integer;
    property OrdinalKind: TOrdType read GetOrdinalKind;
    property MinValue: Integer read GetMinValue;
    property MaxValue: Integer read GetMaxValue;
  end;

  ITypeEnum = interface(ITypeOrdinal)
    ['{DDAE24FA-7712-4E19-A176-09AB93CE8D07}']
    function GetBaseType: ITypeOrdinal;
    function GetNames: IStrings;
    property BaseType: ITypeOrdinal read GetBaseType;
    property Names: IStrings read GetNames;
  end;

  ITypeSet = interface(ITypeInfo)
    ['{9FA0B3D5-0409-492A-A95C-7599D71474CB}']
    function GetEnumType: ITypeEnum;
    property EnumType: ITypeEnum read GetEnumType;
  end;

  ITypeFloat = interface(ITypeInfo)
    ['{2E3CF25D-ED9A-4013-AD1B-7D9F696D5954}']
    function GetFloatType: TFloatType;
    property FloatType: TFloatType read GetFloatType;
  end;

  ITypeString = interface(ITypeInfo)
    ['{7BC5DF60-2A43-4F31-992D-8E6289F2165D}']
    function GetCharType: ITypeInfo;
    function GetMaxLength: Integer;
    property CharType: ITypeInfo read GetCharType;
    property MaxLength: Integer read GetMaxLength;
  end;

  ITypeLarge = interface(ITypeInfo)
    ['{5B1FFA2A-97EE-491D-B153-10F6111852AF}']
    function GetMinValue: LargeInt;
    function GetMaxValue: LargeInt;
    property MinValue: LargeInt read GetMinValue;
    property MaxValue: LargeInt read GetMaxValue;
  end;

  ITypeClass = interface(ITypeInfo)
    ['{9A2B979F-F0CF-4BE2-AC9C-DE17E12C9AFA}']
    function GetClassType: TClass;
    function GetParentType: ITypeClass;
    function GetPropList: ITypeProperties;
    property ClassType: TClass read GetClassType;
    property ParentType: ITypeClass read GetParentType;
    property PropList: ITypeProperties read GetPropList;
  end;

  ITypeMethod = interface(ITypeInfo)
    ['{5E3D4D71-4BD8-4169-BF39-05D9F43DD30F}']
    function GetMethodKind: TMethodKind;
    function GetCallingKind: TCallingKind;
    function GetParams: ITypeParams;
    function GetResult: ITypeParam;
    property MethodKind: TMethodKind read GetMethodKind;
    property CallingKind: TCallingKind read GetCallingKind;
    property Params: ITypeParams read GetParams;
    property Result: ITypeParam read GetResult;
  end;

  ITypeInterface = interface(ITypeInfo)
    ['{E1E14463-58EA-4FA4-B022-8B26DB7D136C}']
    function GetParentType: ITypeInterface;
    function GetFlags: TIntfFlagsBase;
    function GetGUID: TGUID;
    function GetMethodCount: Integer;
    function GetMethods: ITypeMethods;
    property ParentType: ITypeInterface read GetParentType;
    property Flags: TIntfFlagsBase read GetFlags;
    property GUID: TGUID read GetGUID;
    property MethodCount: Integer read GetMethodCount;
    property Methods: ITypeMethods read GetMethods;
  end;

  ITypeInterfaceMethod = interface (ITypeMethod)
    ['{89A5F663-1186-4D44-B809-A8A1E614536A}']
    function GetOwner: ITypeInterface;
    function GetIndex: Integer;
    function GetPosition: Integer;
    property Owner: ITypeInterface read GetOwner;
    property Index: Integer read GetIndex;
    property Position: Integer read GetPosition;
  end;

  ITypeArray = interface(ITypeInfo)
    ['{940BAC1E-6E2A-4368-AB22-A227A0234FCB}']
    function GetBaseType: ITypeInfo;
    function GetElemType: ITypeInfo;
    property BaseType: ITypeInfo read GetBaseType;
    property ElemType: ITypeInfo read GetElemType;
  end;

  ITypeStaticArray = interface(ITypeArray)
    ['{90249101-9D0A-42BD-BA07-8ED1A8DD9A9D}']
    function GetElemCount: Integer;
    property ElemCount: Integer read GetElemCount;
  end;

  ITypeDynarray = interface(ITypeArray)
    ['{1948C087-145A-4484-A6D5-B248D1D81F21}']
    function GetElemSize: Integer;
    function GetVarType: Integer;
    property ElemSize: Integer read GetElemSize;
    property VarType: Integer read GetVarType;
  end;

implementation
end.
