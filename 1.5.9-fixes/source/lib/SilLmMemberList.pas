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

unit SilLmMemberList;

{$INCLUDE Defines.inc}

interface

uses
  SilBeTypes,
  SilBeTypeinfo,
  SilLkExtensible,

  SilLiEnumerator,
  SilLiContainerTypes,
  SilLiContainer,
  SilLiInterfaceList,
  SilLiParameters,
  SilLeMemberList,
  SilLiMemberList;

type
  PIContainerDynamic = ^IContainerDynamic;

type
  TMemberField = (
      fdMemberInstance,
      fdMemberName,
      fdMemberTypeinfo
    );

type
  TSilMemberList = class(
    TSilExtensibleObject,
    IEnumerable,
    IArguments,
    IParameters,
    IArgumentList,
    IParameterList,
    IMemberList )
  private
    FList: PIContainerDynamic;
    FPrefix: string;
  private
    procedure DoCheckList;
    function DoCompare(Data1, Data2: HData; Param: Pointer): Integer;
    function DoEnumerate(var Enum: IEnumerator; out Item: PMemberHeader): Boolean; 
    function DoFind(Field: TMemberField; Data: HData; Item: PPMemberHeader = nil; const Comparer: ITypeComparator = nil): Boolean; overload;
    function DoRemove(Field: TMemberField; Data: HData): Integer; overload;
  protected
    function GetValue(Item: PMemberHeader): Variant;
    procedure SetValue(Item: PMemberHeader; const Value: Variant);
    function HasMembers: Boolean;
  protected // IEnumerable
    function GetEnumerator(out Enum: IEnumerator; Locked: Boolean): Boolean;
  protected // IArguments
    function GetCount: Integer;
    function GetItem(const Name: string): Variant;
    function Find(const Name: string; out Value: Variant): Boolean; overload;
  protected // IParameters
    function Contains(const Name: string): Boolean;
    function Enumerate(var Enum: IEnumerator; out Item: RParameter): Boolean; overload;
    function Slice(const Name: string): IParameters;
    function Get(const Name: String; Default: Variant): Variant;
    function GetParameter(Index: Integer): RParameter;
  protected // IArgumentList
    procedure PutItem(const Name: string; const Value: Variant);
  protected // IParameterList
    procedure Merge(const Source: IParameters; Precedence: TMergePrecedence = mkSource);
    function Remove(const Name: string): Integer; overload;
    procedure Delete(Index: Integer);
    procedure Clear;
  protected // IMemberList
    function GetIsEmpty: Boolean;
    function GetFirstMember: PMemberHeader;
    function GetLastMember: PMemberHeader;
    function Enumerate(var Enum: IEnumerator; out Item: RMember): Boolean; overload;
    function Find(const Name: string; Item: PMember = nil): Boolean; overload;
    function Find(const Name: string; out Item: PMemberHeader): Boolean; overload;
    function Define(const Name: string; Member: Pointer; TypeInfo: Pointer; Default: Variant): IMemberList;
  public
    constructor Create(const List: PIContainerDynamic; const Prefix: string = ''; Locked: Boolean = False); reintroduce;    
    destructor Destroy; override;
  end;

  TSilMemberListImpl = class(TSilMemberList)
  private
    FList: IContainerDynamic;
  public 
    constructor Create(Locked: Boolean = False); 
  end;

implementation

uses
  TypInfo,
  SilBtText,
  SilBtMem,
  SilBtError,
  SilBtTypeInfo,
  SilBtStr,
  SilBtVart,
  SilLmContainerTypes,
  SilLmContainerEnumerator,
  SilLmLockableExtension,
  SilLtContainer;

procedure Convert(Item: PMemberHeader; Data: PMemberValue; const Value: Variant); overload; 
var
  TypeInfo: PTypeInfo;
begin
  TypeInfo := Item.Handler.TypeInfo; 
  case TypeInfo.Kind of
    tkInteger, tkChar, tkEnumeration, tkSet, tkWChar:
      case  GetTypeData(TypeInfo).OrdType of
        otSByte:    Data.VShortint := Value;
        otUByte:    Data.VByte := Value;
        otSWord:    Data.VSmallint := Value;
        otUWord:    Data.VWord := Value;
        otSLong:    Data.VInteger := Value;
        else        Data.VLongWord := Value;
      end;
    tkLString:      AnsiString(Data.VAnsiString) := Value;
    tkInt64:        Data.VInt64 := Value;
    tkFloat:
      case GetTypeData(TypeInfo).FloatType of
        ftSingle:   Data.VSingle := Value;
        ftDouble:   Data.VDouble := Value;
        ftExtended: Data.VExtended := Value;
        ftComp:     Data.VComp := Value;
        else        Data.VCurrency := Value;
      end;
    tkInterface:    PUnknown(@Data.VInterface)^ := Value;
    tkWString:      PWideString(@Data.VWideString)^ := Value;
    tkVariant:      PVariant(@Data.VVariant)^ := Value;
    else            raise Error.Create('Unsupported TypeKind: %s', [EnumTool.Name(System.TypeInfo(TTypeKind), Ord(TypeInfo.Kind), 'tk')]);
  end
end;

function Convert(Item: PMemberHeader; Data: PMemberValue): Variant; overload; 
var
  TypeInfo: PTypeInfo;
begin
  TypeInfo := Item.Handler.TypeInfo; 
  case TypeInfo.Kind of
    tkInteger, tkChar, tkEnumeration, tkSet, tkWChar:
      case  GetTypeData(TypeInfo).OrdType of
        otSByte:    Result := Data.VShortint;
        otUByte:    Result := Data.VByte;
        otSWord:    Result := Data.VSmallint;
        otUWord:    Result := Data.VWord;
        otSLong:    Result := Data.VInteger;
        else        Result := Data.VLongWord;
      end;
    tkLString:      Result := AnsiString(Data.VAnsiString);
    tkInt64:        Result := Data.VInt64;
    tkFloat:
      case GetTypeData(TypeInfo).FloatType of
        ftSingle:   Result := Data.VSingle;
        ftDouble:   Result := Data.VDouble;
        ftExtended: Result := Data.VExtended;
        ftComp:     Result := Data.VComp;
        else        Result := Data.VCurrency;
      end;
    tkInterface:    Result := PUnknown(@Data.VInterface)^;
    tkWString:      Result := PWideString(@Data.VWideString)^;
    tkVariant:      Result := PVariant(@Data.VVariant)^;
    else            raise Error.Create('Unsupported TypeKind: %s', [EnumTool.Name(System.TypeInfo(TTypeKind), Ord(TypeInfo.Kind), 'tk')]);
  end
end;

function AddPrefix(const Name: string; const Prefix: string = ''): string;
begin
  Result := Prefix;
  Str.Add(Result, Name, '.');
end;

function RemovePrefix(const Name: string; const Prefix: string = ''): string;
begin
  if Str.Len(Prefix) > 0 then
    Result := Str.Replace(Name, Prefix, '', True) else
    Result := Name;
end;

type
  TSilMemberValueHandler = class(TSilTypeHandler)
  protected
    procedure Initialize(Data: HData; Count: Integer); override; 
    procedure Finalize(Data: HData; Count: Integer); override;
    procedure Copy(Data: HData; Source: HData; Count, Step: Integer); override; 
  end;

{ TSilMemberValueHandler }

procedure TSilMemberValueHandler.Initialize(Data: HData; Count: Integer);
var
  Member: PMemberHeader;
begin
  Member := HData(Integer(Data) - SizeOf(Member^));
  for Count := Count - 1 downto 0 do
  begin
    Mem.Clear(Member.Value, Size, 0);
    if Count = 0 then Break;
    Inc(PByte(Member), SizeOf(Member^));
  end;
end;

procedure TSilMemberValueHandler.Finalize(Data: HData; Count: Integer);
var
  Member: PMemberHeader;
begin
  Member := HData(Integer(Data) - SizeOf(Member^));
  for Count := Count - 1 downto 0 do
  begin
    if Assigned(Member.Handler) then Member.Handler.Finalize(@Member.Value);
    if Count = 0 then Break;
    Inc(PByte(Member), SizeOf(Member^));
  end;
end;

procedure TSilMemberValueHandler.Copy(Data, Source: HData; Count, Step: Integer);
var
  Origin, Target: PMemberHeader;
begin
  Origin := HData(Integer(Source) - SizeOf(Origin^));
  Target := HData(Integer(Data) - SizeOf(Target^));
  for Count := Count - 1 downto 0 do
  begin
    if Assigned(Target.Handler) then Target.Handler.Copy(@Target.Value, @Origin.Value);
    if Count = 0 then Break;
    Inc(PByte(Origin), SizeOf(Origin^));
    Inc(PByte(Source), SizeOf(Target^));
  end;
end;

{ TSilMemberList }

constructor TSilMemberList.Create(const List: PIContainerDynamic; const Prefix: string; Locked: Boolean);
begin
  inherited Create;
  ASSERT(List <> nil);
  if Locked then Extension.Add(TSilLockableExtension); 
  FPrefix := Prefix;
  FList := List;
end;

destructor TSilMemberList.Destroy;
begin
  Clear;
  inherited;
end;

function TSilMemberList.GetEnumerator(out Enum: IEnumerator; Locked: Boolean): Boolean;
begin
  Result := HasMembers();
  if Result then Enum := TSilContainerEnumerator.Create(FList^, Sequence.Create(FList^)); 
end;

function TSilMemberList.GetCount: Integer;
begin
  if Assigned(FList^) then
    Result := FList.Items.Count else
    Result := 0;
end;

function TSilMemberList.GetItem(const Name: string): Variant;
begin
  if not Find(Name, Result) then
    Result := Vart.Unassigned;
end;

function TSilMemberList.Find(const Name: string; out Value: Variant): Boolean;
var
  Item: RMember;
begin
  Result := Find(Name, @Item);
  if Result then Value := Item.Value;
end;

function TSilMemberList.Contains(const Name: string): Boolean;
begin
  Result := Find(Name);
end;

function TSilMemberList.Enumerate(var Enum: IEnumerator; out Item: RParameter): Boolean;
var
  Data: RMember; 
begin
  Result := Enumerate(Enum, Data);
  if Result  then
  begin
    Item.Name := Data.Header.Name;
    Item.Value := Data.Value;
  end else
    Enum := nil;
end;

function TSilMemberList.Slice(const Name: string): IParameters;
begin
  Result := TSilMemberList.Create(FList, AddPrefix(Name, FPrefix), IsLockable);  
end;

function TSilMemberList.Get(const Name: String; Default: Variant): Variant;
begin
  if not Find(Name, Result) then
    Result := Default;
end;

function TSilMemberList.GetParameter(Index: Integer): RParameter;
var
  Data: PMemberHeader;
begin
  if HasMembers() and FList.Items.IsValid(Index) then
  begin
    Data := FList^[Index];
    Result.Name := Data.Name;
    Result.Value := GetValue(Data);
  end;
end;

procedure TSilMemberList.PutItem(const Name: string; const Value: Variant);
var
  Data: PMemberHeader;
begin
  if Find(Name, Data) then
    SetValue(Data, Value) else
    Define(Name, nil, nil, Value);
end;

procedure TSilMemberList.Merge(const Source: IParameters; Precedence: TMergePrecedence);
var
  Enum: IEnumerator;
  Item: RParameter;
  Value: Variant;
begin
  if Assigned(Source) then
    while Source.Enumerate(Enum, Item) do
    begin
      case Precedence of
        mkSource:
          if not Vart.IsOK(Item.Value) then
            Continue;
        mkDestination:
          if Find(Item.Name, Value) then
            if Vart.IsOK(Value) then
              Continue;
      end;
      PutItem(Item.Name, Item.Value);
    end;
end;

function TSilMemberList.Remove(const Name: string): Integer;
var
  FullName: string;
begin
  FullName := AddPrefix(Name, FPrefix);
  Result := DoRemove(fdMemberName, @FullName);
end;

procedure TSilMemberList.Delete(Index: Integer);
begin
  if HasMembers() and FList.Items.IsValid(Index) then
    FList.Items.Delete(Index);
end;

procedure TSilMemberList.Clear;
begin
  if HasMembers() then
    FList.Items.Clear;
end;

function TSilMemberList.GetIsEmpty: Boolean;
begin
  Result := HasMembers();
end;

function TSilMemberList.GetFirstMember: PMemberHeader;
begin
  ASSERT(HasMembers(), 'La lista está vacía');
  Result := PMemberHeader(FList.Value[FList.Items.First]);
end;

function TSilMemberList.GetLastMember: PMemberHeader;
begin
  ASSERT(HasMembers(), 'La lista está vacía');
  Result := PMemberHeader(FList.Value[FList.Items.Last]);
end;

function TSilMemberList.Enumerate(var Enum: IEnumerator; out Item: RMember): Boolean;
var
  Data: PMemberHeader; 
begin
  Result := DoEnumerate(Enum, Data);
  if Result then
  begin
    Item.Header.Name := RemovePrefix(Data.Name, FPrefix);
    Item.Header.Handler := Data.Handler;
    Item.Header.Member := Data.Member;
    Item.Value := GetValue(Data);
  end;
end;

function TSilMemberList.Find(const Name: string; Item: PMember): Boolean; 
var
  Data: PMemberHeader; 
begin
  Result := Find(Name, Data);
  if Result and Assigned(Item) then
  begin
    Item.Header.Name := RemovePrefix(Data.Name, FPrefix);
    Item.Header.Handler := Data.Handler;
    Item.Header.Member := Data.Member;
    Item.Value := GetValue(Data);
  end;
end;

function TSilMemberList.Find(const Name: string; out Item: PMemberHeader): Boolean;
var
  FullName: string;
begin
  FullName := AddPrefix(Name, FPrefix);
  Result := DoFind(fdMemberName, @FullName, @Item);
end;

function TSilMemberList.Define(const Name: string; Member, TypeInfo: Pointer; Default: Variant): IMemberList;
var
  Data: packed record
    Header: RMemberHeader;
    Value: RMemberValue;
  end;
  Item: HData;
begin
  DoCheckList;

  Data.Header.Name := AddPrefix(Name, FPrefix);
  Data.Header.Member := Member;

  if Assigned(TypeInfo) then
    Data.Header.Handler := Handler.Create(TypeInfo) else
    Data.Header.Handler := Handler.Create(System.TypeInfo(Variant));

  Data.Header.Handler.Initialize(@Data.Header.Value);

  FList.Add(Item, 1, @Data);

  SetValue(Item, Default);
  Result := Self;
end;

function TSilMemberList.GetValue(Item: PMemberHeader): Variant;
var
  Data: PMemberValue;
begin
  if Assigned(Item.Member) then
    Data := Item.Member else
    Data := @Item.Value;
  Result := Convert(Item, Data);
end;

function TSilMemberList.HasMembers: Boolean;
begin
  Result := Assigned(FList^) and (FList.Items.Count > 0);
end;

procedure TSilMemberList.SetValue(Item: PMemberHeader; const Value: Variant);
var
  Data: PMemberValue;
begin
  if Assigned(Item.Member) then
    Data := Item.Member else
    Data := @Item.Value;
  Convert(Item, Data, Value);
end;

procedure TSilMemberList.DoCheckList;
begin
  if not Assigned(FList^) then
    FList^ := Vector.Create(
      Handler.Compose([
        Handler.Create(TypeInfo(RMemberHeader)),
        TSilMemberValueHandler.Create(nil, SizeOf(RMemberValue))],
      Compare.Create(DoCompare)));
end;

function TSilMemberList.DoCompare(Data1, Data2: HData; Param: Pointer): Integer;
begin
  case TMemberField(Param) of
    fdMemberInstance:   Result := Integer(Data1) - Integer(Data2);
    fdMemberName:       Result := Text.Compare(string(Data1^), RMemberHeader(Data2^).Name);
    fdMemberTypeinfo:   Result := Integer(Data1) - Integer(RMemberHeader(Data2^).Handler.TypeInfo);
    else                Result := -1;
  end;
end;

function TSilMemberList.DoEnumerate(var Enum: IEnumerator; out Item: PMemberHeader): Boolean;
begin
  if Enum <> nil then
    Result := Enum.Next
  else if GetEnumerator(Enum, Lockable <> nil) then
    Result := Enum.HasMore
  else
    Result := false;

  if Result then
    Enum.Get(Item) else
    Enum := nil;
end;

function TSilMemberList.DoFind(Field: TMemberField; Data: HData; Item: PPMemberHeader; const Comparer: ITypeComparator): Boolean;
var
  Index: HItem;
begin
  Result := HasMembers() and FList.Find(Data, @Index, Comparer, Pointer(Field));
  if Result and Assigned(Item) then Item^ := FList^[Index];
end;

function TSilMemberList.DoRemove(Field: TMemberField; Data: HData): Integer;
begin
  if HasMembers() then
    Result := FList.Remove(Data, Pointer(Field)) else
    Result := -1;
end;

{ TSilMemberListImpl }

constructor TSilMemberListImpl.Create(Locked: Boolean);
begin
  inherited Create(@FList, '', Locked);
end;

end.
