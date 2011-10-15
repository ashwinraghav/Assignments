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

unit SilSmFirebirdBindings;

{$INCLUDE Defines.inc}

interface

uses
  Sil, SilContainer,
  SilSeFirebirdClient,
  SilSiFirebird,
  SilShFirebird;

type
  TFbBindingField = (
      fbidName,
      fbidBinding
    );

  TSilFirebirdBindingList = class(
    TSilObject,
    IFbBindingsInternal )
  private
    FSession: IFbSessionInternal;
    FList: IContainerDynamic;
    FSize: LongWord;
  private
    procedure DoCheckList;
    function DoCompare(Data1, Data2: HData; Param: Pointer): Integer;
  protected // IEnumerable
    function GetEnumerator(out Enum: IEnumerator; Locked: Boolean = False): Boolean; virtual;
  protected // IFbBindings
    function GetSession: IFbSession;
    function GetCount: Integer;
    function GetSize: LongWord;
    function GetValues(Index: Integer): IFbBinding;
    function GetBindingByName(const Name: string): IFbBinding;
    function Add(const Binding: IFbBinding): Integer; overload;
    function Add(const Source: IFbVariable; const Domain: RFbDomainData): IFbBinding; overload; 
    function Add(const Source: IFbVariable): IFbBinding; overload; 
    function Add(const Name: string; const Domain: RFbDomainData): IFbBinding; overload; 
    function Find(const Binding: IFbVariable; out Index: Integer): Boolean; overload;
    function Find(const Source: IFbVariable; out Item: IFbBinding): Boolean; overload;
    function Find(const Name: string; out Item: IFbBinding): Boolean; overload;
    function Find(Position: Integer; out Item: IFbBinding): Boolean; overload;
    function Enumerate(var Enum: IEnumerator; out Item: IFbBinding): Boolean; overload;
    function Remove(const Item: IFbBinding): Integer;
    procedure Clear;
  protected // IFbBindingsInternal
    function DoGetSession: IFbSessionInternal;
    function Enumerate(var Enum: IEnumerator; out Item: IFbBindingInternal): Boolean; overload;
  public
    constructor Create(const Session: IFbSessionInternal); overload;
    destructor Destroy; override;
  end;

implementation

uses
  SilLmContainerEnumerator,
  SilSfFirebirdDomain,
  SilSmFirebirdBinding;

const
  FindByName        = Pointer(fbidName);
  FindByBinding     = Pointer(fbidBinding);

{ TSilFirebirdBindingList }

constructor TSilFirebirdBindingList.Create(const Session: IFbSessionInternal);
begin
  inherited Create;
  FSession := Session;
  FSize := 0;
end;

destructor TSilFirebirdBindingList.Destroy;
begin
  FList := nil;
  FSession := nil;
  inherited;
end;

function TSilFirebirdBindingList.GetEnumerator(out Enum: IEnumerator; Locked: Boolean): Boolean;
begin
  Result := Assigned(FList) and (FList.Items.Count > 0);
  if Result then Enum := TSilContainerEnumerator.Create(FList, Sequence.Create(FList));
end;

function TSilFirebirdBindingList.GetSession: IFbSession;
begin
  Result := FSession;
end;

function TSilFirebirdBindingList.GetCount: Integer;
begin
  if Assigned(FList) then
    Result := FList.Items.Count else
    Result := 0;
end;

function TSilFirebirdBindingList.GetSize: LongWord;
begin
  Result := FSize;
end;

function TSilFirebirdBindingList.GetValues(Index: Integer): IFbBinding;
begin
  if Assigned(FList) then
    Result := IFbBinding(FList[Index]^) else
    raise Sil.Error.Create('List is empty');
end;

function TSilFirebirdBindingList.GetBindingByName(const Name: string): IFbBinding;
begin
  if not Find(Name, Result) then
    raise Sil.Error.Create('El elemento ''%s'' no está en la lista', [Name]);
end;

function TSilFirebirdBindingList.Add(const Binding: IFbBinding): Integer;
begin
  DoCheckList;  
  Result := FList.Add(@Binding);
end;

function TSilFirebirdBindingList.Add(const Source: IFbVariable; const Domain: RFbDomainData): IFbBinding;
var
  Internal: IFbBindingInternal; 
begin
  if not Find(Source, IFbBinding(Internal)) then
  begin
    Internal := IFbBindingInternal(TSilFirebirdBinding.Create(Self, Source.Name, Domain, FSize));
    Inc(FSize, Internal.Size);
  end;
  Result := Internal;
end;

function TSilFirebirdBindingList.Add(const Source: IFbVariable): IFbBinding;
begin
  Result := Add(Source, Domain(Source.Domain));
end;

function TSilFirebirdBindingList.Add(const Name: string; const Domain: RFbDomainData): IFbBinding;
var
  Internal: IFbBindingInternal; 
begin
  if not Find(Name, IFbBinding(Internal)) then
  begin
    Internal := IFbBindingInternal(TSilFirebirdBinding.Create(Self, Name, Domain, FSize));
    Inc(FSize, Internal.Size);
  end;
  Result := Internal;
end;

function TSilFirebirdBindingList.Find(const Binding: IFbVariable; out Index: Integer): Boolean;
begin
  Result := Assigned(FList) and FList.Find(@Binding, @Index, nil, FindByBinding);
end;

function TSilFirebirdBindingList.Find(const Source: IFbVariable; out Item: IFbBinding): Boolean;
begin
  Result := Find(Source.Name, Item);
end;

function TSilFirebirdBindingList.Find(const Name: string; out Item: IFbBinding): Boolean;
var
  Index: HItem;
begin
  Result := Assigned(FList) and FList.Find(@Name, @Index, nil, FindByName);
  if Result then Item := IFbBinding(FList[Index]^);
end;

function TSilFirebirdBindingList.Find(Position: Integer; out Item: IFbBinding): Boolean;
var
  Index: HItem;
begin
  Index := Pred(Position); 
  Result := FList.Items.IsValid(Index);
  if Result then Item := IFbBinding(FList[Index]^);
end;

function TSilFirebirdBindingList.Enumerate(var Enum: IEnumerator; out Item: IFbBinding): Boolean;
var
  Data: ^IFbBindingInternal;
begin
  if Enum <> nil then
    Result := Enum.Next
  else if GetEnumerator(Enum) then
    Result := Enum.HasMore
  else
    Result := False;

  if not Result then
    Enum := nil
  else if Enum.Get(Data) then
    Item := Data^;
end;

function TSilFirebirdBindingList.Remove(const Item: IFbBinding): Integer;
begin
  if Assigned(FList) and FList.Find(@Item, @Result, nil, FindByBinding) then
    FList.Items.Delete(Result) else
    Result := -1;
end;

procedure TSilFirebirdBindingList.Clear;
begin
  if Assigned(FList) then
    FList.Items.Clear;
end;

function TSilFirebirdBindingList.DoGetSession: IFbSessionInternal;
begin
  Result := FSession;
end;

function TSilFirebirdBindingList.Enumerate(var Enum: IEnumerator; out Item: IFbBindingInternal): Boolean;
begin
  Result := Enumerate(Enum, IFbBinding(Item));
end;

function TSilFirebirdBindingList.DoCompare(Data1, Data2: HData; Param: Pointer): Integer;
begin
  case TFbBindingField(Param) of
    fbidName:           Result := Sil.Str.TextCompare(string(Data1^), IFbBinding(Data2^).Name);
    fbidBinding:        Result := Sil.Ref.Compare(IFbBinding(Data1^), IFbBinding(Data2^));
    else                Result := -1;
  end;
end;

procedure TSilFirebirdBindingList.DoCheckList;
begin
  if not Assigned(FList) then
    FList := Vector.Create(TypeInfo(IFbBinding), DoCompare);
end;

end.
