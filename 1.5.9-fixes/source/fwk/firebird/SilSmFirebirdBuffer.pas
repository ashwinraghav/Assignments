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

unit SilSmFirebirdBuffer;

{$INCLUDE Defines.inc}

interface

uses
  Sil, SilContainer,
  SilSeFirebirdClient,
  SilSiFirebird,
  SilShFirebird;

type
  TFbDataField = (
      fbidInterface,
      fbidBinding,
      fbidName,
      fbidOrdinal
    );

type
  TSilFirebirdBuffer = class(
    TSilObject,
    IEnumerable,
    IFbBufferInternal )
  private
    FCommand: Pointer; 
    FBindings: IFbBindingsInternal;
    FValues: IFbValuesInternal;
    FList: IContainerDynamic;
    FMemory: PChar;
    FSize: LongWord;
    FHoldCommand: Boolean;
  private
    procedure DoCheckList;
    function DoCompare(Data1, Data2: HData; Param: Pointer): Integer;
    procedure DoSetBindings(const Bindings: IFbBindingsInternal);
    procedure DoClear;
    procedure DoLoad;
    function DoCreate(const Item: IFbBindingInternal; Buffer: PFbBufferEntry): IFbDataInternal;
    function DoAdd(const Item: IFbDataInternal): Integer;
  protected // IEnumerable
    function GetEnumerator(out Enum: IEnumerator; Locked: Boolean = False): Boolean; virtual;
  protected // IFbBuffer
    function GetBindings: IFbBindings;
    function GetCommand: IFbCommand;
    function GetCount: Integer;
    function GetItems(Index: Integer): IFbData;
    function GetByOrdinal(Ordinal: LongWord): IFbData;
    function GetByBinding(const Binding: IFbBinding): IFbData;
    function GetByName(const Name: string): IFbData;
    function Enumerate(var Enum: IEnumerator; out Item: IFbData): Boolean; overload;
  protected // IFbBufferInternal
    function DoGetCommand: IFbCommandInternal;
    function DoGetBindings: IFbBindingsInternal;
    function DoGetValues: IFbValuesInternal;
    function DoGetMemory: PChar;
    function DoGetSize: LongWord;
  protected
    property Command: IFbCommandInternal read DoGetCommand;
  public
    constructor Create(const Command: IFbCommandInternal; const Bindings: IFbBindingsInternal; HoldCommand: Boolean = True);
    destructor Destroy; override;
  end;

implementation

uses
  SilLmContainerEnumerator,
  SilSfFirebirdClient,
  SilSgFirebirdData,
  SilSmFirebirdValues;

const
  FindByInterface       = Pointer(Ord(fbidInterface));
  FindByBinding         = Pointer(Ord(fbidBinding));
  FindByName            = Pointer(Ord(fbidName));
  FindByOrdinal         = Pointer(Ord(fbidOrdinal));

{ TSilFirebirdBuffer }

constructor TSilFirebirdBuffer.Create(const Command: IFbCommandInternal; const Bindings: IFbBindingsInternal; HoldCommand: Boolean);
begin
  inherited Create;
  FHoldCommand := HoldCommand;
  FCommand := Pointer(Command);
  if FHoldCommand then IUnknown(FCommand)._AddRef;
  DoSetBindings(Bindings);
end;

destructor TSilFirebirdBuffer.Destroy;
begin
  DoSetBindings(nil);
  FList := nil;
  if FHoldCommand and Assigned(FCommand) then
    IUnknown(FCommand) := nil else
    FCommand := nil;
  inherited;
end;

function TSilFirebirdBuffer.GetEnumerator(out Enum: IEnumerator; Locked: Boolean): Boolean;
begin
  Result := Assigned(FList) and (FList.Items.Count > 0);
  if Result then Enum := TSilContainerEnumerator.Create(FList, Sequence.Create(FList));
end;

function TSilFirebirdBuffer.GetCommand: IFbCommand;
begin
  Result := IFbCommandInternal(FCommand);
end;

function TSilFirebirdBuffer.GetBindings: IFbBindings;
begin
  Result := FBindings;
end;

function TSilFirebirdBuffer.GetCount: Integer;
begin
  if Assigned(FList) then
    Result := FList.Items.Count else
    Result := 0;
end;

function TSilFirebirdBuffer.GetItems(Index: Integer): IFbData;
begin
  if Assigned(FList) then
    Result := IFbDataInternal(FList[Index]^) else
    raise Sil.Error.Create('El Buffer no tiene elementos');
end;

function TSilFirebirdBuffer.GetByOrdinal(Ordinal: LongWord): IFbData;
var
  Index: Integer;
begin
  if not FList.Find(@Ordinal, @Index, nil, FindByOrdinal) then
    raise Sil.Error.Create('No existe un elemento que corresponda el Ordinal especificado');
  Result := GetItems(Index);
end;

function TSilFirebirdBuffer.GetByBinding(const Binding: IFbBinding): IFbData;
var
  Index: Integer;
begin
  if not FList.Find(@Binding, @Index, nil, FindByBinding) then
    raise Sil.Error.Create('No existe un elemento que corresponda el Binding especificado');
  Result := GetItems(Index);
end;

function TSilFirebirdBuffer.GetByName(const Name: string): IFbData;
var
  Index: Integer;
begin
  if not FList.Find(@Name, @Index, nil, FindByName) then
    raise Sil.Error.Create('No existe un elemento que corresponda al nombre (''%s'') especificado', [Name]);
  Result := GetItems(Index);
end;

function TSilFirebirdBuffer.Enumerate(var Enum: IEnumerator; out Item: IFbData): Boolean;
var
  Data: ^IFbDataInternal;
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

function TSilFirebirdBuffer.DoGetCommand: IFbCommandInternal;
begin
  Result := IFbCommandInternal(FCommand);
end;

function TSilFirebirdBuffer.DoGetBindings: IFbBindingsInternal;
begin
  Result := FBindings;
end;

function TSilFirebirdBuffer.DoGetValues: IFbValuesInternal;
begin
  if FValues = nil then FValues := TSilFirebirdValues.Create(Command, Self);
  Result := FValues;
end;

function TSilFirebirdBuffer.DoGetMemory: PChar;
begin
  Result := FMemory;
end;

function TSilFirebirdBuffer.DoGetSize: LongWord;
begin
  Result := FSize;
end;

procedure TSilFirebirdBuffer.DoCheckList;
begin
  if not Assigned(FList) then
    FList := Vector.Create(TypeInfo(IFbDataInternal), DoCompare);
end;

function TSilFirebirdBuffer.DoCompare(Data1, Data2: HData; Param: Pointer): Integer;
begin
  case TFbDataField(Param) of
    fbidInterface:      Result := Sil.Ref.Compare(IFbDataInternal(Data1^), IFbDataInternal(Data2^));
    fbidBinding:        Result := Sil.Ref.Compare(IFbBinding(Data1^), IFbDataInternal(Data2^).Binding);
    fbidName:           Result := Sil.Str.TextCompare(string(Data1^), IFbDataInternal(Data2^).Binding.Name);
    fbidOrdinal:        Result := Integer(Data1^) - IFbDataInternal(Data2^).Binding.Position;
    else                Result := -1;
  end;
end;

procedure TSilFirebirdBuffer.DoSetBindings(const Bindings: IFbBindingsInternal);
begin
  if Assigned(FBindings) then DoClear;
  FBindings := Bindings;
  if Assigned(FBindings) then DoLoad;
end;

procedure TSilFirebirdBuffer.DoClear;
begin
  if Assigned(FBindings) then
  begin
    FValues := nil;
    if Assigned(FList) then FList.Items.Clear;
    if (FSize > 0) and Assigned(FMemory) then Sil.Mem.Free(FMemory);
    FMemory := nil;
    FSize := 0;
    FBindings := nil;
  end;
end;

procedure TSilFirebirdBuffer.DoLoad;
var
  Enum: IEnumerator;
  Item: IFbBindingInternal;
  Data: IFbDataInternal;
  Buffer: PChar;
begin
  if Assigned(FBindings) then
  begin
    FSize := FBindings.Size;
    if FSize = 0 then Exit;
    FMemory := Sil.Mem.Alloc(FSize);
    Buffer := FMemory;
    with FBindings do
      while Enumerate(Enum, Item) do
      begin
        Data := DoCreate(Item, Pointer(Buffer));
        Inc(Buffer, Data.TotalSize);
      end;
  end;
end;

function TSilFirebirdBuffer.DoCreate(const Item: IFbBindingInternal; Buffer: PFbBufferEntry): IFbDataInternal;
begin
  DoGetClass(Item.Domain.DataType).Create(Self, Item, Buffer).GetInterface(IFbDataInternal, Result);
  DoAdd(Result);
end;

function TSilFirebirdBuffer.DoAdd(const Item: IFbDataInternal): Integer;
begin
  DoCheckList;
  Result := FList.Add(@Item);
end;

end.
