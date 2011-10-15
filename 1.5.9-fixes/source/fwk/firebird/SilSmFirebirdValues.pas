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

unit SilSmFirebirdValues;

{$INCLUDE Defines.inc}

interface

uses
  Sil, SilContainer,
  SilSeFirebirdClient,
  SilSiFirebird,
  SilShFirebird,
  SilSkFirebirdHandled;

type
  TSilFirebirdValues = class(
    TSilFirebirdHandled,
    IEnumerable,
    IFbValuesInternal )
  private
    FCommand: Pointer;
    FBindings: IFbBindingsInternal;
    FList: IContainerDynamic;
    FBuffer: Pointer;
  private
    procedure DoCheckList;
    function DoCompare(Data1, Data2: HData; Param: Pointer): Integer;
    function DoAlloc(Count: Integer; Version: Integer): PXSQLDA_V1;
    procedure DoFree(Buffer: PPXSQLDA_V1);
    procedure DoSetBuffer(const Buffer: IFbBufferInternal);
    procedure DoAttach(const Buffer: IFbBufferInternal);
    procedure DoRelease;
  protected
    procedure DoCloseHandle(const Sender: IFbHandle); override;
  protected // IEnumerable
    function GetEnumerator(out Enum: IEnumerator; Locked: Boolean = False): Boolean; virtual;
  protected // IFbValuesInternal
    function DoGetCommand: IFbCommandInternal;
    function DoGetBindings: IFbBindingsInternal;
    function DoGetBuffer: IFbBufferInternal;
    function DoGetHandle: PXSQLDA_V1;
    procedure DoSetHandle(Value: PXSQLDA_V1);
    function Enumerate(var Enum: IEnumerator; out Item: IFbValueInternal): Boolean; overload; 
    function Add(const Item: IFbValueInternal): Integer;
    function Remove(const Item: IFbValueInternal): Integer;
  protected
    property Handle: PXSQLDA_V1 read DoGetHandle write DoSetHandle;
    property Buffer: IFbBufferInternal read DoGetBuffer;
  public
    constructor Create(const Command: IFbCommandInternal; const Bindings: IFbBindingsInternal); overload;
    constructor Create(const Command: IFbCommandInternal; const Buffer: IFbBufferInternal); overload;
    destructor Destroy; override;
  end;

implementation

uses
  SilLmContainerEnumerator,
  SilSmFirebirdValue,
  SilSfFirebirdClient, SilLiContainer;

{ TSilFirebirdValues }

constructor TSilFirebirdValues.Create(const Command: IFbCommandInternal; const Bindings: IFbBindingsInternal);
begin
  inherited Create;
  ASSERT(Assigned(Command));
  ASSERT(Assigned(Bindings));
  ASSERT(Bindings.Count > 0);
  FBindings := Bindings;
  FCommand := Pointer(Command);
  Handle := DoAlloc(FBindings.Count, SQLDA_VERSION1);
end;

constructor TSilFirebirdValues.Create(const Command: IFbCommandInternal; const Buffer: IFbBufferInternal);
begin
  Create(Command, Buffer.Bindings);
  DoSetBuffer(Buffer);
end;

destructor TSilFirebirdValues.Destroy;
begin
  DoSetBuffer(nil);
  Close;
  FCommand := nil;
  FBindings := nil;
  FList := nil;
  inherited;
end;

function TSilFirebirdValues.GetEnumerator(out Enum: IEnumerator; Locked: Boolean): Boolean;
begin
  Result := Assigned(FList) and (FList.Items.Count > 0);
  if Result then Enum := TSilContainerEnumerator.Create(FList, Sequence.Create(FList));
end;

function TSilFirebirdValues.DoGetCommand: IFbCommandInternal;
begin
  Result := IFbCommandInternal(FCommand);
end;

function TSilFirebirdValues.DoGetBindings: IFbBindingsInternal;
begin
  Result := FBindings;
end;

function TSilFirebirdValues.DoGetBuffer: IFbBufferInternal;
begin
  Result := IFbBufferInternal(FBuffer);
end;

function TSilFirebirdValues.DoGetHandle: PXSQLDA_V1;
begin
  Result := PXSQLDA_V1(inherited Handle.Value^);
end;

procedure TSilFirebirdValues.DoSetHandle(Value: PXSQLDA_V1);
begin
  PXSQLDA_V1(inherited Handle.Value^) := Value;
end;

function TSilFirebirdValues.Enumerate(var Enum: IEnumerator; out Item: IFbValueInternal): Boolean;
var
  Data: ^IFbValueInternal;
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

function TSilFirebirdValues.Add(const Item: IFbValueInternal): Integer;
begin
  DoCheckList;
  Result := FList.Add(@Item);
end;

function TSilFirebirdValues.Remove(const Item: IFbValueInternal): Integer;
begin
  if Assigned(FList) then
    Result := FList.Remove(@Item) else
    Result := -1;
end;

procedure TSilFirebirdValues.DoCheckList;
begin
  if not Assigned(FList) then
    FList := Vector.Create(TypeInfo(IFbValueInternal), DoCompare);
end;

function TSilFirebirdValues.DoCompare(Data1, Data2: HData; Param: Pointer): Integer;
begin
  Result := Sil.Ref.Compare(IUnknown(Data1^), IUnknown(Data2^));
end;

function TSilFirebirdValues.DoAlloc(Count: Integer; Version: Integer): PXSQLDA_V1;
begin
  if Count > 0 then
  begin
    Result := Sil.Mem.Alloc(XSQLDA_LENGTH(Count, Version));
    Result.version := Version;
    Result.sqln := Count;
    Result.sqld := Result.sqln;
  end else
    Result := nil;
end;

procedure TSilFirebirdValues.DoFree(Buffer: PPXSQLDA_V1);
begin
  Sil.Mem.Free(Buffer^);
end;

procedure TSilFirebirdValues.DoSetBuffer(const Buffer: IFbBufferInternal);
begin
  if Pointer(Buffer) <> FBuffer then
  begin
    if Assigned(Buffer) and Assigned(FBindings) then ASSERT(Sil.Ref.SameObject(FBindings, Buffer.Bindings));
    if Assigned(FBuffer) then DoRelease;
    FBuffer := Pointer(Buffer);
    if Assigned(FBuffer) then DoAttach(IFbBufferInternal(FBuffer));
  end;
end;

procedure TSilFirebirdValues.DoCloseHandle(const Sender: IFbHandle);
begin
  if Sender.IsAssigned then DoFree(Sender.Value);
end;

procedure TSilFirebirdValues.DoAttach(const Buffer: IFbBufferInternal);
var
  Enum: IEnumerator;
  Binding: IFbBindingInternal;
begin
  with FBindings do
    while Enumerate(Enum, Binding) do
      TSilFirebirdValue.Create(Self, IFbDataInternal(Buffer.Items[Binding.Index]));
end;

procedure TSilFirebirdValues.DoRelease;
var
  Item: IFbValueInternal;
begin
  if Assigned(FList) then
    with FList do
      while Items.Count > 0 do
      begin
        Item := IFbValueInternal(Value[Items.Last]^);
        try
          Item.Detach;
        finally
          Item := nil;
        end;
      end;
end;

end.
