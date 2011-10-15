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

unit SilVectorSmParameters;

interface

{$include Sil.inc}

uses
  Sil,
  SilVectorSiVector,
  SilVectorSkVector;

type
  TSilVectorParameter = class (
    TSilObject,
    IArguments,
    IArgumentList,
    IParameterList,
    ISerializable,
    IParameters)
  private
    FLocked: Boolean;
    FPrefix: string;
    FList: IVectorString;
  private
    function DoGet(const ID: string; out Value: Variant): Boolean;
    function DoGetByName(const ID: string; out Value: Variant): Boolean;
    procedure DoPutByName(const ID: string; const Value: Variant);
    function DoFindByName(const ID: string; out Index: Integer): Boolean;
  protected
    procedure DoAddItem(const ID: string; const Value: Variant); virtual;
    function DoGetByIndex(const ID: Integer; out Value: Variant): Boolean; virtual;
    procedure DoPutByIndex(const ID: Integer; const Value: Variant); virtual;
  protected // IArguments
    function GetCount: Integer;
    function GetItem(const ID: string): Variant;
    function Find(const ID: string; out Value: Variant): Boolean;
  protected // IParameters
    function Contains(const ID: string): Boolean;
    function Enumerate(var Enum: IEnumerator; out Item: RParameter): Boolean;
    function Slice(const Name: string): IParameters;
    function Get(const Name: String; Default: Variant): Variant;
    function GetParameter(Index: Integer): RParameter;
  protected // IArgumentList
    procedure PutItem(const ID: string; const Value: Variant);
  protected // IParameterList
    procedure Merge(const Source: IParameters; Precedence: TMergePrecedence);
    function Remove(const ID: string): Integer;
    procedure Delete(Index: Integer);
    procedure Clear;
  protected // ISerializable 
    procedure SaveToStream(const Stream: IStream);
    procedure LoadFromStream(const Stream: IStream);
  public
    constructor Create(Locked: Boolean); overload;
    constructor Create(const List: IVectorString; const Prefix: string; Locked: Boolean); overload;
    destructor Destroy; override;
  end;

implementation

uses
  SilVector,
  SilLtStream,
  SilVectorSmString;

{ TSilVectorParameter }

constructor TSilVectorParameter.Create(Locked: Boolean);
var
  List: IVectorString;
begin
  List := TSilVectorString.Create(locked);
  List.IgnoreCase := True;
  Create(List, '', Locked);
end;

constructor TSilVectorParameter.Create(const List: IVectorString; const Prefix: string; Locked: Boolean);
begin
  inherited Create;
  FLocked := Locked;
  if not Str.IsEmpty(Prefix) then
    FPrefix := Str.ToUpper(Prefix) + '.' else
    FPrefix := '';
  FList := List;
end;

destructor TSilVectorParameter.Destroy;
begin
  clear;
  FList := nil;
  inherited;
end;

function TSilVectorParameter.GetCount: Integer;
var
  i: Integer;
begin
  if Str.NotEmpty(FPrefix) then
  begin
    try
      FList.Lock;
      Result := 0;

      for i := 0 to FList.Count - 1 do
        if Str.Pos(FPrefix, Str.ToUpper(FList.Items[i])) = 1 then
          Inc(Result);
    finally
      FList.Unlock;
    end;
  end else
    Result := FList.Count;
end;

function TSilVectorParameter.Contains(const ID: string): Boolean;
var
  Dummy: Integer;
begin
  Result := DoFindByName(ID, Dummy);
end;

function TSilVectorParameter.Find(const ID: string; out Value: Variant): Boolean;
var
  Index: Integer;
begin
  Result := DoFindByName(ID, Index);
  if Result then
    Result := DoGetByIndex(Index, Value);
end;

function TSilVectorParameter.GetItem(const ID: string): Variant;
begin
  if not DoGet(ID, Result) then
    Result := Vart.Unassigned;
end;

procedure TSilVectorParameter.PutItem(const ID: string; const Value: Variant);
begin
  DoPutByName(ID, Value);
end;

function TSilVectorParameter.Slice(const Name: string): IParameters;
begin
  Result := TSilVectorParameter.Create(Flist, Name, FLocked);
end;

function TSilVectorParameter.Get(const Name: String; Default: Variant): Variant;
begin
  if not DoGetByName(Name, Result) then
    Result := Default;
end;

function TSilVectorParameter.GetParameter(Index: Integer): RParameter;
begin
  Result.Name := FList[Index];
  Result.Value := PVariant(FList.Ptrs[Index])^;
end;

function TSilVectorParameter.DoGet(const ID: string; out Value: Variant): Boolean;
begin
  Result := DoGetByName(ID, Value);
end;

function TSilVectorParameter.DoFindByName(const ID: string; out Index: Integer): Boolean;
begin
  Index := FList.IndexOf(FPrefix + ID);
  Result := Index <> -1;
end;

procedure TSilVectorParameter.DoAddItem(const ID: string; const Value: Variant);
var
  ptr: PVariant;
begin
  new(ptr);
  ptr^ := value;
  FList.Add(FPrefix + ID, ptr);
end;

function TSilVectorParameter.DoGetByName(const ID: string; out Value: Variant): Boolean;
var
  Index: Integer;
begin
  Result := DoFindByName(ID, Index);
  if Result then
    Result := DoGetByIndex(Index, Value);
end;

procedure TSilVectorParameter.DoPutByName(const ID: string; const Value: Variant);
var
  Index: Integer;
begin
  if DoFindByName(ID, Index) then
    DoPutByIndex(Index, Value) else
    DoAddItem(ID, Value);
end;

function TSilVectorParameter.DoGetByIndex(const ID: Integer; out Value: Variant): Boolean;
begin
  Result := (ID >= 0) and (ID < FList.Count);
  if Result then
    Value := PVariant(FList.Ptrs[ID])^;
end;

procedure TSilVectorParameter.DoPutByIndex(const ID: Integer; const Value: Variant);
var
  ptr: PVariant;
begin
  if (ID >= 0) and (ID < FList.Count) then
  begin
    new(ptr);
    ptr^ := value;
    FList.Ptrs[ID] := ptr;
  end;
end;

function TSilVectorParameter.Enumerate(var Enum: IEnumerator; out Item: RParameter): Boolean;
var
  Name: string;
begin
  repeat
    Result := SilVector.Enumerate(enum, FList, name);

    if Result then
    begin
      if Str.IsEmpty(FPrefix) then
        Break;

      if Sil.Text.StartsWith(FPrefix, name) then
      begin
        Str.Extract(Name, 1, Str.Len(FPrefix));
        Break;
      end;
    end;
  until not Result;

  if Result then
  begin
    Item.Name := Name;
    DoGet(Name, Item.Value);
  end;
end;

procedure TSilVectorParameter.Delete(Index: Integer);
begin
  Dispose(PVariant(FList.Ptrs[Index]));
  FList.Delete(Index);
end;

function TSilVectorParameter.Remove(const ID: string): Integer;
begin
  if DoFindByName(ID, Result) then
    FList.Delete(Result) else
    Result := -1;
end;

procedure TSilVectorParameter.Merge(const Source: IParameters; Precedence: TMergePrecedence);
var
  Enum: IEnumerator;
  Item: RParameter;
  Index: Integer;
  Value: Variant;
begin
  if Assigned(Source) then
    while Source.Enumerate(Enum, Item) do
      case Precedence of
        mkSource:
          if Vart.IsOK(Item.Value) then
            DoPutByName(Item.Name, Item.Value);

        mkDestination:
          if not (DoFindByName(Item.Value, Index) and DoGetByIndex(Index, Value) and Vart.IsOK(Value)) then
            DoPutByIndex(Index, Item.Value);
      end;
end;

procedure TSilVectorParameter.Clear;
var
  enum: IVectorStringEnumerator;
  dummy: string;
begin
  enum := FList.Enumerator;

  while enum.Enumerate(dummy) do
    Dispose(PVariant(FList.Ptrs[enum.index]));

  FList.Clear;
end;

procedure TSilVectorParameter.SaveToStream(const Stream: IStream);
var
  Enum: IEnumerator;
  Item: RParameter;
  Packet: IPacket;
begin
  Packet := SilLtStream.Stream.Typed.Packet;
  Packet.Writer.WriteLongWord(GetCount);

  while Enumerate(Enum, Item) do
  begin
    Packet.Writer.WriteString(Item.Name);
    Packet.Writer.WriteVariant(Item.Value);
  end;

  Stream.Write(Packet.Buffer.Memory^, Packet.Buffer.Size);
end;

procedure TSilVectorParameter.LoadFromStream(const Stream: IStream);
var
  Count, i: LongWord;
  Packet: IPacket;
  Item: RParameter;
begin
  Packet := SilLtStream.Stream.Typed.Packet(Stream);
  Count := Packet.Reader.ReadLongWord;

  with Packet.Reader do
    for i := 0 to Count - 1 do
    begin
      Item.Name := ReadString;
      Item.Value := ReadVariant;
      DoPutByName(Item.Name, Item.Value);
    end;
end;

end.
