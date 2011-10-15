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

unit SilLtList;

{$I Defines.inc}

interface

uses
  SilBeTypes,
  SilBkTool,
  SilBkPtr,
  SilLiEnumerator,
  SilLiList,
  SilLiCompare,
  SilLiStringList,
  SilLiInterfaceList,
  SilLiEventList,
  SilLiValueList,
  SilLiStream,
  SilLiPointerList,
  SilLiRandomPointerList,
  SilLiScheduler,
  SilLiFiler,
  SilLiLinkedList,
  SilLiParameters,
  SilLiMemberList,
  SilLiWideStringList,
  SilLiQueue,
  SilLiInterfaceQueue,
  SilLiPointerQueue,
  SilLiPacket,
  SilOsTypes;

type
  ParamListClass = class of ParamListTool;
  MemberListClass = class of MemberListTool;

  ListTool = class(Tool)
    class function Params: ParamListClass;
    class function Parameters(Locked: Boolean = False): IParameterList; overload;
    class function Parameters(const List: IParameters; Precedence: TMergePrecedence = mkSource; Locked: Boolean = False): IParameterList; overload;
    class function Parameters(const Items: array of RParameter; Locked: Boolean = False): IParameterList; overload;
    class function Members: MemberListClass;
    class function MemberList(Locked: Boolean = False): IMemberList; overload;
    class function MemberList(const List: IParameters; Precedence: TMergePrecedence = mkSource; Locked: Boolean = False): IMemberList; overload;
    class function MemberList(const Items: array of RParameter; Locked: Boolean = False): IMemberList; overload;
    class function ParamGet(const Params: IParameters; const Name: String; const Default: Variant): Variant; {$IFDEF USE_DEPRECATED} deprecated; {$ENDIF}
    class function ListEnumerator(const List: IList; const Locked: Boolean = False; const TypeHandler: HandlerType = nil; const CastData: Pointer = nil): IEnumerator;
    class function StringList(Locked: Boolean = False; TypeHandler: HandlerType = nil; const Separator: String = sLineBreak): IStringList; overload;
    class function StringList(const Text: String; Locked: Boolean = False; TypeHandler: HandlerType = nil): IStringList; overload;
    class function StringList(const Text, Separator: String; Locked: Boolean = False; TypeHandler: HandlerType = nil): IStringList; overload;
    class function InterfaceList(Locked: Boolean = False): IInterfaceList;
    class function EventList(const Controller: IUnknown = nil): IEventList;
    class function ValueList(IgnoreCase: Boolean = False): IValueList;
    class function PointerList(Locked: Boolean = False; TypeHandler: HandlerType = nil): IPointerList;
    class function RandomPointerList(Locked: Boolean = False; TypeHandler: HandlerType = nil): IRandomPointerList;
    class function TokenList(const Buffer, Separator: String): IStringList;
    class function WideStringList(Locked: Boolean = False; TypeHandler: HandlerType = nil): IWideStringList; overload;
    class function WideStringList(const Text: WideString; Locked: Boolean = False; TypeHandler: HandlerType = nil): IWideStringList; overload;
    class function LinkedList(Locked: Boolean = false; const TypeHandler: HandlerType = nil; const TypeData: Pointer = nil): ILinkedList;
    class function InterfaceQueue(const InitialCount: Integer = 0; const MaxCount: Integer = 0; const Name: PChar = nil): IInterfaceQueue;
    class function PointerQueue(const InitialCount: Integer = 0; const MaxCount: Integer = 0; const Name: PChar = nil): IPointerQueue;
    class procedure CancelQueue(const Queue: IQueue);
    class function ReplaceLiterals(const Text: string; out Literals: IParameters; const LDelimiter: string = '"'; const RDelimiter: string = '"'): string;
  end;

  ParamListTool = class(Tool)
    class function Create(Locked: Boolean = False): IParameterList; overload;
    class function Create(const Items: array of RParameter; Locked: Boolean = False): IParameterList; overload;
    class function List(const Items: array of RParameter): RParameters;
    class function Add(const List: IParameterList; const Items: array of RParameter; Locked: Boolean = False): IParameterList; overload;
    class function Check(const List: IParameters; Locked: Boolean = False): IParameters; overload;
    class function Check(const List: IParameterList; Locked: Boolean = False): IParameterList; overload;
    class function Get(const List: IParameters; const Name: string; Value: PVariant): IParameters; overload;
    class function Get(const List: IParameterList; const Name: string; Value: PVariant): IParameterList; overload;
    class function Get(const List: IParameters; const Name: string; out Value: Variant; const Default: Variant): IParameters; overload;
    class function Get(const List: IParameterList; const Name: string; out Value: Variant; const Default: Variant): IParameterList; overload;
    class function Put(const List: IParameterList; const Param: RParameter): IParameterList; overload;
    class function Get(const List: IParameters; const Name: string; Default: Variant): Variant; overload;
    class function Void(const Name: string): RParameter; overload;
    class function Null(const Name: string): RParameter; overload;
    class function Empty(const Name: string): RParameter; overload;
    class function Boolean(const Name: string; Value: Boolean): RParameter;
    class function Integer(const Name: string; Value: Integer): RParameter; 
    class function LongWord(const Name: string; Value: LongWord): RParameter; 
    class function LargeInt(const Name: string; Value: LargeInt): RParameter; 
    class function Float(const Name: string; Value: Double): RParameter;
    class function Datetime(const Name: string; Value: TDateTime): RParameter;
    class function AnsiString(const Name: string; const Value: string): RParameter;
    class function WideString(const Name: string; const Value: WideString): RParameter; overload;
    class function Item(const Name: string; Value: Boolean): RParameter; overload;
    class function Item(const Name: string; Value: Integer): RParameter; overload;
    class function Item(const Name: string; Value: LongWord): RParameter; overload;
    class function Item(const Name: string; Value: LargeInt): RParameter; overload;
    class function Item(const Name: string; Value: Double): RParameter; overload;
    class function Item(const Name: string; const Value: string): RParameter; overload;
    class function Item(const Name: string; const Value: WideString): RParameter; overload;
    {$IFDEF D60}
    class function Item(const Name: string; const Value: Variant): RParameter; overload;
    {$ENDIF}
    class function Variant(const Name: string; const Value: Variant): RParameter; overload;
    class function Pack(const List: IParameters; const Packet: IPacket): IParameters; overload; 
    class function Pack(const List: IParameterList; const Packet: IPacket): IParameterList; overload; 
    class function Unpack(const List: IArgumentList; const Packet: IPacket): IArgumentList; overload;
    class function Unpack(const List: IParameterList; const Packet: IPacket): IParameterList; overload;
    class function Unpack(const Packet: IPacket; const List: IParameterList = nil; Locked: Boolean = False): IParameterList; overload;
    class function ToStrings(const List: IParameters): IStringList;
    class function FromStrings(const List: IStringList): IParameterList;
  end;

  MemberListTool = class(ParamListTool)
    class function Create(Locked: Boolean = False): IMemberList; reintroduce; overload;
    class function Create(const Items: array of RParameter; Locked: Boolean = False): IMemberList; reintroduce; overload;
    class function Create(const List: IParameters; Precedence: TMergePrecedence = mkSource; Locked: Boolean = False): IMemberList; reintroduce; overload;
    class function Add(const List: IMemberList; const Items: array of RParameter; Locked: Boolean = False): IMemberList; reintroduce; overload;
    class function Add(const List: IMemberList; Member: Pointer; const Name: string; Default: Variant; TypeInfo: Pointer = nil): IMemberList; overload; 
    class function Add(const List: IMemberList; Member: Pointer; const Name: string; TypeInfo: Pointer = nil): IMemberList; overload; 
    class function Add(const List: IMemberList; var Member: Boolean; const Name: string; Default: Boolean = False): IMemberList; overload;
    class function Add(const List: IMemberList; var Member: Byte; const Name: string; Default: Byte = 0): IMemberList; overload;
    class function Add(const List: IMemberList; var Member: Smallint; const Name: string; Default: Smallint = 0): IMemberList; overload;
    class function Add(const List: IMemberList; var Member: Word; const Name: string; Default: Word = 0): IMemberList; overload;
    class function Add(const List: IMemberList; var Member: Integer; const Name: string; Default: Integer = 0): IMemberList; overload;
    class function Add(const List: IMemberList; var Member: LongWord; const Name: string; Default: LongWord = 0): IMemberList; overload;
    class function Add(const List: IMemberList; var Member: AnsiString; const Name: string; const Default: AnsiString = ''): IMemberList; overload;
    class function Add(const List: IMemberList; var Member: WideString; const Name: string; const Default: WideString = ''): IMemberList; overload;
    class function Add(const List: IMemberList; var Member: Double; const Name: string; const Default: Double = 0.0): IMemberList; overload;
    class function Add(const List: IMemberList; var Member: TDateTime; const Name: string; const Default: TDateTime = 0.0): IMemberList; overload;
    class function Add(const List: IMemberList; var Member: Single; const Name: string; const Default: Single = 0.0): IMemberList; overload;
    class function Add(const List: IMemberList; var Member: IUnknown; const Name: string; const Default: IUnknown = nil): IMemberList; overload;
    class function Add(const List: IMemberList; var Member: Variant; const Name: string; const Default: Variant): IMemberList; overload;
    class function Add(const List: IMemberList; var Member: OleVariant; const Name: string; const Default: OleVariant): IMemberList; overload;
  end;

implementation

uses
  SilBtStr,
  SilBtInt,
  SilBtVart,
  SilLfParameters,
  SilLmStringList,
  SilLmInterfaceList,
  SilLmPointerList,
  SilLmRandomPointerList,
  SilLmListEnumerator,
  SilLmStringCompare,
  SilLtReference,
  SilLmEventList,
  SilLmValueList,
  SilLmScheduler,
  SilLmReader,
  SilLmWriter,
  SilLmLinkedList,
  SilLmParameters,
  SilLmMemberList,
  SilLmInterfaceQueue,
  SilLmPointerQueue,
  SilLmWideStringList;

{ ListTool }

class function ListTool.Params: ParamListClass;
begin
  Result := ParamListTool;
end;

class function ListTool.Parameters(Locked: Boolean): IParameterList;
begin
  Result := SilLfParameters.Params(Locked);
end;

class function ListTool.Parameters(const List: IParameters; Precedence: TMergePrecedence; Locked: Boolean): IParameterList;
begin
  Result := Parameters(Locked);
  Result.Merge(List, Precedence);
end;

class function ListTool.Parameters(const Items: array of RParameter; Locked: Boolean): IParameterList;
begin
  Result := SilLfParameters.Params(Items, Locked);
end;

class function ListTool.Members: MemberListClass;
begin
  Result := MemberListTool;
end;

class function ListTool.MemberList(Locked: Boolean): IMemberList;
begin
  Result := Members.Create(Locked);
end;

class function ListTool.MemberList(const List: IParameters; Precedence: TMergePrecedence; Locked: Boolean): IMemberList;
begin
  Result := Members.Create(List, Precedence, Locked);
end;

class function ListTool.MemberList(const Items: array of RParameter; Locked: Boolean): IMemberList;
begin
  Result := Members.Create(Items, Locked);
end;

{$IFDEF USE_DEPRECATED} {$WARN SYMBOL_DEPRECATED OFF} {$ENDIF}
class function ListTool.ParamGet(const Params: IParameters; const Name: String; const Default: Variant): Variant;
begin
  if Assigned(Params) then
    Result := Params.Get(Name, Default) else
    Result := Default;
end;
{$IFDEF USE_DEPRECATED} {$WARN SYMBOL_DEPRECATED ON} {$ENDIF}

class function ListTool.ListEnumerator(const List: IList; const Locked: Boolean; const TypeHandler: HandlerType; const CastData: Pointer): IEnumerator;
begin
  Result := TListEnumerator.Create(List, Locked, TypeHandler, CastData);
end;

class function ListTool.StringList(Locked: Boolean; TypeHandler: HandlerType; const Separator: String): IStringList;
begin
  Result := TSilStringList.Create(Locked, TypeHandler, Separator);
end;

class function ListTool.StringList(const Text: String; Locked: Boolean; TypeHandler: HandlerType): IStringList;
begin
  Result := TSilStringList.CreateFrom(Text, Locked, TypeHandler);
end;

class function ListTool.StringList(const Text, Separator: String; Locked: Boolean; TypeHandler: HandlerType): IStringList;
begin
  Result := TSilStringList.CreateFrom(Text, Locked, TypeHandler, Separator);
end;

class function ListTool.InterfaceList(Locked: Boolean): IInterfaceList;
begin
  Result := TSilInterfaceList.Create(Locked);
end;

class function ListTool.EventList(const Controller: IUnknown): IEventList;
begin
  Result := TEventList.Create(Controller);
end;

class function ListTool.ValueList(IgnoreCase: Boolean): IValueList;
begin
  Result := TValueList.Create(true);
  Result.IgnoreCase := IgnoreCase;
end;

class function ListTool.PointerList(Locked: Boolean; TypeHandler: HandlerType): IPointerList;
begin
  Result := TSilPointerList.Create(Locked, TypeHandler);
end;

class function ListTool.RandomPointerList(Locked: Boolean; TypeHandler: HandlerType): IRandomPointerList;
begin
  Result := TRandomPointerList.Create(Locked, TypeHandler);
end;

class function ListTool.TokenList(const Buffer, Separator: String): IStringList;
begin
  Result := TSilStringList.CreateFrom(Buffer, False, nil, Separator);;
end;

class function ListTool.WideStringList(Locked: Boolean; TypeHandler: HandlerType): IWideStringList;
begin
  Result := TSilWideStringList.Create(Locked, TypeHandler);
end;

class function ListTool.WideStringList(const Text: WideString; Locked: Boolean; TypeHandler: HandlerType): IWideStringList;
begin
  Result := TSilWideStringList.CreateFrom(Text, Locked, TypeHandler);
end;

class function ListTool.LinkedList(Locked: Boolean; const TypeHandler: HandlerType; const TypeData: Pointer): ILinkedList;
begin
  Result := TSilLinkedList.Create(TypeHandler, Locked, TypeData);
end;

class function ListTool.InterfaceQueue(const InitialCount, MaxCount: Integer; const Name: PChar): IInterfaceQueue;
begin
  Result := TInterfaceQueue.Create(InitialCount, MaxCount, Name);
end;

class function ListTool.PointerQueue(const InitialCount, MaxCount: Integer; const Name: PChar): IPointerQueue;
begin
  Result := TPointerQueue.Create(InitialCount, MaxCount, Name);
end;

class procedure ListTool.CancelQueue(const Queue: IQueue);
begin
  if Assigned(Queue) then Queue.Cancel;
end;

class function ListTool.ReplaceLiterals(const Text: string; out Literals: IParameters; const LDelimiter, RDelimiter: string): string;
var
  Buffer, Output, Literal: string;
  List: IParameterList;
begin
  List := ListTool.Parameters();

  Buffer := Text;
  while Str.Between(Buffer, LDelimiter, RDelimiter, Output) do
  begin
    Literal := '$' + Int.ToStr(List.Count);
    List[Literal] := Output;
    Buffer := Str.Replace(Buffer, LDelimiter + Output + RDelimiter, Literal);
  end;

  Literals := List;
  Result := Buffer;
end;

{ ParamListTool }

class function ParamListTool.Create(Locked: Boolean): IParameterList;
begin
  Result := SilLfParameters.Params(Locked);
end;

class function ParamListTool.Create(const Items: array of RParameter; Locked: Boolean): IParameterList;
begin
  Result := SilLfParameters.Params(Items, Locked);
end;

class function ParamListTool.List(const Items: array of RParameter): RParameters;
var
  I: System.Integer;
begin
  SetLength(Result, Length(Items));
  for I := System.Low(Result) to System.High(Result) do
    Result[I] := Items[I];
end;

class function ParamListTool.Add(const List: IParameterList; const Items: array of RParameter; Locked: Boolean): IParameterList;
begin
  Result := SilLfParameters.Params(Check(List, Locked), Items);
end;

class function ParamListTool.Check(const List: IParameterList; Locked: Boolean): IParameterList;
begin
  Result := List;
  if not Assigned(Result) then Result := Create(Locked);
end;

class function ParamListTool.Check(const List: IParameters; Locked: Boolean): IParameters;
begin
  Result := List;
  if not Assigned(Result) then Result := Create(Locked);
end;

class function ParamListTool.Get(const List: IParameters; const Name: string; Value: PVariant): IParameters;
begin
  ASSERT(Value <> nil);
  Result := Check(List);
  Value^ := Result[Name];
end;

class function ParamListTool.Get(const List: IParameterList; const Name: string; Value: PVariant): IParameterList;
begin
  ASSERT(Value <> nil);
  Result := Check(List);
  Value^ := Result[Name];
end;

class function ParamListTool.Get(const List: IParameters; const Name: string; out Value: Variant; const Default: Variant): IParameters;
begin
  Result := Check(List);
  Value := Result.Get(Name, Default);
end;

class function ParamListTool.Get(const List: IParameterList; const Name: string; out Value: Variant; const Default: Variant): IParameterList;
begin
  Result := Check(List);
  Value := Result.Get(Name, Default);
end;

class function ParamListTool.Get(const List: IParameters; const Name: string; Default: Variant): Variant;
begin
  Get(List, Name, Result, Default);
end;

class function ParamListTool.Put(const List: IParameterList; const Param: RParameter): IParameterList;
begin
  Result := Check(List);
  Result[Param.Name] := Param.Value;
end;

class function ParamListTool.Void(const Name: string): RParameter;
begin
  Result := SilLfParameters.ParamVoid(Name);
end;

class function ParamListTool.Null(const Name: string): RParameter;
begin
  Result := SilLfParameters.ParamNull(Name);
end;

class function ParamListTool.Empty(const Name: string): RParameter;
begin
  Result := SilLfParameters.ParamEmpty(Name);
end;

class function ParamListTool.Boolean(const Name: string; Value: Boolean): RParameter;
begin
  Result := SilLfParameters.ParamBoolean(Name, Value);
end;

class function ParamListTool.Integer(const Name: string; Value: Integer): RParameter;
begin
  Result := SilLfParameters.ParamInteger(Name, Value);
end;

class function ParamListTool.LongWord(const Name: string; Value: LongWord): RParameter;
begin
  Result := SilLfParameters.ParamLongWord(Name, Value);
end;

class function ParamListTool.LargeInt(const Name: string; Value: LargeInt): RParameter;
begin
  Result := SilLfParameters.ParamLargeInt(Name, Value);
end;

class function ParamListTool.Float(const Name: string; Value: Double): RParameter;
begin
  Result := SilLfParameters.ParamFloat(Name, Value);
end;

class function ParamListTool.Datetime(const Name: string; Value: TDateTime): RParameter;
begin
  Result := SilLfParameters.ParamDateTime(Name, Value);
end;

class function ParamListTool.AnsiString(const Name, Value: string): RParameter;
begin
  Result := SilLfParameters.ParamAnsiString(Name, Value);
end;

class function ParamListTool.WideString(const Name: string; const Value: WideString): RParameter;
begin
  Result := SilLfParameters.ParamWideString(Name, Value);
end;

class function ParamListTool.Item(const Name: string; Value: Boolean): RParameter;
begin
  Result := SilLfParameters.ParamBoolean(Name, Value);
end;

class function ParamListTool.Item(const Name: string; Value: Integer): RParameter;
begin
  Result := SilLfParameters.ParamInteger(Name, Value);
end;

class function ParamListTool.Item(const Name: string; Value: LongWord): RParameter;
begin
  Result := SilLfParameters.ParamLongWord(Name, Value);
end;

class function ParamListTool.Item(const Name: string; Value: LargeInt): RParameter;
begin
  Result := SilLfParameters.ParamLargeInt(Name, Value);
end;

class function ParamListTool.Item(const Name: string; Value: Double): RParameter;
begin
  Result := SilLfParameters.ParamFloat(Name, Value);
end;

class function ParamListTool.Item(const Name, Value: string): RParameter;
begin
  Result := SilLfParameters.ParamAnsiString(Name, Value);
end;

class function ParamListTool.Item(const Name: string; const Value: WideString): RParameter;
begin
  Result := SilLfParameters.ParamWideString(Name, Value);
end;

{$IFDEF D60}
class function ParamListTool.Item(const Name: string; const Value: Variant): RParameter;
begin
  Result := SilLfParameters.ParamVariant(Name, Value);
end;
{$ENDIF}

class function ParamListTool.Variant(const Name: string; const Value: Variant): RParameter;
begin
  Result := SilLfParameters.ParamVariant(Name, Value);
end;

class function ParamListTool.Pack(const List: IParameters; const Packet: IPacket): IParameters;
var
  Enum: IEnumerator;
  Item: RParameter;
begin
  if Assigned(List) then
  begin
    Packet.Writer.WriteInteger(List.Count);
    with List do
      while Enumerate(Enum, Item) do
      begin
        Packet.Writer.WriteWideString(Item.Name);
        Packet.Writer.WriteVariant(Item.Value);
      end;
  end else
    Packet.Writer.WriteInteger(0);
end;

class function ParamListTool.Pack(const List: IParameterList; const Packet: IPacket): IParameterList;
begin
  Pack(IParameters(List), Packet);
  Result := List;
end;

class function ParamListTool.Unpack(const List: IArgumentList; const Packet: IPacket): IArgumentList;
var
  Count: System.Integer;
  Item: RParameter;
begin
  Count := Packet.Reader.ReadInteger();
  if Count > 0 then
    while Int.Dec(Count, True) > 0 do
    begin
      Item.Name := Packet.Reader.ReadWideString();
      Item.Value := Packet.Reader.ReadVariant();
      if Assigned(List) then List[Item.Name] := Item.Value;
    end;
end;

class function ParamListTool.Unpack(const List: IParameterList; const Packet: IPacket): IParameterList;
var
  Count: System.Integer;
  Item: RParameter;
begin
  Count := Packet.Reader.ReadInteger();
  if Count > 0 then
    while Int.Dec(Count, True) > 0 do
    begin
      Item.Name := Packet.Reader.ReadWideString();
      Item.Value := Packet.Reader.ReadVariant();
      if Assigned(List) then List[Item.Name] := Item.Value;
    end;
end;

class function ParamListTool.Unpack(const Packet: IPacket; const List: IParameterList; Locked: Boolean): IParameterList;
begin
  Result := List;
  if not Assigned(Result) then Result := Create(Locked);
  Unpack(Result, Packet);
end;

class function ParamListTool.ToStrings(const List: IParameters): IStringList;
var
  Enum: IEnumerator;
  Item: RParameter;
begin
  Result := ListTool.StringList;

  while List.Enumerate(Enum, Item) do
    Result.Add(Str.Format('%s=%s', [Item.Name, Vart.ToStr(Item.Value)]));
end;

class function ParamListTool.FromStrings(const List: IStringList): IParameterList;
var
  Enum: IEnumerator;
  Item, Name, Value: String;
begin
  Result := ListTool.Parameters;

  while List.Enumerate(Enum, Item) do
    if Str.Split(Item, '=', Name, Value) then
      Result[Str.Trim(Name)] := Str.Trim(Value);
end;

{ MemberListTool }

class function MemberListTool.Create(Locked: Boolean): IMemberList;
begin
  Result := TSilMemberListImpl.Create(Locked);
end;

class function MemberListTool.Create(const Items: array of RParameter; Locked: Boolean): IMemberList;
begin
  Result := Add(Create(Locked), Items, Locked);
end;

class function MemberListTool.Create(const List: IParameters; Precedence: TMergePrecedence; Locked: Boolean): IMemberList;
begin
  Result := Create(Locked);
  Result.Merge(List, Precedence);
end;

class function MemberListTool.Add(const List: IMemberList; const Items: array of RParameter; Locked: Boolean): IMemberList;
var
  I: System.Integer;
begin
  if not Assigned(List) then
    Result := Create(Locked) else
    Result := List;
    
  if (Length(Items) > 0) then
    for I := Low(Items) to High(Items) do
      with Items[I] do
        Result[Name] := Value;       
end;

class function MemberListTool.Add(const List: IMemberList; Member: Pointer; const Name: string; Default: Variant; TypeInfo: Pointer): IMemberList;
begin
  Result := List;
  if Assigned(Result) then
    List.Define(Name, Member, TypeInfo, Default);
end;

class function MemberListTool.Add(const List: IMemberList; Member: Pointer; const Name: string; TypeInfo: Pointer): IMemberList;
begin
  Result := Add(List, Member, Name, Vart.Unassigned, TypeInfo);
end;

class function MemberListTool.Add(const List: IMemberList; var Member: Boolean; const Name: string; Default: Boolean): IMemberList;
begin
  Result := Add(List, @Member, Name, Default, System.TypeInfo(System.Boolean));
end;

class function MemberListTool.Add(const List: IMemberList; var Member: Byte; const Name: string; Default: Byte): IMemberList;
begin
  Result := Add(List, @Member, Name, Default, System.TypeInfo(System.Byte));
end;

class function MemberListTool.Add(const List: IMemberList; var Member: Smallint; const Name: string; Default: Smallint): IMemberList;
begin
  Result := Add(List, @Member, Name, Default, System.TypeInfo(System.Smallint));
end;

class function MemberListTool.Add(const List: IMemberList; var Member: Word; const Name: string; Default: Word): IMemberList;
begin
  Result := Add(List, @Member, Name, Default, System.TypeInfo(System.Word));
end;

class function MemberListTool.Add(const List: IMemberList; var Member: Integer; const Name: string; Default: Integer): IMemberList;
begin
  Result := Add(List, @Member, Name, Default, System.TypeInfo(System.Integer));
end;

class function MemberListTool.Add(const List: IMemberList; var Member: LongWord; const Name: string; Default: LongWord): IMemberList;
begin
  Result := Add(List, @Member, Name, Default, System.TypeInfo(System.LongWord));
end;

class function MemberListTool.Add(const List: IMemberList; var Member: AnsiString; const Name: string; const Default: AnsiString): IMemberList;
begin
  Result := Add(List, @Member, Name, Default, System.TypeInfo(System.AnsiString));
end;

class function MemberListTool.Add(const List: IMemberList; var Member: WideString; const Name: string; const Default: WideString): IMemberList;
begin
  Result := Add(List, @Member, Name, Default, System.TypeInfo(System.WideString));
end;

class function MemberListTool.Add(const List: IMemberList; var Member: Double; const Name: string; const Default: Double): IMemberList;
begin
  Result := Add(List, @Member, Name, Default, System.TypeInfo(System.Double));
end;

class function MemberListTool.Add(const List: IMemberList; var Member: TDateTime; const Name: string; const Default: TDateTime): IMemberList;
begin
  Result := Add(List, @Member, Name, Default, System.TypeInfo(System.TDateTime));
end;

class function MemberListTool.Add(const List: IMemberList; var Member: Single; const Name: string; const Default: Single): IMemberList;
begin
  Result := Add(List, @Member, Name, Default, System.TypeInfo(System.Single));
end;

class function MemberListTool.Add(const List: IMemberList; var Member: IInterface; const Name: string; const Default: IInterface): IMemberList;
begin
  Result := Add(List, @Member, Name, Default, System.TypeInfo(System.IInterface));
end;

class function MemberListTool.Add(const List: IMemberList; var Member: Variant; const Name: string; const Default: Variant): IMemberList;
begin
  Result := Add(List, @Member, Name, Default, System.TypeInfo(System.Variant));
end;

class function MemberListTool.Add(const List: IMemberList; var Member: OleVariant; const Name: string; const Default: OleVariant): IMemberList;
begin
  Result := Add(List, @Member, Name, Default, System.TypeInfo(System.OleVariant));
end;

end.
