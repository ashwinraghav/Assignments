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

unit SilLtTool;

{$INCLUDE Defines.inc}

interface

uses
  SilBkTool,
  SilBkPtr,
  SilLiEnumerator,
  SilLiList,
  SilLiCompare,
  SilLiStringList,
  SilLiInterfaceList,
  SilLiInterfaceQueue,
  SilLiPointerQueue,
  SilLiEventList,
  SilLiValueList,
  SilLiStream,
  SilLiPointerList,
  SilLiRandomPointerList,
  SilLiScheduler,
  SilLiReadWriteLock,
  SilLiFiler,
  SilLiPacket,
  SilLiFactory,
  SilLjFactory;

type
  Tk = class(Tool)
    class function Factory(const Tool: FactoryType): IFactory; overload;
    class function ListEnumerator(const List: IList; const Locked: Boolean = False; const TypeHandler: HandlerType = nil; const TypeData: Pointer = nil): IEnumerator; {$IFDEF USE_DEPRECATED} deprecated; {$ENDIF}
    class function StringList(Locked: Boolean = false; PtrKind: HandlerType = nil): IStringList; overload; {$IFDEF USE_DEPRECATED} deprecated; {$ENDIF}
    class function StringList(const Text: String; Locked: Boolean = false; PtrKind: HandlerType = nil): IStringList; overload; {$IFDEF USE_DEPRECATED} deprecated; {$ENDIF}
    class function StringComparable(IgnoreCase: Boolean = false; const Value: String = ''): IComparable;
    class function StringComparator(IgnoreCase: Boolean = false): IComparator;
    class function InterfaceList(Locked: Boolean = false): IInterfaceList; {$IFDEF USE_DEPRECATED} deprecated; {$ENDIF}
    class function InterfaceQueue(const InitialCount: Integer = 0; const MaxCount: Integer = 0; const Name: PChar = nil): IInterfaceQueue; {$IFDEF USE_DEPRECATED} deprecated; {$ENDIF}
    class function PointerQueue(const InitialCount: Integer = 0; const MaxCount: Integer = 0; const Name: PChar = nil): IPointerQueue; {$IFDEF USE_DEPRECATED} deprecated; {$ENDIF}
    class function ValueList(IgnoreCase: Boolean = False): IValueList; {$IFDEF USE_DEPRECATED} deprecated; {$ENDIF}
    class function MemoryStream(Buffer: PChar = nil; Size: LongWord = 0): IMemoryStream;
    class function BufferStream(const Source: IStream = nil; Buffer: PChar = nil; Size: LongWord = 0): IBufferStream;
    class function PointerList(Locked: Boolean = false; PtrKind: HandlerType = nil): IPointerList; {$IFDEF USE_DEPRECATED} deprecated; {$ENDIF}
    class function RandomPointerList(Locked: Boolean = false; PtrKind: HandlerType = nil): IRandomPointerList; {$IFDEF USE_DEPRECATED} deprecated; {$ENDIF}
    class function StreamBuffer(const AStream: IRandomStream; BufferSize: Integer): IRandomStream;
    class function Enumeration(const AObject: IUnknown; Locked: Boolean = false): IEnumeration;
    class function Enumerator(const AObject: IUnknown; out Enum: IEnumerator; Locked: Boolean = false): Boolean;
    class function Scheduling: IScheduling;
    class function ReadWriteLock: IReadWriteLock;
    class function StreamReader(const Stream: IStream): IReader; {$IFDEF USE_DEPRECATED} deprecated; {$ENDIF}
    class function StreamWriter(const Stream: IStream = nil): IWriter; {$IFDEF USE_DEPRECATED} deprecated; {$ENDIF}
    class function Packet(const Source: IStream = nil): IPacket; {$IFDEF USE_DEPRECATED} deprecated; {$ENDIF}
  end;

implementation

uses
  SilLmStringList,
  SilLmInterfaceList,
  SilLmMemoryStream,
  SilLmPointerList,
  SilLmRandomPointerList,
  SilLmStreamBuffer,
  SilLmListEnumerator,
  SilLmStringCompare,
  SilLmEnumeration,
  SilLtReference,
  SilLmValueList,
  SilLmScheduler,
  SilLmReadWriteLock,
  SilLmInterfaceQueue,
  SilLmPointerQueue,
  SilLmBufferStream,
  SilLmRawPacket,
  SilLmFactory,
  SilLtList,
  SilLtRawFiler;

{ Tk }

class function Tk.MemoryStream(Buffer: PChar; Size: LongWord): IMemoryStream;
begin
  Result := TMemoryStream.Create(Buffer, Size);
end;

class function Tk.BufferStream(const Source: IStream; Buffer: PChar; Size: LongWord): IBufferStream;
begin
  Result := TBufferStream.Create(Source);
end;

class function Tk.StreamBuffer(const AStream: IRandomStream; BufferSize: Integer): IRandomStream;
begin
  Result := TStreamBuffer.Create(AStream, BufferSize);
end;

class function Tk.StringComparable(IgnoreCase: Boolean; const Value: String): IComparable;
begin
  Result := TStringCompare.Create(IgnoreCase, Value);
end;

class function Tk.StringComparator(IgnoreCase: Boolean = false): IComparator;
begin
  Result := TStringCompare.Create(IgnoreCase);
end;

class function Tk.Enumeration(const AObject: IUnknown; Locked: Boolean): IEnumeration;
var
  Ator: IEnumerator;
begin
  if not Enumerator(AObject, Ator, Locked) then
    Ator := nil;
  Result := TEnumeration.Create(Ator);
end;

class function Tk.Enumerator(const AObject: IUnknown; out Enum: IEnumerator; Locked: Boolean = false): Boolean;
var
  Able: IEnumerable;
begin
  Result := Reference.GetInterface(AObject, IEnumerator, Enum)
             or ( Reference.GetInterface(AObject, IEnumerable, Able)
                and Able.GetEnumerator(Enum, Locked));
end;

class function Tk.Scheduling: IScheduling;
begin
  result := TScheduling.Create;
end;

class function Tk.ReadWriteLock: IReadWriteLock;
begin
  Result := TReadWriteLock.Create;
end;

class function Tk.Factory(const Tool: FactoryType): IFactory;
begin
  Result := TSilToolFactory.Create(Tool);
end;

{$IFDEF USE_DEPRECATED}
{$WARN SYMBOL_DEPRECATED OFF}
{$ENDIF}

class function Tk.InterfaceQueue(const InitialCount, MaxCount: Integer; const Name: PChar): IInterfaceQueue;
begin
  Result := ListTool.InterfaceQueue(InitialCount, MaxCount, Name);
end;

class function Tk.PointerQueue(const InitialCount, MaxCount: Integer; const Name: PChar): IPointerQueue;
begin
  Result := ListTool.PointerQueue(InitialCount, MaxCount, Name);
end;

class function Tk.Packet(const Source: IStream): IPacket;
begin
  Result := TSilRawPacket.Create(Source, false);
end;

class function Tk.StreamReader(const Stream: IStream): IReader;
begin
  Result := SilLtRawFiler.Factory.Reader(Stream);
end;

class function Tk.StreamWriter(const Stream: IStream): IWriter;
begin
  Result := SilLtRawFiler.Factory.Writer(Stream);
end;

class function Tk.ListEnumerator(const List: IList; const Locked: Boolean = False; const TypeHandler: HandlerType = nil; const TypeData: Pointer = nil): IEnumerator;
begin
  Result := ListTool.ListEnumerator(List, Locked, TypeHandler, TypeData);
end;

class function Tk.InterfaceList(Locked: Boolean): IInterfaceList;
begin
  Result := ListTool.InterfaceList(Locked);
end;

class function Tk.PointerList(Locked: Boolean; PtrKind: HandlerType): IPointerList;
begin
  Result := ListTool.PointerList(Locked, PtrKind);
end;

class function Tk.RandomPointerList(Locked: Boolean; PtrKind: HandlerType): IRandomPointerList;
begin
  Result := ListTool.RandomPointerList(Locked, PtrKind);
end;

class function Tk.StringList(Locked: Boolean; PtrKind: HandlerType): IStringList;
begin
  Result := ListTool.StringList(Locked, PtrKind);
end;

class function Tk.StringList(const Text: String; Locked: Boolean; PtrKind: HandlerType): IStringList;
begin
  Result := ListTool.StringList(Text, Locked, PtrKind);
end;

class function Tk.ValueList(IgnoreCase: Boolean): IValueList;
begin
  Result := ListTool.ValueList(IgnoreCase);
end;

{$IFDEF USE_DEPRECATED}
{$WARN SYMBOL_DEPRECATED ON}
{$ENDIF}

end.
