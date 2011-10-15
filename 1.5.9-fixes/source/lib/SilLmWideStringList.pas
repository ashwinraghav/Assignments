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

unit SilLmWideStringList;

{$I Defines.inc}

interface

uses
  SilBkPtr,
  SilBtWideStringPtr,
  SilLmListEnumerator,
  SilLkList,
  SilLiWideStringList,
  SilLiSerializer,
  SilLiEnumerator,
  SilLiStream;

type
  PWideStringItem = ^RWideStringItem;

  RWideStringItem = record
    Buffer: WideString;
    Ptr: Pointer;
  end;

  WideStringPtrsHandler = class (WideStringHandler)
    class procedure Copy(const Source: Pointer; const Dest: Pointer; const Data: Pointer = nil); override;
  end;

  WideStringListHandler = class (DataHandler)
    class procedure Alloc(var Result: Pointer; const Data: Pointer = nil); override;
    class procedure Dispose(var Value: Pointer; const Data: Pointer = nil); override;
    class procedure Copy(const Source: Pointer; const Dest: Pointer; const Data: Pointer = nil); override;
    class procedure Clear(var Obj; const Data: Pointer = nil); override;
    class function Compare(const Value1; const Value2; Data: Pointer = nil): Integer; override;
  end;

{ TSilWideStringList }

  TSilWideStringList = class (
    // extends
    TSilList,
    // implements
    IWideStringList,
    ISerializable)
  private
    FIgnoreCase: Boolean;
    FTypeHandler: HandlerType;
  protected
    function Search(const Value: Variant; var Index: Integer): Boolean; override;
    function Sort: Boolean; override;
  protected // IStringList
    function GetItem(Index: Integer): WideString;
    procedure SetItem(Index: Integer; const Value: WideString);
    function GetPtr(Index: Integer): Pointer;
    procedure SetPtr(Index: Integer; Value: Pointer);
    function GetText: WideString;
    procedure SetText(const Value: WideString);
    function GetIgnoreCase: Boolean;
    procedure SetIgnoreCase(Value: Boolean);
    function Add(const Value: WideString; Ptr: Pointer): Integer; virtual;
    procedure AddStrings(const Source: IWideStringList); virtual;
    function IndexOf(const Value: WideString): Integer; virtual;
    function IndexOfPtr(Value: Pointer): Integer; virtual;
    procedure Insert(Index: Integer; const Value: WideString; Ptr: Pointer); virtual;
    function First: WideString; virtual;
    function Last: WideString; virtual;
  protected // IEnumerable
    function GetEnumerator(out Enum: IEnumerator; Locked: Boolean): Boolean; override;
  protected // ISerializable
    procedure SaveToStream(const Stream: IStream);
    procedure LoadFromStream(const Stream: IStream);
    procedure SaveToFile(const FileName: WideString);
    procedure LoadFromFile(const FileName: WideString);
  public
    constructor Create(Locked: Boolean = false; TypeHandler: HandlerType = nil); reintroduce;
    class function CreateFrom(const Text: WideString; Locked: Boolean = false; TypeHandler: HandlerType = nil): IWideStringList;
  end;

implementation

uses
  SilBcChr,
  SilBtStr,
  SilLtReference,
  SilOiFile,
  SilOtTool,
  SilOsTool,
  SilLtTool;

{ TSilWideStringList }

constructor TSilWideStringList.Create(Locked: Boolean; TypeHandler: HandlerType);
begin
  FTypeHandler := BaseHandler.Check(TypeHandler);
  inherited Create(Locked, WideStringListHandler, FTypeHandler);
end;

class function TSilWideStringList.CreateFrom(const Text: WideString; Locked: Boolean = false; TypeHandler: HandlerType = nil): IWideStringList;
begin
  Result := Self.Create(Locked, TypeHandler);
  Result.Text := Text;
end;

function TSilWideStringList.Add(const Value: WideString; Ptr: Pointer): Integer;
var
  PItem: RWideStringItem;
begin
  PItem.Buffer := Value;
  PItem.Ptr := nil;
  if Ptr <> nil then
    FTypeHandler.ToObj(Ptr, PItem.Ptr);
  Result := ItemAdd(PItem);
end;

function TSilWideStringList.First: WideString;
begin
  Result := GetItem(0);
end;

function TSilWideStringList.GetItem(Index: Integer): WideString;
var
  PItem: RWideStringItem;
begin
  ItemGet(Index, PItem);
  Result := PItem.Buffer;
end;

function TSilWideStringList.GetPtr(Index: Integer): Pointer;
var
  PItem: RWideStringItem;
begin
  ItemGet(Index, PItem);
  Result := PItem.Ptr;
end;

function TSilWideStringList.IndexOf(const Value: WideString): Integer;
begin
  for Result := 0 to Count - 1 do
    if (not FIgnoreCase and (GetItem(Result) = Value)) or
      (FIgnoreCase and (OsWStr.CompareText(GetItem(Result), Value, true) = 0)) then
      Exit;

  Result := -1;
end;

function TSilWideStringList.IndexOfPtr(Value: Pointer): Integer;
var
  PItem: Pointer;
begin
  for Result := 0 to Count - 1 do
  begin
    PItem := GetPtr(Result);
    if FTypeHandler.Compare(PItem, Value) = 0 then
      Exit;
  end;

  Result := -1;
end;

procedure TSilWideStringList.Insert(Index: Integer; const Value: WideString; Ptr: Pointer);
var
  PItem: RWideStringItem;
begin
  PItem.Buffer := Value;
  PItem.Ptr := nil;
  if Ptr <> nil then
    FTypeHandler.ToObj(Ptr, PItem.Ptr);
  ItemInsert(Index, PItem);
end;

function TSilWideStringList.Last: WideString;
begin
  Result := GetItem(Count - 1);
end;

procedure TSilWideStringList.SetItem(Index: Integer; const Value: WideString);
var
  PItem: PWideStringItem;
begin
  PItem := ItemPtr(Index);
  PItem.Buffer := Value;
end;

procedure TSilWideStringList.SetPtr(Index: Integer; Value: Pointer);
var
  PItem: PWideStringItem;
begin
  PItem := ItemPtr(Index);
  FTypeHandler.Free(PItem.Ptr);
  FTypeHandler.ToObj(Value, PItem.Ptr);
end;

procedure TSilWideStringList.AddStrings(const Source: IWideStringList);
var
  i: Integer;
begin
  for i := 0 to Source.Count - 1 do
    Add(Source.Items[i], Source.Ptrs[i]);
end;

function TSilWideStringList.GetText: WideString;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to Count - 1 do
  begin
    if OsWStr.NotEmpty(Result) then
      Result := Result + ccCRLF;
    Result := Result + GetItem(i);
  end;
end;

procedure TSilWideStringList.SetText(const Value: WideString);
var
  P, Start: PWideChar;
  sItem: WideString;
begin
  Clear;
  P := Pointer(Value);

  if P <> nil then
    while P^ <> #0 do
    begin
      Start := P;
      while (P^ <> #0) and (P^ <> #10) and (P^ <> #13) do Inc(P);
      SetString(sItem, Start, P - Start);
      Add(sItem, nil);
      if P^ = #13 then Inc(P);
      if P^ = #10 then Inc(P);
    end;
end;

procedure TSilWideStringList.LoadFromStream(const Stream: IStream);
var
  sBuffer, sResult: WideString;
  iSize, iRead: Integer;
  Random: IRandomStream;
begin
  iSize := 64;

  if Reference.GetInterface(Stream, IRandomStream, Random) then
    iSize := Random.Size - Random.Position;

  sResult := '';

  repeat
    SetLength(sBuffer, iSize div 2);
    iRead := Stream.Read(sBuffer[1], iSize) div 2;

    if iRead = Length(sBuffer) then
      OsWStr.Add(sResult, sBuffer) else
    if iRead > 0 then
      OsWStr.Add(sResult, OsWStr.Copy(sBuffer, 1, iRead));
  until (iRead = 0);
  
  SetText(sResult);
end;

procedure TSilWideStringList.SaveToStream(const Stream: IStream);
var
  sBuffer: WideString;
begin
  sBuffer := GetText;
  Stream.Write(sBuffer[1], Length(sBuffer) * 2);
end;

procedure TSilWideStringList.LoadFromFile(const FileName: WideString);
var
  Dest: IFile;
begin
  Dest := OS.FileSystem.OpenFile(FileName);
  LoadFromStream(Dest.Stream);
end;

procedure TSilWideStringList.SaveToFile(const FileName: WideString);
var
  Dest: IFile;
begin
  Dest := OS.FileSystem.CreateFile(FileName);
  SaveToStream(Dest.Stream);
end;

function TSilWideStringList.Search(const Value: Variant; var Index: Integer): Boolean;
begin
  Result := false;//inherited CustomSearch(Tk.WideStringComparable(FIgnoreCase, Value), Index);
end;

function TSilWideStringList.Sort: Boolean;
begin
  Result := false;//inherited CustomSort(Tk.StringComparator(FIgnoreCase));
end;

function TSilWideStringList.GetIgnoreCase: Boolean;
begin
  Result := FIgnoreCase;
end;

procedure TSilWideStringList.SetIgnoreCase(Value: Boolean);
begin
  FIgnoreCase := Value;
end;

function TSilWideStringList.GetEnumerator(out Enum: IEnumerator; Locked: Boolean): Boolean;
begin
  Result := Count > 0;
  if Result then Enum := TListEnumerator.Create(Self, Locked, WideStringPtrsHandler);
end;

{ WideStringListHandler }

class procedure WideStringListHandler.Alloc(var Result: Pointer; const Data: Pointer);
begin
  System.New(PWideString(Result));
end;

class procedure WideStringListHandler.Dispose(var Value: Pointer; const Data: Pointer);
begin
  System.Dispose(PWideString(Value));
end;

class procedure WideStringListHandler.Clear(var Obj; const Data: Pointer);
begin
  HandlerType(Data).Clear(PWideStringItem(@Obj).Ptr);
end;

class function WideStringListHandler.Compare(const Value1; const Value2; Data: Pointer): Integer;
begin
  Result := OsWStr.Compare(PWideStringItem(Value1).Buffer, PWideStringItem(Value2).Buffer);
end;

class procedure WideStringListHandler.Copy(const Source, Dest, Data: Pointer);
begin
  PWideStringItem(Dest)^ := PWideStringItem(Source)^;
end;

(*)
class function WideStringListHandler.Compare(const Value1; const Value2; Data: Pointer): Integer;
begin
  Result := OsWStr.Compare(RWideStringItem(Value1).Buffer, RWideStringItem(Value2).Buffer);
end;

class function WideStringListHandler.Copy(Value: Pointer; out Obj; Data: Pointer): Boolean;
begin
  Result := (Value <> nil);
  if Result then RWideStringItem(Obj) := PWideStringItem(Value)^;
end;

class procedure WideStringListHandler.Free(var Value; Data: Pointer);
begin
  HandlerType(Data).Free(PWideStringItem(Value).Ptr);
  Dispose(PWideStringItem(Value));
end;

class function WideStringListHandler.ToPtr(const Obj; out Value: Pointer; Data: Pointer): Boolean;
var
  PBuf: PWideStringItem;
begin
  Result := true; // Pointer(Obj) <> nil; // '' -> nil
  if Result then
  begin
    New(PBuf);
    PBuf^ := RWideStringItem(Obj);
    Value := PBuf;
  end;
end;
(*)

{ WideStringPtrsHandler }

class procedure WideStringPtrsHandler.Copy(const Source, Dest, Data: Pointer);
begin
  inherited Copy(@PWideStringItem(Source).Buffer, Dest, Data);
end;

end.
