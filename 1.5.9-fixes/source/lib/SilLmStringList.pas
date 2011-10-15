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

unit SilLmStringList;

{$I Defines.inc}

interface

uses
  SilBkPtr,
  SilBtStringPtr,
  SilLiStringList,
  SilLiSerializer,
  SilLiEnumerator,
  SilLiStream,
  SilLiCompare,
  SilLkObject,
  SilLkList,
  SilLmListEnumerator,
  SilOsTypes;

type
  StringPtrsHandler = class (StringHandler)
    class procedure Copy(const Source: Pointer; const Dest: Pointer; const Data: Pointer = nil); override;
  end;

  StringListHandler = class (DataHandler)
    class procedure Alloc(var Result: Pointer; const Data: Pointer = nil); override;
    class procedure Dispose(var Value: Pointer; const Data: Pointer = nil); override;
    class procedure Copy(const Source: Pointer; const Dest: Pointer; const Data: Pointer = nil); override;
    class procedure Clear(var Obj; const Data: Pointer = nil); override;
    class function Compare(const Value1; const Value2; Data: Pointer = nil): Integer; override;
  end;

{ TSilStringList }

  TSilStringList = class (
    // extends
    TSilList,
    // implements
    IStrings,
    IStringList,
    ISerializable )
  private
    FIgnoreCase: Boolean;
    FTypeHandler: HandlerType;
    FSeparator: string;
  private
    function DoAddText(const Text: string; Separator: string = ''): Integer;
    function DoGetText(Separator: string): string; overload;
  protected
    function Search(const Value: Variant; var Index: Integer): Boolean; override;
    function Sort: Boolean; override;
  protected // IStrings
    function GetItem(Index: Integer): String;
    function GetPtr(Index: Integer): Pointer;
    function DoGetText: String; overload;
    function GetIgnoreCase: Boolean;
    function IndexOf(const Value: String): Integer; virtual;
    function IndexOfMask(const Value: String): Integer;
    function IndexOfPtr(Value: Pointer): Integer; virtual;
    function First: String; virtual;
    function Last: String; virtual;
    function IStrings.Enumerate = DoEnumerate;
  protected // IStringList
    function IStringList.Enumerate = DoEnumerate;
    procedure SetItem(Index: Integer; const Value: String);
    procedure SetPtr(Index: Integer; Value: Pointer);
    procedure DoSetText(const Value: String);
    procedure SetIgnoreCase(Value: Boolean);
    function Add(const Value: String; Ptr: Pointer): Integer; virtual;
    procedure AddStrings(const Source: IStringList); virtual;
    procedure Insert(Index: Integer; const Value: String; Ptr: Pointer); virtual;
    function Remove(const Value: String; All: Boolean): Integer;
    function AddText(const Value: String; const Separator: string = ''): Integer;
    function GetText(const Separator: string = ''): string;
  protected // IEnumerable
    function GetEnumerator(out Enum: IEnumerator; Locked: Boolean): Boolean; override;
  protected // ISerializable
    procedure SaveToStream(const Stream: IStream);
    procedure LoadFromStream(const Stream: IStream);
    procedure SaveToFile(const FileName: String);
    procedure LoadFromFile(const FileName: String);
  protected
    function DoEnumerate(var Enum: IEnumerator; out Item: string): Boolean;
  public
    constructor Create(Locked: Boolean = false; TypeHandler: HandlerType = nil; const Separator: string = sLinebreak); reintroduce;
    class function CreateFrom(const Text: String; Locked: Boolean = false; TypeHandler: HandlerType = nil; const Separator: string = sLinebreak): IStringList;
  end;

  TStringListCompare = class (
    // extends
    TSilObject,
    // implements
    IComparable,
    IComparator)
  private
    FValue: String;
    FIgnoreCase: Boolean;
  protected // IComparable
    function CompareTo(const Item; Arg: Pointer = nil): Integer; virtual;
  protected // IComparator
    function Compare(const Item1, Item2; Arg: Pointer = nil): Integer; virtual;
  public
    constructor Create(IgnoreCase: Boolean = false; const Value: String = '');
  end;

implementation

uses
  SilBtText,
  SilBcChr,
  SilBtStr,
  SilLtReference,
  SilOiFile,
  SilOtTool,
  SilLtTool;

{ TSilStringList }

constructor TSilStringList.Create(Locked: Boolean; TypeHandler: HandlerType; const Separator: string);
begin
  FTypeHandler := BaseHandler.Check(TypeHandler);
  inherited Create(Locked, StringListHandler, FTypeHandler);

  FSeparator := Separator;
end;

class function TSilStringList.CreateFrom(const Text: string; Locked: Boolean; TypeHandler: HandlerType; const Separator: string): IStringList;
begin
  Result := Self.Create(Locked, TypeHandler, Separator);
  Result.Text := Text;
end;

function TSilStringList.Add(const Value: String; Ptr: Pointer): Integer;
var
  PItem: RStringItem;
begin
  PItem.Buffer := Value;
  PItem.Ptr := Ptr;

  if Ptr <> nil then
    FTypeHandler.ToPtr(Ptr^, PItem.Ptr);

  Result := ItemAdd(PItem);
end;

function TSilStringList.First: String;
begin
  Result := GetItem(0);
end;

function TSilStringList.GetItem(Index: Integer): String;
var
  PItem: RStringItem;
begin
  ItemGet(Index, PItem);
  Result := PItem.Buffer;
end;

function TSilStringList.GetPtr(Index: Integer): Pointer;
var
  PItem: RStringItem;
begin
  ItemGet(Index, PItem);
  Result := PItem.Ptr;
end;

function TSilStringList.IndexOf(const Value: String): Integer;
begin
  try
    if Lockable <> nil then Lockable.Lock;

    for Result := 0 to Count - 1 do
      if (not FIgnoreCase and (GetItem(Result) = Value)) or
        (FIgnoreCase and (Text.Compare(GetItem(Result), Value) = 0)) then
        Exit;

    Result := -1;
  finally
    if Lockable <> nil then Lockable.Unlock;
  end;
end;

function TSilStringList.IndexOfMask(const Value: String): Integer;
begin
  try
    if Lockable <> nil then Lockable.Lock;

    for Result := 0 to Count - 1 do
      if Str.WildCard(GetItem(Result), Value, FIgnoreCase) then
        Exit;

    Result := -1;
  finally
    if Lockable <> nil then Lockable.Unlock;
  end;
end;

function TSilStringList.Remove(const Value: String; All: Boolean): Integer;
begin
  Result := IndexOf(Value);
  if Result >= 0 then Delete(Result);
end;

function TSilStringList.IndexOfPtr(Value: Pointer): Integer;
var
  PItem: Pointer;
  PValue: Pointer;
begin
  if Value <> nil then
  begin
    FTypeHandler.ToPtr(Value^, PValue);
    try
      if Lockable <> nil then Lockable.Lock;

      for Result := 0 to Count - 1 do
      begin
        PItem := GetPtr(Result);
        if FTypeHandler.Compare(PItem, PValue) = 0 then
          Exit;
      end;

      Result := -1;
    finally
      FTypeHandler.Clear(PValue);
      if Lockable <> nil then Lockable.Unlock;
    end;
  end else
    Result := -1;
end;

procedure TSilStringList.Insert(Index: Integer; const Value: String; Ptr: Pointer);
var
  PItem: RStringItem;
begin
  PItem.Buffer := Value;
  PItem.Ptr := nil;
  if Ptr <> nil then
    FTypeHandler.ToPtr(Ptr^, PItem.Ptr);
  ItemInsert(Index, PItem);
end;

function TSilStringList.Last: String;
begin
  Result := GetItem(Count - 1);
end;

function TSilStringList.DoEnumerate(var Enum: IEnumerator; out Item: string): Boolean;
begin
  Result := inherited Enumerate(Enum, Item);  
end;

procedure TSilStringList.SetItem(Index: Integer; const Value: String);
var
  PItem: PStringItem;
begin
  PItem := ItemPtr(Index);
  PItem.Buffer := Value;
end;

procedure TSilStringList.SetPtr(Index: Integer; Value: Pointer);
var
  PItem: PStringItem;
begin
  PItem := ItemPtr(Index);
  FTypeHandler.Free(PItem.Ptr);
  if Value <> nil then
    FTypeHandler.ToPtr(Value^, PItem.Ptr);
end;

procedure TSilStringList.AddStrings(const Source: IStringList);
var
  i: Integer;
begin
  if Assigned(Source) then
    try
      if Lockable <> nil then Lockable.Lock;
      Source.Locked;

      for i := 0 to Source.Count - 1 do
        Add(Source.Items[i], Source.Ptrs[i]);
    finally
      if Lockable <> nil then Lockable.Unlock;
    end;
end;

procedure TSilStringList.DoSetText(const Value: String);
begin
  Clear;
  DoAddText(Value);
end;

function TSilStringList.DoGetText: String;
begin
  Result := DoGetText(FSeparator);
end;

function TSilStringList.AddText(const Value, Separator: string): Integer;
begin
  Result := DoAddText(Value, Separator);
end;

function TSilStringList.GetText(const Separator: string): string;
begin
  Result := DoGetText(Separator);
end;

procedure TSilStringList.LoadFromStream(const Stream: IStream);
var
  sBuffer, sResult: String;
  iSize, iRead: Integer;
  Random: IRandomStream;
begin
  iSize := 64;

  if Reference.GetInterface(Stream, IRandomStream, Random) then
    iSize := Random.Size - Random.Position;

  sResult := '';

  repeat
    SetLength(sBuffer, iSize);
    iRead := Stream.Read(sBuffer[1], iSize);

    if iRead = Length(sBuffer) then
      Str.Add(sResult, sBuffer) else
    if iRead > 0 then
      Str.Add(sResult, Str.Copy(sBuffer, 1, iRead));
  until (iRead = 0);
  
  DoSetText(sResult);
end;

procedure TSilStringList.SaveToStream(const Stream: IStream);
var
  sBuffer: String;
begin
  sBuffer := GetText;
  Stream.Write(sBuffer[1], Length(sBuffer));
end;

procedure TSilStringList.LoadFromFile(const FileName: String);
var
  Dest: IFile;
begin
  Dest := OS.FileSystem.OpenFile(FileName);
  LoadFromStream(Dest.Stream);
end;

procedure TSilStringList.SaveToFile(const FileName: String);
var
  Dest: IFile;
begin
  Dest := OS.FileSystem.CreateFile(FileName);
  SaveToStream(Dest.Stream);
end;

function TSilStringList.Search(const Value: Variant; var Index: Integer): Boolean;
var
  Comparator: IComparable;
begin
  Comparator := TStringListCompare.Create(FIgnoreCase, Value);
  Result := inherited CustomSearch(Comparator, Index);
end;

function TSilStringList.Sort: Boolean;
var
  Comparator: IComparator;
begin
  Comparator := TStringListCompare.Create(FIgnoreCase);
  Result := inherited CustomSort(Comparator);
end;

(*)procedure TSilStringList.SetText(const Value: String);
var
  P, Start: PChar;
  Item: String;
begin
  Clear;
  P := Pointer(Value);

  if P <> nil then
    while P^ <> #0 do
    begin
      Start := P;
      while not (P^ in [#0, #10, #13]) do Inc(P);
      SetString(Item, Start, P - Start);
      Add(Item, nil);
      if P^ = #13 then Inc(P);
      if P^ = #10 then Inc(P);
    end;
end;(*)

function TSilStringList.DoAddText(const Text: string; Separator: string): Integer;
var
  I: Integer;
  Item: String;
begin
  if Str.IsEmpty(Separator) then Separator := FSeparator;
  Result := 0;
  I := 0;
  
  while Str.Enumerate(Text, Separator, Item, I) do
  begin
    Add(Item, nil);
    Inc(Result);
  end;
end;

function TSilStringList.DoGetText(Separator: string): string;
var
  i, DelSize, BufSize, Count: Integer;
  PRes: PChar;

  procedure DoAdd(const Buf: String);
  begin
    BufSize := Length(Buf);
    Move(Buf[1], PRes^, BufSize);
    Inc(PRes, BufSize);
  end;
  
begin
  try
    if Lockable <> nil then Lockable.Lock;

    BufSize := 0;
    Count := Self.Count;

    if Str.IsEmpty(Separator) then Separator := FSeparator;
    DelSize := Length(Separator);

    for i := 0 to Count - 1 do
      Inc(BufSize, Length(GetItem(i)) + DelSize);

    Dec(BufSize, DelSize);
    SetString(Result, nil, BufSize);
    PRes := Pointer(Result);

    for i := 0 to Count - 1 do
    begin
      DoAdd(GetItem(i));
      if i < Count - 1 then DoAdd(Separator);
    end;
  finally
    if Lockable <> nil then Lockable.Unlock;
  end;
end;

(*)function TSilStringList.DoGetText(Separator: string): string;
var
  Enum: IEnumerator;
  Item: string;
begin
  if Str.IsEmpty(Separator) then Separator := FSeparator;
  Result := '';
  while Enumerate(Enum, Item) do
    Str.Add(Result, Item, Separator);
end;(*)

function TSilStringList.GetIgnoreCase: Boolean;
begin
  Result := FIgnoreCase;
end;

procedure TSilStringList.SetIgnoreCase(Value: Boolean);
begin
  FIgnoreCase := Value;
end;

function TSilStringList.GetEnumerator(out Enum: IEnumerator; Locked: Boolean): Boolean;
begin
  Result := Count > 0;
  if Result then Enum := TListEnumerator.Create(Self, Locked, StringPtrsHandler);
end;

{ StringListHandler }

class procedure StringListHandler.Alloc(var Result: Pointer; const Data: Pointer);
begin
  System.New(PStringItem(Result));
end;

class procedure StringListHandler.Dispose(var Value: Pointer; const Data: Pointer);
begin
  System.Dispose(PStringItem(Value));
end;

class procedure StringListHandler.Clear(var Obj; const Data: Pointer);
var
  Rec: PStringItem;
begin
  Rec := PStringItem(@Obj);
  if Rec.Ptr <> nil then
    HandlerType(Data).Free(Rec.Ptr);
end;

class function StringListHandler.Compare(const Value1; const Value2; Data: Pointer): Integer;
begin
  Result := Str.Compare(PStringItem(Value1).Buffer, PStringItem(Value2).Buffer);
end;

class procedure StringListHandler.Copy(const Source: Pointer; const Dest: Pointer; const Data: Pointer);
begin
  PStringItem(Dest)^ := PStringItem(Source)^;
end;

(*
class procedure StringListHandler.Free(var Value; Data: Pointer);
begin
  HandlerType(Data).Free(PStringItem(Value).Ptr);
  Dispose(PStringItem(Value));
end;

class function StringListHandler.ToPtr(const Obj; out Value: Pointer; Data: Pointer): Boolean;
var
  PBuf: PStringItem;
begin
  Result := true; // Pointer(Obj) <> nil; // '' -> nil
  if Result then
  begin
    New(PBuf);
    PBuf^ := RStringItem(Obj);
    Value := PBuf;
  end;
end;
*)

{ StringPtrsHandler }

class procedure StringPtrsHandler.Copy(const Source: Pointer; const Dest: Pointer; const Data: Pointer);
begin
  inherited Copy(@PStringItem(Source)^.Buffer, Dest, Data);
end;

{ TStringListCompare }

constructor TStringListCompare.Create(IgnoreCase: Boolean = false; const Value: String = '');
begin
  inherited Create;
  FValue := Value;
  FIgnoreCase := IgnoreCase;
end;

function TStringListCompare.CompareTo(const Item; Arg: Pointer): Integer;
begin
  if FIgnoreCase then
    Result := Text.Compare(PStringItem(Item).Buffer, FValue) else
    Result := Str.Compare(PStringItem(Item).Buffer, FValue);
end;

function TStringListCompare.Compare(const Item1; const Item2; Arg: Pointer): Integer;
begin
  if FIgnoreCase then
    Result := Text.Compare(PStringItem(Item1).Buffer, PStringItem(Item2).Buffer) else
    Result := Str.Compare(PStringItem(Item1).Buffer, PStringItem(Item2).Buffer);
end;

end.
