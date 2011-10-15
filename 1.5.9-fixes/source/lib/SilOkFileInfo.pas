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

unit SilOkFileInfo;

{$I Defines.inc}

interface

uses
  SilBeTypes,
  SilLkInterfaced,
  SilLiCompare,
  SilLmInterfaceList,
  SilOiHandle,
  SilOiFile,
  SilOiVersion;

type
  TSilBaseFileInfo = class (
    // extends
    TSilInterfacedObject,
    // implements
    IFileInfo,
    IFileInfoDef)
  private
    procedure DoSetFullName(const Value: String);
  protected
    FPath: String;
    FName: String;
  protected // IFileInfo
    function GetPath: String; virtual;
    function GetName: String; virtual;
    function GetFullName: String; virtual; 
    function GetTime: TDateTime; virtual; abstract;
    function GetAttributes: TFileAttributes; virtual; abstract;
    function GetSize: LongWord; virtual; abstract;
    function GetVersion: IVersionInfo; virtual;
  protected // IFileInfoDef
    procedure SetPath(const Value: String); virtual; 
    procedure SetName(const Value: String); virtual;
    procedure SetTime(Time: TDateTime); virtual; abstract;
    procedure SetAttributes(Value: TFileAttributes); virtual; abstract;
    procedure SetSize(Value: LongWord); virtual; abstract;
  public
    constructor Create(const FileName: String = ''; const Time: TDateTime = 0; Attributes: TFileAttributes = []; Size: LongWord = 0); overload; virtual;
    constructor Create(const Handle: IHandle; const FileName: String); overload; virtual;
    destructor Destroy; override;
  end;

  TSilFileInfo = class (TSilBaseFileInfo)
  protected
    FTime: TDateTime;
    FAttributes: TFileAttributes;
    FSize: LongWord;
  protected // IFileInfo
    function GetTime: TDateTime; override;
    function GetAttributes: TFileAttributes; override;
    function GetSize: LongWord; override;
  protected // IFileInfoDef
    procedure SetTime(Time: TDateTime); override;
    procedure SetAttributes(Value: TFileAttributes); override;
    procedure SetSize(Value: LongWord); override;
  public
    constructor Create(const FileName: String = ''; const Time: TDateTime = 0; Attributes: TFileAttributes = []; Size: LongWord = 0); override; 
  end;

  TSilFileInfoCompare = class (
    // extends
    TSilInterfacedObject,
    // implements
    IComparable,
    IComparator)
  private
    FValue: String;
    FKind: TFileInfoKind;
  protected // IComparable
    function CompareTo(const Item; Arg: Pointer = nil): Integer; virtual;
  protected // IComparator
    function Compare(const Item1, Item2; Arg: Pointer = nil): Integer; virtual;
  public
    constructor Create(Kind: TFileInfoKind; const Value: Variant); overload;
    constructor Create(Kind: TFileInfoKind); overload;
  end;

  TSilFileInfoList = class (
    // extends
    TSilInterfaceList,
    // implements
    IFileInfoList)
  protected
    function Search(const Value: Variant; var Index: Integer): Boolean; override;
    function Sort: Boolean; override;
  protected
    function GetItem(Index: Integer): IFileInfo;
    procedure SetItem(Index: Integer; const Value: IFileInfo);
    function Add(const Item: IFileInfo): Integer; reintroduce;
    procedure AddList(const Source: IFileInfoList); reintroduce;
    function IndexOf(const Item: IFileInfo): Integer; reintroduce;
    procedure Insert(Index: Integer; const Item: IFileInfo); reintroduce;
    function Remove(const Item: IFileInfo): Integer; reintroduce;
    function First: IFileInfo;
    function Last: IFileInfo;
  end;

implementation

uses
  SilBtText,
  SilAfAtoB,
  SilBtDateTime,
  SilBtStr,
  SilBtVart,
  SilOsTypes,
  SilOtTool;

{ TSilBaseFileInfo }

constructor TSilBaseFileInfo.Create(const FileName: String; const Time: TDateTime; Attributes: TFileAttributes; Size: LongWord);
begin
  inherited Create;

  DoSetFullName(FileName);
end;

constructor TSilBaseFileInfo.Create(const Handle: IHandle; const FileName: String);
begin
  inherited Create;

  DoSetFullName(FileName);
end;

function TSilBaseFileInfo.GetFullName: String;
begin
  Result := FPath + FName;
end;

function TSilBaseFileInfo.GetName: String;
begin
  Result := FName;
end;

function TSilBaseFileInfo.GetPath: String;
begin
  Result := FPath;
end;

destructor TSilBaseFileInfo.Destroy;
begin
  inherited;
end;

function TSilBaseFileInfo.GetVersion: IVersionInfo;
begin
  Result := OS.Version.Info(GetFullName());
end;

procedure TSilBaseFileInfo.DoSetFullName(const Value: String);
var
  iPos: Integer;
begin
  //iPos := Str.LastPos(CPathSeparator, Value);
  // perdon, tenia que hacerlo. pensemos en una mejor solucion
  iPos := Str.LastPos('/', Value);
  if iPos = 0 then iPos := Str.LastPos('\', Value);

  if iPos > 1 then
    FPath := Str.Copy(Value, 1, iPos);

  if iPos < Length(Value) then
    FName := Str.Copy(Value, iPos + 1);
end;

procedure TSilBaseFileInfo.SetName(const Value: String);
begin
  DoSetFullName(Value);
end;

procedure TSilBaseFileInfo.SetPath(const Value: String);
begin
  DoSetFullName(Value);
end;

{ TSilFileInfoList }

function TSilFileInfoList.Add(const Item: IFileInfo): Integer;
begin
  Result := inherited Add(Item);
end;

procedure TSilFileInfoList.AddList(const Source: IFileInfoList);
var
  i: Integer;
begin
  for i := 0 to Source.Count - 1 do Add(Source.Items[i]);
end;

function TSilFileInfoList.First: IFileInfo;
begin
  Result := GetItem(0);
end;

function TSilFileInfoList.GetItem(Index: Integer): IFileInfo;
begin
  CheckIndex(Index);
  Result := IFileInfo(inherited GetItem(Index));
end;

function TSilFileInfoList.IndexOf(const Item: IFileInfo): Integer;
begin
  Result := inherited IndexOf(Item);
end;

procedure TSilFileInfoList.Insert(Index: Integer; const Item: IFileInfo);
begin
  inherited Insert(Index, Item);
end;

function TSilFileInfoList.Last: IFileInfo;
begin
  Result := GetItem(Count - 1);
end;

function TSilFileInfoList.Remove(const Item: IFileInfo): Integer;
begin
  Result := inherited Remove(Item);
end;

procedure TSilFileInfoList.SetItem(Index: Integer; const Value: IFileInfo);
begin
  CheckIndex(Index);
  inherited SetItem(Index, Value);
end;

function TSilFileInfoList.Search(const Value: Variant; var Index: Integer): Boolean;
var
  Comparator: IComparable;
begin
  Comparator := TSilFileInfoCompare.Create(ikName, Value);
  Result := inherited CustomSearch(Comparator, Index);
end;

function TSilFileInfoList.Sort: Boolean;
var
  Comparator: IComparator;
begin
  Comparator := TSilFileInfoCompare.Create(ikName);
  Result := inherited CustomSort(Comparator);
end;

{ TSilFileInfoCompare }

constructor TSilFileInfoCompare.Create(Kind: TFileInfoKind);
begin
  Create(Kind, Vart.Null);
end;

constructor TSilFileInfoCompare.Create(Kind: TFileInfoKind; const Value: Variant);
var
  Dummy: Pointer;
  Len: Integer;
begin
  inherited Create;
  FKind := Kind;
  Len := Vart.GetInfo(Value, Dummy);
  if Len > 0 then
    begin
      SetLength(FValue, Len);
      Vart.Move(Value, FValue[1], Length(FValue));
    end
  else
    FValue := Str.Null;
end;

function TSilFileInfoCompare.Compare(const Item1; const Item2; Arg: Pointer): Integer;
var
  a1, a2: Integer;
  s1, s2: TFileAttributes;
begin
  case FKind of
    ikName:       Result := Text.Compare(IFileInfo(Item1).Name, IFileInfo(Item2).Name);
    ikDate:       Result := DateTime.Compare(IFileInfo(Item1).Time, IFileInfo(Item2).Time);
    ikSize:       Result := IFileInfo(Item1).Size - IFileInfo(Item2).Size;
    ikPath:       Result := Text.Compare(IFileInfo(Item1).FullName, IFileInfo(Item2).FullName);
    ikAttributes:
    begin
      s1 := IFileInfo(Item1).Attributes;
      s2 := IFileInfo(Item2).Attributes;
      AtoB(s1, a1);
      AtoB(s2, a2);
      Result := a1 - a2;
    end;
    else          Result := 0;
  end;
end;

function TSilFileInfoCompare.CompareTo(const Item; Arg: Pointer): Integer;
var
  a1: Integer;
  s1: TFileAttributes;
begin
  case FKind of
    ikName:       Result := Text.Compare(IFileInfo(Item).Name, FValue);
    ikDate:       Result := DateTime.Compare(IFileInfo(Item).Time, PDateTime(@FValue[1])^);
    ikSize:       Result := IFileInfo(Item).Size - PLongWord(@FValue[1])^;
    ikPath:       Result := Text.Compare(IFileInfo(Item).FullName, FValue);
    ikAttributes:
    begin
      s1 := IFileInfo(Item).Attributes;
      AtoB(s1, a1);
      Result := a1 - Integer(PLongWord(@FValue[1])^);
    end;
    else          Result := 0;
  end;
end;

{ TSilFileInfo }

constructor TSilFileInfo.Create(const FileName: String; const Time: TDateTime; Attributes: TFileAttributes; Size: LongWord);
begin
  inherited;

  FTime := Time;
  FAttributes := Attributes;
  FSize := Size;
end;

function TSilFileInfo.GetAttributes: TFileAttributes;
begin
  Result := FAttributes;
end;

function TSilFileInfo.GetTime: TDateTime;
begin
  Result := FTime;
end;

function TSilFileInfo.GetSize: LongWord;
begin
  Result := FSize;
end;

procedure TSilFileInfo.SetAttributes(Value: TFileAttributes);
begin
  FAttributes := Value;
end;

procedure TSilFileInfo.SetTime(Time: TDateTime);
begin
  FTime := Time;
end;

procedure TSilFileInfo.SetSize(Value: LongWord);
begin
  FSize := Value;
end;

end.
