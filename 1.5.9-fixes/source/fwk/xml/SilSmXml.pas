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

unit SilSmXml;

{$I Defines.inc}

interface

uses
  SilBeTypes,
  SilLkObject,
  SilLkInterfaced,
  SilLiParameters,
  SilLiLock,
  SilLiEnumerator,
  SilLiStream,
  SilLiKey,
  SilSiXml,
  SilLiStringList,

  SilVector,
  SilVectorSiVector,
  SilVectorSmInterface,
  SilVectorSmString;

const
  SInvalidNodeKind = 'SInvalidNodeKind';
  CTagSeparator = '/';

const  
  BooleanStr: array[Boolean] of String = ('False', 'True');

type
  TXmlTree = class (
    // extends
    TSilInterfacedObject,
    // implements
    IXmlTree)
  private
    FRoot: IXmlTag;
    FLineSize: LongWord;
    FModified: Boolean;
  protected // IXmlTree
    function GetRoot: IXmlNode;
    function GetLineSize: LongWord;
    procedure SetLineSize(Value: LongWord);
    function GetModified: Boolean;
    procedure SetModified(Value: Boolean);
    procedure Clear;
    function Locked: ILock; override; 
    function Write(const Stream: IStream; LineFeed: Boolean = true): Boolean;
    function FindTag(const Path: String; out Node: IXmlTag): Boolean;
    function GetTag(const Path: String; CanCreate: Boolean = false): IXmlTag;
    function CreateRoot: IXmlNode;
    property Root: IXmlNode read GetRoot;
    property LineSize: LongWord read GetLineSize write SetLineSize;
    property Modified: Boolean read GetModified write SetModified;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TXmlNode = class (
    // extends
    TSilObject, //TSilInterfacedObject,
    // implements
    IXmlNode)
  private
    FOwner: Pointer;
    FParent: Pointer;
    FChilds: IXmlNodes;
    FNodeKind: TXmlNodeKind;
  protected // IXmlNode
    function GetNodeKind: TXmlNodeKind;
    function GetParent: IXmlNode;
    function GetOwner: IXmlTree;
    function GetChilds: IXmlNodes; virtual;
    function GetAsTag: IXmlTag; virtual;
    function GetAsComment: IXmlComment; virtual;
    function Write(const Stream: IStream; Level: PWriteLevel): Boolean; virtual; abstract;
    procedure SetChilds(const Value: IXmlNodes);
    procedure SetParent(const Value: IXmlNode);
  public
    constructor Create(NodeKind: TXmlNodeKind; const Owner: IXmlTree; const Parent: IXmlNode = nil); virtual;
    destructor Destroy; override;
  end;

  TXmlNodes = class (
    // extends
    TSilObject, //TSilInterfacedObject,
    // imlements
    IXmlNodes,
    INamedValues)
  private
    FList: IVectorInterface;
    FOwner: Pointer;
    FParent: Pointer;
    function GetOwner: IXmlTree;
    function DoFind(const Name: String; out Tag: IXmlTag): Boolean;
  private
    function DoCreateNode(NodeKind: TXmlNodeKind; const Owner: IXmlTree; const Parent: IXmlNode): IXmlNode;
  protected // INamedItems
    function Remove(const Name: String): Boolean; overload;
    function Enumerate(var Enum: IEnumerator; out Name: string): Boolean; overload;
  protected // INamedValues
    function Exists(const Name: String): Boolean;
    function GetDataType(const Name: String): TNamedKeyDataType;
    function GetDataSize(const Name: String): LongWord;
    function ReadString(const Name: String; const Default: String = ''; CanCreate: Boolean = false): String;
    function ReadInteger(const Name: String; const Default: Integer = 0; CanCreate: Boolean = false): Integer;
    function ReadLargeInt(const Name: String; const Default: LargeInt = 0; CanCreate: Boolean = false): LargeInt;
    function ReadBoolean(const Name: String; const Default: Boolean = false; CanCreate: Boolean = false): Boolean;
    function ReadFloat(const Name: String; const Default: Double = 0; CanCreate: Boolean = false): Double;
    function ReadStrings(const Name: String; const Default: IStringList = nil; CanCreate: Boolean = false): IStringList;
    function WriteString(const Name: String; const Value: String): Boolean;
    function WriteInteger(const Name: String; Value: Integer): Boolean;
    function WriteLargeInt(const Name: String; Value: LargeInt): Boolean;
    function WriteBoolean(const Name: String; Value: Boolean): Boolean;
    function WriteFloat(const Name: String; Value: Double): Boolean;
    function WriteStrings(const Name: String; const Value: IStringList): Boolean;
  protected // IXmlNodes
    function GetCount: Integer;
    procedure Clear;
    function Add(Kind: TXmlNodeKind): IXmlNode; reintroduce; overload;
    function Add(const Value: IXmlNode): IXmlNode; reintroduce; overload;
    function AddTag(const Name: String): IXmlTag; overload;
    function AddTag(const Value: IXmlTag): IXmlTag; overload;
    function AddComment: IXmlComment; overload;
    function AddComment(const Value: IXmlComment): IXmlComment; overload;
    function Insert(const Before: IXmlNode; Kind: TXmlNodeKind): IXmlNode; reintroduce;
    function Remove(const Item: IXmlNode): Boolean; overload;
    function Enumerate(var Enum: IEnumerator; out Item: IXmlNode): Boolean; overload;
    function Enumerate(var Enum: IEnumerator; out Item: IXmlTag): Boolean; overload;
    function GetNext(const Value: IXmlNode; out Node: IXmlNode): Boolean;
    function GetPrev(const Value: IXmlNode; out Node: IXmlNode): Boolean;
    function GetFirst(out Node: IXmlNode): Boolean;
    function GetLast(out Node: IXmlNode): Boolean;
  public
    constructor Create(const Owner: IXmlTree; const Parent: IXmlNode);
    destructor Destroy; override;
  end;

  TXmlArguments = class (
    // extends
    TSilVectorString,
    // imlements
    IXmlArguments,
    INamedValues)
  private
    FOwner: Pointer;
  protected
    procedure DisposePtr(Index: Integer);
    procedure DisposeItem(Index: Integer); override;
  protected // INamedItems
    function Remove(const Name: String): Boolean;
    function Enumerate(var Enum: IEnumerator; out Name: string): Boolean; reintroduce;
  protected // INamedValues
    function Exists(const Name: String): Boolean;
    function GetDataType(const Name: String): TNamedKeyDataType;
    function GetDataSize(const Name: String): LongWord;
    function ReadString(const Name: String; const Default: String = ''; CanCreate: Boolean = false): String;
    function ReadInteger(const Name: String; const Default: Integer = 0; CanCreate: Boolean = false): Integer;
    function ReadLargeInt(const Name: String; const Default: LargeInt = 0; CanCreate: Boolean = false): LargeInt;
    function ReadBoolean(const Name: String; const Default: Boolean = false; CanCreate: Boolean = false): Boolean;
    function ReadFloat(const Name: String; const Default: Double = 0; CanCreate: Boolean = false): Double;
    function ReadStrings(const Name: String; const Default: IStringList = nil; CanCreate: Boolean = false): IStringList;
    function WriteString(const Name: String; const Value: String): Boolean;
    function WriteInteger(const Name: String; Value: Integer): Boolean;
    function WriteLargeInt(const Name: String; Value: LargeInt): Boolean;
    function WriteBoolean(const Name: String; Value: Boolean): Boolean;
    function WriteFloat(const Name: String; Value: Double): Boolean;
    function WriteStrings(const Name: String; const Value: IStringList): Boolean;
  protected // IXmlArguments
    function CreateNew(const Name, Value: String): Integer; reintroduce;
    function GetValueType(const Name: String): Word; // VType
    function GetOwner: IXmlTag;
    function Write(const Stream: IStream): Boolean;
    function AsParameters: IParameters;
  public
    constructor Create(const Owner: IXmlTag);
  end;

  TXmlTag = class (
    // extends
    TXmlNode,
    // imlements
    IXmlTag)
  private
    FName: String;
    FArguments: IXmlArguments;
    FTagKind: TXmlTagKind;
    FData: IStringList;
  protected // IXmlNode
    function GetChilds: IXmlNodes; override;
    function GetAsTag: IXmlTag; override;
    function Write(const Stream: IStream; Level: PWriteLevel): Boolean; override;
    procedure SetChilds(const Value: IXmlNodes);
  protected // IXmlTag
    function GetName: String;
    procedure SetName(const Value: String);
    function GetArguments: IXmlArguments;
    procedure SetArguments(const Value: IXmlArguments);
    function GetTagKind: TXmlTagKind;
    procedure SetTagKind(Value: TXmlTagKind);
    function GetData: IStringList;
    procedure SetData(const Value: IStringList);
    function GetHasData: Boolean;
    function GetHasChilds: Boolean;
    function FindTag(const Path: String; out Node: IXmlTag): Boolean;
    function GetTag(const Path: String; CanCreate: Boolean): IXmlTag;
    function FindArgument(const TagName, Argument, Value: String; out Node: IXmlTag; CanCreate: Boolean): Boolean;
  public
    constructor Create(const Owner: IXmlTree; const Parent: IXmlNode = nil); reintroduce;
    destructor Destroy; override;
  end;

  TXmlStrings = class (
    // extends
    TXmlNode,
    // imlements
    IXmlComment)
  private
    FLines: IStringList;
  protected
    function DoGetKind: TXmlNodeKind; virtual; abstract;
  protected // IXmlComment
    function GetLines: IStringList;
    procedure SetLines(const Value: IStringList);
  public
    constructor Create(const Owner: IXmlTree; const Parent: IXmlNode = nil); reintroduce;
    destructor Destroy; override;
  end;

  TXmlComment = class (
    // extends
    TXmlStrings)
  protected
    function GetAsComment: IXmlComment; override;
    function DoGetKind: TXmlNodeKind; override;
  protected // IXmlNode
    function Write(const Stream: IStream; Level: PWriteLevel): Boolean; override;
  end;

implementation

uses
  SysUtils,

  SilBtText,
  SilBtStr,
  SilBtFloat,
  SilBtInt,
  SilBtVart,
  SilBtLarge,
  SilBtError,
  SilBcChr,
  SilBtStringPtr,
  SilLtList,
  SilLtReference,
  SilSmMimeCoder,
  SilVectorSkVector;

function GetLevel(Level: PWriteLevel): Integer;
begin
  if Assigned(Level) then
    Result := Level.Current else
    Result := 0;
end;

{ TXmlNode }

constructor TXmlNode.Create(NodeKind: TXmlNodeKind; const Owner: IXmlTree; const Parent: IXmlNode);
begin
  inherited Create;

  FNodeKind := NodeKind;
  FOwner := Pointer(Owner);
  FParent := Pointer(Parent);
end;

destructor TXmlNode.Destroy;
begin
  FOwner := nil;
  FParent := nil;
  FChilds := nil;
  inherited;
end;

function TXmlNode.GetAsComment: IXmlComment;
begin
  Error.Throw(SInvalidNodeKind);
end;

function TXmlNode.GetAsTag: IXmlTag;
begin
  Error.Throw(SInvalidNodeKind);
end;

function TXmlNode.GetChilds: IXmlNodes;
begin
  if FChilds = nil then FChilds := TXmlNodes.Create(GetOwner, Self);
  Result := FChilds;
end;

function TXmlNode.GetNodeKind: TXmlNodeKind;
begin
  Result := FNodeKind;
end;

function TXmlNode.GetOwner: IXmlTree;
begin
  Result := IXmlTree(FOwner);
end;

function TXmlNode.GetParent: IXmlNode;
begin
  Result := IXmlNode(FParent);
end;

procedure TXmlNode.SetChilds(const Value: IXmlNodes);
var
  Enum: IEnumerator;
  Node: IXmlNode;
begin
  if Value.Count > 0 then
  begin
    GetChilds.Clear;

    while Value.Enumerate(Enum, Node) do
      case Node.NodeKind of
        nkTag:      FChilds.AddTag(Node.AsTag);
        nkComment:  FChilds.AddComment(Node.AsComment);
      end;
  end;
end;

procedure TXmlNode.SetParent(const Value: IXmlNode);
var
  parent: IXmlNode;
begin
  parent := GetParent;

  if not ref.SameObject(value, parent) then
  begin
    if parent <> nil then
      parent.Childs.Remove(self);

    FParent := Pointer(Parent);
    Value.Childs.Add(Self);
  end;
end;

{ TXmlNodes }

constructor TXmlNodes.Create(const Owner: IXmlTree; const Parent: IXmlNode);
begin
  inherited Create;

  FList := SilVector.InterfaceList;
  FOwner := Pointer(Owner);
  FParent := Pointer(Parent);
end;

destructor TXmlNodes.Destroy;
begin
  FOwner := nil;
  FParent := nil;
  FList := nil;

  inherited;
end;

function TXmlNodes.DoCreateNode(NodeKind: TXmlNodeKind; const Owner: IXmlTree; const Parent: IXmlNode): IXmlNode;
begin
  case NodeKind of
    nkTag:      Result := TXmlTag.Create(Owner, Parent);
    nkComment:  Result := TXmlComment.Create(Owner, Parent);
    else        Result := nil;
  end;
end;

function TXmlNodes.Add(Kind: TXmlNodeKind): IXmlNode;
begin
  Result := DoCreateNode(Kind, IXmlTree(FOwner), IXmlNode(FParent));
  Add(result);
end;

function TXmlNodes.Add(const Value: IXmlNode): IXmlNode;
begin
  if value <> nil then
  begin
    FList.Add(value);
    GetOwner.Modified := true;
  end;
end;

function TXmlNodes.Insert(const Before: IXmlNode; Kind: TXmlNodeKind): IXmlNode;
var
  iIdx: Integer;
begin
  iIdx := FList.IndexOf(Before);
  if iIdx > -1 then
  begin
    Result := DoCreateNode(Kind, IXmlTree(FOwner), IXmlNode(FParent));
    FList.Insert(iIdx, Result);
    GetOwner.Modified := true;
  end;
end;

function TXmlNodes.Remove(const Item: IXmlNode): Boolean;
begin
  Result := FList.Remove(Item) > -1;
  if Result then GetOwner.Modified := true;
end;

function TXmlNodes.GetOwner: IXmlTree;
begin
  Result := IXmlTree(FOwner);
end;

function TXmlNodes.Enumerate(var Enum: IEnumerator; out Item: IXmlNode): Boolean;
begin
  Result := SilVector.Enumerate(enum, FList, item);
end;

function TXmlNodes.Enumerate(var Enum: IEnumerator; out Item: IXmlTag): Boolean;
var
  Node: IXmlNode;
begin
  Result := false;

  while true do
  begin
    Result := SilVector.Enumerate(enum, FList, node);

    if Result then
    begin
      if Node.NodeKind = nkTag then
      begin
        Item := Node.AsTag;
        Break;
      end;
    end else
      Break;
  end;
end;

function TXmlNodes.DoFind(const Name: String; out Tag: IXmlTag): Boolean;
var
  enum: IVectorInterfaceEnumerator;
  Node: IXmlNode;
begin
  enum := FList.Enumerator;
  
  while enum.Enumerate(Node) do
    if Node.NodeKind = nkTag then
    begin
      Tag := Node.AsTag;

      if Text.Compare(Name, Tag.Name) = 0 then
      begin
        Result := true;
        Exit;
      end;
    end;

  Result := false;
end;

function TXmlNodes.Exists(const Name: String): Boolean;
var
  Tag: IXmlTag;
begin
  Result := DoFind(Name, Tag);
end;

function TXmlNodes.GetDataSize(const Name: String): LongWord;
var
  Tag: IXmlTag;
begin
  if DoFind(Name, Tag) then
    Result := Length(Tag.Data.Text) else
    Result := 0;
end;

function TXmlNodes.GetDataType(const Name: String): TNamedKeyDataType;
begin
  Result := kdUnknown;
end;

function TXmlNodes.ReadBoolean(const Name: String; const Default: Boolean; CanCreate: Boolean): Boolean;
begin
  Result := Text.Compare(ReadString(Name, BooleanStr[Default], CanCreate), BooleanStr[true]) = 0;
end;

function TXmlNodes.ReadFloat(const Name: String; const Default: Double; CanCreate: Boolean): Double;
var
  sVal: String;
begin
  sVal := ReadString(Name, Str.Replace(Float.ToStr(Default), DecimalSeparator, '.'), CanCreate);
  Result := Str.ToFloat(Str.Replace(sVal, '.', DecimalSeparator), 0);
end;

function TXmlNodes.ReadInteger(const Name: String; const Default: Integer; CanCreate: Boolean): Integer;
begin
  Result := Str.ToInt(ReadString(Name, Int.ToStr(Default), CanCreate), 0);
end;

function TXmlNodes.ReadLargeInt(const Name: String; const Default: LargeInt; CanCreate: Boolean): LargeInt;
begin
  Result := Str.ToLarge(ReadString(Name, Int.ToStr(Default), CanCreate), 0);
end;

function TXmlNodes.ReadString(const Name, Default: String; CanCreate: Boolean): String;
var
  Tag: IXmlTag;
begin
  if not DoFind(Name, Tag) then
  begin
    Result := Default;

    if CanCreate then
    begin
      Tag := Add(nkTag).AsTag;
      Tag.Name := Name;
      Tag.Data.Text := Result;
    end;
  end else
    Result := Tag.Data.Text;
end;

function TXmlNodes.ReadStrings(const Name: String; const Default: IStringList; CanCreate: Boolean): IStringList;
var
  Tag: IXmlTag;
begin
  if not DoFind(Name, Tag) then
  begin
    Result := Default;

    if CanCreate then
    begin
      Tag := Add(nkTag).AsTag;
      Tag.Name := Name;
      Tag.Data := Result;
    end;
  end else
    Result := Tag.Data;
end;

function TXmlNodes.WriteBoolean(const Name: String; Value: Boolean): Boolean;
begin
  Result := WriteString(Name, BooleanStr[Value]);
end;

function TXmlNodes.WriteFloat(const Name: String; Value: Double): Boolean;
begin
  Result := WriteString(Name, Float.ToStr(Value, '.'));
end;

function TXmlNodes.WriteInteger(const Name: String; Value: Integer): Boolean;
begin
  Result := WriteString(Name, Int.ToStr(Value));
end;

function TXmlNodes.WriteLargeInt(const Name: String; Value: LargeInt): Boolean;
begin
  Result := WriteString(Name, Large.ToStr(Value));
end;

function TXmlNodes.WriteString(const Name, Value: String): Boolean;
var
  Tag: IXmlTag;
begin
  if not DoFind(Name, Tag) then
  begin
    Tag := Add(nkTag).AsTag;
    Tag.Name := Name;
    Tag.TagKind := tkBlock;
  end;

  Tag.Data.Text := Value;
  GetOwner.Modified := true;
  Result := true;
end;

function TXmlNodes.WriteStrings(const Name: String; const Value: IStringList): Boolean;
var
  Tag: IXmlTag;
begin
  if not DoFind(Name, Tag) then
  begin
    Tag := Add(nkTag).AsTag;
    Tag.Name := Name;
    Tag.TagKind := tkBlock;
  end;

  Tag.Data := Value;
  GetOwner.Modified := true;
  Result := true;
end;

function TXmlNodes.Enumerate(var Enum: IEnumerator; out Name: string): Boolean;
var
  Item: IXmlNode;
begin
  Result := Enumerate(Enum, Item);

  if Item.NodeKind = nkTag then
    Name := Item.AsTag.Name else
    Name := '';
end;

function TXmlNodes.Remove(const Name: String): Boolean;
var
  Tag: IXmlTag;
begin
  Result := DoFind(Name, Tag);
  if Result then Remove(Tag);
end;

procedure TXmlNodes.Clear;
begin
  FList.Clear;
end;

function TXmlNodes.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TXmlNodes.AddTag(const Value: IXmlTag): IXmlTag;
begin
  Result := AddTag(Value.Name);
  Result.Arguments := Value.Arguments;

  if Value.HasChilds then
    Result.Childs := Value.Childs;

  if Value.HasData then
    Result.Data.Text := Value.Data.Text;
end;

function TXmlNodes.AddComment(const Value: IXmlComment): IXmlComment;
begin
  Result := AddComment(Value);
  Result.Lines.Text := Value.Lines.Text;
end;

function TXmlNodes.AddComment: IXmlComment;
begin
  Result := Add(nkComment).AsComment;
end;

function TXmlNodes.AddTag(const Name: String): IXmlTag;
begin
  Result := Add(nkTag).AsTag;
  Result.Name := Name;
end;

function TXmlNodes.GetFirst(out Node: IXmlNode): Boolean;
begin
  Result := FList.Count > 0;
  if Result then Node := FList.First as IXmlNode;
end;

function TXmlNodes.GetLast(out Node: IXmlNode): Boolean;
begin
  Result := FList.Count > 0;
  if Result then Node := FList.Last as IXmlNode;
end;

function TXmlNodes.GetNext(const Value: IXmlNode; out Node: IXmlNode): Boolean;
var
  Idx: Integer;
begin
  Idx := FList.IndexOf(Value);
  Result := (Idx > -1) and (Idx < FList.Count - 1);
  if Result then Node := FList.Items[Idx + 1] as IXmlNode;
end;

function TXmlNodes.GetPrev(const Value: IXmlNode; out Node: IXmlNode): Boolean;
var
  Idx: Integer;
begin
  Idx := FList.IndexOf(Value);
  Result := Idx > 0;
  if Result then Node := FList.Items[Idx - 1] as IXmlNode;
end;

{ TXmlArguments }

constructor TXmlArguments.Create(const Owner: IXmlTag);
begin
  inherited Create(false);

  SetIgnoreCase(true);
  FOwner := Pointer(Owner);
end;

function TXmlArguments.Write(const Stream: IStream): Boolean;
var
  e: IVectorStringEnumerator;
  Item, Value, sBuf: String;
begin
  if Stream = nil then
  begin
    Result := false;
    Exit;
  end;

  e := Enumerator;

  while e.Enumerate(Item) do
  begin
    Value := TXmlCoder.EncodeLine(PString(GetPtr(e.Index))^);
    sBuf := Item + '="' + Value + '"' + Str.IIf(e.Index < GetCount - 1, ' ', '');
    Stream.Write(sBuf[1], Length(sBuf));
  end;

  Result := true;
end;

function TXmlArguments.CreateNew(const Name, Value: String): Integer;
var
  p: pstring;
begin
  new(p);
  p^ := value;
  Result := Add(Name, p);
end;

function TXmlArguments.GetOwner: IXmlTag;
begin
  Result := IXmlTag(FOwner);
end;

function TXmlArguments.Exists(const Name: String): Boolean;
begin
  Result := IndexOf(Name) > -1;
end;

function TXmlArguments.GetDataSize(const Name: String): LongWord;
begin
  Result := 0;
end;

function TXmlArguments.GetDataType(const Name: String): TNamedKeyDataType;
begin
  Result := kdUnknown;
end;

function TXmlArguments.ReadBoolean(const Name: String; const Default: Boolean; CanCreate: Boolean): Boolean;
begin
  Result := Text.Compare(ReadString(Name, BooleanStr[Default], CanCreate), BooleanStr[true]) = 0;
end;

function TXmlArguments.ReadFloat(const Name: String; const Default: Double; CanCreate: Boolean): Double;
var
  sVal: String;
begin
  sVal := ReadString(Name, Str.Replace(Float.ToStr(Default), DecimalSeparator, '.'), CanCreate);
  Result := Str.ToFloat(Str.Replace(sVal, '.', DecimalSeparator), 0);
end;

function TXmlArguments.ReadLargeInt(const Name: String; const Default: LargeInt; CanCreate: Boolean): LargeInt;
begin
  Result := Str.ToLarge(ReadString(Name, Int.ToStr(Default), CanCreate), 0);
end;

function TXmlArguments.ReadInteger(const Name: String; const Default: Integer; CanCreate: Boolean): Integer;
begin
  Result := Str.ToInt(ReadString(Name, Int.ToStr(Default), CanCreate), 0);
end;

function TXmlArguments.ReadString(const Name, Default: String; CanCreate: Boolean): String;
var
  i: Integer;
begin
  i := IndexOf(Name);

  if i = -1 then
  begin
    Result := Default;
    if CanCreate then WriteString(Name, Default);
  end else
    Result := PString(GetPtr(i))^;
end;

function TXmlArguments.ReadStrings(const Name: String; const Default: IStringList; CanCreate: Boolean): IStringList;
begin
  Result := nil;
end;

function TXmlArguments.Remove(const Name: String): Boolean;
var
  i: Integer;
begin
  i := IndexOf(Name);
  Result := i > -1;
  if Result then Delete(i);
end;

function TXmlArguments.Enumerate(var Enum: IEnumerator; out Name: string): Boolean;
begin
  Result := SilVector.Enumerate(enum, self, name);
end;

function TXmlArguments.WriteBoolean(const Name: String; Value: Boolean): Boolean;
begin
  Result := WriteString(Name, BooleanStr[Value]);
end;

function TXmlArguments.WriteFloat(const Name: String; Value: Double): Boolean;
begin
  Result := WriteString(Name, Float.ToStr(Value, '.'));
end;

function TXmlArguments.WriteInteger(const Name: String; Value: Integer): Boolean;
begin
  Result := WriteString(Name, Int.ToStr(Value));
end;

function TXmlArguments.WriteLargeInt(const Name: String; Value: LargeInt): Boolean;
begin
  Result := WriteString(Name, Large.ToStr(Value));
end;

function TXmlArguments.WriteString(const Name, Value: String): Boolean;
var
  i: Integer;
  p: pstring;
begin
  i := IndexOf(Name);
  Result := true;
  new(p);
  p^ := value;

  if i = -1 then
    Add(Name, p) else
  begin
    DisposePtr(i);
    SetPtr(i, p);
  end;

  GetOwner.Owner.Modified := true;
end;

function TXmlArguments.WriteStrings(const Name: String; const Value: IStringList): Boolean;
begin
  Result := false;
end;

function TXmlArguments.AsParameters: IParameters;
var
  Enum: IEnumerator;
  Name: String;
  List: IParameterList;
begin
  List := ListTool.Parameters;

  while Enumerate(Enum, Name) do
    List[Name] := PString(GetPtr(Enum.Iteration))^;

  Result := List;
end;

function TXmlArguments.GetValueType(const Name: String): Word;
var
  i: Integer;
  Value: String;
begin
  Value := ReadString(Name);

  if Str.FirstChar(Value) in ['0'..'9', '-'] then
  begin
    Result := varInteger;

    for i := 1 to Length(Value) do
      if not (Value[i] in ['0'..'9', '-']) then
      begin
        if (Value[i] in ['.', ',']) and (Result = varInteger) then
          Result := varDouble else
        begin
          Result := varString;
          Break;
        end;
      end;
  end else
    Result := varString;

  // todo! varDate
end;

procedure TXmlArguments.DisposePtr(Index: Integer);
var
  p: PString;
begin
  p := getptr(index);
  if p <> nil then dispose(p);
end;

procedure TXmlArguments.DisposeItem(Index: Integer);
begin
  DisposePtr(index);
  inherited;
end;

{ TXmlTag }

constructor TXmlTag.Create(const Owner: IXmlTree; const Parent: IXmlNode);
begin
  inherited Create(nkTag, Owner, Parent);
  FTagKind := tkUnknown;
end;

destructor TXmlTag.Destroy;
begin
  FArguments := nil;
  inherited;
end;

procedure TXmlTag.SetArguments(const Value: IXmlArguments);
var
  Enum: IEnumerator;
  ArgName: String;
begin
  GetArguments;

  while Value.Enumerate(Enum, ArgName) do
    FArguments.WriteString(ArgName, Value.ReadString(ArgName));
end;

function TXmlTag.GetArguments: IXmlArguments;
begin
  if FArguments = nil then FArguments := TXmlArguments.Create(Self);
  Result := FArguments;
end;

function TXmlTag.GetAsTag: IXmlTag;
begin
  Result := Self;
end;

function TXmlTag.GetName: String;
begin
  Result := FName;
end;

function TXmlTag.GetTagKind: TXmlTagKind;
begin
  Result := FTagKind;
end;

function TXmlTag.Write(const Stream: IStream; Level: PWriteLevel): Boolean;
var
  e: IEnumerator;
  Item: IXmlNode;
  sLevel, sItem, sBuf, sCRLF: String;
  DataCount: Integer;
  TagKind: TXmlTagKind;
begin
  if Stream = nil then
  begin
    Result := false;
    Exit;
  end;

  sBuf := '';

  if Assigned(Level) then
  begin
    sCRLF := ccCRLF;
    sLevel := Str.Replicate(' ', Level.Current * 2 + 2);

    if (Level.Previous >= Level.Current) and
      ((Assigned(FChilds) and (FChilds.Count > 0) or
      (Assigned(FData) and (FData.Count > 1)))) then
      sBuf := sCRLF;

    Level.Previous := Level.Current;
  end else
  begin
    sCRLF := '';
    sLevel := '';
  end;

  sBuf := sBuf + Str.Replicate(' ', GetLevel(Level) * 2) + '<' + FName +
    Str.IIf(Assigned(FArguments) and (FArguments.Count > 0), ' ', '');

  Stream.Write(sBuf[1], Length(sBuf));

  if FArguments <> nil then
    FArguments.Write(Stream);

  TagKind := FTagKind;

  if TagKind in [tkUnknown, tkInline] then
    if (Assigned(FChilds) and (FChilds.Count > 0)) or (Assigned(FData) and (FData.Count > 0)) then
      TagKind := tkBlock else
      TagKind := tkInline;

  case TagKind of
    tkBlock:    sBuf := '>';
    tkInline:
    begin
      sBuf := '/>' + sCRLF;
      if Assigned(Level) then Dec(Level.Previous);
    end;
    tkUnknown:  sBuf := '';
  end;

  Stream.Write(sBuf[1], Length(sBuf));

  if Assigned(FData) and (FData.Count > 0) then
  begin
    DataCount := FData.Count;

    if DataCount > 1 then
      Stream.Write(sCRLF[1], Length(sCRLF));

    while FData.Enumerate(e, sItem) do
    begin
      sBuf := Str.IIf(DataCount > 1, sLevel) + TXmlCoder.EncodeLine(Str.Trim(sItem));

      if DataCount > 1 then
      begin
        if e.Iteration = DataCount - 1 then
          sBuf := sBuf + sCRLF else
          sBuf := sBuf + ccCRLF;
      end;

      Stream.Write(sBuf[1], Length(sBuf));
    end;

    if FData.Count = 0 then DataCount := 1;
  end else
  if Assigned(FChilds) and (FChilds.Count > 0) then
  begin
    DataCount := FChilds.Count + 1;
    Stream.Write(sCRLF[1], Length(sCRLF));

    while FChilds.Enumerate(e, Item) do
    begin
      if Assigned(Level) then Inc(Level.Current);
      Item.Write(Stream, Level);
      if Assigned(Level) then Dec(Level.Current);
    end;
  end else
    DataCount := 0;

  if TagKind = tkBlock then
  begin
    sBuf := Str.IIf(DataCount > 1, Str.Replicate(' ', GetLevel(Level) * 2)) + '</' + FName + '>' + sCRLF;
    Stream.Write(sBuf[1], Length(sBuf));
  end;

  Result := true;
end;

procedure TXmlTag.SetName(const Value: String);
begin
  FName := Value;
  GetOwner.Modified := true;
end;

procedure TXmlTag.SetTagKind(Value: TXmlTagKind);
begin
  FTagKind := Value;
end;

function TXmlTag.GetData: IStringList;
begin
  if FData = nil then FData := ListTool.StringList();
  Result := FData;
end;

procedure TXmlTag.SetData(const Value: IStringList);
begin
  FData := Value;
end;

function TXmlTag.GetHasData: Boolean;
begin
  Result := FData <> nil;
end;

function TXmlTag.GetHasChilds: Boolean;
begin
  Result := FChilds <> nil;
end;

function TXmlTag.FindTag(const Path: String; out Node: IXmlTag): Boolean;
begin
  Node := GetTag(Path, false);
  Result := Node <> nil;
end;

function TXmlTag.GetTag(const Path: String; CanCreate: Boolean): IXmlTag;
var
  e: IEnumerator;
  Item: IXmlNode;
  Tag: IXmlTag;
  i: Integer;
  sName: String;
begin
  Result := nil;

  i := 0;
  sName := Str.Token(Path, CTagSeparator, i);
  if Length(sName) = 0 then Exit;

  if (FChilds = nil) and CanCreate then GetChilds;

  if FChilds <> nil then
    while FChilds.Enumerate(e, Item) do
      if Item.NodeKind = nkTag then
      begin
        Tag := Item.AsTag;

        if Text.Compare(Tag.Name, sName) = 0 then
          if i = 0 then
          begin
            Result := Tag;
            Exit;
          end else
          begin
            Result := Tag.GetTag(Str.Copy(Path, i), CanCreate);
            if Result <> nil then Exit;
          end;
      end;

  if (Result = nil) and CanCreate then
  begin
    Result := FChilds.Add(nkTag).AsTag;
    Result.Name := sName;

    if i > 0 then
    begin
      sName := Str.Copy(Path, i);

      if Str.NotEmpty(sName) then
        Result := Result.GetTag(sName, CanCreate);
    end;
  end;
end;

function TXmlTag.GetChilds: IXmlNodes;
begin
  Result := inherited GetChilds;
end;

function TXmlTag.FindArgument(const TagName, Argument, Value: String; out Node: IXmlTag; CanCreate: Boolean): Boolean;
var
  Childs: IXmlNodes;
  Enum: IEnumerator;
  Item, Tag: IXmlTag;
  FindName: String;
  Pos: Integer;
begin
  Pos := Str.LastPos(CTagSeparator, TagName);

  if Pos > 0 then
  begin
    FindName := Str.Copy(TagName, 1, Pos - Length(CTagSeparator));
    Tag := GetTag(FindName, CanCreate);

    if Assigned(Tag) then
    begin
      Childs := Tag.Childs;
      FindName := Str.Copy(TagName, Pos + 1);
    end;
  end else
  begin
    FindName := TagName;
    Childs := GetChilds;
  end;

  if Assigned(Childs) then
    while Childs.Enumerate(Enum, Item) do
      if (Text.Compare(Item.Name, FindName) = 0) and (Item.Arguments.ReadString(Argument) = Value) then
      begin
        Node := Item;
        Result := true;
        Exit;
      end;

  Result := CanCreate;

  if CanCreate then
  begin
    Node := Childs.Add(nkTag).AsTag;
    Node.Name := FindName;
    Node.Arguments.WriteString(Argument, Value);
  end;
end;

procedure TXmlTag.SetChilds(const Value: IXmlNodes);
begin
  inherited;
end;

{ TXmlStrings }

constructor TXmlStrings.Create(const Owner: IXmlTree; const Parent: IXmlNode);
begin
  inherited Create(DoGetKind, Owner, Parent);
  FLines := ListTool.StringList();
end;

destructor TXmlStrings.Destroy;
begin
  FLines := nil;
  inherited;
end;

function TXmlStrings.GetLines: IStringList;
begin
  Result := FLines;
end;

procedure TXmlStrings.SetLines(const Value: IStringList);
begin
  FLines := Value;
end;

{ TXmlComment }

function TXmlComment.DoGetKind: TXmlNodeKind;
begin
  Result := nkComment;
end;

function TXmlComment.GetAsComment: IXmlComment;
begin
  Result := Self;
end;

function TXmlComment.Write(const Stream: IStream; Level: PWriteLevel): Boolean;
var
  e: IEnumerator;
  sLevel, sLevel2, sBuf, sCRLF: String;
  bMulti: Boolean;
begin
  sBuf := '';

  if Assigned(Level) then
  begin
    sCRLF := ccCRLF;
    sLevel := Str.Replicate(' ', Level.Current * 2);
    sLevel2 := Str.Replicate(' ', Level.Current * 2 + 2);

    if Level.Previous >= Level.Current then sBuf := sCRLF;
    Level.Previous := Level.Current;
  end else
  begin
    sCRLF := '';
    sLevel := '';
    sLevel2 := '';
  end;

  bMulti := FLines.Count > 1;

  sBuf := sBuf + sLevel + '<!--' + Str.IIf(not bMulti, ' ', sCRLF);
  Stream.Write(sBuf[1], Length(sBuf));

  while FLines.Enumerate(e, sBuf) do
  begin
    sBuf := TXmlCoder.EncodeLine(sBuf);
    if bMulti then sBuf := sLevel2 + sBuf + Str.IIf(e.Iteration = FLines.Count - 1, sCRLF, ccCRLF);
    Stream.Write(sBuf[1], Length(sBuf));
  end;

  sBuf := Str.IIf(bMulti, sLevel, ' ') + '-->' + sCRLF;
  Stream.Write(sBuf[1], Length(sBuf));
  
  Result := true;
end;

{ TXmlTree }

constructor TXmlTree.Create;
begin
  inherited Create;
end;

destructor TXmlTree.Destroy;
begin
  FRoot := nil;
  inherited;
end;

procedure TXmlTree.Clear;
begin
  if FRoot <> nil then
  begin
    FRoot.Childs.Clear;
    FRoot := nil;
  end;
end;

function TXmlTree.GetRoot: IXmlNode;
begin
  if not Assigned(FRoot) then CreateRoot;
  Result := FRoot;
end;

function TXmlTree.Locked: ILock;
begin
  Result := nil;
end;

function TXmlTree.Write(const Stream: IStream; LineFeed: Boolean): Boolean;
var
  Level: RWriteLevel;
  PLevel: PWriteLevel;
  xmlve: string;
begin
  Level.Current := 0;
  Level.Previous := -1;

  if LineFeed then
    PLevel := @Level else
    PLevel := nil;

  Result := FRoot <> nil;

  if result then
  begin
    xmlve := '<?xml version="1.0" encoding="UTF-8"?>' + str.iif(linefeed, ccCRLF);
    Stream.Write(xmlve[1], length(xmlve));
    FRoot.Write(Stream, PLevel);
  end;
end;

function TXmlTree.CreateRoot: IXmlNode;
begin
  FRoot := TXmlTag.Create(Self);
  Result := FRoot;
end;

function TXmlTree.GetLineSize: LongWord;
begin
  Result := FLineSize;
end;

procedure TXmlTree.SetLineSize(Value: LongWord);
begin
  FLineSize := Value;
end;

function TXmlTree.FindTag(const Path: String; out Node: IXmlTag): Boolean;
begin
  Node := GetTag(Path, false);
  Result := Node <> nil;
end;

function TXmlTree.GetTag(const Path: String; CanCreate: Boolean): IXmlTag;
var
  i: Integer;
  Right, Left: string;
begin
  i := 0;

  if not Assigned(FRoot) then
  begin
    CreateRoot;
    FRoot.Name := Str.Token(Path, CTagSeparator, i);
    i := 0;
  end;

  if Assigned(FRoot)
    and Str.Split(Path, CTagSeparator, Left, Right)
    and (Text.Compare(Left, FRoot.Name) = 0) then
      Result := FRoot else
      Result := nil;

  if Assigned(Result) and Str.IsAssigned(Right) then
    Result := Result.GetTag(Right, CanCreate);
end;

function TXmlTree.GetModified: Boolean;
begin
  Result := FModified;
end;

procedure TXmlTree.SetModified(Value: Boolean);
begin
  FModified := Value;
end;

end.
