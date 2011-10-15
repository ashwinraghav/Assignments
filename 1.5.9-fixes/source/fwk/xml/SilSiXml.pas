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

unit SilSiXml;

{$I Defines.inc}

interface

uses
  SilLiParameters,
  SilLiLock,
  SilLiEnumerator,
  SilLiStream,
  SilLiStringList,
  SilLiKey;

type
  TXmlNodeKind = (nkUnknown, nkTag, nkComment);
  TXmlTagKind = (tkUnknown, tkBlock, tkInline);

  IXmlNode = interface;
  IXmlTag = interface;
  IXmlComment = interface;

  IXmlTree = interface
    ['{651D8244-C031-4B23-B897-38C614A29972}']
    function GetRoot: IXmlNode;
    function GetLineSize: LongWord;
    procedure SetLineSize(Value: LongWord);
    function GetModified: Boolean;
    procedure SetModified(Value: Boolean);
    procedure Clear;
    function Locked: ILock;
    function Write(const Stream: IStream; LineFeed: Boolean = true): Boolean;
    function FindTag(const Path: String; out Node: IXmlTag): Boolean;
    function GetTag(const Path: String; CanCreate: Boolean = false): IXmlTag;
    function CreateRoot: IXmlNode;
    //function Enumerate(var Enum: IEnumerator; out Item): Boolean;
    property Root: IXmlNode read GetRoot;
    property LineSize: LongWord read GetLineSize write SetLineSize;
    property Modified: Boolean read GetModified write SetModified;
  end;

  IXmlNodes = interface (INamedValues)
    ['{7644E108-F06B-45D2-8818-F8B222466B5F}']
    function Locked: ILock;
    function Remove(const Name: String): Boolean; overload;
    function GetCount: Integer;
    function Add(Kind: TXmlNodeKind): IXmlNode; overload;
    function Add(const Value: IXmlNode): IXmlNode; overload;
    function AddTag(const Name: String): IXmlTag; overload;
    function AddTag(const Value: IXmlTag): IXmlTag; overload;
    function AddComment: IXmlComment; overload;
    function AddComment(const Value: IXmlComment): IXmlComment; overload;
    function Insert(const Before: IXmlNode; Kind: TXmlNodeKind): IXmlNode;
    function Remove(const Value: IXmlNode): Boolean; overload;
    function Enumerate(var Enum: IEnumerator; out Name: string): Boolean; overload;
    function Enumerate(var Enum: IEnumerator; out Item: IXmlNode): Boolean; overload;
    function Enumerate(var Enum: IEnumerator; out Item: IXmlTag): Boolean; overload;
    function GetNext(const Value: IXmlNode; out Node: IXmlNode): Boolean;
    function GetPrev(const Value: IXmlNode; out Node: IXmlNode): Boolean;
    function GetFirst(out Node: IXmlNode): Boolean;
    function GetLast(out Node: IXmlNode): Boolean;
    procedure Clear;
    property Count: Integer read GetCount;
  end;

  PWriteLevel = ^RWriteLevel;
  RWriteLevel = record
    Current: Integer;
    Previous: Integer;
  end;

  IXmlNode = interface
    ['{E868D9D5-FA6F-4239-A5B0-C244E7FBCED2}']
    function GetNodeKind: TXmlNodeKind;
    function GetParent: IXmlNode;
    function GetOwner: IXmlTree;
    function GetChilds: IXmlNodes;
    function GetAsTag: IXmlTag;
    function GetAsComment: IXmlComment;
    function Write(const Stream: IStream; Level: PWriteLevel = nil): Boolean;
    procedure SetParent(const Value: IXmlNode);
    procedure SetChilds(const Value: IXmlNodes);
    property Parent: IXmlNode read GetParent write SetParent;
    property Owner: IXmlTree read GetOwner;
    property Childs: IXmlNodes read GetChilds write SetChilds;
    property NodeKind: TXmlNodeKind read GetNodeKind;
    property AsTag: IXmlTag read GetAsTag;
    property AsComment: IXmlComment read GetAsComment;
  end;

  IXmlArguments = interface (INamedValues)
    ['{3DC3BF5D-6D0C-4ACF-B077-4778A8E4FBD4}']
    function GetOwner: IXmlTag;
    function CreateNew(const Name, Value: String): Integer;
    procedure Clear;
    function GetValueType(const Name: String): Word; // VType
    function GetCount: Integer;
    function Write(const Stream: IStream): Boolean;
    function AsParameters: IParameters;
    property Count: Integer read GetCount;
    property Owner: IXmlTag read GetOwner;
  end;

  IXmlTag = interface (IXmlNode)
    ['{CF40D908-48F4-46E1-AB16-A80BBB755608}']
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
    function GetTag(const Path: String; CanCreate: Boolean = false): IXmlTag;
    function FindArgument(const TagName, Argument, Value: String; out Node: IXmlTag; CanCreate: Boolean = false): Boolean;
    property Name: String read GetName write SetName;
    property Arguments: IXmlArguments read GetArguments write SetArguments;
    property TagKind: TXmlTagKind read GetTagKind write SetTagKind;
    property HasChilds: Boolean read GetHasChilds;
    property HasData: Boolean read GetHasData;
    property Data: IStringList read GetData write SetData;
  end;

  IXmlComment = interface (IXmlNode)
    ['{CEED5F3E-C50D-458F-9D7B-F55BCD54672F}']
    function GetLines: IStringList;
    procedure SetLines(const Value: IStringList);
    property Lines: IStringList read GetLines write SetLines;
  end;

implementation

end.
