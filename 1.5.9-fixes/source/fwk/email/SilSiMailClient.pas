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

unit SilSiMailClient;

interface

uses
  SilLiList,
  SilLiStringList,
  SilLiStream;

type

  TProgressKind = (pkDecode, pkEncode);
  TProgressCallback = procedure(const Size, Current: Integer; const Kind: TProgressKind) of object;

{ IMailInterfaceList }

  IMailInterfaceList = interface (IList)
    ['{C41E13D1-0919-11D4-9152-00C0261013CD}']
    function Add(const Item: IUnknown): Integer;
    function IndexOf(const Item: IUnknown): Integer;
    procedure Insert(Index: Integer; const Item: IUnknown);
    function Remove(const Item: IUnknown): Integer;
  end;

{ IMimeParam }

  IMimeParam = interface
    ['{CAF52488-080A-11D4-9150-00C0261013CD}']
    function GetName: String;
    procedure SetName(const Value: String);
    function GetValue: String;
    procedure SetValue(const Value: String);
    property Name: String read GetName write SetName;
    property Value: String read GetValue write SetValue;
  end;

{ IMimeParamList }

  IMimeParamList = interface(IMailInterfaceList)
    ['{CAF52489-080A-11D4-9150-00C0261013CD}']
    function First: IMimeParam;
    function Last: IMimeParam;
    function GetItem(Index: Integer): IMimeParam;
    procedure SetItem(Index: Integer; const Value: IMimeParam);
    function AddNew(const Name, Value: String): IMimeParam;
    function Find(const Name: String): IMimeParam;
    property Items[Index: Integer]: IMimeParam read GetItem write SetItem; default;
  end;

{ IMimeLabel }

  IMimeLabel = interface
    ['{CAF52487-080A-11D4-9150-00C0261013CD}']
    function GetName: String;
    procedure SetName(const Value: String);
    function GetValue: String;
    procedure SetValue(const Value: String);
    function GetParams: IMimeParamList;
    function GetCoders: IStringList;
    function FindCoder(const Name: String; out Coder): Boolean;
    procedure SetCoders(const Value: IStringList);
    function Lines: IStringList;
    property Name: String read GetName write SetName;
    property Value: String read GetValue write SetValue; 
    property Params: IMimeParamList read GetParams;
    property Coders: IStringList read GetCoders write SetCoders;
  end;

{ IMimeLabelList }

  IMimeLabelList = interface(IMailInterfaceList)
    ['{CAF5248A-080A-11D4-9150-00C0261013CD}']
    function First: IMimeLabel;
    function Last: IMimeLabel;
    function GetItem(Index: Integer): IMimeLabel;
    procedure SetItem(Index: Integer; const Value: IMimeLabel);
    function GetByName(const Index: String): String;
    procedure SetByName(const Index: String; const Value: String);
    function CreateNew(const Name: String; const Value: String = ''): IMimeLabel;
    function Find(const Name: String): IMimeLabel;
    property Items[Index: Integer]: IMimeLabel read GetItem write SetItem;
    property Names[const Index: String]: String read GetByName write SetByName; default;
  end;

{ IMailPart }

  IMailPart = interface
    ['{CAF52484-080A-11D4-9150-00C0261013CD}']
    function GetBoundary: String;
    procedure SetBoundary(const Value: String);
    function GetLabels: IMimeLabelList;
    function GetBody: IStringList;
    procedure Clear;
    function AttachFile(const FileName: String; const CoderName: String = ''): Boolean; overload;
    function AttachFile(const FileName: String; const Stream: IRandomStream; const CoderName: String = ''): Boolean; overload;
    function Lines: IStringList;
    property Labels: IMimeLabelList read GetLabels;
    property Body: IStringList read GetBody;
    property Boundary: String read GetBoundary write SetBoundary;
  end;

{ IMailPartList }

  IMailPartList = interface(IMailInterfaceList)
    ['{CAF52486-080A-11D4-9150-00C0261013CD}']
    function First: IMailPart;
    function Last: IMailPart;
    function GetItem(Index: Integer): IMailPart;
    procedure SetItem(Index: Integer; const Value: IMailPart);
    function CreateNew: IMailPart;
    property Items[Index: Integer]: IMailPart read GetItem write SetItem; default;
  end;

{ IMailAttachmentList }

  IMailAttachmentList = interface(IMailPartList)
    ['{5E2B8383-1C52-11D4-987F-00104B0FA1EF}']
    function FileNames: IStringList;
    function SaveToStream(const Name: String; const Stream: IRandomStream): Boolean;
  end;

{ IMailMessage }

  IMailMessage = interface
    ['{CAF52483-080A-11D4-9150-00C0261013CD}']
    function GetHeader: IMailPart;
    function GetAttachments: IMailAttachmentList;
    function GetMailParts: IMailPartList;
    function GetNumber: Integer;
    procedure SetNumber(const Value: Integer);
    function GetSize: Integer;
    procedure SetSize(const Value: Integer);
    function GetUidl: String;
    procedure SetUidl(const Value: String);
    procedure Clear;
    property Header: IMailPart read GetHeader;
    property Attachments: IMailAttachmentList read GetAttachments;
    property Parts: IMailPartList read GetMailParts;
    property Number: Integer read GetNumber write SetNumber;
    property Size: Integer read GetSize write SetSize;
    property Uidl: String read GetUidl write SetUidl;
  end;

{ IMailMessageList }

  IMailMessageList = interface(IMailInterfaceList)
    ['{CAF5248B-080A-11D4-9150-00C0261013CD}']
    function First: IMailMessage;
    function Last: IMailMessage;
    function GetItem(Index: Integer): IMailMessage;
    procedure SetItem(Index: Integer; const Value: IMailMessage);
    function CreateNew: IMailMessage;
    function IndexOfUidl(const Value: String): Integer;
    property Items[Index: Integer]: IMailMessage read GetItem write SetItem; default;
  end;

implementation

end.
