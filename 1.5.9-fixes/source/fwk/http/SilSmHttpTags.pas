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

unit SilSmHttpTags;

{$I Defines.inc}

interface

uses
  Sil,
  SilTokens,
  SilLexer,
  SilSkHttpObject,
  SilSiHttpTypes,
  SilSiHttpTags;

type
  TSilHttpTagList = class(
    TSilHttpObject,
    IHttpTags,
    IHttpTagList )
  private
    FList: IStringList;
  private
    procedure DoParseTags(const Lexer: ITokenLexer);
    procedure DoParseTag(const Lexer: ITokenLexer);
  protected
    procedure WriteTo(const Writer: IWriter); override; 
  protected // IHttpTags
    function GetCount: Integer;
    function Enumerate(var Enum: IEnumerator; out Tag: IHttpTag): Boolean; overload;
    function Enumerate(var Enum: IEnumerator; const IID: TGUID; out Tag): Boolean; overload;
    function Get(const Name: string): IHttpTag;
    function Find(const IID: TGUID; out Tag): Boolean; overload;
    function Find(const Name: string; out Tag: IHttpTag): Boolean; overload;
    function Find(const ID: THttpTag; out Tag: IHttpTag): Boolean; overload;
    function IsPresent(const IID: TGUID): Boolean; overload;
    function IsPresent(const Name: string): Boolean; overload;
    function IsPresent(const ID: THttpTag): Boolean; overload;
  protected // IHttpTagList
    function Add(const Tag: IHttpTag): IHttpTag; overload;
    function Add(const ID: THttpTag; const Value: string = ''): IHttpTag; overload;
    function Add(const Name: string; const Value: string = ''): IHttpTag; overload;
  public
    constructor Create; overload;
    constructor Create(const Lexer: ITokenLexer); overload; override;
    destructor Destroy; override;
  end;

implementation

uses
  SilScHttpTokens,
  SilSfHttpParser,
  SilSmHttpTag;

{ TSilHttpTagList }

constructor TSilHttpTagList.Create;
begin
  inherited Create;
  FList := Sil.List.StringList(False, InterfaceHandler);
end;

constructor TSilHttpTagList.Create(const Lexer: ITokenLexer);
begin
  Create;
  DoParseTags(Lexer);  
end;

destructor TSilHttpTagList.Destroy;
begin
  FList := nil;
  inherited;
end;

function TSilHttpTagList.Enumerate(var Enum: IEnumerator; out Tag: IHttpTag): Boolean;
var
  Name: string;
begin
  Result := FList.Enumerate(Enum, Name);
  if Result then Tag := IUnknown(FList.Ptrs[Enum.Iteration]) as IHttpTag;
end;

function TSilHttpTagList.Enumerate(var Enum: IEnumerator; const IID: TGUID; out Tag): Boolean;
var
  Item: IHttpTag;
begin
  repeat
    Result := Enumerate(Enum, Item);
  until not Result or (Item.QueryInterface(IID, Tag) = 0); 
end;

function TSilHttpTagList.Find(const Name: string; out Tag: IHttpTag): Boolean;
var
  Index: Integer;
begin
  Index := FList.IndexOf(Name);
  Result := FList.ValidIndex(Index);
  if Result then
    Tag := IUnknown(FList.Ptrs[Index]) as IHttpTag;
end;

function TSilHttpTagList.Find(const IID: TGUID; out Tag): Boolean;
var
  Enum: IEnumerator;
begin
  Result := Enumerate(Enum, IID, Tag);
end;

function TSilHttpTagList.Find(const ID: THttpTag; out Tag: IHttpTag): Boolean;
begin
  Result := Find(HttpTagName(ID), Tag);
end;

function TSilHttpTagList.IsPresent(const IID: TGUID): Boolean;
var
  Dummy: IHttpTag;
begin
  Result := Find(IID, Dummy);
end;

function TSilHttpTagList.IsPresent(const Name: string): Boolean;
var
  Dummy: IHttpTag;
begin
  Result := Find(Name, Dummy);
end;

function TSilHttpTagList.IsPresent(const ID: THttpTag): Boolean;
var
  Dummy: IHttpTag;
begin
  Result := Find(ID, Dummy);
end;

function TSilHttpTagList.Get(const Name: string): IHttpTag;
begin
  if not Find(Name, Result) then
    Result := Add(Name);
end;

function TSilHttpTagList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TSilHttpTagList.Add(const Tag: IHttpTag): IHttpTag;
begin
  FList.Add(Tag.Name, @Tag);
  Result := Tag;
end;

function TSilHttpTagList.Add(const Name, Value: string): IHttpTag;
begin
  if not Find(Name, Result) then
    Result := Add(TSilHttpTag.Create(Self, Name, Value));
end;

function TSilHttpTagList.Add(const ID: THttpTag; const Value: string): IHttpTag;
begin
  if not Find(ID, Result) then
    Result := Add(TSilHttpTag.Create(Self, ID, Value));
end;

procedure TSilHttpTagList.DoParseTags(const Lexer: ITokenLexer);
begin
  while Assigned(Lexer.Current) and not Token.InValue(Lexer.Current.Symbol, [TOKEN_CRLF, TOKEN_EOF]) do
    DoParseTag(Lexer);
  HttpCheck(Lexer, TOKEN_CRLF, False);
end;

procedure TSilHttpTagList.DoParseTag(const Lexer: ITokenLexer);
var
  Tag, Value: string;
begin
  Tag := HttpParseToken(Lexer);
  HttpCheck(Lexer, TOKEN_SEP_COLON);
  Value := HttpParseToken(Lexer, [TOKEN_CRLF, TOKEN_EOF]);
  HttpCheck(Lexer, TOKEN_CRLF);
  Add(Tag, Value);
end;

procedure TSilHttpTagList.WriteTo(const Writer: IWriter);
var
  Enum: IEnumerator;
  Item: IHttpTag;
begin
  while Enumerate(Enum, Item) do
  begin
    Writer.WriteString(Item.Name);
    Writer.WriteChar(':');
    HttpWrite(Writer, Item);
    Writer.WriteString(ccCRLF);
  end;
end;

end.
