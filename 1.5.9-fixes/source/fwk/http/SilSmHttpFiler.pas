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

unit SilSmHttpFiler;

{$I Defines.inc}

interface

uses
  Sil,
  SilTokens,
  SilLexer,
  SilSiHttpTypes,
  SilSiHttpData;

type
  TSilHttpFiler = class(
    TSilObject,
    IHttpFiler )
  private
    FTokens: ITokenTable;
    FDictionary: ILexDictionary;
  protected // IHttpFiler
    function Read(const Stream: IStream; out Request: IHttpRequest): Boolean; overload;
    function Read(const Stream: IStream; out Response: IHttpResponse): Boolean; overload; 
    procedure Write(const Stream: IStream; const Instance: IHttpObject); overload;
    procedure Write(const Stream: IStream; const Request: IHttpRequest); overload;
    procedure Write(const Stream: IStream; const Response: IHttpResponse); overload; 
    function ToStr(const Request: IHttpObject): string; overload;
    function ToStr(const Request: IHttpRequest): string; overload;
    function ToStr(const Response: IHttpResponse): string; overload;
  public
    constructor Create;
    destructor Destroy; override; 
  end;

implementation

uses
  SilSfHttpTokens, SilSfHttpParser,
  SilSmHttpRequest, SilSmHttpResponse;

{ TSilHttpFiler }

constructor TSilHttpFiler.Create;
begin
  inherited Create;
  FTokens := SilLexer.Tool.TokenTable();
  FDictionary := SilSfHttpTokens.DefineTokens(SilLexer.Dictionary.Create());
end;

destructor TSilHttpFiler.Destroy;
begin
  FDictionary := nil;
  FTokens := nil;
  inherited;
end;

function TSilHttpFiler.Read(const Stream: IStream; out Request: IHttpRequest): Boolean;
var
  Lexer: ITokenLexer;
begin
  Lexer := SilLexer.Tool.TokenLexer(Stream, FTokens, FDictionary); 
  Request := TSilHttpRequest.Create(Lexer);
  Result := True;
end;

function TSilHttpFiler.Read(const Stream: IStream; out Response: IHttpResponse): Boolean;
var
  Lexer: ITokenLexer;
begin
  Lexer := SilLexer.Tool.TokenLexer(Stream, FTokens, FDictionary); 
  Response := TSilHttpResponse.Create(Lexer);
  Result := True;
end;

procedure TSilHttpFiler.Write(const Stream: IStream; const Instance: IHttpObject);
var
  Buffer: IMemoryStream;
  Writer: IWriter;
begin
  Buffer := Sil.MemoryStream.Create;
  Writer := Sil.Stream.Raw.Writer(Buffer);
  HttpWrite(Writer, Instance);
  Stream.Write(Buffer.Memory^, Buffer.Size);
end;

procedure TSilHttpFiler.Write(const Stream: IStream; const Request: IHttpRequest);
begin
  Write(Stream, IHttpObject(Request));
end;

procedure TSilHttpFiler.Write(const Stream: IStream; const Response: IHttpResponse);
begin
  Write(Stream, IHttpObject(Response));
end;

function TSilHttpFiler.ToStr(const Request: IHttpObject): string;
var
  Stream: IMemoryStream;
begin
  Stream := Sil.MemoryStream.Create();
  Write(Stream, Request);
  Result := Stream.Memory;
end;

function TSilHttpFiler.ToStr(const Request: IHttpRequest): string;
begin
  Result := ToStr(IHttpObject(Request));
end;

function TSilHttpFiler.ToStr(const Response: IHttpResponse): string;
begin
  Result := ToStr(IHttpObject(Response));
end;

end.
