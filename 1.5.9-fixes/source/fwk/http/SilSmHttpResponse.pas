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

unit SilSmHttpResponse;

interface

uses
  Sil,
  SilTokens,
  SilLexer,
  SilSkHttpObject,
  SilSkHttpMessage,
  SilSiHttpTypes,
  SilSiHttpData,
  SilSiHttpTags;

type
  TSilHttpResponse = class(
    TSilHttpMessage,
    IHttpResponse )
  private
    FVersion: IHttpVersion;
    FStatus: IHttpResponseStatus;
    FTags: IHttpResponseTags;
  private
    procedure DoParseStatusLine(const Lexer: ITokenLexer);
    procedure DoParseEntity(const Lexer: ITokenLexer);
  protected
    procedure WriteTo(const Writer: IWriter); override; 
  protected // IHttpResponse
    function GetVersion: IHttpVersion;
    function GetStatus: IHttpResponseStatus;
    function GetResponseTags: IHttpResponseTags;
  public
    constructor Create; overload;
    constructor Create(const Lexer: ITokenLexer); overload; override;
    destructor Destroy; override;
  end;

  TSilHttpResponseStatus = class(
    TSilHttpObject,
    IHttpResponseStatus )
  private
    FValue: THttpStatus;
    FCode: Word;
    FReason: string;
  protected
    procedure WriteTo(const Writer: IWriter); override;
  protected // IHttpResponseStatus
    function GetValue: THttpStatus;
    function GetCode: Word;
    function GetReason: string;
    procedure SetValue(Value: THttpStatus);
    procedure SetCode(Value: Word);
    procedure SetReason(const Value: string);
  public
    constructor Create; overload;
    constructor Create(const Lexer: ITokenLexer); overload; override;
    destructor Destroy; override;
  end;

implementation

uses
  SilScHttpTokens,
  SilSfHttpParser, SilSgHttpStatus,
  SilSmHttpResponseTags, SilSmHttpTypes, SilSmHttpEntity;

{ TSilHttpResponse }

constructor TSilHttpResponse.Create;
begin
  inherited Create;
  FVersion := TSilHttpVersion.Create();
  FTags := TSilHttpResponseTags.Create(Self.Tags);
  FStatus := TSilHttpResponseStatus.Create();
  FEntity := TSilHttpEntity.Create(Self.Tags);
end;

constructor TSilHttpResponse.Create(const Lexer: ITokenLexer);
begin
  DoParseStatusLine(Lexer);
  inherited Create(Lexer);
  FTags := TSilHttpResponseTags.Create(Self.Tags);
  if not HttpIsSymbol(Lexer.Current, [TOKEN_EOF]) then
    DoParseEntity(Lexer);
end;

destructor TSilHttpResponse.Destroy;
begin
  FTags := nil;
  inherited;
end;

function TSilHttpResponse.GetResponseTags: IHttpResponseTags;
begin
  Result := FTags;
end;

function TSilHttpResponse.GetStatus: IHttpResponseStatus;
begin
  Result := FStatus;
end;

function TSilHttpResponse.GetVersion: IHttpVersion;
begin
  Result := FVersion;
end;

procedure TSilHttpResponse.DoParseStatusLine(const Lexer: ITokenLexer);
begin
  FVersion := TSilHttpVersion.Create(Lexer);
  HttpCheck(Lexer, TOKEN_LWS);
  FStatus := TSilHttpResponseStatus.Create(Lexer);
  HttpCheck(Lexer, TOKEN_CRLF);
end;

procedure TSilHttpResponse.DoParseEntity(const Lexer: ITokenLexer);
begin
  FEntity := TSilHttpEntity.Create(Lexer, Self.Tags);
end;

procedure TSilHttpResponse.WriteTo(const Writer: IWriter);
begin
  HttpWrite(Writer, FVersion);
  Writer.WriteChar(ccSPC);
  HttpWrite(Writer, FStatus);
  Writer.WriteString(ccCRLF);
  inherited;
end;

{ TSilHttpResponseStatus }

constructor TSilHttpResponseStatus.Create;
begin
  inherited Create;  
end;

constructor TSilHttpResponseStatus.Create(const Lexer: ITokenLexer);
begin
  inherited Create;
  FCode := HttpParseInteger(Lexer);
  HttpCheck(Lexer, TOKEN_LWS);
  FReason := HttpParseToken(Lexer, [TOKEN_CRLF, TOKEN_EOF]);
end;

destructor TSilHttpResponseStatus.Destroy;
begin
  inherited;
end;

procedure TSilHttpResponseStatus.WriteTo(const Writer: IWriter);
begin
  Writer.WriteString(Int.ToStr(FCode));
  Writer.WriteChar(ccSPC);
  Writer.WriteString(FReason);
end;

function TSilHttpResponseStatus.GetValue: THttpStatus;
begin
  Result := FValue;  
end;

function TSilHttpResponseStatus.GetCode: Word;
begin
  Result := FCode;
end;

function TSilHttpResponseStatus.GetReason: string;
begin
  Result := FReason;
end;

procedure TSilHttpResponseStatus.SetCode(Value: Word);
begin
  FCode := Value;
  SetValue(HttpStatusFromCode(Value));
end;

procedure TSilHttpResponseStatus.SetReason(const Value: string);
begin
  if FValue = hsxExtension then
    FReason := Value else
    raise Sil.Error.Create('no se puede cambiar');
end;

procedure TSilHttpResponseStatus.SetValue(Value: THttpStatus);
begin
  FValue := Value;
  if FValue <> hsxExtension then
  begin
    FCode := GHttpStatusCode[FValue];
    FReason := GHttpStatusReason[FValue];
  end;
end;

end.
