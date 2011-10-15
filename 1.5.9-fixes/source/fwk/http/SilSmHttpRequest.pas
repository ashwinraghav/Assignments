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

unit SilSmHttpRequest;

interface

uses
  Sil,
  SilUrl,
  SilTokens,
  SilLexer,
  SilSkHttpObject,
  SilSkHttpMessage,
  SilSiHttpTypes,
  SilSiHttpData,
  SilSiHttpTags;

type
  TSilHttpRequest = class(
    TSilHttpMessage,
    IHttpRequest,
    IHttpRequestParams )
  private
    FMethod: THttpMethod;
    FResource: IHttpRequestUri;
    FVersion: IHttpVersion;
    FTags: IHttpRequestTags;
  private
    procedure DoParseRequestLine(const Lexer: ITokenLexer);
    procedure DoWriteRequestLine(const Writer: IWriter);
  protected
    procedure WriteTo(const Writer: IWriter); override;
  protected // IHttpRequest
    function GetRequestTags: IHttpRequestTags;
    function GetRequest: IHttpRequestParams;
  protected // IHttpRequestParams
    function GetMethod: THttpMethod;
    procedure SetMethod(Value: THttpMethod);
    function GetResource: IHttpRequestUri;
    function GetVersion: IHttpVersion;
  public
    constructor Create; overload;
    constructor Create(const Lexer: ITokenLexer); overload; override;
    destructor Destroy; override;
  end;

  TSilHttpRequestUri = class(
    TSilHttpObject,
    IHttpRequestUri )
  private
    FUrl: IUrl;
    FAll: Boolean;
  private
    procedure DoParse(const Lexer: ITokenLexer);
    procedure DoParseStar(const Lexer: ITokenLexer; const Tok: IToken);
    procedure DoParseUri(const Lexer: ITokenLexer; const Tok: IToken);
  protected
    procedure WriteTo(const Writer: IWriter); override;
  protected // IHttpRequestUri
    function GetUrl: IUrl;
  public
    constructor Create; overload;
    constructor Create(const Lexer: ITokenLexer); overload; override;
    destructor Destroy; override;
  end;

implementation

uses
  SilScHttpTokens,
  SilSfHttpParser,
  SilSmHttpTypes, SilSmHttpRequestTags;

{ TSilHttpRequest }

constructor TSilHttpRequest.Create;
begin
  inherited Create;
  FTags := TSilHttpRequestTags.Create(Self.Tags);
  FVersion := TSilHttpVersion.Create();
  FResource := TSilHttpRequestUri.Create();
end;

constructor TSilHttpRequest.Create(const Lexer: ITokenLexer);
begin
  DoParseRequestLine(Lexer);
  inherited Create(Lexer);
  FTags := TSilHttpRequestTags.Create(Self.Tags);
end;

destructor TSilHttpRequest.Destroy;
begin
  FResource := nil;
  FVersion := nil;
  inherited;
end;

procedure TSilHttpRequest.WriteTo(const Writer: IWriter);
begin
  DoWriteRequestLine(Writer);
  inherited;
end;

procedure TSilHttpRequest.DoParseRequestLine(const Lexer: ITokenLexer);
begin
  FMethod := HttpParseMethod(Lexer);
  HttpCheck(Lexer, TOKEN_LWS);
  FResource := TSilHttpRequestUri.Create(Lexer);
  HttpCheck(Lexer, TOKEN_LWS);
  FVersion := TSilHttpVersion.Create(Lexer);
  HttpCheck(Lexer, TOKEN_CRLF);
end;

procedure TSilHttpRequest.DoWriteRequestLine(const Writer: IWriter);
begin
  Writer.WriteString(HttpMethodName(FMethod));
  Writer.WriteChar(ccSPC);
  HttpWrite(Writer, FResource);
  Writer.WriteChar(ccSPC);
  HttpWrite(Writer, FVersion);
  Writer.WriteString(ccCRLF);
end;

function TSilHttpRequest.GetMethod: THttpMethod;
begin
  Result := FMethod;
end;

procedure TSilHttpRequest.SetMethod(Value: THttpMethod);
begin
  FMethod := Value;
end;

function TSilHttpRequest.GetRequest: IHttpRequestParams;
begin
  Result := Self;
end;

function TSilHttpRequest.GetRequestTags: IHttpRequestTags;
begin
  Result := FTags;
end;

function TSilHttpRequest.GetResource: IHttpRequestUri;
begin
  Result := FResource;
end;

function TSilHttpRequest.GetVersion: IHttpVersion;
begin
  Result := FVersion;
end;

{ TSilHttpRequestUri }

constructor TSilHttpRequestUri.Create;
begin
  inherited Create;
  FUrl := SilUrl.Url.Create();
end;

constructor TSilHttpRequestUri.Create(const Lexer: ITokenLexer);
begin
  inherited Create;
  DoParse(Lexer);
end;

destructor TSilHttpRequestUri.Destroy;
begin
  FUrl := nil;
  inherited;
end;

procedure TSilHttpRequestUri.DoParse(const Lexer: ITokenLexer);
var
  Tok: IToken;
begin
  case HttpGetSymbol(Lexer, Tok) of
    TOKEN_SEP_STAR: DoParseStar(Lexer, Tok);
    else            DoParseUri(Lexer, Tok);
  end;
end;

procedure TSilHttpRequestUri.WriteTo(const Writer: IWriter);
begin
  if FAll then
    Writer.WriteChar('*') else
    Writer.WriteString(FUrl.Text);
end;

procedure TSilHttpRequestUri.DoParseStar(const Lexer: ITokenLexer; const Tok: IToken);
begin
  FAll := True;
end;

procedure TSilHttpRequestUri.DoParseUri(const Lexer: ITokenLexer; const Tok: IToken);
begin
  FUrl := HttpParseUrl(Lexer);
end;

function TSilHttpRequestUri.GetUrl: IUrl;
begin
  Result := FUrl;
end;

end.
