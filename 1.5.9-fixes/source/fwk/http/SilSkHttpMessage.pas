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

unit SilSkHttpMessage;

interface

uses
  Sil,
  SilTokens,
  SilLexer,
  SilSkHttpObject,
  SilSiHttpTypes,
  SilSiHttpData,
  SilSiHttpTags;

type
  TSilHttpMessage = class(
    TSilHttpObject,
    IHttpMessage )
  private
    FTags: IHttpTagList;
    FGeneral: IHttpGeneralTags;
  protected
    FEntity: IHttpEntity;
  protected // IHttpMessage
    function GetHeader: IHttpGeneralTags;
    function GetEntity: IHttpEntity;
  protected
    procedure WriteTo(const Writer: IWriter); override; 
  public
    constructor Create; overload;
    constructor Create(const Tags: IHttpTagList); overload;
    constructor Create(const Lexer: ITokenLexer); overload; override; 
    destructor Destroy; override;
  public
    property Tags: IHttpTagList read FTags;
  end;

implementation

uses
  SilScHttpTokens, SilSfHttpParser,
  SilSmHttpTags, SilSmHttpEntity, SilSmHttpGeneralTags;

{ TSilHttpMessage }

constructor TSilHttpMessage.Create(const Tags: IHttpTagList);
begin
  inherited Create;
  FTags := Tags;
  FGeneral := TSilHttpGeneralTags.Create(FTags);
end;

constructor TSilHttpMessage.Create;
var
  List: IHttpTagList;
begin
  List := TSilHttpTagList.Create();
  Create(List);
  FEntity := TSilHttpEntity.Create(List);
end;

constructor TSilHttpMessage.Create(const Lexer: ITokenLexer);
var
  List: IHttpTagList;
begin
  List := TSilHttpTagList.Create(Lexer);
  Create(List);
end;

destructor TSilHttpMessage.Destroy;
begin
  FGeneral := nil;
  FTags := nil;
  inherited;
end;

function TSilHttpMessage.GetEntity: IHttpEntity;
begin
  Result := FEntity;
end;

function TSilHttpMessage.GetHeader: IHttpGeneralTags;
begin
  Result := FGeneral;
end;

procedure TSilHttpMessage.WriteTo(const Writer: IWriter);
begin
  HttpWrite(Writer, FTags);
  Writer.WriteString(ccCRLF);
  HttpWrite(Writer, FEntity);
end;

end.