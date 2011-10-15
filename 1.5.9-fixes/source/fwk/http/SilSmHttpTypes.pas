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

unit SilSmHttpTypes;

{$I Defines.inc}

interface

uses
  Sil,
  SilLexer,
  SilSiHttpTypes,
  SilSkHttpObject;

type
  TSilHttpVersion = class(
    TSilHttpObject,
    IHttpVersion )
  private
    FMajor: Integer;
    FMinor: Integer;
  protected
    procedure WriteTo(const Writer: IWriter); override; 
  protected // IHttpVersion
    function GetMajor: Integer;
    procedure SetMajor(Value: Integer);
    function GetMinor: Integer;
    procedure SetMinor(Value: Integer);
  public
    constructor Create; overload;
    constructor Create(const Lexer: ITokenLexer); overload; override;
  end;

implementation

uses
  SilScHttpTokens, SilSfHttpParser;

const
  CHttp: PChar = 'HTTP';

{ TSilHttpVersion }

constructor TSilHttpVersion.Create;
begin
  inherited Create;
  FMajor := 1;
  FMinor := 1;
end;

constructor TSilHttpVersion.Create(const Lexer: ITokenLexer);
begin
  inherited Create;
  HttpCheck(Lexer, TOKEN_TOKEN, CHttp);
  HttpCheck(Lexer, TOKEN_SEP_SLASH);
  FMajor := HttpParseInteger(Lexer);
  HttpCheck(Lexer, TOKEN_SEP_PERIOD);
  FMinor := HttpParseInteger(Lexer);
end;

procedure TSilHttpVersion.WriteTo(const Writer: IWriter);
begin
  Writer.WriteString(CHttp);
  Writer.WriteChar('/');
  Writer.WriteString(Int.ToStr(FMajor));
  Writer.WriteChar('.');
  Writer.WriteString(Int.ToStr(FMinor));
end;

function TSilHttpVersion.GetMajor: Integer;
begin
  Result := FMajor;
end;

function TSilHttpVersion.GetMinor: Integer;
begin
  Result := FMinor;
end;

procedure TSilHttpVersion.SetMajor(Value: Integer);
begin
  FMajor := Value;
end;

procedure TSilHttpVersion.SetMinor(Value: Integer);
begin
  FMinor := Value;
end;

end.
