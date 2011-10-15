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

unit SilLayerSmToken;

interface

{$include Sil.inc}

uses
  Sil,
  SilVector,
  SilLjFiler,
  SilLayerSiGlobal;

type
  TSilLayerToken = class (TSilObject, ILayerToken)
  private
    FParams: IParameterList;
    FBuffer: IMemoryStream;
    FReader: IReader;
    FWriter: IWriter;
    FFiler: FilerFactoryType;
  protected // ILayerToken
    function GetParams: IParameterList;
    function GetBuffer: IMemoryStream;
    procedure Clear;
    function Reader: IReader;
    function Writer: IWriter;
    procedure Configure(Filer: FilerFactoryType);
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TSilLayerToken }

constructor TSilLayerToken.Create;
begin
  inherited Create;

  FParams := SilVector.ParameterList;
  FBuffer := Sil.Stream.Memory;
end;

destructor TSilLayerToken.Destroy;
begin
  FParams := nil;
  FReader := nil;
  FWriter := nil;
  FBuffer := nil;

  inherited;
end;

function TSilLayerToken.GetBuffer: IMemoryStream;
begin
  Result := FBuffer;
end;

function TSilLayerToken.GetParams: IParameterList;
begin
  Result := FParams;
end;

procedure TSilLayerToken.Clear;
begin
  FParams.Clear;
  FBuffer.Size := 0;
end;

function TSilLayerToken.Reader: IReader;
begin
  Result := freader;
end;

function TSilLayerToken.Writer: IWriter;
begin
  Result := fwriter;
end;

procedure TSilLayerToken.Configure(Filer: FilerFactoryType);
begin
  if ffiler <> filer then
  begin
    ffiler := filer;
    FWriter := Filer.Writer(FBuffer);
    FReader := Filer.Reader(FBuffer);
  end;
end;

end.
