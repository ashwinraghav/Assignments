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

unit SilSmTextLineCompleter;

{$I Defines.inc}

interface

uses
  SilSmPacketCompleter;

type

{ TTextLineCompleter }

  TTextLineCompleter = class(TPacketCompleter)
  public
    constructor Create; 
    procedure ExtractHeader; override;
    function CheckData: Boolean; override;
  end;

implementation

uses
  SilBcChr,
  SilBtStr;

{ TTextLineCompleter }

constructor TTextLineCompleter.Create;
begin
  inherited Create;
  FControlSize := 0;
end;

function TTextLineCompleter.CheckData: Boolean;
var
  P: Integer;
begin
  Result := FCurrentSize > 0;

  if Result then
  begin
    P := Str.Pos(ccCRLF, FBuffer);
    Result := P > 0;
    if Result then FPacketSize := P + 1;
  end;
end;

procedure TTextLineCompleter.ExtractHeader;
begin
  FHeaderReceived := true;
end;

end.
