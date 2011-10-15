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

unit SilSmAsciiProtocol;
 
interface

uses
  SilLkInterfaced,
  SilLiConnection,
  SilLiEventList,
  SilSiAsciiProtocol;

type
  TAsciiProtocol = class (
    // extends
    TSilInterfacedObject,
    // implements
    IAsciiProtocol)
  protected
    FBuffer: String;
  private
    function FireWriteLine(const Command: String): Boolean;
    function FireReadLine(out Response: String): Boolean;
  protected // IAsciiProtocol
    function WriteLn(const Command: String): Boolean; virtual;
    function ReadLn(out Response: String): Boolean; virtual;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  SilBcChr,
  SilBtStr,
  SilLiEnumerator;

{ TAsciiProtocol }

constructor TAsciiProtocol.Create;
begin
  inherited Create;
end;

destructor TAsciiProtocol.Destroy;
begin
  inherited;
end;

function TAsciiProtocol.ReadLn(out Response: String): Boolean;
begin
  Result := FireReadLine(Response);
end;

function TAsciiProtocol.WriteLn(const Command: String): Boolean;
begin
  Result := FireWriteLine(Command);
end;

function TAsciiProtocol.FireReadLine(out Response: String): Boolean;
var
  e: IEnumerator;
  Sink: IAsciiProtocolEvents;
  Event: RReadLineEvent;
  iPos: Integer;

  function DoGetLine: Boolean;
  begin
    iPos := Str.Pos(ccCRLF, FBuffer);
    Result := iPos > 0;

    if Result then
    begin
      Response := Str.Copy(FBuffer, 1, iPos - 1);
      FBuffer := Str.Copy(FBuffer, iPos + 2);
      Event.Result := true;
    end;
  end;

begin
  if HasConnections then 
  begin
    Event.Sender := Self;
    Result := DoGetLine;

    if not Result then
    begin
      repeat
        Event.Result := false;
        Event.Text := '';

        while Events.Enumerate(e, Sink, IAsciiProtocolEvents) do
          Sink.OnReadLine(Event);

        FBuffer := FBuffer + Event.Text;
      until DoGetLine or not Event.Result;

      Result := Event.Result;
    end;
  end else
    Result := false;
end;

function TAsciiProtocol.FireWriteLine(const Command: String): Boolean;
var
  e: IEnumerator;
  Sink: IAsciiProtocolEvents;
  Event: RWriteLineEvent;
begin
  if HasConnections then 
  begin
    Event.Sender := Self;
    Event.Text := Command + ccCRLF;
    Event.Result := false;

    while Events.Enumerate(e, Sink, IAsciiProtocolEvents) do
      Sink.OnWriteLine(Event);

    Result := Event.Result;
  end else
    Result := false;
end;

end.
