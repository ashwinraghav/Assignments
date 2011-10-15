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

unit SilSmLayerPackerText;

interface

{$include Defines.inc}

uses
  Sil,
  SilSiLayer,
  SilSmLayerPacker;

type
  TSilLayerPackerText = class (TSilLayerPacker)
  private
    FReadDelimiter: String;
    FWriteDelimiter: String;
  protected
    procedure Write(const Command: ILayerCommand); override;
    function DoGetPacket(const Packet: IPacket): Boolean; override;
    function Duplicate(out Obj: IUnknown; const Context: IUnknown = nil): Boolean; override;
  protected // ILayerControl
    procedure LayerActivate(const Link: ILayerLink = nil; const Context: IUnknown = nil); override;
  end;

implementation

uses
  SilStLayer,
  SilScLayer;

{ TSilLayerPackerText }

procedure TSilLayerPackerText.LayerActivate(const Link: ILayerLink; const Context: IInterface);
begin
  FReadDelimiter := Parameters['ReadDelimiter'];    // ccCRLF
  FWriteDelimiter := Parameters['WriteDelimiter'];  // ccCRLF

  inherited;
end;

function TSilLayerPackerText.Duplicate(out Obj: IUnknown; const Context: IUnknown): Boolean;
begin
  Obj := TSilLayerPackerText.Create(Parameters);
  Result := true;
end;

function TSilLayerPackerText.DoGetPacket(const Packet: IPacket): Boolean;
var
  iPos: Integer;
begin
  iPos := Str.Pos(FReadDelimiter, Buffer);
  Result := iPos > 0;

  if iPos > 0 then
  begin
    Packet.Buffer.Write(Buffer[1], iPos - 1);
    Packet.Buffer.Position := 0;
    DoBufferExtract(iPos + Length(FReadDelimiter) - 1);

    if Debug.Check(dlPacket, CDebugLayer) then Sil.Trace.Log(ClassName + '.DoGetPacket size=%d', [iPos - 1]);
  end;
end;

procedure TSilLayerPackerText.Write(const Command: ILayerCommand);
var
  sBuf: String;
  FormattedPacket: IPacket;
begin
  Command.Packet.Buffer.Position := 0;
  SetLength(sBuf, Command.Packet.Buffer.Size);
  Command.Packet.Buffer.Read(sBuf[1], Length(sBuf));

  FormattedPacket := Sil.Stream.Raw.Packet;
  FormattedPacket.Writer.WriteString(sBuf + FWriteDelimiter);

  inherited Write(Cmd.Create(Command, FormattedPacket));
end;

end.
