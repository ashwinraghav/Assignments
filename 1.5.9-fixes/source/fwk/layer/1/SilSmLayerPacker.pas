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

unit SilSmLayerPacker;

interface

{$include Defines.inc}

uses
  Sil,
  SilSiLayer,
  SilSmLayer;

type
  TSilLayerPacker = class (TSilLayer)
  private
    FBuffer: String;
  protected
    property Buffer: String read FBuffer;
  protected
    procedure DoAddBuffer(Buf: PChar; Size: LongWord);
    procedure DoResetBuffer;
    procedure DoBufferExtract(Size: LongWord);
    procedure DoReceive(const Command: ILayerCommand); virtual;
    procedure DoPutPacket(const Packet: IPacket); virtual;
    function DoGetPacket(const Packet: IPacket): Boolean; virtual; abstract;
  protected // ILayerOperation
    procedure Receive(const Command: ILayerCommand); override;
    procedure Read(const Command: ILayerCommand); override;
  protected // ILayerControl
    procedure LayerDeactivate(const Link: ILayerLink; const Context: IUnknown; IsBroken: Boolean); override;
  public
    destructor Destroy; override;
  end;

implementation

uses
  SilLayer,
  SilSmLayerCommand;

{ TSilLayerPacker }

destructor TSilLayerPacker.Destroy;
begin
  inherited;
end;

procedure TSilLayerPacker.DoAddBuffer(Buf: PChar; Size: LongWord);
var
  lwBuffer: LongWord;
begin
  if Size > 0 then
  begin
    lwBuffer := Length(FBuffer);
    SetLength(FBuffer, lwBuffer + Size);
    Move(Buf^, FBuffer[lwBuffer + 1], Size);
  end;
end;

procedure TSilLayerPacker.DoPutPacket(const Packet: IPacket);
var
  sBuf: String;
  lwSize: LongWord;
begin
  Packet.Buffer.Position := 0;
  SetLength(sBuf, Packet.Buffer.Size);
  lwSize := Packet.Buffer.Read(sBuf[1], Length(sBuf));
  Packet.Buffer.Position := 0;
  DoAddBuffer(PChar(sBuf), lwSize);
end;

procedure TSilLayerPacker.DoResetBuffer;
begin
  FBuffer := '';
end;

procedure TSilLayerPacker.DoBufferExtract(Size: LongWord);
begin
  FBuffer := Str.Copy(FBuffer, Size + 1);
end;

procedure TSilLayerPacker.LayerDeactivate(const Link: ILayerLink; const Context: IUnknown; IsBroken: Boolean);
begin
  DoResetBuffer;                       
end;

procedure TSilLayerPacker.Read(const Command: ILayerCommand);
var
  RawCommand: ILayerCommand;
  Context: IUnknown;
begin
  if Debug.Check(dlInOut, CDebugLayer) then Sil.Trace.Log(ClassName + '.Read size=%d', [Command.Packet.Buffer.Size]);

  if not DoGetPacket(Command.Packet) and Assigned(Command.Link.Lower) then
    while Command.Link.Lower.Control.IsActive do
    begin
      RawCommand := Cmd.Create(Command, Sil.Stream.Raw.Packet, Command.Target, Context);

      Cmd.Read(RawCommand);

      if Debug.Check(dlInOut, CDebugLayer) then Sil.Trace.Log(ClassName + '.Read: packer=[%s]', [Sil.Mem.Dump(Command.Packet.Buffer.Memory, Command.Packet.Buffer.Size, 1, '')]);

      if Assigned(RawCommand.Packet) then
      begin
        RawCommand.Packet.Buffer.Position := 0;

        if RawCommand.Packet.Buffer.Size > 0 then
        begin
          DoPutPacket(RawCommand.Packet);
          if DoGetPacket(Command.Packet) then Break;
        end;
      end else
        Break;
    end;
end;

procedure TSilLayerPacker.Receive(const Command: ILayerCommand);
var
  BufferedPacket: IPacket;
begin
  if Debug.Check(dlInOut, CDebugLayer) then Sil.Trace.Log(ClassName + '.Receive: packer=[%s]', [Sil.Mem.Dump(Command.Packet.Buffer.Memory, Command.Packet.Buffer.Size, 1, '')]);

  DoPutPacket(Command.Packet);

  if Assigned(Command.Link.Upper) then
    while true do
    begin
      BufferedPacket := Sil.Stream.Raw.Packet;

      if DoGetPacket(BufferedPacket) then
        DoReceive(Cmd.Create(Command, BufferedPacket)) else
        Break;
    end;
end;

procedure TSilLayerPacker.DoReceive(const Command: ILayerCommand);
begin
  Cmd.Receive(Command);
end;

end.
