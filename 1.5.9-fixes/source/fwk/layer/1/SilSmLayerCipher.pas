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

unit SilSmLayerCipher;

interface

{$include Defines.inc}

uses
  Sil,
  SilCoder,
  SilSiLayer,
  SilSmLayer;

type
  TSilLayerCipher = class (TSilLayer)
  private
    FCipher: ICipher;
  private
    function DoProcess(const Command: ILayerCommand; Encode: Boolean): IPacket;
  protected // ILayerOperation
    procedure Receive(const Command: ILayerCommand); override;
    procedure Read(const Command: ILayerCommand); override;
    procedure Write(const Command: ILayerCommand); override;
  protected // ILayerControl
    procedure LayerActivate(const Link: ILayerLink = nil; const Context: IUnknown = nil); override;
  public
    constructor Create(const Parameters: IParameters; const Controller: IUnknown = nil); override; 
    destructor Destroy; override;
  end;

implementation

uses
  SilStLayer;

{ TSilLayerCipher }

constructor TSilLayerCipher.Create(const Parameters: IParameters; const Controller: IUnknown);
var
  Hash: IHash;
begin
  inherited;

  Vart.ToInterface(Self.Parameters['Cipher'], ICipher, FCipher);
  Vart.ToInterface(Self.Parameters['Hash'], IHash, Hash);
  FCipher.Initialize(Self.Parameters['Key'], Hash);
end;

destructor TSilLayerCipher.Destroy;
begin
  FCipher := nil;
  inherited;
end;

procedure TSilLayerCipher.LayerActivate(const Link: ILayerLink; const Context: IInterface);
begin
  Id := Tk.GetUpperID(Link.Upper);
end;

function TSilLayerCipher.DoProcess(const Command: ILayerCommand; Encode: Boolean): IPacket;
var
  Source, Dest: String;
begin
  Tk.GetBuffer(Command.Packet, Source);

  if Encode then
    FCipher.Encode(Source[1], Dest, Length(Source)) else
    FCipher.Decode(Source[1], Dest, Length(Source));

  Result := Sil.Stream.Raw.Packet;
  Result.Buffer.Write(Dest[1], Length(Dest));
end;

procedure TSilLayerCipher.Read(const Command: ILayerCommand);
begin
  Cmd.Write(Command, DoProcess(Command, false));
end;

procedure TSilLayerCipher.Receive(const Command: ILayerCommand);
begin
  Cmd.Receive(Command, DoProcess(Command, false));
end;

procedure TSilLayerCipher.Write(const Command: ILayerCommand);
begin
  Cmd.Write(Command, DoProcess(Command, true));
end;

end.
