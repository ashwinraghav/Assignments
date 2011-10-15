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

unit SilSmLayerRuntime;

{$include Defines.inc}

interface

uses
  Sil,
  SilSiLayer,
  SilSkSharedObject;

type
  TSilLayerRuntime = class(
    TSilSharedObject,
    ILayerRuntime,
    ILayerRuntimeFactory,
    ILayerRuntimeCommands )
  protected // ILayerRuntime
    function GetFactory: ILayerRuntimeFactory;
    function GetCommand: ILayerRuntimeCommands;
  protected // ILayerRuntimeFactory
    function Chain(const Parameters: IParameters = nil): ILayerChain;
    function List(const Parameters: IParameters = nil): ILayerLinkList;
    function Slot(const Parameters: IParameters = nil): ILayerSlot;
    function Cipher(const Parameters: IParameters = nil): ILayer;
    function Layer(const Parameters: IParameters = nil): ILayer;
    function Dispatcher(const Parameters: IParameters = nil): ILayer;
    function Command(const Caller: ILayerLink; const Packet: IPacket; const Target: Variant; const Context: IUnknown): ILayerCommand; overload;
    function Command(const Caller: ILayerLink; const Packet: IPacket): ILayerCommand; overload;
    function Command(const Caller: ILayerLink; const Link: ILayerLink; const Packet: IPacket; const Target: Variant; const Context: IUnknown): ILayerCommand; overload;
    function Command(const Cmd: ILayerCommand; const Packet: IPacket): ILayerCommand; overload;
    function Command(const Cmd: ILayerCommand; const Packet: IPacket; const Context: IUnknown): ILayerCommand; overload;
    function Command(const Cmd: ILayerCommand; const Packet: IPacket; const Target: Variant): ILayerCommand; overload;
    function Command(const Cmd: ILayerCommand; const Packet: IPacket; const Target: Variant; const Context: IUnknown): ILayerCommand; overload;
  protected // ILayerRuntimeCommands
    procedure Read(const Cmd: ILayerCommand; const Packet: IPacket = nil; const Context: IUnknown = nil); overload;
    procedure Read(const Cmd: ILayerCommand; const Link: ILayerLink; const Context: IUnknown = nil); overload;
    procedure Read(const Operation: ILayerOperations; const Packet: IPacket = nil; const Context: IUnknown = nil); overload;
    procedure Read(const Link: ILayerLink; const Packet: IPacket = nil; const Context: IUnknown = nil); overload;
    procedure Write(const Cmd: ILayerCommand; const Packet: IPacket = nil; const Context: IUnknown = nil); overload;
    procedure Write(const Cmd: ILayerCommand; const Link: ILayerLink; const Context: IUnknown = nil); overload;
    procedure Write(const Operation: ILayerOperations; const Packet: IPacket = nil; const Context: IUnknown = nil); overload;
    procedure Write(const Link: ILayerLink; const Packet: IPacket = nil; const Context: IUnknown = nil); overload;
    procedure Receive(const Cmd: ILayerCommand; const Packet: IPacket = nil; const Context: IUnknown = nil); overload;
    procedure Receive(const Cmd: ILayerCommand; const Link: ILayerLink; const Context: IUnknown = nil); overload;
    procedure Receive(const Operation: ILayerOperations; const Packet: IPacket = nil; const Context: IUnknown = nil); overload;
    procedure Receive(const Link: ILayerLink; const Packet: IPacket = nil; const Context: IUnknown = nil); overload;
    function GetLink(const Layer: IUnknown; out Link: ILayerLink): Boolean;
    function GetLowerSlot(const Layer: IUnknown; out Slot: ILayerSlot): Boolean;
    function FindLower(const Link: ILayerLink; const IID: TGuid; out Obj): Boolean;
    function FindUpper(const Link: ILayerLink; const IID: TGuid; out Obj): Boolean;
    function Activate(const Obj: IInterface): Boolean;
    function ShareSlot(const Child, Sibling: IInterface): Boolean;
    procedure DropChain(const Chain: ILayerChain);
    procedure GetBuffer(const Packet: IPacket; out Buffer: String);
    function GetUpperID(const Link: ILayerLink): Variant;
  end;

implementation

uses
  SilTool,
  SilSmLayerCommand,
  SilSmLayerDispatcher,
  SilSmLayerChain,
  SilSmLayerCipher,
  SilSmLayerLinkList,
  SilSmLayerSlot;

{ TSilLayerRuntime }

function TSilLayerRuntime.GetCommand: ILayerRuntimeCommands;
begin
  Result := Self;
end;

function TSilLayerRuntime.GetFactory: ILayerRuntimeFactory;
begin
  Result := Self;
end;

function TSilLayerRuntime.Layer(const Parameters: IParameters): ILayer;
begin
{ TODO : !!!!!!!!!!!!!! }
end;

function TSilLayerRuntime.List(const Parameters: IParameters): ILayerLinkList;
begin
  Result := TSilLayerLinkList.Create(Parameters);
end;

function TSilLayerRuntime.Slot(const Parameters: IParameters): ILayerSlot;
begin
  Result := TSilLayerSlot.Create(Parameters);
end;

function TSilLayerRuntime.Chain(const Parameters: IParameters): ILayerChain;
begin
  Result := TSilLayerChain.Create(Parameters);
end;

function TSilLayerRuntime.Cipher(const Parameters: IParameters): ILayer;
begin
  Result := TSilLayerCipher.Create(Parameters);
end;

function TSilLayerRuntime.Command(
  const Caller: ILayerLink;
  const Packet: IPacket): ILayerCommand;
begin
  Result := TSilLayerCommand.Create(Caller, Caller, Packet, Vart.Unassigned, nil);
end;

function TSilLayerRuntime.Command(
  const Caller: ILayerLink;
  const Packet: IPacket;
  const Target: Variant;
  const Context: IInterface): ILayerCommand;
begin
  Result := TSilLayerCommand.Create(Caller, Caller, Packet, Target, Context);
end;

function TSilLayerRuntime.Command(
  const Cmd: ILayerCommand;
  const Packet: IPacket): ILayerCommand;
begin
  Result := TSilLayerCommand.Create(Cmd.Caller, Cmd.Link, Packet, Cmd.Target, Cmd.Context);
end;

function TSilLayerRuntime.Command(
  const Cmd: ILayerCommand;
  const Packet: IPacket;
  const Target: Variant): ILayerCommand;
begin
  Result := TSilLayerCommand.Create(Cmd.Caller, Cmd.Link, Packet, Target, Cmd.Context);
end;

function TSilLayerRuntime.Command(
  const Cmd: ILayerCommand;
  const Packet: IPacket;
  const Context: IInterface): ILayerCommand;
begin
  Result := TSilLayerCommand.Create(Cmd.Caller, Cmd.Link, Packet, Cmd.Target, Sil.Ref.IsNull(Context, Cmd.Context));
end;

function TSilLayerRuntime.Command(
  const Cmd: ILayerCommand;
  const Packet: IPacket;
  const Target: Variant;
  const Context: IInterface): ILayerCommand;
begin
  Result := TSilLayerCommand.Create(Cmd.Caller, Cmd.Link, Packet, Target, Ref.IsNull(Context, Cmd.Context));
end;

function TSilLayerRuntime.Command(
  const Caller, Link: ILayerLink;
  const Packet: IPacket;
  const Target: Variant;
  const Context: IInterface): ILayerCommand;
begin
  Result := TSilLayerCommand.Create(Caller, Link, Packet, Target, Context);
end;

function TSilLayerRuntime.Dispatcher(const Parameters: IParameters): ILayer;
begin
  Result := TSilLayerDispatcher.Create(Parameters);
end;

function TSilLayerRuntime.Activate(const Obj: IInterface): Boolean;
var
  Control: ILayerControl;
  Link: ILayerLink;
begin
  if Ref.GetInterface(Obj, ILayerLink, Link) then
  begin
    Result := true;
    Link.Control.Activate;
  end else
  if Ref.GetInterface(Obj, ILayerControl, Control) then
  begin
    Result := true;
    Control.Activate;
  end else
    Result := false;
end;

procedure TSilLayerRuntime.DropChain(const Chain: ILayerChain);
begin
  Chain.Control.Deactivate;
  Chain.Clear(true);
end;

function TSilLayerRuntime.FindLower(
  const Link: ILayerLink;
  const IID: TGuid;
    out Obj): Boolean;
begin
  Result := Ref.GetInterface(Link, IID, Obj);

  if not Result and Assigned(Link.Lower) then
    Result := FindLower(Link.Lower, IID, Obj);
end;

function TSilLayerRuntime.FindUpper(
  const Link: ILayerLink;
  const IID: TGuid;
    out Obj): Boolean;
begin
  Result := Ref.GetInterface(Link, IID, Obj);

  if not Result and Assigned(Link.Upper) then
    Result := FindUpper(Link.Upper, IID, Obj);
end;

procedure TSilLayerRuntime.GetBuffer(
  const Packet: IPacket;
    out Buffer: String);
begin
  SetLength(Buffer, Packet.Buffer.Size);
  Move(Packet.Buffer.Memory^, Buffer[1], Length(Buffer));
end;

function TSilLayerRuntime.GetLink(
  const Layer: IInterface;
    out Link: ILayerLink): Boolean;
var
  Terminal: ILayerTerminal;
begin
  if Ref.GetInterface(Layer, ILayerTerminal, Terminal) then
    Link := Terminal.Link;

  Result := Assigned(Link);
end;

function TSilLayerRuntime.GetLowerSlot(
  const Layer: IInterface;
    out Slot: ILayerSlot): Boolean;
var
  Link: ILayerLink;
begin
  if GetLink(Layer, Link) then
    Result := FindLower(Link, ILayerSlot, Slot) else
    Result := false;
end;

function TSilLayerRuntime.GetUpperID(const Link: ILayerLink): Variant;
begin
  Result := Vart.Unassigned;

  if Assigned(Link) then
    if Assigned(Link.Operation) and Vart.IsOK(Link.Operation.Id) then
      Result := Link.Operation.Id else
    if Assigned(Link.Upper) then
      Result := GetUpperID(Link.Upper);
end;

procedure TSilLayerRuntime.Read(
  const Operation: ILayerOperations;
  const Packet: IPacket;
  const Context: IInterface);
var
  Caller, Link: ILayerLink;
begin
  Operation.Read(Command(Caller, Link, Packet, Vart.Unassigned, Context));
end;

procedure TSilLayerRuntime.Read(
  const Cmd: ILayerCommand;
  const Link: ILayerLink;
  const Context: IInterface);
begin
  Link.Operation.Read(Command(Cmd.Caller, Link, Cmd.Packet, Cmd.Target, Ref.IsNull(Context, Cmd.Context)));
end;

procedure TSilLayerRuntime.Read(
  const Link: ILayerLink;
  const Packet: IPacket;
  const Context: IInterface);
begin
  Link.Operation.Read(Command(Link, Link, Packet, Vart.Unassigned, Context));
end;

procedure TSilLayerRuntime.Read(
  const Cmd: ILayerCommand;
  const Packet: IPacket;
  const Context: IInterface);
var
  Link: ILayerLink;
begin
  Link := Cmd.Link.Lower;

  if Assigned(Link) then
    Link.Operation.Read(Command(Cmd.Caller, Link, IPacket(Ref.IsNull(Packet, Cmd.Packet)), Cmd.Target, Ref.IsNull(Context, Cmd.Context)));
end;

procedure TSilLayerRuntime.Receive(
  const Operation: ILayerOperations;
  const Packet: IPacket;
  const Context: IInterface);
var
  Caller, Link: ILayerLink;
begin
  Operation.Receive(Command(Caller, Link, Packet, Vart.Unassigned, Context));
end;

procedure TSilLayerRuntime.Receive(
  const Link: ILayerLink;
  const Packet: IPacket;
  const Context: IInterface);
begin
  Link.Operation.Receive(Command(Link, Link, Packet, Vart.Unassigned, Context));
end;

procedure TSilLayerRuntime.Receive(
  const Cmd: ILayerCommand;
  const Packet: IPacket;
  const Context: IInterface);
var
  Link: ILayerLink;
begin
  Link := Cmd.Link.Upper;

  if Assigned(Link) then
    Link.Operation.Receive(Command(Cmd.Caller, Link, IPacket(Ref.IsNull(Packet, Cmd.Packet)), Cmd.Target, Ref.IsNull(Context, Cmd.Context)));
end;

procedure TSilLayerRuntime.Receive(
  const Cmd: ILayerCommand;
  const Link: ILayerLink;
  const Context: IInterface);
begin
  Link.Operation.Receive(Command(Cmd.Caller, Link, Cmd.Packet, Cmd.Target, Ref.IsNull(Context, Cmd.Context)));
end;

function TSilLayerRuntime.ShareSlot(const Child, Sibling: IInterface): Boolean;
var
  Slot: ILayerSlot;
begin
  Result := GetLowerSlot(Child, Slot);

  if Result then
  begin
    Slot.Add(Sibling);
    if Slot.Control.IsActive then Activate(Sibling);
  end;
end;

procedure TSilLayerRuntime.Write(
  const Operation: ILayerOperations;
  const Packet: IPacket;
  const Context: IInterface);
var
  Caller, Link: ILayerLink;
begin
  Operation.Write(Command(Caller, Link, Packet, Vart.Unassigned, Context));
end;

procedure TSilLayerRuntime.Write(
  const Link: ILayerLink;
  const Packet: IPacket;
  const Context: IInterface);
begin
  Link.Operation.Write(Command(Link, Link, Packet, Vart.Unassigned, Context));
end;

procedure TSilLayerRuntime.Write(
  const Cmd: ILayerCommand;
  const Packet: IPacket;
  const Context: IInterface);
var
  Link: ILayerLink;
begin
  Link := Cmd.Link.Lower;

  if Assigned(Link) then
    Link.Operation.Write(Command(Cmd.Caller, Link, IPacket(Ref.IsNull(Packet, Cmd.Packet)), Cmd.Target, Ref.IsNull(Context, Cmd.Context)));
end;

procedure TSilLayerRuntime.Write(
  const Cmd: ILayerCommand;
  const Link: ILayerLink;
  const Context: IInterface);
begin
  Link.Operation.Write(Command(Cmd.Caller, Link, Cmd.Packet, Cmd.Target, Ref.IsNull(Context, Cmd.Context)));
end;

initialization
  SilTool.Sv.SharedObject.Register(GuidLayerRuntime, TSilLayerRuntime, SLayerRuntime);

end.
