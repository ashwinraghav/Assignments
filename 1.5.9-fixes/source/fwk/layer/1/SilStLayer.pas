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

unit SilStLayer;

interface

{$include Defines.inc}

uses
  Sil,
  SilXml,
  
  SilSiSmtp,
  SilSiLayer,
  SilSeLayerPacket,
  SilSeLayerProtocol,
  SilSiLayerProtocolBlind,
  SilSiLayerProtocolEblis,
  SilSiLayerProtocolFile,
  SilSiLayerConnection;

type
  Cmd = class (Tool)
    class function Create(const Caller: ILayerLink; const Packet: IPacket; const Target: Variant; const Context: IUnknown): ILayerCommand; overload;
    class function Create(const Caller: ILayerLink; const Packet: IPacket): ILayerCommand; overload;
    class function Create(const Caller: ILayerLink; const Link: ILayerLink; const Packet: IPacket; const Target: Variant; const Context: IUnknown): ILayerCommand; overload;
    class function Create(const Command: ILayerCommand; const Packet: IPacket): ILayerCommand; overload;
    class function Create(const Command: ILayerCommand; const Packet: IPacket; const Context: IUnknown): ILayerCommand; overload;
    class function Create(const Command: ILayerCommand; const Packet: IPacket; const Target: Variant): ILayerCommand; overload;
    class function Create(const Command: ILayerCommand; const Packet: IPacket; const Target: Variant; const Context: IUnknown): ILayerCommand; overload;
    class procedure Read(const Command: ILayerCommand; const Packet: IPacket = nil; const Context: IUnknown = nil); overload;
    class procedure Read(const Command: ILayerCommand; const Link: ILayerLink; const Context: IUnknown = nil); overload;
    class procedure Read(const Operation: ILayerOperation; const Packet: IPacket = nil; const Context: IUnknown = nil; const Params: IParameterList = nil); overload;
    class procedure Read(const Link: ILayerLink; const Packet: IPacket = nil; const Context: IUnknown = nil; const Params: IParameterList = nil); overload;
    class procedure Write(const Command: ILayerCommand; const Packet: IPacket = nil; const Context: IUnknown = nil); overload;
    class procedure Write(const Command: ILayerCommand; const Link: ILayerLink; const Context: IUnknown = nil); overload;
    class procedure Write(const Operation: ILayerOperation; const Packet: IPacket = nil; const Context: IUnknown = nil; const Params: IParameterList = nil); overload;
    class procedure Write(const Link: ILayerLink; const Packet: IPacket = nil; const Context: IUnknown = nil; const Params: IParameterList = nil); overload;
    class procedure Receive(const Command: ILayerCommand; const Packet: IPacket = nil; const Context: IUnknown = nil); overload;
    class procedure Receive(const Command: ILayerCommand; const Link: ILayerLink; const Context: IUnknown = nil); overload;
    class procedure Receive(const Operation: ILayerOperation; const Packet: IPacket = nil; const Context: IUnknown = nil; const Params: IParameterList = nil); overload;
    class procedure Receive(const Link: ILayerLink; const Packet: IPacket = nil; const Context: IUnknown = nil; const Params: IParameterList = nil); overload;
  end;

  Tk = class (Tool)
    class function GetLink(const Layer: IUnknown; out Link: ILayerLink): Boolean;
    class function GetLowerSlot(const Layer: IUnknown; out Slot: ILayerSlot): Boolean;
    class function FindLower(const Link: ILayerLink; const IID: TGuid; out Obj): Boolean;
    class function FindUpper(const Link: ILayerLink; const IID: TGuid; out Obj): Boolean;
    class function Activate(const Obj: IInterface): Boolean;
    class function ShareSlot(const Child, Sibling: IInterface): Boolean;
    class function Bindings: ILayerBindings;
    class procedure DropChain(const Chain: ILayerChain);
    class function ReadXmlChain(const Tag: IXmlTag; const Bindings: ILayerBindings = nil): ILayerChain;
    class procedure GetBuffer(const Packet: IPacket; out Buffer: String);
    class function GetUpperID(const Link: ILayerLink): Variant;
  end;

  Layer = class (Tool)
    class function Chain(const Parameters: IParameters = nil): ILayerChain; overload;
    class function Chain(out Obj: ILayerChain; const Parameters: IParameters = nil): ILayerChain; overload;
    class function List(const Parameters: IParameters = nil): ILayerLinkList;
    class function Slot(const Parameters: IParameters = nil): ILayerSlot; overload; 
    class function Slot(out Obj: ILayerSlot; const Parameters: IParameters = nil): ILayerSlot; overload; 
    class function Cipher(const Parameters: IParameters = nil): ILayer;
    class function Dispatcher(const Parameters: IParameters = nil): ILayer;
  end;

  Packer = class (Tool)
    class function Text(const Parameters: IParameters = nil): ILayer;
    class function Imate(const Parameters: IParameters = nil): ILayer;
    //class function Probase(const Parameters: IParameters = nil): ILayer;
  end;

  Device = class (Tool)
    class function SocketClient(const Parameters: IParameters): ILayer; overload;
    class function SocketServer(const Parameters: IParameters): ILayer; overload;
    class function SocketDatagram(const Parameters: IParameters): ILayer;
    class function PipeClient(const Parameters: IParameters): ILayer;
    class function PipeServer(const Parameters: IParameters): ILayer;
    class function SerialPort(const Parameters: IParameters): ILayer; overload;
    class function FileStream(const Parameters: IParameters): ILayer; overload;
  end;

  Protocol = class (Tool)
    class function Imate(const Parameters: IParameters = nil): ILayerProtocol; overload;
    class function Imate(Id: LongWord; const Parameters: IParameters = nil): ILayerProtocol; overload;
    class function SmtpClient(const Parameters: IParameters = nil): ISmtpClient;
    class function Blind(const Parameters: IParameters = nil): IBlindProtocol; overload;
    class function Blind(out Obj: IBlindProtocol; const Parameters: IParameters = nil): IBlindProtocol; overload;
    class function Eblis(const Parameters: IParameters = nil): IEblisProtocol; overload;
    class function Eblis(out Obj: IEblisProtocol; const Parameters: IParameters = nil): IEblisProtocol; overload;
    class function FileClient(const Parameters: IParameters = nil): IClientSideFileProtocol;
    class function FileServer(const Parameters: IParameters = nil): IServerSideFileProtocol;
  end;

  Connection = class (Tool)
    class function Server(const Parameters: IParameters = nil): ILayerConnectionServer;
    class function Client(const Parameters: IParameters = nil): ILayerConnection;
    class function Peer(const List: ILayerConnectionList; const Chain: ILayerChain): ILayerConnection;
  end;

implementation

uses
  SilLtReference,
  SilBtVart,

  SilSfLayerXml,
  SilSmLayer,
  SilSmLayerChain,
  SilSmLayerLink,
  SilSmLayerLinkList,
  SilSmLayerDispatcher,
  SilSmLayerCommand,
  SilSmLayerSlot,
  SilSmLayerCipher,
  SilSmLayerPackerText,
  SilSmLayerPackerImate,
  //SilSmLayerPackerProbase,
  SilSmLayerProtocolBlind,
  SilSmLayerProtocolEblis,
  SilSmLayerProtocolImate,
  SilSmLayerProtocolFile,
  SilSmLayerProtocolSmtpClient,
  SilSmLayerDeviceSocketServer,
  SilSmLayerDeviceSocketClient,
  SilSmLayerDeviceSerialPort,
  SilSmLayerDeviceFile,
  SilSmLayerDeviceNamedPipeClient,
  SilSmLayerDeviceNamedPipeServer,
  SilSmLayerConnection,
  SilSmLayerBindings,
  SilSmLayerDeviceSocketDatagram;

{ Cmd }

class function Cmd.Create(const Caller: ILayerLink; const Packet: IPacket): ILayerCommand;
begin
  Result := TSilLayerCommand.Create(Caller, Caller, Packet, Vart.Unassigned, nil);
end;

class function Cmd.Create(const Caller: ILayerLink; const Packet: IPacket; const Target: Variant; const Context: IInterface): ILayerCommand;
begin
  Result := TSilLayerCommand.Create(Caller, Caller, Packet, Target, Context);
end;

class function Cmd.Create(const Command: ILayerCommand; const Packet: IPacket): ILayerCommand;
begin
  Result := TSilLayerCommand.Create(Command.Caller, Command.Link, Packet, Command.Target, Command.Context);
end;

class function Cmd.Create(const Command: ILayerCommand; const Packet: IPacket; const Context: IUnknown): ILayerCommand;
begin
  Result := TSilLayerCommand.Create(Command.Caller, Command.Link, Packet, Command.Target, Ref.IsNull(Context, Command.Context));
end;

class function Cmd.Create(const Command: ILayerCommand; const Packet: IPacket; const Target: Variant): ILayerCommand;
begin
  Result := TSilLayerCommand.Create(Command.Caller, Command.Link, Packet, Target, Command.Context);
end;

class function Cmd.Create(const Command: ILayerCommand; const Packet: IPacket; const Target: Variant; const Context: IUnknown): ILayerCommand;
begin
  Result := TSilLayerCommand.Create(Command.Caller, Command.Link, Packet, Target, Ref.IsNull(Context, Command.Context));
end;

class function Cmd.Create(const Caller: ILayerLink; const Link: ILayerLink; const Packet: IPacket; const Target: Variant; const Context: IUnknown): ILayerCommand;
begin
  Result := TSilLayerCommand.Create(Caller, Link, Packet, Target, Context);
end;

class procedure Cmd.Read(const Command: ILayerCommand; const Packet: IPacket; const Context: IInterface);
var
  Link: ILayerLink;
begin
  Link := Command.Link.Lower;

  if Assigned(Link) then
    Link.Operation.Read(Cmd.Create(Command.Caller, Link, IPacket(Ref.IsNull(Packet, Command.Packet)), Command.Target, Ref.IsNull(Context, Command.Context)));
end;

class procedure Cmd.Receive(const Command: ILayerCommand; const Packet: IPacket; const Context: IInterface);
var
  Link: ILayerLink;
begin
  Link := Command.Link.Upper;

  if Assigned(Link) then
    Link.Operation.Receive(Cmd.Create(Command.Caller, Link, IPacket(Ref.IsNull(Packet, Command.Packet)), Command.Target, Ref.IsNull(Context, Command.Context)));
end;

class procedure Cmd.Write(const Command: ILayerCommand; const Packet: IPacket; const Context: IInterface);
var
  Link: ILayerLink;
begin
  Link := Command.Link.Lower;

  if Assigned(Link) then
    Link.Operation.Write(Cmd.Create(Command.Caller, Link, IPacket(Ref.IsNull(Packet, Command.Packet)), Command.Target, Ref.IsNull(Context, Command.Context)));
end;

class procedure Cmd.Write(const Command: ILayerCommand; const Link: ILayerLink; const Context: IInterface);
begin
  Link.Operation.Write(Cmd.Create(Command.Caller, Link, Command.Packet, Command.Target, Ref.IsNull(Context, Command.Context)));
end;

class procedure Cmd.Read(const Command: ILayerCommand; const Link: ILayerLink; const Context: IInterface);
begin
  Link.Operation.Read(Cmd.Create(Command.Caller, Link, Command.Packet, Command.Target, Ref.IsNull(Context, Command.Context)));
end;

class procedure Cmd.Receive(const Command: ILayerCommand; const Link: ILayerLink; const Context: IInterface);
begin
  Link.Operation.Receive(Cmd.Create(Command.Caller, Link, Command.Packet, Command.Target, Ref.IsNull(Context, Command.Context)));
end;

class procedure Cmd.Read(const Operation: ILayerOperation; const Packet: IPacket; const Context: IUnknown; const Params: IParameterList);
var
  Com: ILayerCommand;
  Caller, Link: ILayerLink;
begin
  Com := Cmd.Create(Caller, Link, Packet, Vart.Unassigned, Context);
  if Assigned(Params) then Com.Params.Merge(Params);
  Operation.Read(Com);
end;

class procedure Cmd.Write(const Operation: ILayerOperation; const Packet: IPacket; const Context: IUnknown; const Params: IParameterList);
var
  Com: ILayerCommand;
  Caller, Link: ILayerLink;
begin
  Com := Cmd.Create(Caller, Link, Packet, Vart.Unassigned, Context);
  if Assigned(Params) then Com.Params.Merge(Params);
  Operation.Write(Com);
end;

class procedure Cmd.Receive(const Operation: ILayerOperation; const Packet: IPacket; const Context: IUnknown; const Params: IParameterList);
var
  Com: ILayerCommand;
  Caller, Link: ILayerLink;
begin
  Com := Cmd.Create(Caller, Link, Packet, Vart.Unassigned, Context);
  if Assigned(Params) then Com.Params.Merge(Params);
  Operation.Receive(Com);
end;

class procedure Cmd.Read(const Link: ILayerLink; const Packet: IPacket; const Context: IUnknown; const Params: IParameterList);
var
  Com: ILayerCommand;
begin
  Com := Cmd.Create(Link, Link, Packet, Vart.Unassigned, Context);
  if Assigned(Params) then Com.Params.Merge(Params);
  Link.Operation.Read(Com);
end;

class procedure Cmd.Write(const Link: ILayerLink; const Packet: IPacket; const Context: IUnknown; const Params: IParameterList);
var
  Com: ILayerCommand;
begin
  Com := Cmd.Create(Link, Link, Packet, Vart.Unassigned, Context);
  if Assigned(Params) then Com.Params.Merge(Params);
  Link.Operation.Write(Com);
end;

class procedure Cmd.Receive(const Link: ILayerLink; const Packet: IPacket; const Context: IUnknown; const Params: IParameterList);
var
  Com: ILayerCommand;
begin
  Com := Cmd.Create(Link, Link, Packet, Vart.Unassigned, Context);
  if Assigned(Params) then Com.Params.Merge(Params);
  Link.Operation.Receive(Com);
end;

{ List }

class function Layer.Chain(const Parameters: IParameters): ILayerChain;
begin
  Result := TSilLayerChain.Create(Parameters);
end;

class function Layer.Chain(out Obj: ILayerChain; const Parameters: IParameters): ILayerChain;
begin
  Obj := Chain(Parameters);
  Result := Obj;
end;

class function Layer.Cipher(const Parameters: IParameters): ILayer;
begin
  Result := TSilLayerCipher.Create(Parameters);
end;

class function Layer.Dispatcher(const Parameters: IParameters): ILayer;
begin
  Result := TSilLayerDispatcher.Create(Parameters, nil);
end;

class function Layer.List(const Parameters: IParameters): ILayerLinkList;
begin
  Result := TSilLayerLinkList.Create(Parameters);
end;

class function Layer.Slot(const Parameters: IParameters): ILayerSlot;
begin
  Result := TSilLayerSlot.Create(Parameters);
end;

class function Layer.Slot(out Obj: ILayerSlot; const Parameters: IParameters): ILayerSlot;
begin
  Obj := Slot(Parameters);
  Result := Obj;
end;

{ Packer }

class function Packer.Imate(const Parameters: IParameters): ILayer;
begin
  Result := TSilLayerPackerImate.Create(Parameters);
end;

(*)class function Packer.Probase(const Parameters: IParameters): ILayer;
begin
  Result := TSilLayerPackerProbase.Create(Parameters);
end;(*)

class function Packer.Text(const Parameters: IParameters): ILayer;
begin
  Result := TSilLayerPackerText.Create(Parameters);
end;

{ Device }

class function Device.FileStream(const Parameters: IParameters): ILayer;
begin
  Result := TSilFileLayer.Create(Parameters);
end;

class function Device.PipeClient(const Parameters: IParameters): ILayer;
begin
  Result := TSilNamedPipeClientLayer.Create(Parameters);
end;

class function Device.PipeServer(const Parameters: IParameters): ILayer;
begin
  Result := TSilNamedPipeServerLayer.Create(Parameters);
end;

class function Device.SerialPort(const Parameters: IParameters): ILayer;
begin
  Result := TSilSerialPortLayer.Create(Parameters);
end;

class function Device.SocketClient(const Parameters: IParameters): ILayer;
begin
  Result := TSilSocketClientLayer.Create(Parameters);
end;

class function Device.SocketServer(const Parameters: IParameters): ILayer;
begin
  Result := TSilSocketServerLayer.Create(Parameters);
end;

class function Device.SocketDatagram(const Parameters: IParameters): ILayer;
begin
  Result := TSilSocketDatagramLayer.Create(Parameters);
end;

{ Protocol }

class function Protocol.Blind(const Parameters: IParameters): IBlindProtocol;
begin
  Result := TSilBlindProtocol.Create(Parameters);
end;

class function Protocol.Blind(out Obj: IBlindProtocol; const Parameters: IParameters): IBlindProtocol;
begin
  Obj := Blind(Parameters);
  Result := Obj;
end;

class function Protocol.Eblis(const Parameters: IParameters): IEblisProtocol;
begin
  Result := TSilEblisProtocol.Create(Parameters);
end;

class function Protocol.Eblis(out Obj: IEblisProtocol; const Parameters: IParameters): IEblisProtocol;
begin
  Obj := Eblis(Parameters);
  Result := Obj;
end;

class function Protocol.FileClient(const Parameters: IParameters): IClientSideFileProtocol;
begin
  Result := TSilClientSideFileProtocol.Create(Parameters);
end;

class function Protocol.FileServer(const Parameters: IParameters): IServerSideFileProtocol;
begin
  Result := TSilServerSideFileProtocol.Create(Parameters);
end;

class function Protocol.Imate(const Parameters: IParameters): ILayerProtocol;
begin
  Result := TSilImateProtocolLayer.Create(Parameters);
end;

class function Protocol.Imate(Id: LongWord; const Parameters: IParameters): ILayerProtocol;
var
  List: IParameterList;
begin
  List := Sil.List.Parameters();
  List.Merge(Parameters);
  List['Id'] := Id;
  Result := Imate(List);
end;

class function Protocol.SmtpClient(const Parameters: IParameters): ISmtpClient;
var
  Layer: IUnknown;
begin
  Layer := TSilSmtpClientLayer.Create(Parameters);
  Result := Layer as ISmtpClient;
end;

{ Tk }

class procedure Tk.GetBuffer(const Packet: IPacket; out Buffer: String);
begin
  SetLength(Buffer, Packet.Buffer.Size);
  Move(Packet.Buffer.Memory^, Buffer[1], Length(Buffer));
end;

class function Tk.GetUpperID(const Link: ILayerLink): Variant;
begin
  Result := Vart.Unassigned;

  if Assigned(Link) then
    if Assigned(Link.Operation) and Vart.IsOK(Link.Operation.Id) then
      Result := Link.Operation.Id else
    if Assigned(Link.Upper) then
      Result := GetUpperID(Link.Upper);
end;

class function Tk.GetLink(const Layer: IUnknown; out Link: ILayerLink): Boolean;
var
  Terminal: ILayerTerminal;
begin
  if Ref.GetInterface(Layer, ILayerTerminal, Terminal) then
    Link := Terminal.Link;

  Result := Assigned(Link);
end;

class function Tk.FindUpper(const Link: ILayerLink; const IID: TGuid; out Obj): Boolean;
begin
  Result := Ref.GetInterface(Link, IID, Obj);

  if not Result and Assigned(Link.Upper) then
    Result := Tk.FindUpper(Link.Upper, IID, Obj);
end;

class function Tk.FindLower(const Link: ILayerLink; const IID: TGuid; out Obj): Boolean;
begin
  Result := Ref.GetInterface(Link, IID, Obj);

  if not Result and Assigned(Link.Lower) then
    Result := Tk.FindLower(Link.Lower, IID, Obj);
end;

class function Tk.GetLowerSlot(const Layer: IUnknown; out Slot: ILayerSlot): Boolean;
var
  Link: ILayerLink;
begin
  if Tk.GetLink(Layer, Link) then
    Result := Tk.FindLower(Link, ILayerSlot, Slot) else
    Result := false;
end;

class function Tk.ShareSlot(const Child, Sibling: IUnknown): Boolean;
var
  Slot: ILayerSlot;
begin
  Result := Tk.GetLowerSlot(Child, Slot);

  if Result then
  begin
    Slot.Add(Sibling);
    if Slot.Control.IsActive then Tk.Activate(Sibling);
  end;
end;

class function Tk.Activate(const Obj: IUnknown): Boolean;
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

class function Tk.Bindings: ILayerBindings;
begin
  Result := TSilLayerBindings.Create(false);
end;

class procedure Tk.DropChain(const Chain: ILayerChain);
begin
  Chain.Control.Deactivate;
  Chain.Clear(true);
end;

class function Tk.ReadXmlChain(const Tag: IXmlTag; const Bindings: ILayerBindings): ILayerChain;
begin
  Result := SilSfLayerXml.ReadXmlChain(Tag, Bindings);
end;

{ Connection }

class function Connection.Client(const Parameters: IParameters): ILayerConnection;
begin
  Result := TSilLayerConnection.Create;
end;

class function Connection.Peer(const List: ILayerConnectionList; const Chain: ILayerChain): ILayerConnection;
begin
  Result := TSilLayerConnectionPeer.CreatePeer(List, Chain);
end;

class function Connection.Server(const Parameters: IParameters): ILayerConnectionServer;
begin
  Result := TSilLayerConnectionServer.Create;
end;

end.
