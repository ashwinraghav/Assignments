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

unit SilSmLayerProtocolImate;

interface

{$include Defines.inc}

uses
  Sil,
  SilSeLayerProtocol,
  SilSiLayer,
  SilSmLayerProtocol;

type
  TSilImateProtocolLayer = class (
    // extends
    TSilProtocolLayer)
  private
    FVersion: LongWord;
    FDispatchable: IDispatchable;
    FSendExceptions: Boolean;
    function DoCreateException(Error: Exception; Id: LongWord): IPacket;
    function DoEmptyReply(Data: Integer; var Packet: IPacket): IPacket;
  private
    procedure DoSendException(Error: Exception; Id: LongWord; const Context: IUnknown = nil);
    procedure DoRaiseException(const Packet: IPacket);
  protected
    procedure LayerActivate(const Link: ILayerLink; const Context: IUnknown); override;
    procedure LayerDeactivate(const Link: ILayerLink; const Context: IUnknown; IsBroken: Boolean); override;
  protected // ILayerProtocol
    function CreatePacket(Data: Integer; Flags: LongWord): IPacket; override;
    procedure SendPacket(const Packet: IPacket; const Context: IUnknown; const Params: IParameterList); override;
    procedure SendPacket(const Packet: IPacket; out Reply: IPacket; Data: Integer; Timeout: LongWord; const Context: IUnknown; const Params: IParameterList); override;
    function ComparePacket(const Wait: ILayerWaitPacket; const Command: ILayerCommand): Boolean; override;
    function IsValidMessage(const Command: ILayerCommand): Boolean; override;
    function ExtractPacket(const Packet: IPacket): IPacket; override;
    procedure DispatchMessage(const Msg: ILayerCommand); override;
  public
    constructor Create(const Parameters: IParameters; const Controller: IUnknown = nil); override;
    destructor Destroy; override;
  end;

  ESilImateProtocolException = class (Exception)
  end;

implementation

uses
  SilStLayer,
  SilScLayer, 
  SilSmLayer;

{ TSilImateProtocolLayer }

constructor TSilImateProtocolLayer.Create(const Parameters: IParameters; const Controller: IUnknown);
begin
  inherited Create(Parameters, Controller);
  FSendExceptions := Self.Parameters.Get('SendExceptions', true);
end;

destructor TSilImateProtocolLayer.Destroy;
begin
  FDispatchable := nil;
  inherited;
end;

procedure TSilImateProtocolLayer.LayerActivate(const Link: ILayerLink; const Context: IInterface);
begin
  inherited;
  Ref.GetInterface(ProtocolControl, IDispatchable, FDispatchable);

  Id := Parameters['Id'];
  FVersion := Parameters.Get('Version', 0);
end;

procedure TSilImateProtocolLayer.LayerDeactivate(const Link: ILayerLink; const Context: IInterface; IsBroken: Boolean);
begin
  inherited;
  FDispatchable := nil;
end;

function TSilImateProtocolLayer.IsValidMessage(const Command: ILayerCommand): Boolean;
var
  ProtocolId: LongWord;
begin
  ProtocolId := Command.Target;
  Result := (ProtocolId = Id) and (Command.Packet.Buffer.Size >= SizeOf(RImateProtocolHeader));
  (*)
  if Result then
  begin
    Packet := Sil.Stream.Raw.Packet;
    Packet.Buffer.Size := Command.Packet.Buffer.Remaining;
    Command.Packet.Buffer.Read(Packet.Buffer.Memory^, Packet.Buffer.Size);
  end;
  (*)
end;

function TSilImateProtocolLayer.ExtractPacket(const Packet: IPacket): IPacket;
begin
  try
    Result := Sil.Stream.Typed.Packet;
    Packet.Buffer.Position := SizeOf(RImateProtocolHeader);
    Result.Buffer.Size := Packet.Buffer.Remaining;
    Packet.Buffer.Read(Result.Buffer.Memory^, Result.Buffer.Size);
  except
    if Debug.Check(dlException, CDebugLayer) then
      Sil.Trace.Exception(ClassName + '.ExtractPacket');
  end;
end;

procedure TSilImateProtocolLayer.DispatchMessage(const Msg: ILayerCommand);
var
  Header: RImateProtocolHeader;
  MsgRec: RImateProtocolMessage;
  Packet: IPacket;
  Dispatchable: IDispatchable;
begin
  Dispatchable := FDispatchable;

  if Dispatchable <> nil then
  begin
    Packet := Msg.Packet;
    Packet.Buffer.Position := 0;
    Packet.Buffer.Read(Header, SizeOf(RImateProtocolHeader));

    MsgRec.Packet := ExtractPacket(Packet);
    MsgRec.Context := Msg.Context;

    MsgRec.Id := Header.Id;
    MsgRec.Version := Header.Version;
    MsgRec.Flags := Header.Flags;

    try
      if Header.Flags and ifException = 0 then
        Dispatchable.Dispatch(MsgRec) else
        {DoFireException};
    except
      on e: Exception do
      begin
        if Debug.Check(dlException, CDebugLayer) then
          Sil.Trace.Error(ClassName + '.DispatchMessage id %d %s', [Header.Id, e.message]);

        if Header.Flags and ifException = 0 then
          if FSendExceptions then
            DoSendException(e, Header.Id, Msg.Context) else
            raise;
      end;
    end;
  end;
end;

function TSilImateProtocolLayer.DoCreateException(Error: Exception; Id: LongWord): IPacket;
begin
  Result := CreatePacket(Id, ifException);
  Result.Writer.WriteString(Error.ClassName);
  Result.Writer.WriteString(Error.Message);
end;

procedure TSilImateProtocolLayer.DoSendException(Error: Exception; Id: LongWord; const Context: IUnknown);
var
  Packet: IPacket;
begin
  Packet := DoCreateException(Error, Id);
  SendPacket(Packet, Context, nil);
end;

procedure TSilImateProtocolLayer.DoRaiseException(const Packet: IPacket);
var
  ErrStr: String;
begin
  ErrStr := '(' + Packet.Reader.ReadString + ') ' + Packet.Reader.ReadString;

  if Debug.Check(dlException, CDebugLayer) then
    Sil.Trace.Error(ClassName + '.DoRaiseException"%S"', [ErrStr]);

  raise Error.Create(ErrStr, ESilImateProtocolException);
end;

function TSilImateProtocolLayer.ComparePacket(const Wait: ILayerWaitPacket; const Command: ILayerCommand): Boolean;
var
  Query, Reply: PImateProtocolHeader;
begin
  try
    Query := PImateProtocolHeader(Wait.Request.Buffer.Memory);
    Reply := PImateProtocolHeader(Command.Packet.Buffer.Current);
    Result := (Reply.Id = Wait.Data) or ((ifException and Reply.Flags <> 0) and (Query.Id = Reply.Id));
  except
    Result := false;

    if Debug.Check(dlException, CDebugLayer) then
      Sil.Trace.Exception(ClassName + '.ComparePacket');
  end;
end;

function TSilImateProtocolLayer.CreatePacket(Data: Integer; Flags: LongWord): IPacket;
var
  Header: RImateProtocolHeader;
begin
  Header.Id := Data;
  Header.Version := FVersion;
  Header.Flags := Flags;
  Header.Size := 0;

  Result := Sil.Stream.Typed.Packet;
  Result.Writer.Write(Header, SizeOf(Header));
end;

procedure TSilImateProtocolLayer.SendPacket(const Packet: IPacket; const Context: IUnknown; const Params: IParameterList);
begin
  try
    if Assigned(Link) then
    begin
      PImateProtocolHeader(Packet.Buffer.Memory)^.Size := Packet.Buffer.Size;
      Cmd.Write(Link, Packet, Context, Params);
    end else
      raise Sil.Error.Create('no se encontro el dispositivo');
  except
    if Debug.Check(dlException, CDebugLayer) then
      Sil.Trace.Exception(ClassName + '.SendPacket packet');

    raise;
  end;
end;

function TSilImateProtocolLayer.DoEmptyReply(Data: Integer; var Packet: IPacket): IPacket;
begin
  if not Assigned(Packet) then
    Packet := DoCreateException(Error.Create('no hay reply'), Data);

  Result := ExtractPacket(Packet);
end;

procedure TSilImateProtocolLayer.SendPacket(const Packet: IPacket; out Reply: IPacket; Data: Integer; Timeout: LongWord; const Context: IUnknown; const Params: IParameterList);
var
  Source: IPacket;
  Header: RImateProtocolHeader;
begin
  if Data = -1 then
  begin
    Packet.Buffer.Position := 0;
    Packet.Buffer.Read(Header, SizeOf(Header));
    Data := Header.Id;
  end;

  inherited SendPacket(Packet, Source, Reply, Data, Timeout, Context, Params);

  if not Assigned(Reply) then
    Reply := DoEmptyReply(Data, Source);

  Source.Buffer.Position := 0;
  Source.Buffer.Read(Header, SizeOf(RImateProtocolHeader));

  if Header.Flags and ifException > 0 then
    DoRaiseException(Reply) else
    Reply.Buffer.Position := 0;
end;

end.
