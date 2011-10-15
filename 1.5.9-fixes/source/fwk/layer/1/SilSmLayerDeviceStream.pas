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

unit SilSmLayerDeviceStream;

interface

{$include Defines.inc}

uses
  Sil,
  SilScLayer,
  SilSiLayer,
  SilSmLayer;

type
  TSilLayerStream = class (
    // extends
    TSilLayer,
    // impements
    IRunnable,
    ILayerStream,
    ILayerTerminal)
  private
    FStream: IStream;
    FReadBlockSize: LongWord;
    FEnableReceive: Boolean;
    FReconnect: Integer;
    FReader: IThread;
    FLink: Pointer;
    FReadLock: IMutex;
    FWriteLock: IMutex;
    FWriteTimeout: LongWord;
    FReadTimeout: LongWord;
    FIsBottomTerminal: Boolean;
    FDeactivating: Boolean;
    FStarted: IEvent;
    FStop: IEvent;
    procedure DoRaiseIO;
  protected
    procedure DoStreamWrite(const Buffer; Size: LongWord; const Context: IUnknown); virtual;
  protected // IRunnable
    procedure Run(const Thread: IThread);
  protected
    procedure DoConnect; virtual;
    procedure DoDisconnect; virtual;
  protected
    procedure DoCheck(const Command: ILayerCommand); virtual;
  protected // property
    property Stream: IStream read FStream write FStream;
    property ReadBlockSize: LongWord read FReadBlockSize write FReadBlockSize;
  protected // ILayer
    procedure LayerActivate(const Link: ILayerLink; const Context: IUnknown); override;
    procedure LayerDeactivate(const Link: ILayerLink; const Context: IUnknown; IsBroken: Boolean); override;
    function GetIsActive: Boolean; override;
  protected // TLayer
    procedure Write(const Command: ILayerCommand); override;
    procedure Read(const Command: ILayerCommand); override;
  protected // ILayerStream
    function GetDevice: IUnknown; virtual; abstract;
    procedure SetDevice(const Value: IUnknown); virtual; abstract;
  protected // property
    property Device: IUnknown read GetDevice write SetDevice;
  protected // ILayerTerminal
    procedure StartLayer(const Context: IUnknown);
    procedure StopLayer(const Context: IUnknown);
    function GetLink: ILayerLink;
  protected // property
    property Link: ILayerLink read GetLink;
  public
    constructor Create(const Parameters: IParameters; const Controller: IUnknown); override;
    destructor Destroy; override;
  end;

implementation

uses
  SilLayer,
  SilSmLayerCommand;

{ TSilLayerStream }

constructor TSilLayerStream.Create(const Parameters: IParameters; const Controller: IUnknown);
begin
  inherited;
  FReadBlockSize := 1024 * 4;
end;

destructor TSilLayerStream.Destroy;
begin
  FStream := nil;
  inherited;
end;

procedure TSilLayerStream.LayerActivate(const Link: ILayerLink; const Context: IUnknown);
begin
  FDeactivating := false;

  if not IsActive then
    try
      FIsBottomTerminal := not Assigned(Link.Lower);

      if FReadLock = nil then FReadLock := Sil.OS.Ipc.Mutex;
      if FWriteLock = nil then FWriteLock := Sil.OS.Ipc.Mutex;

      FWriteTimeout := Parameters.Get('writetimeout', INFINITE);
      FReadTimeout := Parameters.Get('readtimeout', INFINITE);
      FEnableReceive := Parameters.Get('enablereceive', true);
      FReconnect := Parameters.Get('reconnect', 0);

      if FReconnect <= 0 then
        DoConnect;

      MakeRef(Link, @FLink);
    except
      raise
    end;
end;

procedure TSilLayerStream.StartLayer(const Context: IUnknown);
begin
  try
    if FEnableReceive and not Assigned(FReader) then
    begin
      FStarted := Sil.OS.Ipc.Event;
      FStop := Sil.OS.Ipc.Event;
      FReader := Sil.OS.Thread.Spawn(Self);
    end;
  except
    if Debug.Check(dlException, CDebugLayer) then
      Sil.Trace.Exception(ClassName + '.StartLayer');
  end;
end;

function TSilLayerStream.GetIsActive: Boolean;
begin
  Result := Assigned(FStream);
end;

procedure TSilLayerStream.LayerDeactivate(const Link: ILayerLink; const Context: IUnknown; IsBroken: Boolean);
var
  Started: IEvent;
begin
  if (FStop <> nil) and (not IsBroken or (FReconnect = 0)) then
    FStop.Signal;

  if FReader <> nil then
    try
      Started := FStarted;
      if Started <> nil then
        Started.WaitFor;

      FDeactivating := true;
      if not IsBroken then FReconnect := 0;

      DoDisconnect;

      if FReconnect = 0 then
      begin
        Sil.OS.Thread.Wait(FReader);

        FStream := nil;
        FReader := nil;

        DropRef(@FLink);
      end;

      //FReadLock := nil;
      //FWriteLock := nil;
    except
      // 
    end;
end;

procedure TSilLayerStream.StopLayer(const Context: IUnknown);
begin
end;

procedure TSilLayerStream.DoConnect;
begin
end;

procedure TSilLayerStream.DoDisconnect;
begin
end;

function TSilLayerStream.GetLink: ILayerLink;
begin
  Result := ILayerLink(FLink);
end;

procedure TSilLayerStream.Run(const Thread: IThread);
var
  Command: ILayerCommand;
begin
  try
    repeat
      try
        try
          DoConnect;
          ILayerLink(FLink).Chain.Control.Activate;
        finally
          FStarted.Signal;
        end;

        while IsActive do
        begin
          try
            try
              Command := Cmd.Create(nil, Link, Sil.Stream.Raw.Packet, Vart.Null, Device);
              Cmd.Read(Command, Link);

              if Command.Packet.Buffer.Size > 0 then
              begin
                if FIsBottomTerminal then
                  Cmd.Receive(Command)
                else
                  Cmd.Write(Command);
              end else
                Break;
            finally
              Command := nil;
            end;
          except
            if Debug.Check(dlException, CDebugLayer) then
              Sil.Trace.Exception(ClassName + '.Run');
          end;
        end;
      except
        on e: Exception do
        begin
          DoDisconnect;
          FStop.WaitFor(FReconnect);
        end;
      end;
    until (FReconnect = 0) or FStop.IsSignaled;
  except
    if Debug.Check(dlException, CDebugLayer) then
      Sil.Trace.Exception(ClassName + '.Run');
  end;
end;

procedure TSilLayerStream.DoRaiseIO;
begin
  raise Error.Create('i/o timeout: locked');
end;

procedure TSilLayerStream.Write(const Command: ILayerCommand);
var
  Lock: IMutex;
begin
  if Debug.Check(dlInOut, CDebugLayer) then Sil.Trace.Log(ClassName + '.Write');

  Lock := FWriteLock;

  with Command do
  begin
    if Lock.WaitFor(FWriteTimeout) <> wrSignaled then
      DoRaiseIO;

    try
      DoStreamWrite(Packet.Buffer.Memory^, Packet.Buffer.Size, Context);
    finally
      Lock.Release;
    end;

    DoCheck(Command);
  end;
end;

procedure TSilLayerStream.DoStreamWrite(const Buffer; Size: LongWord; const Context: IUnknown);
begin
  if Assigned(FStream) then FStream.Write(Buffer, Size);
end;

procedure TSilLayerStream.Read(const Command: ILayerCommand);
var
  Buf: String;
  lwSize: LongWord;
  Lock: IMutex;
begin
  if Debug.Check(dlInOut, CDebugLayer) then Sil.Trace.Log(ClassName + '.Read');

  Lock := FReadLock;
  
  with Command do
  begin
    if Packet.Buffer.Size <> 0 then
      SetLength(Buf, Packet.Buffer.Size)
    else
      SetLength(Buf, FReadBlockSize);

    if Lock.WaitFor(FReadTimeout) <> wrSignaled then
      DoRaiseIO;

    try
      if Assigned(FStream) then
        lwSize := FStream.Read(Buf[1], Length(Buf))
      else
        lwSize := 0;

      (*)if lwSize > 0 then
      begin
        sil.trace.log(sil.str.ReplaceControlChars(pchar(buf), lwsize));
      end;(*)
    finally
      Lock.Release;
    end;

    Packet.Buffer.Write(Buf[1], lwSize);
    DoCheck(Command);
  end;
end;

procedure TSilLayerStream.DoCheck(const Command: ILayerCommand);
var
  Link: ILayerLink;
  Chain: ILayerChain;
  Control: ILayerLinkControl;
begin
  if not FDeactivating and (Command.Packet.Buffer.Size <= 0) then
  begin
    if Debug.Check(dlConnection, CDebugLayer) then Sil.Trace.Log(ClassName + '.DoCheck desconectado');

    Link := GetLink;

    if Assigned(Link) then
    begin
      Chain := Link.Chain;

      if Assigned(Chain) then
      begin
        Control := Chain.Control;

        if Assigned(Control) then
          Control.Deactivate(Link, true);
      end;
    end;
  end;
end;

end.
