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

unit SilOmPipe;

{$I Defines.inc}

interface
{$IFNDEF FP20}
uses
  SilLiStream,
  SilOiHandle,
  SilOiIpc,
  SilOiPipe,
  SilOkPipe,
  SilOsTypes;

type
  TLinuxNamedPipeServer = class(
    TSilNamedPipeServer)
  private
    FTermination: IEvent;
  protected
    function Connect(out Pipe: INamedPipeServerClient): Boolean; override;
    procedure Close; override;
  public
    constructor Create(const Parameters: INamedPipeParameters); override;
    destructor Destroy; override;
  end;

  TLinuxPipe = class(TSilPipe)
  protected
    function DoPipeGetSize: LongWord; override;
    function DoPipeRead(var Buffer; Count: LongWord): LongWord; override;
    function DoPipeWrite(const Buffer; Count: LongWord): LongWord; override;
  end;

  TLinuxNamedPipeClient = class(
    TLinuxPipe,
    INamedPipeClient )
  private
    FParameters: INamedPipeParameters;
  protected //-
    function DoOpenNamedPipe(const Parameters: INamedPipeParameters): IHandle;
  protected //- INamedPipeClient
    function GetParameters: INamedPipeParameters;
  public
    constructor Create(const Parameters: INamedPipeParameters);
    destructor Destroy; override;
    property Parameters: INamedPipeParameters read FParameters;
  end;

  TLinuxNamedPipeServerClient = class(
    TLinuxPipe,
    INamedPipeClient,
    INamedPipeServerClient )
  private
    FServer: TLinuxNamedPipeServer;
  protected //- INamedPipeClient
    function GetParameters: INamedPipeParameters;
    property Parameters: INamedPipeParameters read GetParameters;
  protected //- INamedPipeServerClient
    function GetServer: INamedPipeServer;
    property Server: INamedPipeServer read GetServer;
  public
    constructor Create(const Server: TLinuxNamedPipeServer; const Pipe: THandle);
    destructor Destroy; override;
  end;
{$ENDIF}
implementation
{$IFNDEF FP20}
uses
(*)  Linux,(*)
  SysUtils,

  SilOeWait,
  SilBtInt,
  SilOtTool;

(*)const
  MPipeAccess:   array[TPipeAccess] of LongWord = (PIPE_ACCESS_DUPLEX, PIPE_ACCESS_INBOUND, PIPE_ACCESS_OUTBOUND);
  MPipeReadMode: array[TPipeMode]   of Integer = (PIPE_READMODE_BYTE, PIPE_READMODE_MESSAGE);
  MPipeType:     array[TPipeMode]   of Integer = (PIPE_TYPE_BYTE, PIPE_TYPE_MESSAGE);(*)

{ TSilNamedPipe }

function TLinuxPipe.DoPipeGetSize: LongWord;
begin(*)
  if not Linux.PeekNamedPipe(Handle.Value, nil, 0, nil, @Result, nil) then
    raise Os.Error.Create(Linux.GetLastError(), 'TLinuxPipe.DoPipeGetSize [Linux.PeekNamedPipe]');
(*)end;

function TLinuxPipe.DoPipeRead(var Buffer; Count: LongWord): LongWord;(*)
var
  Ov: OVERLAPPED;
  Ev: IEvent;
  Handle: THandle;(*)
begin(*)
  Ev := Os.IPC.Event();
  Handle := Self.Handle.Value;
  Ov.hEvent := Os.Handle.GetHandle(Ev).Value;

  if not Linux.ReadFile(Handle, Buffer, Count, Result, @Ov) and not Os.Error.Check(Linux.GetLastError, [ERROR_IO_PENDING]) then
    raise Os.Error.Create(Linux.GetLastError(), 'TLinuxPipe.DoPipeRead [Linux.ReadFile]');

  if Ev.WaitFor(INFINITE) = wrSignaled then
    Linux.GetOverlappedResult(Handle, Ov, Result, False) else
    Result := 0;
(*)end;

function TLinuxPipe.DoPipeWrite(const Buffer; Count: LongWord): LongWord;(*)
var
  Ov: OVERLAPPED;
  Ev: IEvent;
  Handle: THandle;(*)
begin(*)
  Ev := Os.IPC.Event();
  Handle := Self.Handle.Value;
  Ov.hEvent := Os.Handle.GetHandle(Ev).Value;

  if not Linux.WriteFile(Handle, Buffer, Count, Result, @Ov) and not Os.Error.Check(Linux.GetLastError, [ERROR_IO_PENDING]) then
    raise Os.Error.Create(Linux.GetLastError(), 'TLinuxPipe.DoPipeWrite [Linux.WriteFile]');

  if Ev.WaitFor(INFINITE) = wrSignaled then
    Linux.GetOverlappedResult(Handle, Ov, Result, False) else
    Result := 0;
(*)end;

{ TLinuxNamedPipeServer }

constructor TLinuxNamedPipeServer.Create(const Parameters: INamedPipeParameters);
begin
  raise Exception.CreateFmt('%s: not implemented', ['TLinuxNamedPipeServer.Create']);

  inherited;
  FTermination := Os.IPC.Event();
end;

destructor TLinuxNamedPipeServer.Destroy;
begin
  inherited;
  FTermination := nil;
end;

function TLinuxNamedPipeServer.Connect(out Pipe: INamedPipeServerClient): Boolean;(*)
var
  Handle: THandle;
  Ov: OVERLAPPED;
  Ev: IEvent;
  Signaled: Integer;(*)
begin(*)
  Ev := Os.IPC.Event();

  Ov.hEvent := Os.Handle.GetHandle(Ev).Value;

  Handle := Linux.CreateNamedPipe(
              PChar(Self.Parameters.Name),
              FILE_FLAG_WRITE_THROUGH or FILE_FLAG_OVERLAPPED or MPipeAccess[Parameters.Access] or GENERIC_READ or GENERIC_WRITE,
              MPipeType[Parameters.PipeType] or MPipeReadMode[Parameters.ReadMode],
              Int.Iif(Parameters.MaxInstances = piUnlimited, PIPE_UNLIMITED_INSTANCES, Parameters.MaxInstances),
              Parameters.OutBufferSize,
              Parameters.InBufferSize,
              Parameters.DefaultTimeout,
              nil // SECURITY_ATTRIBUTES
              );

  Result := Handle <> INVALID_HANDLE_VALUE;

  if not Result  then
    raise Os.Error.Create(Linux.GetLastError(), 'TLinuxNamedPipeServer.Connect [Linux.CreateNamedPipe]');

  try
    try
      Result := Linux.ConnectNamedPipe(Handle, @Ov); // se bloquea hasta que se conecta un cliente

      if not Result and not Os.Error.Check(GetLastError, [ERROR_IO_PENDING, ERROR_PIPE_CONNECTED]) then
        raise Os.Error.Create(GetLastError(), 'TLinuxNamedPipeServer.Connect [Linux.ConnectNamedPipe]');

        Result := Os.Wait.Any([Ev, FTermination], INFINITE, Signaled) and (Signaled = 0);

      if Result then
        Pipe := TLinuxNamedPipeServerClient.Create(Self, Handle);

    except
      Result := False;  // hay que cerrarlo porque el CreateNamedPipe fue successful!
      raise;
    end;
  finally
    if not Result then
      CloseHandle(Handle);
  end;
(*)end;

procedure TLinuxNamedPipeServer.Close;
begin
  FTermination.Signal;
end;

{ TLinuxNamedPipeServerClient }

constructor TLinuxNamedPipeServerClient.Create(const Server: TLinuxNamedPipeServer; const Pipe: THandle);
begin
  inherited Create(Pipe);
  FServer := Server;
end;

destructor TLinuxNamedPipeServerClient.Destroy;
begin
  FServer := nil;
  inherited;
end;

function TLinuxNamedPipeServerClient.GetParameters: INamedPipeParameters;
begin
  Result := FServer.Parameters;
end;

function TLinuxNamedPipeServerClient.GetServer: INamedPipeServer;
begin
  Result := FServer;
end;

{ TLinuxNamedPipeClient }

constructor TLinuxNamedPipeClient.Create(const Parameters: INamedPipeParameters);
begin
  inherited Create(DoOpenNamedPipe(Parameters));
  FParameters := Parameters;
end;

destructor TLinuxNamedPipeClient.Destroy;
begin
  FParameters := nil;
  inherited;
end;

function TLinuxNamedPipeClient.DoOpenNamedPipe(const Parameters: INamedPipeParameters): IHandle;(*)
var
  Handle: THandle;
  Mode: LongWord;(*)
begin(*)
  Handle := Linux.CreateFile(
                PChar(Parameters.Name),
                MPipeAccess[Parameters.Access] or GENERIC_READ or GENERIC_WRITE,
                FILE_SHARE_READ or FILE_SHARE_WRITE,
                nil,
                OPEN_EXISTING,
                FILE_FLAG_WRITE_THROUGH or FILE_FLAG_OVERLAPPED, 0);

  if Handle = INVALID_HANDLE_VALUE then

    raise Os.Error.Create(Linux.GetLastError(), 'TLinuxNamedPipeClient.DoOpenNamedPipe [Linux.CreateFile]');

  try
    Mode := MPipeReadMode[Parameters.ReadMode];

    if not Linux.SetNamedPipeHandleState(
                Handle,
                Mode,
                nil, nil) then

    raise Os.Error.Create(Linux.GetLastError(), 'TLinuxNamedPipeClient.DoOpenNamedPipe [Linux.SetNamedPipeHandleState]');

    Result := DoCreateHandle(Handle);
  except
    if Handle <> INVALID_HANDLE_VALUE then
      CloseHandle(Handle);
    raise;
  end;
(*)end;

function TLinuxNamedPipeClient.GetParameters: INamedPipeParameters;
begin
  Result := FParameters;
end;
{$ENDIF}
end.
