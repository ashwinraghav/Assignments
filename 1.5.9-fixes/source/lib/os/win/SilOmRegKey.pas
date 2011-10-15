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

unit SilOmRegKey;

{$I Defines.inc}

interface

uses
  SysUtils,
  Windows,
  SilBeTypes,

  SilLiLock,
  SilLiKey,
  SilLiEnumerator,
  SilLkNamedKey,

  SilOiHandle,
  SilOiThread,
  SilOiIpc,
  SilOeRegistry,

  SilOmHandle;

type
  TSilWindowsRegistryHandle = class(TSilHandle)
  protected
    procedure HandleIsValid(var Result: Boolean); override; 
    procedure HandleClose; override; 
  end;

  TSilWindowsRegistryKey = class(
    TSilNamedKey,
    IHandledObject )
  private
    FHandle: IHandle;
    FFinish: IEvent;
    FThread: IThread;
    FWatcher: ILockable;
  private 
    procedure DoCreateWatcher(const Filters: TNamedKeyNotificationFilters; const Subtree: Boolean);
    procedure DoDestroyWatcher;
  protected //- TSilNamedKey
    function GetParentPath: string; override;
    procedure DoClose; override;
    function DoGetKeys: INamedKeys; override;
    function DoGetValues: INamedValues; override;
    procedure DoNotify(const Sink: IUnknown; const Filters: TNamedKeyNotificationFilters; const Subtree: Boolean; KeepRef: Boolean); override;
  protected // IHandledObject
    function GetHandle: IHandle;
  protected //- IConnectable
    procedure AddListener(const Listener: IUnknown; KeepRef: Boolean); override;
    procedure RemoveListener(const Listener: IUnknown); override;
  protected //- ILockable
    procedure Lock; override;
    procedure Unlock; override;
  public
    constructor Create; overload;
    constructor Create(const Parent: INamedKey; const Name: String; IsNew: Boolean; const Key: HKey); overload;
    destructor Destroy; override;
  public
    property Handle: IHandle read FHandle;
  end;

implementation

uses
  SilLtConnection,
  SilBtStr,
  SilOcTypes,
  SilOtError,
  SilOtTool,
  SilOmRegWatcher,
  SilOmRegKeys,
  SilOmRegValues;

{ TSilWindowsRegistryHandle }

procedure TSilWindowsRegistryHandle.HandleIsValid(var Result: Boolean);
begin
  inherited;
end;

procedure TSilWindowsRegistryHandle.HandleClose;
begin
  RegCloseKey(Self.Handle);
end;

{ TSilWindowsRegistryKey }

constructor TSilWindowsRegistryKey.Create;
begin
  inherited Create;
end;

constructor TSilWindowsRegistryKey.Create(const Parent: INamedKey; const Name: String; IsNew: Boolean; const Key: HKey);
begin
  inherited Create(Parent, Name, IsNew);
  FHandle := TSilWindowsRegistryHandle.Create(Key);
end;

destructor TSilWindowsRegistryKey.Destroy;
begin
  DoDestroyWatcher;
  FHandle := nil;
  inherited;
end;

function TSilWindowsRegistryKey.GetParentPath: string;
begin
  Result := inherited GetParentPath;
  if Str.LastChar(Result) <> CPathSeparator then Str.Add(Result, CPathSeparator);
end;

procedure TSilWindowsRegistryKey.DoClose;
begin
  if Assigned(FHandle) then
  begin
    FHandle.Close;
    FHandle := nil;
  end;
end;

function TSilWindowsRegistryKey.DoGetKeys: INamedKeys;
begin
  Result := TSilWindowsRegistryKeys.Create(Self);;
end;

function TSilWindowsRegistryKey.DoGetValues: INamedValues;
begin
  Result := TSilWindowsRegistryValues.Create(Self);;
end;

procedure TSilWindowsRegistryKey.DoNotify(const Sink: IInterface; const Filters: TNamedKeyNotificationFilters; const Subtree: Boolean; KeepRef: Boolean);
begin
  if FWatcher = nil then DoCreateWatcher(Filters, Subtree);
  SilLtConnection.Sink.Connect(FWatcher, Sink, KeepRef);
end;

function TSilWindowsRegistryKey.GetHandle: IHandle;
begin
  Result := FHandle;
end;

procedure TSilWindowsRegistryKey.AddListener(const Listener: IUnknown; KeepRef: Boolean);
begin
  Self.Notify(Listener, knChangeAny, True, KeepRef);
end;

procedure TSilWindowsRegistryKey.RemoveListener(const Listener: IUnknown);
begin
  SilLtConnection.Sink.Disconnect(FWatcher, Listener);
end;

procedure TSilWindowsRegistryKey.Lock;
begin
  if FWatcher <> nil then
    FWatcher.Lock;
end;

procedure TSilWindowsRegistryKey.Unlock;
begin
  if FWatcher <> nil then
    FWatcher.Unlock;   
end;

procedure TSilWindowsRegistryKey.DoCreateWatcher(const Filters: TNamedKeyNotificationFilters; const Subtree: Boolean);
begin
  FFinish := OS.Ipc.Event;
  FWatcher := TSilWindowsRegistryWatcher.Create(Self, FFinish, Subtree, Filters);
  FThread := Os.Thread.Spawn(FWatcher as IRunnable)
end;

procedure TSilWindowsRegistryKey.DoDestroyWatcher;
begin
  if FWatcher <> nil then Sink.Disconnect(FWatcher, Self);

  if FThread <> nil then
  begin
    FFinish.Signal;
    FThread.Termination.WaitFor(INFINITE, True);
    FThread := nil;
    FFinish := nil;
  end;

  FWatcher := nil;
end;

end.
