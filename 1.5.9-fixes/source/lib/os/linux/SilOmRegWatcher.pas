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

unit SilOmRegWatcher;

{$I Defines.inc}

interface

uses
(*)  Windows,(*)
  SilOsTypes,
  
  SilLiKey,
  SilLiEnumerator,
  SilLkInterfaced,
  SilOiIpc,
  SilOiThread,
  SilOeRegistry,
  SilOmRegKey;

type
  TSilLinuxRegistryWatcher = class(
  //- extends
    TSilInterfacedObject,
  //- implements
    IRunnable )
  private
    FOwner: TSilLinuxRegistryKey;
    FEvent: IEvent;
    FLocks: Integer;
    FWatchFilters: TNamedKeyNotificationFilters;
    FWatchSubtree: Boolean;
    FChanged: Boolean;
  private
    function DoWatch: Boolean;
    procedure DoChanged;
    procedure DoFireChanged;
  protected //- IRunnable
    procedure Run(const Thread: IThread);
  protected //- ILockable
    procedure Lock; override;
    procedure Unlock; override;
  public
    constructor Create(const Owner: TSilLinuxRegistryKey; const WatchSubtree: Boolean; const Filter: TNamedKeyNotificationFilters);
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  SilOgRegistry,
  SilOtTool;

{ TSilLinuxRegistryWatcher }

constructor TSilLinuxRegistryWatcher.Create(const Owner: TSilLinuxRegistryKey; const WatchSubtree: Boolean; const Filter: TNamedKeyNotificationFilters);
begin
  raise Exception.CreateFmt('%s: not implemented', ['TSilLinuxRegistryWatcher.Create']);

  inherited Create;
  FOwner := Owner;
  FEvent := Os.IPC.Event(False);
  FWatchSubtree := WatchSubtree;
  FWatchFilters := Filter;
end;

destructor TSilLinuxRegistryWatcher.Destroy;
begin
  FOwner := nil;
  FEvent := nil;
  inherited;
end;


procedure TSilLinuxRegistryWatcher.DoChanged;
begin
  if FLocks = 0 then
  begin
    DoFireChanged();
    FChanged := False;
  end else
    FChanged := True;
end;

procedure TSilLinuxRegistryWatcher.DoFireChanged;
var
  Enum: IEnumerator;
  Sink: IEvNamedKeyChanged;
begin
  if HasConnections then
  begin
    Lock;
    try
      with Self.Events do
        while Enumerate(Enum, Sink, IEvNamedKeyChanged) do
          Sink.OnChanged(FOwner);
    finally
      Unlock;
    end;
  end;
end;

function TSilLinuxRegistryWatcher.DoWatch: Boolean;
begin
(*)  Result := RegNotifyChangeKeyValue(FOwner.Key, FWatchSubtree, FiltersToInteger(FWatchFilters), Os.Handle.GetHandle(FEvent).Value, True) = 0;(*)
end;

procedure TSilLinuxRegistryWatcher.Lock;
begin
  Os.Locked.Increment(FLocks);
end;

procedure TSilLinuxRegistryWatcher.Unlock;
begin
  if (Os.Locked.Decrement(FLocks) = 0) and FChanged then
    DoChanged;
end;

procedure TSilLinuxRegistryWatcher.Run(const Thread: IThread);
var
  Signaled: Integer;
begin
  while not Thread.IsTerminated and DoWatch() do
  begin
    if Os.Wait.Any([FEvent, Thread.Termination], INFINITE, Signaled, False) and (Signaled = 0) then
      DoChanged;
  end;
end;

end.
