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
(*)  Windows,(*)
  SilBeTypes,
  
  SilLiLock,
  SilLiKey,
  SilLiEnumerator,
  SilLkNamedKey,

  SilOsTypes,
  
  SilOiThread,
  SilOeRegistry;

type
	TSilLinuxRegistryKey = class(TSilNamedKey)
	private
		(*)FKey: HKey;(*)
    FThread: IThread;
    FWatcher: ILockable;
    procedure DoCreateWatcher(const Filters: TNamedKeyNotificationFilters; const Subtree: Boolean);
    procedure DoDestroyWatcher;
  protected //- TSilNamedKey
    procedure DoClose; override;
    function DoGetKeys: INamedKeys; override;
    function DoGetValues: INamedValues; override;
    procedure DoNotify(const Sink: IUnknown; const Filters: TNamedKeyNotificationFilters; const Subtree: Boolean; KeepRef: Boolean); override;
  protected //- IConnectable
    procedure AddListener(const Listener: IUnknown; KeepRef: Boolean); override;
    procedure RemoveListener(const Listener: IUnknown); override;
  protected //- ILockable
    procedure Lock; override;
    procedure Unlock; override;
  public
    constructor Create; overload;
    (*)constructor Create(const Name: String; IsNew: Boolean; const Key: HKey); overload;(*)
    destructor Destroy; override;
    (*)property Key: HKey read FKey;(*)
  end;

implementation

uses
  SilLtConnection,
  SilOtTool,
  SilOmRegWatcher,
  SilOmRegKeys,
  SilOmRegValues;

{ TSilLinuxRegistryKey }

constructor TSilLinuxRegistryKey.Create;
begin
  raise Exception.CreateFmt('%s: not implemented', ['TSilLinuxRegistryKey.Create']);
  inherited Create;
end;

(*)constructor TSilLinuxRegistryKey.Create(const Name: String; IsNew: Boolean; const Key: HKey);
begin
  inherited Create(Name, IsNew);
  FKey := Key;
end;(*)

destructor TSilLinuxRegistryKey.Destroy;
begin
  DoDestroyWatcher;
  inherited;
end;

procedure TSilLinuxRegistryKey.DoClose;
begin
(*)  RegCloseKey(FKey);
  FKey := 0;(*)
end;

function TSilLinuxRegistryKey.DoGetKeys: INamedKeys;
begin
  Result := TSilLinuxRegistryKeys.Create(Self);;
end;

function TSilLinuxRegistryKey.DoGetValues: INamedValues;
begin
  Result := TSilLinuxRegistryValues.Create(Self);;
end;

procedure TSilLinuxRegistryKey.DoNotify(const Sink: IUnknown; const Filters: TNamedKeyNotificationFilters; const Subtree: Boolean; KeepRef: Boolean); 
begin
  if FWatcher = nil then DoCreateWatcher(Filters, Subtree);
  SilLtConnection.Sink.Connect(FWatcher, Sink);
end;

procedure TSilLinuxRegistryKey.AddListener(const Listener: IUnknown; KeepRef: Boolean);
begin
  Self.Notify(Listener);
end;

procedure TSilLinuxRegistryKey.RemoveListener(const Listener: IUnknown);
begin
  SilLtConnection.Sink.Disconnect(FWatcher, Listener);
end;

procedure TSilLinuxRegistryKey.Lock;
begin
  if FWatcher <> nil then
    FWatcher.Lock;
end;

procedure TSilLinuxRegistryKey.Unlock;
begin
  if FWatcher <> nil then
    FWatcher.Unlock;   
end;

procedure TSilLinuxRegistryKey.DoCreateWatcher(const Filters: TNamedKeyNotificationFilters; const Subtree: Boolean);
begin
  FWatcher := TSilLinuxRegistryWatcher.Create(Self, Subtree, Filters);
  FThread := Os.Thread.Spawn(FWatcher as IRunnable)
end;

procedure TSilLinuxRegistryKey.DoDestroyWatcher;
begin
  if FWatcher <> nil then Sink.Disconnect(FWatcher, Self);

  if FThread <> nil then
  begin
    (*)FThread.Termination.Signal;(*)
    Os.Wait.Single(FThread, INFINITE, True);
    FThread := nil;
  end;

  FWatcher := nil;
end;

end.
