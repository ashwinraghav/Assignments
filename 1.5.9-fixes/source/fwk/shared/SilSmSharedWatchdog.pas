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

unit SilSmSharedWatchdog;

{$INCLUDE Defines.inc}

interface

uses
  Sil,
  SilShSharedManager;

type
  TSilSharedWatchdog = class(
    TSilInterfacedObject,
    ISharedObjectWatchdog,
    IRunnable )
  private
    FManager: Pointer;
    FThread: IThread;
    FQueue: IInterfaceQueue;
  private
    function DoGetManager: ISharedObjectManager;
  protected // ISharedObjectWatchdog
    function GetThread: IThread;
    procedure Initialize(const Key: INamedKey);
    procedure Finalize;
    procedure Post(const Action: IAction); 
  protected // IRunnable
    procedure Run(const Thread: IThread); 
  public
    constructor Create(const Manager: ISharedObjectManager);
    destructor Destroy; override;
    property Manager: ISharedObjectManager read DoGetManager;
  end;

implementation

{ TSilSharedWatchdog }

constructor TSilSharedWatchdog.Create(const Manager: ISharedObjectManager);
begin
  inherited Create;
  
  FManager := Pointer(Manager);
  FQueue := Sil.List.InterfaceQueue();
end;

destructor TSilSharedWatchdog.Destroy;
begin
  FQueue := nil;
  FManager := nil;
  
  inherited;
end;

function TSilSharedWatchdog.GetThread: IThread;
begin
  Result := FThread;
end;

procedure TSilSharedWatchdog.Initialize(const Key: INamedKey);
begin
  FThread := Sil.Os.Thread.Spawn('shared', Self);
end;

procedure TSilSharedWatchdog.Finalize;
begin
  Locked;
  
  FQueue.Cancel;
  FThread.Termination.WaitFor();
  FThread := nil;
end;

procedure TSilSharedWatchdog.Run(const Thread: IThread);
var
  Item: IAction;
begin
  while FQueue.Get(IAction, Item) do
    Item.Execute(Thread, nil);
end;

function TSilSharedWatchdog.DoGetManager: ISharedObjectManager;
begin
  Result := ISharedObjectManager(FManager);
end;

procedure TSilSharedWatchdog.Post(const Action: IAction);
begin
  Locked;
 
  if Assigned(FQueue) then FQueue.Put(Action);
end;

end.
