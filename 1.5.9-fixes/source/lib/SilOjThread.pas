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

unit SilOjThread;

{$I Defines.inc}

interface

uses
  SilBkTool,
  SilLiReference,
  SilOsTypes,
  SilOiThread;

type
  SilThread = class(Tool)
    class function Spawn(const Runnable: IRunnable; CreateSuspended: Boolean = false): IThread; overload;
    class function Spawn(const Name: String; const Runnable: IRunnable; CreateSuspended: Boolean = false): IThread; overload;
    class function Spawn(Id: Longword; const Name: String; const Dispatchable: IDispatchable; CreateSuspended: Boolean = false): IThread; overload;
    class function Spawn(Id: Longword; const Dispatchable: IDispatchable; CreateSuspended: Boolean = false): IThread; overload;
    class function Current: IThread;
    class function List: IThreadList; virtual; abstract;
    class function ID: LongWord; virtual; abstract;
    class function IsMain: Boolean; virtual; abstract;
    class function CreateList: IThreadList;
    class procedure SyncCall(Method: TThreadCallMethod; const Param: Pointer); overload; virtual; abstract;
    class procedure SyncCall(Method: TThreadCallMethod); overload; virtual; abstract;
    class procedure SyncCall(Method: TThreadCallMethod; const Obj: IUnknown; Param: Pointer = nil); overload; virtual; abstract;
    class procedure AsyncCall(Method: TThreadCallMethod; const Param: Pointer); overload; virtual; abstract;
    class procedure AsyncCall(Method: TThreadCallMethod); overload; virtual; abstract;
    class procedure AsyncCall(Method: TThreadCallMethod; const Obj: IUnknown; Param: Pointer = nil); overload; virtual; abstract;
    class procedure Wait(const Thread: IThread; Timeout: LongWord = INFINITE; Detach: Boolean = true);
  end;

implementation

uses
  SilOsClasses,
  SilOkThreadList;

{ Thread }

class function SilThread.Spawn(const Runnable: IRunnable; CreateSuspended: Boolean): IThread;
begin
  Result := TSilOsThread.Create(Runnable, CreateSuspended);
end;

class function SilThread.Spawn(const Name: String; const Runnable: IRunnable; CreateSuspended: Boolean): IThread;
begin
  Result := TSilOsThread.Create(Name, Runnable, CreateSuspended);
end;

class function SilThread.Current: IThread;
begin
  Result := TSilOsThread.GetCurrent;
end;

class function SilThread.Spawn(Id: Longword; const Name: String; const Dispatchable: IDispatchable; CreateSuspended: Boolean): IThread;
begin
  Result := TSilOsThread.Create(Id, Name, Dispatchable, CreateSuspended);
end;

class function SilThread.Spawn(Id: Longword; const Dispatchable: IDispatchable; CreateSuspended: Boolean): IThread;
begin
  Result := TSilOsThread.Create(Id, '', Dispatchable, CreateSuspended);
end;

class procedure SilThread.Wait(const Thread: IThread; Timeout: LongWord; Detach: Boolean);
begin
  if Assigned(Thread) then
  begin
    if not Thread.IsCurrent then Thread.Termination.WaitFor(Timeout, true);
    if Detach then Thread.Detach;
  end;
end;

class function SilThread.CreateList: IThreadList;
begin
  Result := TSilThreadList.Create;
end;

end.
