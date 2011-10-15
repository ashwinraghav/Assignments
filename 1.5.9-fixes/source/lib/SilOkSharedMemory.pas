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

unit SilOkSharedMemory;

{$I Defines.inc}

interface

uses
  SilBeTypes,
  SilOsTypes,
  
  SilLiLock,

  SilOiHandle,
  SilOiWait,
  SilOiIpc,
  SilOiSharedMemory,

  SilOsHandled;

type
  TSilSharedMemory = class(
    TSilOsHandledObject,
    IWaitable,
    ISharedMemory )
  private
    FName: String;
    FSize: LongWord;
    FMemory: Pointer;
    FMutex: IMutex;
    function InternalOpen(const Name: PChar; var Size: LongWord): IHandle; reintroduce;
  protected  // ILockable
    procedure Lock; override;
    procedure Unlock; override;
  protected  // ISharedMemory
    function GetName: String;
    function GetSize: Integer;
    function GetBuffer: PChar;
  protected
    function DoCreate(const Name: PChar; Size: LongWord): IHandle; virtual; abstract;
    function DoOpen(const Name: PChar; out Size: LongWord): IHandle; reintroduce; virtual; abstract;
    function DoMap(const Start, Size: LongWord): Pointer; virtual; abstract;
    procedure DoUnmap(const Value: Pointer); virtual; abstract;
    function DoGetWaitable: IWaitable; virtual;
  protected // IWaitable
    property Waitable: IWaitable read DoGetWaitable implements IWaitable;
  public
    constructor Create(const Handle: IHandle; const Name: PChar; const Size: LongWord; const Validate: Boolean = True); overload;
    constructor Create(const Name: PChar; Size: LongWord); overload;
    constructor Create(const Name: PChar); overload;
    destructor Destroy; override;
  public
    property Name: string read FName;
    property Size: LongWord read FSize;
  end;

implementation

uses
  SilOtTool;

{ TSilSharedMemory }

constructor TSilSharedMemory.Create(const Name: PChar; Size: LongWord);
begin
  Create(InternalOpen(Name, Size), Name, Size);
end;

constructor TSilSharedMemory.Create(const Name: PChar);
begin
  Create(Name, 0);
end;

constructor TSilSharedMemory.Create(const Handle: IHandle; const Name: PChar; const Size: LongWord; const Validate: Boolean);
begin
  inherited Create(Handle, Validate);
  FMutex := OS.IPC.Mutex(false, PChar(Name + '_MUTEX'));
  FName := Name;
  FSize := Size;
  FMemory := DoMap(0, FSize);
end;

destructor TSilSharedMemory.Destroy;
begin
  if FMemory <> nil then
    DoUnmap(FMemory);
  FMutex := nil;
  inherited;
end;

function TSilSharedMemory.InternalOpen(const Name: PChar; var Size: LongWord): IHandle;
begin
  if Size <> 0 then
    Result := DoCreate(Name, Size) else
    Result := DoOpen(Name, Size)
end;

function TSilSharedMemory.GetBuffer: PChar;
begin
  Result := FMemory;
end;

function TSilSharedMemory.GetName: String;
begin
  Result := FName;
end;

function TSilSharedMemory.GetSize: Integer;
begin
  Result := FSize;
end;

function TSilSharedMemory.DoGetWaitable: IWaitable;
begin
  Result := FMutex as IWaitable;
end;

procedure TSilSharedMemory.Lock;
begin
  FMutex.WaitFor(INFINITE);
end;

procedure TSilSharedMemory.Unlock;
begin
  FMutex.Release;
end;

end.
