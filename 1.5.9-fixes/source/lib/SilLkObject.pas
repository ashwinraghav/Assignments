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

unit SilLkObject;

{$INCLUDE Defines.inc}

interface

uses
  SilBeTypes,
  SilLiLock,
  SilLiReference,
  SilLiExtensible,
  SilLiObject;

type
  TSilObjectClass = class of TSilObject;

  TSilObject = class(
    TObject,
    IUnknown )
  private
    FRefCount: Integer;
    FExtension: IUnknown;
  protected // IReferenceable
    function GetInstance: Pointer;
  protected // IExtensible
    function GetHasExtension: Boolean;
    function GetExtension: IUnknown;
    procedure SetExtension(const Value: IUnknown);
  protected // ISynchronizable
    function Locked: ILock; virtual;
    function Lock: ILock;
  protected // ILockingSet
    function GetIsLockable: Boolean; virtual;
    procedure SetIsLockable(const Value: Boolean); virtual;
    function GetLockable: ILockable; virtual;
    procedure SetLockable(const Value: ILockable); virtual;
  protected // IObject
    function GetInstanceName: string; virtual;
    function GetRefCount: Integer;
    function GetMainInterface: IUnknown; virtual;
  protected // interface accessors
    function GetILockingSet: ILockingSet;
    function GetISynchronizable: ISynchronizable;
    function GetIExtensible: IExtensible;
  protected
    property Lockable: ILockable read GetLockable write SetLockable;
    property IsLockable: Boolean read GetIsLockable write SetIsLockable;
    property LockingSet: ILockingSet read GetILockingSet;
    property Synchronized: ISynchronizable read GetISynchronizable;
    property Extensible: IExtensible read GetIExtensible;
  public
    constructor CreateNew(const Owner: IUnknown = nil; Param: Pointer = nil); virtual;
    constructor Create; overload;
    destructor Destroy; override;
  public
    class function NewInstance: TObject; override;
    procedure FreeInstance; override;
    procedure BeforeConstruction; virtual;
    procedure AfterDestruction; virtual;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  public // IUnknown
    function QueryInterface(const IID: TGUID; out Obj): HResult; virtual; stdcall;
    function _AddRef: Integer; virtual; stdcall;
    function _Release: Integer; virtual; stdcall;
  public
    procedure Release; virtual;
    procedure Free;
  public
    property RefCount: Integer read FRefCount;
    property InstanceName: string read GetInstanceName;
    property MainInterface: IUnknown read GetMainInterface;
    property HasExtension: Boolean read GetHasExtension;
    property Extension: IUnknown read FExtension write FExtension;
  end;

implementation

uses
  SilBtInt,
  SilBtStr,
  SilLdInterfaced,
  SilLmLockedSection,
  SilLtReference,
  SilOtTool,
  SilOsTypes,
  SilOfLocked;

type
  TSilDummyLock = class(
    TSilObject,
    ILock )
  protected // ILock
    procedure Release; reintroduce; 
  end;

var
  DummyLock: ILock = nil;

{ TSilDummyLock }

procedure TSilDummyLock.Release;
begin
end;

{ TSilObject }

constructor TSilObject.CreateNew(const Owner: IUnknown; Param: Pointer);
begin
  inherited Create;
end;

constructor TSilObject.Create;
begin
  CreateNew();
end;

destructor TSilObject.Destroy;
begin
  FExtension := nil;
  inherited;
end;

class function TSilObject.NewInstance: TObject;
begin
  Result := inherited NewInstance;
  TSilObject(Result).FRefCount := 1;
  TSilObject(Result).BeforeConstruction;
end;

procedure TSilObject.FreeInstance;
var
  iRefCount: Integer;
  sClassName: String;
begin
  AfterDestruction;
  iRefCount := FRefCount;
  sClassName := ClassName;
  inherited;
  if iRefCount <> 1 then
    ASSERT(False, Str.Format('%s: quedaron %d referencias a un objeto destruido', [sClassName, iRefCount - 1]));
end;

procedure TSilObject.BeforeConstruction;
begin
end;

procedure TSilObject.AfterDestruction;
begin
end;

procedure TSilObject.AfterConstruction;
begin
  SilOfLocked.InterlockedDecrement(FRefCount);
end;

procedure TSilObject.BeforeDestruction;
begin
  ASSERT(FRefCount = 0, ClassName + ': RefCount <> 0');
  FRefCount := 1;
end;

function TSilObject.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if Guid.IsEqual(IID, IUnknown) then
  begin
    GetInterface(IID, Obj);
    Result := S_OK;
  end else
  begin
    Result := E_NOINTERFACE;

    if Assigned(FExtension) then
      Result := FExtension.QueryInterface(IID, Obj);

    if (Result <> S_OK) and GetInterface(IID, Obj) then
      Result := S_OK;
  end;
end;

function TSilObject._AddRef: Integer;
begin
  Result := SilOfLocked.InterlockedIncrement(FRefCount);
end;

function TSilObject._Release: Integer;
begin
  Result := SilOfLocked.InterlockedDecrement(FRefCount);
  if Result = 0 then
    Destroy;
end;

procedure TSilObject.Release;
begin
  if FRefCount > 0 then
    _Release else
    Destroy;
end;

procedure TSilObject.Free;
begin
  if Assigned(Self) then
    Release;
end;

function TSilObject.GetInstance: Pointer;
begin
  Result := Self;
end;

function TSilObject.GetHasExtension: Boolean;
begin
  Result := Assigned(FExtension);
end;

function TSilObject.GetExtension: IUnknown;
begin
  Result := FExtension;
end;

procedure TSilObject.SetExtension(const Value: IInterface);
begin
  FExtension := Value;
end;

function TSilObject.Locked: ILock;
begin
  Result := Lock();
end;

function TSilObject.Lock: ILock;
begin
  if IsLockable then
    Result := TSilLockedSection.Create(Lockable) else
  begin
    if DummyLock = nil then
      DummyLock := TSilDummyLock.Create;

    Result := DummyLock; // fake ILock instead of 'nil'
  end;
end;

function TSilObject.GetIsLockable: Boolean;
begin
  Result := False; // To speed up the most common case
end;

procedure TSilObject.SetIsLockable(const Value: Boolean);
begin
end;

function TSilObject.GetLockable: ILockable;
begin
  GetInterface(ILockable, Result);
end;

procedure TSilObject.SetLockable(const Value: ILockable);
begin
end;

function TSilObject.GetInstanceName: string;
begin
  Result := Int.ToHex(Integer(Self), 8);
end;

function TSilObject.GetRefCount: Integer;
begin
  Result := FRefCount;
end;

function TSilObject.GetMainInterface: IUnknown;
begin
  Result := IUnknown(Self);
end;

function TSilObject.GetILockingSet: ILockingSet;
begin
  GetInterface(ILockingSet, Result);
end;

function TSilObject.GetISynchronizable: ISynchronizable;
begin
  GetInterface(ISynchronizable, Result);
end;

function TSilObject.GetIExtensible: IExtensible;
begin
  GetInterface(IExtensible, Result);
end;

end.
