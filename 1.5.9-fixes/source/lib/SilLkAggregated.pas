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

unit SilLkAggregated;

{$I Defines.inc}

interface

uses
  SilLiControlled,
  SilLkInterfaced;

type
  TSilAggregatedClass = class of TSilAggregatedObject;
  TSilAggregatedObject = class(
  // extends
    TSilInterfacedObject,
  // implements
    IUnknown,
    IControlled )
  private
    FController: Pointer;
  protected // IControlled
    function GetIsControlled: Boolean;
    function GetController: IUnknown;
    function GetMainInterface: IUnknown; override; 
  protected 
    property Controller: IUnknown read GetController;
    property MainInterface: IUnknown read GetMainInterface;
  public
    procedure Release; override;
  public // IUnknown
    function IUnknown.QueryInterface = ObjQueryInterface;
    function IUnknown._AddRef = ObjAddRef;
    function IUnknown._Release = ObjRelease;
    function QueryInterface(const IID: TGUID; out Obj): HResult; override;
    function _AddRef: Integer; override;
    function _Release: Integer; override;
  public
    function ObjQueryInterface(const IID: TGUID; out Obj): HResult; virtual; stdcall;
    function ObjAddRef: Integer; virtual; stdcall;
    function ObjRelease: Integer; virtual; stdcall;
  public
    constructor CreateAggregated(const Controller: IUnknown); virtual;
    constructor Create(const Controller: IUnknown = nil; Locked: Boolean = False; const Owner: IUnknown = nil; Param: Pointer = nil); reintroduce; virtual;   
    destructor Destroy; override;
  end;

  TAggregatedObject = class(TSilAggregatedObject) end {$IFDEF USE_DEPRECATED} deprecated {$ENDIF};

implementation

{ TSilAggregatedObject }

constructor TSilAggregatedObject.Create(const Controller: IUnknown; Locked: Boolean; const Owner: IUnknown; Param: Pointer);
begin
  inherited Create(Locked, Owner, Param);
  FController := Pointer(Controller);
end;

constructor TSilAggregatedObject.CreateAggregated(const Controller: IUnknown);
begin
  Create(Controller);
end;

destructor TSilAggregatedObject.Destroy;
begin
  FController := nil;
  inherited;
end;

function TSilAggregatedObject.ObjQueryInterface(const IID: TGUID; out Obj): HResult;
begin
  Result := inherited QueryInterface(IID, Obj);
end;

function TSilAggregatedObject.ObjAddRef: Integer;
begin
  Result := inherited _AddRef;
end;

function TSilAggregatedObject.ObjRelease: Integer;
begin
  Result := inherited _Release;
end;

function TSilAggregatedObject.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if (FController <> nil) {and not Guid.IsEqual(IID, IUnknown)} then
    Result := IUnknown(FController).QueryInterface(IID, Obj) else
    Result := ObjQueryInterface(IID, Obj);
end;

function TSilAggregatedObject._AddRef: Integer;
begin
  if FController <> nil then
    Result := IUnknown(FController)._AddRef else
    Result := ObjAddRef;
end;

function TSilAggregatedObject._Release: Integer;
begin
  if FController <> nil then
    Result := IUnknown(FController)._Release else
    Result := ObjRelease;
end;

function TSilAggregatedObject.GetIsControlled: Boolean;
begin
  Result := Assigned(FController);
end;

function TSilAggregatedObject.GetController: IUnknown;
begin
  Result := IUnknown(FController);
end;

function TSilAggregatedObject.GetMainInterface: IUnknown;
begin
  if Assigned(FController) then
    Result := IUnknown(FController) else
    Result := Self;
end;

procedure TSilAggregatedObject.Release;
begin
  ObjRelease;
end;

end.
