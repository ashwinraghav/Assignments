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

unit SilLkAggregable;

{$I Defines.inc}

interface

uses
  SilBeTypes,
  SilLiObject,
  SilLiControlled,
  SilLkObject;

type
  TSilAggregableClass = class of TSilAggregableObject;
  TSilAggregableObject = class(
    TSilObject,
    IUnknown,
    IObject,
    IControlled )
  private
    FController: Pointer;
  protected // IControlled
    function GetIsControlled: Boolean;
    function GetController: IUnknown;
    function GetMainInterface: IUnknown; override; 
  protected // local IUnknown
    function IUnknown.QueryInterface = ObjQueryInterface;
    function IUnknown._AddRef = ObjAddRef;
    function IUnknown._Release = ObjRelease;
  protected // IUnknown methods
    function ObjQueryInterface(const IID: TGUID; out Obj): HResult; virtual; stdcall;
    function ObjAddRef: Integer; virtual; stdcall;
    function ObjRelease: Integer; virtual; stdcall;
  protected
    function GetMainController(const Controller: IUnknown): IUnknown;
  protected
    property Controller: IUnknown read GetController;
  public
    constructor Create(const Controller: IUnknown = nil; const Owner: IUnknown = nil; Param: Pointer = nil); reintroduce; virtual;
    destructor Destroy; override;
  public // public IUnknown
    function QueryInterface(const IID: TGUID; out Obj): HResult; override;
    function _AddRef: Integer; override;
    function _Release: Integer; override;
  public
    procedure Release; override;
  end;

implementation

{ TSilAggregableObject }

constructor TSilAggregableObject.Create(const Controller: IUnknown; const Owner: IUnknown; Param: Pointer);
begin
  inherited CreateNew(Owner, Param);
  FController := Pointer(Controller);
end;

destructor TSilAggregableObject.Destroy;
begin
  FController := nil;
  inherited;
end;

function TSilAggregableObject.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if Assigned(FController) then
    Result := IUnknown(FController).QueryInterface(IID, Obj) else
    Result := ObjQueryInterface(IID, Obj);
end;

function TSilAggregableObject._AddRef: Integer;
begin
  if Assigned(FController) then
    Result := IUnknown(FController)._AddRef else
    Result := ObjAddRef;
end;

function TSilAggregableObject._Release: Integer;
begin
  if Assigned(FController) then
    Result := IUnknown(FController)._Release else
    Result := ObjRelease;
end;

procedure TSilAggregableObject.Release;
begin
  ObjRelease;
end;

function TSilAggregableObject.GetIsControlled: Boolean;
begin
  Result := Assigned(FController);
end;

function TSilAggregableObject.GetController: IUnknown;
begin
  Result := IUnknown(FController);
end;

function TSilAggregableObject.GetMainInterface: IUnknown;
begin
  if Assigned(FController) then
    Result := IUnknown(FController) else
    Result := inherited GetMainInterface;
end;

function TSilAggregableObject.ObjQueryInterface(const IID: TGUID; out Obj): HResult;
begin
  Result := inherited QueryInterface(IID, Obj);
end;

function TSilAggregableObject.ObjAddRef: Integer;
begin
  Result := inherited _AddRef;
end;

function TSilAggregableObject.ObjRelease: Integer;
begin
  Result := inherited _Release;
end;

function TSilAggregableObject.GetMainController(const Controller: IInterface): IUnknown;
begin
  if not Assigned(Controller) then
    Result := MainInterface else
    Result := Controller;
end;

end.
