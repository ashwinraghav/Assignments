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

unit SilOjMessenger;

{$I Defines.inc}

interface

uses
  SilBkTool,
  SilLiReference,
  SilLiGlobalServices,
  SilOiHandle,
  SilOiMessenger;
                          
type
  SilMessenger = class(GlobalService)
    class function ID: TGuid; override; 
    class function Create: IUnknown; overload; override; 
    class function Create(const Owner: IUnknown): IMessenger; reintroduce; overload; 
    class function Create(const Handle: IHandle; const Owner: IUnknown): IMessenger; reintroduce; overload; virtual; abstract; 
    class function Global: IMessenger;    
  end;

implementation

uses
  SilLtGlobal,
  SilOtTool;

const
  CMessenger: TGUID = '{22359540-A506-4E9C-88BE-F1C3186F5F9B}';

var
  MGlobal: IMessenger = nil;

{ SilMessenger }

class function SilMessenger.ID: TGuid;
begin
  Result := CMessenger;
end;

class function SilMessenger.Create: IUnknown;
var
  Obj: IUnknown;
begin
  Obj := nil;
  Result := Create(Obj);
end;

class function SilMessenger.Create(const Owner: IUnknown): IMessenger;
var
  Dummy: IHandle;
begin
  Result := Create(Dummy, Owner);
end;

class function SilMessenger.Global: IMessenger;
begin
  if not Assigned(MGlobal) then
    SilLtGlobal.Global.Services.Get(Self, IMessenger, @MGlobal);
  Result := MGlobal;
end;

initialization

finalization
  if Assigned(MGlobal) then
    SilLtGlobal.Global.Services.Release(SilMessenger, @MGlobal);

end.
