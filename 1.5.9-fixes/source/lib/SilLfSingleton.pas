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

unit SilLfSingleton;

{$I Defines.inc}

interface

uses
  SilLkSingleton;

function List: ISingletonList;

implementation

uses
  SilLtGlobal,
  SilLmSingletonList; 

const
  CSingletonObject: TGUID = '{EB5798F2-E3DC-4FE9-B191-F3A60EAB5956}';

type
  Singleton = class(GlobalService)
    class function ID: TGuid; override; 
    class function Create: IUnknown; override; 
  end;
  
var
  MList: ISingletonList = nil;

function List: ISingletonList;
begin
  if not Assigned(MList) then
    Global.Services.Get(Singleton, ISingletonList, @MList);
end;

{ Singleton }

class function Singleton.Create: IUnknown;
begin
  Result := TSingletonList.Create(True);
end;

class function Singleton.ID: TGuid;
begin
  Result := CSingletonObject;
end;

end.
 