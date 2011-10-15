{********************************************************************************
 *                  Standard Interface Library (SIL)                            *
 *                                                                              *
 *       General purpose library whose design is based in STRONG                *
 *   use of interfaces.                                                         *
 *                                                   Z                           *
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

unit SilStObjectFactory;

{$I Defines.inc}

interface

uses
  Sil,
  SilSiSharedObject,
  SilSkSharedFactory;

type
  ObjectFactory = class(ClassFactory)
    class function Create(const Factory: ISharedFactory; ClassType: TClass; const Owner: IUnknown = nil; const Controller: IUnknown = nil; Param: Pointer = nil): IUnknown; override;      
  end;

implementation

{ ObjectFactory }

class function ObjectFactory.Create(const Factory: ISharedFactory; ClassType: TClass; const Owner, Controller: IInterface; Param: Pointer): IUnknown;
begin
  Result := TSilObjectClass(ClassType).CreateNew(Owner, Param);
end;

end.
 