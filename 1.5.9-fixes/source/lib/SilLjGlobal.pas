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

unit SilLjGlobal;

{$I Defines.inc}

interface

uses
  SilBkTool,
  SilLiGlobalServices;

type
  ServiceList = class(Tool)
    class function GetServices(const IID: TGUID; out Obj): Integer; virtual; abstract;
    class function ServicesV1: IGlobalServicesV1;
    class function ServicesV2: IGlobalServicesV2;
    class function ListV2: IGlobalServiceListV2;
    class function Services: IGlobalServices; {$IFDEF USE_LIBRARY} library; {$ENDIF}
    class function List: IGlobalServiceList; {$IFDEF USE_LIBRARY} library; {$ENDIF}
  end;

implementation

uses
  SilOtTool;

{ ServiceList }

class function ServiceList.Services: IGlobalServices;
begin
  Os.Error.Check(GetServices(IGlobalServices, Result));
end;

class function ServiceList.List: IGlobalServiceList;
begin
  Os.Error.Check(GetServices(IGlobalServiceList, Result));
end;

class function ServiceList.ServicesV1: IGlobalServicesV1;
begin
  Os.Error.Check(GetServices(IGlobalServicesV1, Result));
end;

class function ServiceList.ServicesV2: IGlobalServicesV2;
begin
  Os.Error.Check(GetServices(IGlobalServicesV2, Result));
end;

class function ServiceList.ListV2: IGlobalServiceListV2;
begin
  Os.Error.Check(GetServices(IGlobalServiceListV2, Result));
end;

end.
