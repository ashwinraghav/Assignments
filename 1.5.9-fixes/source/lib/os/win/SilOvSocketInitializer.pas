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

unit SilOvSocketInitializer;

{$I Defines.inc}

interface

uses
  SilLiGlobalServices;

procedure Get(Reference: PUnknown; const IID: TGUID; Instance: LongWord);
procedure Release(Reference: PUnknown; Instance: LongWord);

const
  GsSocketInitializer: TGUID = '{112472E0-7138-4649-8F7A-D01EDA6F931E}';
      
implementation

uses
  SilLtGlobal,
  SilOmSocketInitializer;

function ServiceCreate(Self: PGlobalService; const Services: IGlobalServiceListV2): IUnknown;
begin
  Result := TSilSocketInitializer.Create();
end;

const
  CService = 'SocketInitializer';
  SService: string[length(CService)] = CService;
  MService: RGlobalService =
    ( ID: @GsSocketInitializer;
      Name: @SService;
      Create: ServiceCreate; );


procedure Get(Reference: PUnknown; const IID: TGUID; Instance: LongWord);
begin
  Global.List.Get(@MService, IID, Reference, Instance);
end;

procedure Release(Reference: PUnknown; Instance: LongWord);
begin
  if Assigned(Reference) and Assigned(Reference^) then
    Global.List.Release(@MService, Reference, Instance);
end;

end.
