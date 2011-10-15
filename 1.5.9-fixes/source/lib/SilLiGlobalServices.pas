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

unit SilLiGlobalServices;

{$I Defines.inc}

interface

uses
  SilLiGlobalServicesV1,
  SilLiGlobalServicesV2;

type
  GlobalServiceType           = SilLiGlobalServicesV1.GlobalServiceType;
  GlobalService               = SilLiGlobalServicesV1.GlobalService;
  IGlobalServicesV1           = SilLiGlobalServicesV1.IGlobalServicesV1;
  PServiceRefV1               = SilLiGlobalServicesV1.PServiceRefV1;
  RServiceRefV1               = SilLiGlobalServicesV1.RServiceRefV1;

type
  PGlobalService              = SilLiGlobalServicesV2.PGlobalService;
  RGlobalService              = SilLiGlobalServicesV2.RGlobalService;
  IGlobalServicesV2           = SilLiGlobalServicesV2.IGlobalServicesV2;
  IGlobalServiceListV2        = SilLiGlobalServicesV2.IGlobalServiceListV2;
  IGlobalListV2               = SilLiGlobalServicesV2.IGlobalListV2;
  IGlobalServiceV2            = SilLiGlobalServicesV2.IGlobalServiceV2;
  IGlobalReferencesV2         = SilLiGlobalServicesV2.IGlobalReferencesV2;
  IGlobalServicesHookV2       = SilLiGlobalServicesV2.IGlobalServicesHookV2;
  IGlobalLinksV2              = SilLiGlobalServicesV2.IGlobalLinksV2;
  PGlobalRef                  = SilLiGlobalServicesV2.PGlobalRef;
  RGlobalRef                  = SilLiGlobalServicesV2.RGlobalRef;
  TGlobalServiceKind          = SilLiGlobalServicesV2.TGlobalServiceKind;

const
  skGlobal                    = TGlobalServiceKind(SilLiGlobalServicesV2.skGlobal);
  skLocal                     = TGlobalServiceKind(SilLiGlobalServicesV2.skLocal);

type
  IGlobalServices             = IGlobalServicesV1;
  PServiceRef                 = PServiceRefV1;
  RServiceRef                 = RServiceRefV1;

type
  IGlobalServiceList          = IGlobalServiceListV2;
  IGlobalList                 = IGlobalListV2;
  IGlobalService              = IGlobalServiceV2;
  IGlobalReferences           = IGlobalReferencesV2;
  IGlobalServicesHook         = IGlobalServicesHookV2;
  IGlobalLinks                = IGlobalLinksV2;            

implementation
end.
