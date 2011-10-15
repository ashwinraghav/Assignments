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

unit SilOfWntNetapi32;

{$I Defines.inc}

interface

uses
  SilOhWntNetapi32;

function DoErrorMessage(Status: TNetbiosStatus): string; 

implementation

uses
  SilOdWntNetapi32;

function DoErrorMessage(Status: TNetbiosStatus): string;
begin
  case Status of
    NRC_GOODRET:      Result := S_NRC_GOODRET     ;
    NRC_BUFLEN:       Result := S_NRC_BUFLEN      ;
    NRC_ILLCMD:       Result := S_NRC_ILLCMD      ;
    NRC_CMDTMO:       Result := S_NRC_CMDTMO      ;
    NRC_INCOMP:       Result := S_NRC_INCOMP      ;
    NRC_BADDR:        Result := S_NRC_BADDR       ;
    NRC_SNUMOUT:      Result := S_NRC_SNUMOUT     ;
    NRC_NORES:        Result := S_NRC_NORES       ;
    NRC_SCLOSED:      Result := S_NRC_SCLOSED     ;
    NRC_CMDCAN:       Result := S_NRC_CMDCAN      ;
    NRC_DUPNAME:      Result := S_NRC_DUPNAME     ;
    NRC_NAMTFUL:      Result := S_NRC_NAMTFUL     ;
    NRC_ACTSES:       Result := S_NRC_ACTSES      ;
    NRC_LOCTFUL:      Result := S_NRC_LOCTFUL     ;
    NRC_REMTFUL:      Result := S_NRC_REMTFUL     ;
    NRC_ILLNN:        Result := S_NRC_ILLNN       ;
    NRC_NOCALL:       Result := S_NRC_NOCALL      ;
    NRC_NOWILD:       Result := S_NRC_NOWILD      ;
    NRC_INUSE:        Result := S_NRC_INUSE       ;
    NRC_NAMERR:       Result := S_NRC_NAMERR      ;
    NRC_SABORT:       Result := S_NRC_SABORT      ;
    NRC_NAMCONF:      Result := S_NRC_NAMCONF     ;
    NRC_IFBUSY:       Result := S_NRC_IFBUSY      ;
    NRC_TOOMANY:      Result := S_NRC_TOOMANY     ;
    NRC_BRIDGE:       Result := S_NRC_BRIDGE      ;
    NRC_CANOCCR:      Result := S_NRC_CANOCCR     ;
    NRC_CANCEL:       Result := S_NRC_CANCEL      ;
    NRC_DUPENV:       Result := S_NRC_DUPENV      ;
    NRC_ENVNOTDEF:    Result := S_NRC_ENVNOTDEF   ;
    NRC_OSRESNOTAV:   Result := S_NRC_OSRESNOTAV  ;
    NRC_MAXAPPS:      Result := S_NRC_MAXAPPS     ;
    NRC_NOSAPS:       Result := S_NRC_NOSAPS      ;
    NRC_NORESOURCES:  Result := S_NRC_NORESOURCES ;
    NRC_INVADDRESS:   Result := S_NRC_INVADDRESS  ;
    NRC_INVDDID:      Result := S_NRC_INVDDID     ;
    NRC_LOCKFAIL:     Result := S_NRC_LOCKFAIL    ;
    NRC_OPENERR:      Result := S_NRC_OPENERR     ;
    NRC_SYSTEM:       Result := S_NRC_SYSTEM      ;
    NRC_PENDING:      Result := S_NRC_PENDING     ;
    else              Result := 'unknown';
  end;
end; 

end.
 