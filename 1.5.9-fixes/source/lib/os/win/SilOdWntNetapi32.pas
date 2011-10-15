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

unit SilOdWntNetapi32;

{$I Defines.inc}

interface

resourcestring
  S_NRC_GOODRET     = 'good return';
  S_NRC_BUFLEN      = 'illegal buffer length'; 
  S_NRC_ILLCMD      = 'illegal command'; 
  S_NRC_CMDTMO      = 'command timed out'; 
  S_NRC_INCOMP      = 'message incomplete, issue another command'; 
  S_NRC_BADDR       = 'illegal buffer address'; 
  S_NRC_SNUMOUT     = 'session number out of range'; 
  S_NRC_NORES       = 'no resource available'; 
  S_NRC_SCLOSED     = 'session closed'; 
  S_NRC_CMDCAN      = 'command cancelled'; 
  S_NRC_DUPNAME     = 'duplicate name'; 
  S_NRC_NAMTFUL     = 'name table full'; 
  S_NRC_ACTSES      = 'no deletions, name has active sessions'; 
  S_NRC_LOCTFUL     = 'local session table full'; 
  S_NRC_REMTFUL     = 'remote session table full'; 
  S_NRC_ILLNN       = 'illegal name number'; 
  S_NRC_NOCALL      = 'no callname'; 
  S_NRC_NOWILD      = 'cannot put * in NCB_NAME'; 
  S_NRC_INUSE       = 'name in use on remote adapter'; 
  S_NRC_NAMERR      = 'name deleted'; 
  S_NRC_SABORT      = 'session ended abnormally'; 
  S_NRC_NAMCONF     = 'name conflict detected'; 
  S_NRC_IFBUSY      = 'interface busy, IRET before retrying'; 
  S_NRC_TOOMANY     = 'too many commands outstanding, retry later'; 
  S_NRC_BRIDGE      = 'NCB_lana_num field invalid'; 
  S_NRC_CANOCCR     = 'command completed while cancel occurring'; 
  S_NRC_CANCEL      = 'command not valid to cancel'; 
  S_NRC_DUPENV      = 'name defined by anther local process'; 
  S_NRC_ENVNOTDEF   = 'environment undefined. RESET required'; 
  S_NRC_OSRESNOTAV  = 'required OS resources exhausted'; 
  S_NRC_MAXAPPS     = 'max number of applications exceeded'; 
  S_NRC_NOSAPS      = 'no saps available for netbios'; 
  S_NRC_NORESOURCES = 'requested resources are not available'; 
  S_NRC_INVADDRESS  = 'invalid ncb address or length > segment'; 
  S_NRC_INVDDID     = 'invalid NCB DDID'; 
  S_NRC_LOCKFAIL    = 'lock of user area failed'; 
  S_NRC_OPENERR     = 'NETBIOS not loaded'; 
  S_NRC_SYSTEM      = 'system error'; 
  S_NRC_PENDING     = 'asynchronous command is not yet finished'; 

implementation

end.
 