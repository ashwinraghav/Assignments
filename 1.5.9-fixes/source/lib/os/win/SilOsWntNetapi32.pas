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

unit SilOsWntNetapi32;

{$I Defines.inc}

interface

uses
  SilOhWntNetapi32,
  SilOiWntNetapi32, 
  SilOtWntNetapi32;

const
  CNetapi32                                 = SilOhWntNetapi32.CNetapi32;
  NERR_SUCCESS                              = SilOhWntNetapi32.NERR_SUCCESS ;
  NERR_BASE                                 = SilOhWntNetapi32.NERR_BASE    ;
  MAX_PREF_MAXLENGTH                        = SilOhWntNetapi32.MAX_PREF_MAXLENGTH ;
  TIMEQ_FOREVER                             = SilOhWntNetapi32.TIMEQ_FOREVER      ;

const
  NCBCALL                                   = SilOhWntNetapi32.NCBCALL;
  NCBLISTEN                                 = SilOhWntNetapi32.NCBLISTEN;
  NCBHANGUP                                 = SilOhWntNetapi32.NCBHANGUP;
  NCBSEND                                   = SilOhWntNetapi32.NCBSEND;
  NCBRECV                                   = SilOhWntNetapi32.NCBRECV;
  NCBRECVANY                                = SilOhWntNetapi32.NCBRECVANY;
  NCBCHAINSEND                              = SilOhWntNetapi32.NCBCHAINSEND;
  NCBDGSEND                                 = SilOhWntNetapi32.NCBDGSEND     ;            
  NCBDGRECV                                 = SilOhWntNetapi32.NCBDGRECV     ;            
  NCBDGSENDBC                               = SilOhWntNetapi32.NCBDGSENDBC   ;            
  NCBDGRECVBC                               = SilOhWntNetapi32.NCBDGRECVBC   ;            
  NCBADDNAME                                = SilOhWntNetapi32.NCBADDNAME    ;            
  NCBDELNAME                                = SilOhWntNetapi32.NCBDELNAME    ;            
  NCBRESET                                  = SilOhWntNetapi32.NCBRESET      ;            
  NCBASTAT                                  = SilOhWntNetapi32.NCBASTAT      ;            
  NCBSSTAT                                  = SilOhWntNetapi32.NCBSSTAT      ;            
  NCBCANCEL                                 = SilOhWntNetapi32.NCBCANCEL     ;            
  NCBADDGRNAME                              = SilOhWntNetapi32.NCBADDGRNAME  ;            
  NCBENUM                                   = SilOhWntNetapi32.NCBENUM       ;            
  NCBUNLINK                                 = SilOhWntNetapi32.NCBUNLINK     ;            
  NCBSENDNA                                 = SilOhWntNetapi32.NCBSENDNA     ;            
  NCBCHAINSENDNA                            = SilOhWntNetapi32.NCBCHAINSENDNA;            
  NCBLANSTALERT                             = SilOhWntNetapi32.NCBLANSTALERT ;            
  NCBACTION                                 = SilOhWntNetapi32.NCBACTION     ;            
  NCBFINDNAME                               = SilOhWntNetapi32.NCBFINDNAME   ;            
  NCBTRACE                                  = SilOhWntNetapi32.NCBTRACE      ;            

const
  NRC_GOODRET                               = SilOhWntNetapi32.NRC_GOODRET;
  NRC_BUFLEN                                = SilOhWntNetapi32.NRC_BUFLEN;
  NRC_ILLCMD                                = SilOhWntNetapi32.NRC_ILLCMD;
  NRC_CMDTMO                                = SilOhWntNetapi32.NRC_CMDTMO;
  NRC_INCOMP                                = SilOhWntNetapi32.NRC_INCOMP;
  NRC_BADDR                                 = SilOhWntNetapi32.NRC_BADDR;
  NRC_SNUMOUT                               = SilOhWntNetapi32.NRC_SNUMOUT;
  NRC_NORES                                 = SilOhWntNetapi32.NRC_NORES;
  NRC_SCLOSED                               = SilOhWntNetapi32.NRC_SCLOSED;
  NRC_CMDCAN                                = SilOhWntNetapi32.NRC_CMDCAN;
  NRC_DUPNAME                               = SilOhWntNetapi32.NRC_DUPNAME;
  NRC_NAMTFUL                               = SilOhWntNetapi32.NRC_NAMTFUL;
  NRC_ACTSES                                = SilOhWntNetapi32.NRC_ACTSES;
  NRC_LOCTFUL                               = SilOhWntNetapi32.NRC_LOCTFUL;
  NRC_REMTFUL                               = SilOhWntNetapi32.NRC_REMTFUL;
  NRC_ILLNN                                 = SilOhWntNetapi32.NRC_ILLNN;
  NRC_NOCALL                                = SilOhWntNetapi32.NRC_NOCALL;
  NRC_NOWILD                                = SilOhWntNetapi32.NRC_NOWILD;
  NRC_INUSE                                 = SilOhWntNetapi32.NRC_INUSE;
  NRC_NAMERR                                = SilOhWntNetapi32.NRC_NAMERR;
  NRC_SABORT                                = SilOhWntNetapi32.NRC_SABORT;
  NRC_NAMCONF                               = SilOhWntNetapi32.NRC_NAMCONF;
  NRC_IFBUSY                                = SilOhWntNetapi32.NRC_IFBUSY;
  NRC_TOOMANY                               = SilOhWntNetapi32.NRC_TOOMANY;
  NRC_BRIDGE                                = SilOhWntNetapi32.NRC_BRIDGE;
  NRC_CANOCCR                               = SilOhWntNetapi32.NRC_CANOCCR;
  NRC_CANCEL                                = SilOhWntNetapi32.NRC_CANCEL;
  NRC_DUPENV                                = SilOhWntNetapi32.NRC_DUPENV;
  NRC_ENVNOTDEF                             = SilOhWntNetapi32.NRC_ENVNOTDEF;
  NRC_OSRESNOTAV                            = SilOhWntNetapi32.NRC_OSRESNOTAV;    
  NRC_MAXAPPS                               = SilOhWntNetapi32.NRC_MAXAPPS;    
  NRC_NOSAPS                                = SilOhWntNetapi32.NRC_NOSAPS;
  NRC_NORESOURCES                           = SilOhWntNetapi32.NRC_NORESOURCES;    
  NRC_INVADDRESS                            = SilOhWntNetapi32.NRC_INVADDRESS;
  NRC_INVDDID                               = SilOhWntNetapi32.NRC_INVDDID;
  NRC_LOCKFAIL                              = SilOhWntNetapi32.NRC_LOCKFAIL;
  NRC_OPENERR                               = SilOhWntNetapi32.NRC_OPENERR;
  NRC_SYSTEM                                = SilOhWntNetapi32.NRC_SYSTEM;
  NRC_PENDING                               = SilOhWntNetapi32.NRC_PENDING;

type
  ENetapiError                              = SilOhWntNetapi32.ENetapiError;
  TNetapiStatus                             = SilOhWntNetapi32.TNetapiStatus;
                                            
type
  PNCB                                      = SilOhWntNetapi32.PNCB;
  TNCB                                      = SilOhWntNetapi32.TNCB;
  PNetLanaEnum                              = SilOhWntNetapi32.PNetLanaEnum;
  TNetLanaEnum                              = SilOhWntNetapi32.TNetLanaEnum;
  PNetAdapterStatus                         = SilOhWntNetapi32.PNetAdapterStatus;
  TNetAdapterStatus                         = SilOhWntNetapi32.TNetAdapterStatus;
  PNetUserInfo0                             = SilOhWntNetapi32.PNetUserInfo0;
  TNetUserInfo0                             = SilOhWntNetapi32.TNetUserInfo0;                 
  PNetUserInfo2                             = SilOhWntNetapi32.PNetUserInfo2;                 
  TNetUserInfo2                             = SilOhWntNetapi32.TNetUserInfo2;                 
  PNetUserInfo3                             = SilOhWntNetapi32.PNetUserInfo3;                 
  TNetUserInfo3                             = SilOhWntNetapi32.TNetUserInfo3;                 
  PNetUserInfo10                            = SilOhWntNetapi32.PNetUserInfo10;
  TNetUserInfo10                            = SilOhWntNetapi32.TNetUserInfo10;
  PNetServerInfo100                         = SilOhWntNetapi32.PNetServerInfo100;
  TNetServerInfo100                         = SilOhWntNetapi32.TNetServerInfo100;             
  PNetServerInfo101                         = SilOhWntNetapi32.PNetServerInfo101;             
  TNetServerInfo101                         = SilOhWntNetapi32.TNetServerInfo101;             
  PNetServerInfo102                         = SilOhWntNetapi32.PNetServerInfo102;             
  TNetServerInfo102                         = SilOhWntNetapi32.TNetServerInfo102;             
  PNetDisplayMachine                        = SilOhWntNetapi32.PNetDisplayMachine;            
  TNetDisplayMachine                        = SilOhWntNetapi32.TNetDisplayMachine;
  PNetDisplayUser                           = SilOhWntNetapi32.PNetDisplayUser;
  TNetDisplayUser                           = SilOhWntNetapi32.TNetDisplayUser;

type
	PNetUserModalsInfo0 											= SilOhWntNetapi32.PNetUserModalsInfo0;
  TNetUserModalsInfo0												= SilOhWntNetapi32.TNetUserModalsInfo0;
	PNetUserModalsInfo1 											= SilOhWntNetapi32.PNetUserModalsInfo1;
  TNetUserModalsInfo1												= SilOhWntNetapi32.TNetUserModalsInfo1;
	PNetUserModalsInfo2 											= SilOhWntNetapi32.PNetUserModalsInfo2;
  TNetUserModalsInfo2												= SilOhWntNetapi32.TNetUserModalsInfo2;
	PNetUserModalsInfo3 											= SilOhWntNetapi32.PNetUserModalsInfo3;
  TNetUserModalsInfo3												= SilOhWntNetapi32.TNetUserModalsInfo3;

type
  TNetServerTypeEnum                        = SilOhWntNetapi32.TNetServerTypeEnum;

const
  NET_SERVER_WORKSTATION                    = SilOhWntNetapi32.NET_SERVER_WORKSTATION        ;
  NET_SERVER_SERVER                         = SilOhWntNetapi32.NET_SERVER_SERVER             ;
  NET_SERVER_SQLSERVER                      = SilOhWntNetapi32.NET_SERVER_SQLSERVER          ;
  NET_SERVER_DOMAIN_CTRL                    = SilOhWntNetapi32.NET_SERVER_DOMAIN_CTRL        ;
  NET_SERVER_DOMAIN_BAKCTRL                 = SilOhWntNetapi32.NET_SERVER_DOMAIN_BAKCTRL     ;
  NET_SERVER_TIME_SOURCE                    = SilOhWntNetapi32.NET_SERVER_TIME_SOURCE        ;
  NET_SERVER_AFP                            = SilOhWntNetapi32.NET_SERVER_AFP                ;
  NET_SERVER_NOVELL                         = SilOhWntNetapi32.NET_SERVER_NOVELL             ;
  NET_SERVER_DOMAIN_MEMBER                  = SilOhWntNetapi32.NET_SERVER_DOMAIN_MEMBER      ;
  NET_SERVER_PRINTQ_SERVER                  = SilOhWntNetapi32.NET_SERVER_PRINTQ_SERVER      ;
  NET_SERVER_DIALIN_SERVER                  = SilOhWntNetapi32.NET_SERVER_DIALIN_SERVER      ;
  NET_SERVER_XENIX_SERVER                   = SilOhWntNetapi32.NET_SERVER_XENIX_SERVER       ;
  NET_SERVER_NT                             = SilOhWntNetapi32.NET_SERVER_NT                 ;
  NET_SERVER_WFW                            = SilOhWntNetapi32.NET_SERVER_WFW                ;
  NET_SERVER_SERVER_MFPN                    = SilOhWntNetapi32.NET_SERVER_SERVER_MFPN        ;
  NET_SERVER_SERVER_NT                      = SilOhWntNetapi32.NET_SERVER_SERVER_NT          ;
  NET_SERVER_POTENTIAL_BROWSER              = SilOhWntNetapi32.NET_SERVER_POTENTIAL_BROWSER  ;
  NET_SERVER_BACKUP_BROWSER                 = SilOhWntNetapi32.NET_SERVER_BACKUP_BROWSER     ;
  NET_SERVER_MASTER_BROWSER                 = SilOhWntNetapi32.NET_SERVER_MASTER_BROWSER     ;
  NET_SERVER_DOMAIN_MASTER                  = SilOhWntNetapi32.NET_SERVER_DOMAIN_MASTER      ;
  NET_SERVER_SERVER_OSF                     = SilOhWntNetapi32.NET_SERVER_SERVER_OSF         ;
  NET_SERVER_SERVER_VMS                     = SilOhWntNetapi32.NET_SERVER_SERVER_VMS         ;
  NET_SERVER_WINDOWS                        = SilOhWntNetapi32.NET_SERVER_WINDOWS            ;
  NET_SERVER_DFS                            = SilOhWntNetapi32.NET_SERVER_DFS                ;
  NET_SERVER_CLUSTER_NT                     = SilOhWntNetapi32.NET_SERVER_CLUSTER_NT         ;
  NET_SERVER_TERMINALSERVER                 = SilOhWntNetapi32.NET_SERVER_TERMINALSERVER     ;
  NET_SERVER_CLUSTER_VS_NT                  = SilOhWntNetapi32.NET_SERVER_CLUSTER_VS_NT      ;
  NET_SERVER_DCE                            = SilOhWntNetapi32.NET_SERVER_DCE                ;
  NET_SERVER_ALTERNATE_XPORT                = SilOhWntNetapi32.NET_SERVER_ALTERNATE_XPORT    ;
  NET_SERVER_LOCAL_LIST_ONLY                = SilOhWntNetapi32.NET_SERVER_LOCAL_LIST_ONLY    ;
  NET_SERVER_DOMAIN_ENUM                    = SilOhWntNetapi32.NET_SERVER_DOMAIN_ENUM        ;

type
  TNetServerType                            = SilOhWntNetapi32.TNetServerType;

const
  NET_SERVER_ALL                            = SilOhWntNetapi32.NET_SERVER_ALL;

type
  TNetJoinStatus                            = SilOhWntNetapi32.TNetJoinStatus;

const
  NET_JOIN_UNKNOWNSTATUS                    = SilOhWntNetapi32.NET_JOIN_UNKNOWNSTATUS;
  NET_JOIN_UNJOINED                         = SilOhWntNetapi32.NET_JOIN_UNJOINED;
  NET_JOIN_WORKGROUPNAME                    = SilOhWntNetapi32.NET_JOIN_WORKGROUPNAME;
  NET_JOIN_DOMAINNAME                       = SilOhWntNetapi32.NET_JOIN_DOMAINNAME;

type
  TNetQueryLevel                            = SilOhWntNetapi32.TNetQueryLevel;

const
  NET_QUERY_USERS                           = SilOhWntNetapi32.NET_QUERY_USERS;
  NET_QUERY_SERVERS                         = SilOhWntNetapi32.NET_QUERY_SERVERS;
  NET_QUERY_GLOBALS                         = SilOhWntNetapi32.NET_QUERY_GLOBALS;

type
  TNetAdapters                              = SilOhWntNetapi32.TNetAdapters;

type
  TNetUseStatusEnum                         = SilOhWntNetapi32.TNetUseStatusEnum;

const
  NET_USE_OK                                = TNetUseStatusEnum(SilOhWntNetapi32.NET_USE_OK);      
  NET_USE_PAUSED                            = TNetUseStatusEnum(SilOhWntNetapi32.NET_USE_PAUSED);   
  NET_USE_DISCONN                           = TNetUseStatusEnum(SilOhWntNetapi32.NET_USE_DISCONN);  
  NET_USE_NETERR                            = TNetUseStatusEnum(SilOhWntNetapi32.NET_USE_NETERR);   
  NET_USE_CONN                              = TNetUseStatusEnum(SilOhWntNetapi32.NET_USE_CONN);     
  NET_USE_RECONN                            = TNetUseStatusEnum(SilOhWntNetapi32.NET_USE_RECONN);   

const
  NET_USE_SESSLOST                          = TNetUseStatusEnum(SilOhWntNetapi32.NET_USE_DISCONN);

type
  TNetUseKindEnum                           = SilOhWntNetapi32.TNetUseKindEnum;

const
  NET_USE_DISKDEV                           = TNetUseKindEnum(SilOhWntNetapi32.NET_USE_DISKDEV );        
  NET_USE_SPOOLDEV                          = TNetUseKindEnum(SilOhWntNetapi32.NET_USE_SPOOLDEV); 
  NET_USE_CHARDEV                           = TNetUseKindEnum(SilOhWntNetapi32.NET_USE_CHARDEV ); 
  NET_USE_IPC                               = TNetUseKindEnum(SilOhWntNetapi32.NET_USE_IPC     );

const
  NET_USE_WILDCARD                          = TNetUseKindEnum(SilOhWntNetapi32.NET_USE_WILDCARD);

type
  TNetUseForceKind                          = SilOhWntNetapi32.TNetUseForceKind;

const
  NET_USE_NOFORCE                           = TNetUseForceKind(SilOhWntNetapi32.NET_USE_NOFORCE               );        
  NET_USE_FORCE                             = TNetUseForceKind(SilOhWntNetapi32.NET_USE_FORCE                 ); 
  NET_USE_LOTS_OF_FORCE                     = TNetUseForceKind(SilOhWntNetapi32.NET_USE_LOTS_OF_FORCE         ); 

type
  PNetUseParamError                         = SilOhWntNetapi32.PNetUseParamError;
  TNetUseParamError                         = SilOhWntNetapi32.TNetUseParamError;

const
  NET_USE_NONE                              = TNetUseParamError(SilOhWntNetapi32.NET_USE_NONE                 );
  NET_USE_LOCAL                             = TNetUseParamError(SilOhWntNetapi32.NET_USE_LOCAL                );
  NET_USE_REMOTE                            = TNetUseParamError(SilOhWntNetapi32.NET_USE_REMOTE               );
  NET_USE_PASSWORD                          = TNetUseParamError(SilOhWntNetapi32.NET_USE_PASSWORD             );
  NET_USE_ASGTYPE                           = TNetUseParamError(SilOhWntNetapi32.NET_USE_ASGTYPE              );
  NET_USE_USERNAME                          = TNetUseParamError(SilOhWntNetapi32.NET_USE_USERNAME             );
  NET_USE_DOMAINNAME                        = TNetUseParamError(SilOhWntNetapi32.NET_USE_DOMAINNAME           );

type
  TNetDomainControllerAddressType           = SilOhWntNetapi32.TNetDomainControllerAddressType;

const
  NET_DS_INET_ADDRESS                       = TNetDomainControllerAddressType(SilOhWntNetapi32.NET_DS_INET_ADDRESS   );
  NET_DS_NETBIOS_ADDRESS                    = TNetDomainControllerAddressType(SilOhWntNetapi32.NET_DS_NETBIOS_ADDRESS);

type
  TNetReturnedDomainControllerFlag          = SilOhWntNetapi32.TNetReturnedDomainControllerFlag;

const
  NET_DS_PDC_FLAG                           = TNetReturnedDomainControllerFlag(SilOhWntNetapi32.NET_DS_PDC_FLAG           );
  NET_DS_GC_FLAG                            = TNetReturnedDomainControllerFlag(SilOhWntNetapi32.NET_DS_GC_FLAG            );
  NET_DS_LDAP_FLAG                          = TNetReturnedDomainControllerFlag(SilOhWntNetapi32.NET_DS_LDAP_FLAG          );
  NET_DS_DS_FLAG                            = TNetReturnedDomainControllerFlag(SilOhWntNetapi32.NET_DS_DS_FLAG            );
  NET_DS_KDC_FLAG                           = TNetReturnedDomainControllerFlag(SilOhWntNetapi32.NET_DS_KDC_FLAG           );
  NET_DS_TIMESERV_FLAG                      = TNetReturnedDomainControllerFlag(SilOhWntNetapi32.NET_DS_TIMESERV_FLAG      );
  NET_DS_CLOSEST_FLAG                       = TNetReturnedDomainControllerFlag(SilOhWntNetapi32.NET_DS_CLOSEST_FLAG       );
  NET_DS_WRITABLE_FLAG                      = TNetReturnedDomainControllerFlag(SilOhWntNetapi32.NET_DS_WRITABLE_FLAG      );
  NET_DS_GOOD_TIMESERV_FLAG                 = TNetReturnedDomainControllerFlag(SilOhWntNetapi32.NET_DS_GOOD_TIMESERV_FLAG );
  NET_DS_NDNC_FLAG                          = TNetReturnedDomainControllerFlag(SilOhWntNetapi32.NET_DS_NDNC_FLAG          );
  NET_DS_DNS_CONTROLLER_FLAG                = TNetReturnedDomainControllerFlag(SilOhWntNetapi32.NET_DS_DNS_CONTROLLER_FLAG);
  NET_DS_DNS_DOMAIN_FLAG                    = TNetReturnedDomainControllerFlag(SilOhWntNetapi32.NET_DS_DNS_DOMAIN_FLAG    );
  NET_DS_DNS_FOREST_FLAG                    = TNetReturnedDomainControllerFlag(SilOhWntNetapi32.NET_DS_DNS_FOREST_FLAG    );

type
  TNetReturnedDomainControllerFlags         = SilOhWntNetapi32.TNetReturnedDomainControllerFlags;

type
  PNetDomainControllerInfo                  = SilOhWntNetapi32.PNetDomainControllerInfo;
  TNetDomainControllerInfo                  = SilOhWntNetapi32.TNetDomainControllerInfo;

type
  TNetFindDomainControllerFlag              = SilOhWntNetapi32.TNetFindDomainControllerFlag;

const
  NET_DS_FORCE_REDISCOVERY                  = TNetFindDomainControllerFlag(SilOhWntNetapi32.NET_DS_FORCE_REDISCOVERY                  ); 
  NET_DS_DIRECTORY_SERVICE_REQUIRED         = TNetFindDomainControllerFlag(SilOhWntNetapi32.NET_DS_DIRECTORY_SERVICE_REQUIRED         ); 
  NET_DS_DIRECTORY_SERVICE_PREFERRED        = TNetFindDomainControllerFlag(SilOhWntNetapi32.NET_DS_DIRECTORY_SERVICE_PREFERRED        ); 
  NET_DS_GC_SERVER_REQUIRED                 = TNetFindDomainControllerFlag(SilOhWntNetapi32.NET_DS_GC_SERVER_REQUIRED                 ); 
  NET_DS_PDC_REQUIRED                       = TNetFindDomainControllerFlag(SilOhWntNetapi32.NET_DS_PDC_REQUIRED                       ); 
  NET_DS_BACKGROUND_ONLY                    = TNetFindDomainControllerFlag(SilOhWntNetapi32.NET_DS_BACKGROUND_ONLY                    ); 
  NET_DS_IP_REQUIRED                        = TNetFindDomainControllerFlag(SilOhWntNetapi32.NET_DS_IP_REQUIRED                        ); 
  NET_DS_KDC_REQUIRED                       = TNetFindDomainControllerFlag(SilOhWntNetapi32.NET_DS_KDC_REQUIRED                       ); 
  NET_DS_TIMESERV_REQUIRED                  = TNetFindDomainControllerFlag(SilOhWntNetapi32.NET_DS_TIMESERV_REQUIRED                  ); 
  NET_DS_WRITABLE_REQUIRED                  = TNetFindDomainControllerFlag(SilOhWntNetapi32.NET_DS_WRITABLE_REQUIRED                  ); 
  NET_DS_GOOD_TIMESERV_PREFERRED            = TNetFindDomainControllerFlag(SilOhWntNetapi32.NET_DS_GOOD_TIMESERV_PREFERRED            ); 
  NET_DS_AVOID_SELF                         = TNetFindDomainControllerFlag(SilOhWntNetapi32.NET_DS_AVOID_SELF                         ); 
  NET_DS_ONLY_LDAP_NEEDED                   = TNetFindDomainControllerFlag(SilOhWntNetapi32.NET_DS_ONLY_LDAP_NEEDED                   ); 
  NET_DS_IS_FLAT_NAME                       = TNetFindDomainControllerFlag(SilOhWntNetapi32.NET_DS_IS_FLAT_NAME                       ); 
  NET_DS_IS_DNS_NAME                        = TNetFindDomainControllerFlag(SilOhWntNetapi32.NET_DS_IS_DNS_NAME                        ); 
  NET_DS_RETURN_DNS_NAME                    = TNetFindDomainControllerFlag(SilOhWntNetapi32.NET_DS_RETURN_DNS_NAME                    ); 
  NET_DS_RETURN_FLAT_NAME                   = TNetFindDomainControllerFlag(SilOhWntNetapi32.NET_DS_RETURN_FLAT_NAME                   ); 

type
  TNetFindDomainControllerFlags             = SilOhWntNetapi32.TNetFindDomainControllerFlags;

type
  INetapi32                                 = SilOiWntNetapi32.INetapi32;

type
  Netapi                                    = SilOtWntNetapi32.Netapi;

implementation
end.
 