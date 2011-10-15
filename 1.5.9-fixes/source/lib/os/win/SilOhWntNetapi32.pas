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

unit SilOhWntNetapi32;

{$I Defines.inc}
{$MINENUMSIZE 4}

interface

uses
  SilOsError,
  Windows; 

const
  CNetapi32 = 'NETAPI32.DLL';

const
  NERR_SUCCESS = 0;
  NERR_BASE = 2100;

const
  NCBNAMSZ = 16;               // absolute length of a net name
  MAX_LANA = 254;              // lana's in range 0 to MAX_LANA inclusive

const
  MAX_PREF_MAXLENGTH = Cardinal(-1);
  TIMEQ_FOREVER      = Cardinal(-1);

const
  NCBCALL         = $10;            // NCB CALL
  NCBLISTEN       = $11;            // NCB LISTEN
  NCBHANGUP       = $12;            // NCB HANG UP
  NCBSEND         = $14;            // NCB SEND
  NCBRECV         = $15;            // NCB RECEIVE
  NCBRECVANY      = $16;            // NCB RECEIVE ANY
  NCBCHAINSEND    = $17;            // NCB CHAIN SEND
  NCBDGSEND       = $20;            // NCB SEND DATAGRAM
  NCBDGRECV       = $21;            // NCB RECEIVE DATAGRAM
  NCBDGSENDBC     = $22;            // NCB SEND BROADCAST DATAGRAM
  NCBDGRECVBC     = $23;            // NCB RECEIVE BROADCAST DATAGRAM
  NCBADDNAME      = $30;            // NCB ADD NAME
  NCBDELNAME      = $31;            // NCB DELETE NAME
  NCBRESET        = $32;            // NCB RESET
  NCBASTAT        = $33;            // NCB ADAPTER STATUS
  NCBSSTAT        = $34;            // NCB SESSION STATUS
  NCBCANCEL       = $35;            // NCB CANCEL
  NCBADDGRNAME    = $36;            // NCB ADD GROUP NAME
  NCBENUM         = $37;            // NCB ENUMERATE LANA NUMBERS
  NCBUNLINK       = $70;            // NCB UNLINK
  NCBSENDNA       = $71;            // NCB SEND NO ACK
  NCBCHAINSENDNA  = $72;            // NCB CHAIN SEND NO ACK
  NCBLANSTALERT   = $73;            // NCB LAN STATUS ALERT
  NCBACTION       = $77;            // NCB ACTION
  NCBFINDNAME     = $78;            // NCB FIND NAME
  NCBTRACE        = $79;            // NCB TRACE

  ASYNCH          = $80;            // high bit set = asynchronous

const // NCB Return codes

  NRC_GOODRET     = $00;    // good return
                            // also returned when ASYNCH request accepted
  NRC_BUFLEN      = $01;    // illegal buffer length
  NRC_ILLCMD      = $03;    // illegal command
  NRC_CMDTMO      = $05;    // command timed out
  NRC_INCOMP      = $06;    // message incomplete, issue another command
  NRC_BADDR       = $07;    // illegal buffer address
  NRC_SNUMOUT     = $08;    // session number out of range
  NRC_NORES       = $09;    // no resource available
  NRC_SCLOSED     = $0a;    // session closed
  NRC_CMDCAN      = $0b;    // command cancelled
  NRC_DUPNAME     = $0d;    // duplicate name
  NRC_NAMTFUL     = $0e;    // name table full
  NRC_ACTSES      = $0f;    // no deletions, name has active sessions
  NRC_LOCTFUL     = $11;    // local session table full
  NRC_REMTFUL     = $12;    // remote session table full
  NRC_ILLNN       = $13;    // illegal name number
  NRC_NOCALL      = $14;    // no callname
  NRC_NOWILD      = $15;    // cannot put * in NCB_NAME
  NRC_INUSE       = $16;    // name in use on remote adapter
  NRC_NAMERR      = $17;    // name deleted
  NRC_SABORT      = $18;    // session ended abnormally
  NRC_NAMCONF     = $19;    // name conflict detected
  NRC_IFBUSY      = $21;    // interface busy, IRET before retrying
  NRC_TOOMANY     = $22;    // too many commands outstanding, retry later
  NRC_BRIDGE      = $23;    // NCB_lana_num field invalid
  NRC_CANOCCR     = $24;    // command completed while cancel occurring
  NRC_CANCEL      = $26;    // command not valid to cancel
  NRC_DUPENV      = $30;    // name defined by anther local process
  NRC_ENVNOTDEF   = $34;    // environment undefined. RESET required
  NRC_OSRESNOTAV  = $35;    // required OS resources exhausted
  NRC_MAXAPPS     = $36;    // max number of applications exceeded
  NRC_NOSAPS      = $37;    // no saps available for netbios
  NRC_NORESOURCES = $38;    // requested resources are not available
  NRC_INVADDRESS  = $39;    // invalid ncb address or length > segment
  NRC_INVDDID     = $3B;    // invalid NCB DDID
  NRC_LOCKFAIL    = $3C;    // lock of user area failed
  NRC_OPENERR     = $3f;    // NETBIOS not loaded
  NRC_SYSTEM      = $40;    // system error

  NRC_PENDING     = $ff;    // asynchronous command is not yet finished


type
  ENetapiError  = class(SilOsError.OsException);

type
  TNetapiStatus  = Windows.DWORD;
  TNetbiosStatus = BYTE;

type
  PNetUserInfo0 = ^TNetUserInfo0;
  TNetUserInfo0 = record
    usri0_name:             PWideChar;
  end;

type
  PNetUserInfo2 = ^TNetUserInfo2;
  TNetUserInfo2 = record
    usri2_name:             PWideChar;
    usri2_password:         PWideChar;
    usri2_password_age:     DWORD;
    usri2_priv:             DWORD;
    usri2_home_dir:         PWideChar;
    usri2_comment:          PWideChar;
    usri2_flags:            DWORD;
    usri2_script_path:      PWideChar;
    usri2_auth_flags:       DWORD;
    usri2_full_name:        PWideChar;
    usri2_usr_comment:      PWideChar;
    usri2_parms:            PWideChar;
    usri2_workstations:     PWideChar;
    usri2_last_logon:       DWORD;
    usri2_last_logoff:      DWORD;
    usri2_acct_expires:     DWORD;
    usri2_max_storage:      DWORD;
    usri2_units_per_week:   DWORD;
    usri2_logon_hours:      PBYTE;
    usri2_bad_pw_count:     DWORD;
    usri2_num_logons:       DWORD;
    usri2_logon_server:     PWideChar;
    usri2_country_code:     DWORD;
    usri2_code_page:        DWORD;
  end;

type
  PNetUserInfo3 = ^TNetUserInfo3;
  TNetUserInfo3 = record
    usri3_name:             PWideChar;
    usri3_password:         PWideChar;
    usri3_password_age:     DWORD;
    usri3_priv:             DWORD;
    usri3_home_dir:         PWideChar;
    usri3_comment:          PWideChar;
    usri3_flags:            DWORD;
    usri3_script_path:      PWideChar;
    usri3_auth_flags:       DWORD;
    usri3_full_name:        PWideChar;
    usri3_usr_comment:      PWideChar;
    usri3_parms:            PWideChar;
    usri3_workstations:     PWideChar;
    usri3_last_logon:       DWORD;
    usri3_last_logoff:      DWORD;
    usri3_acct_expires:     DWORD;
    usri3_max_storage:      DWORD;
    usri3_units_per_week:   DWORD;
    usri3_logon_hours:      PBYTE;
    usri3_bad_pw_count:     DWORD;
    usri3_num_logons:       DWORD;
    usri3_logon_server:     PWideChar;
    usri3_country_code:     DWORD;
    usri3_code_page:        DWORD;
    usri3_user_id:          DWORD;
    usri3_primary_group_id: DWORD;
    usri3_profile:          PWideChar;
    usri3_home_dir_drive:   PWideChar;
    usri3_password_expired: DWORD;
  end;

type
  PNetUserInfo10 = ^TNetUserInfo10;
  TNetUserInfo10 = record
    usri10_name:             PWideChar;
    usri10_comment:          PWideChar;
    usri10_full_name:        PWideChar;
    usri10_usr_comment:      PWideChar;
  end;

type
  PSID  = Windows.PSID;

type
	PNetUserModalsInfo0 = ^TNetUserModalsInfo0;
  TNetUserModalsInfo0 = record
    usrmod0_min_passwd_len: 		DWORD;
    usrmod0_max_passwd_age: 		DWORD;
    usrmod0_min_passwd_age: 		DWORD;
    usrmod0_force_logoff: 			DWORD;
    usrmod0_password_hist_len: 	DWORD;
  end;

type
  PNetUserModalsInfo1 = ^TNetUserModalsInfo1;
  TNetUserModalsInfo1 = record
  	usrmod1_role: 		DWORD;
  	usrmod1_primary:	PWideChar;
  end;

type
  PNetUserModalsInfo2 = ^TNetUserModalsInfo2;
  TNetUserModalsInfo2 = record
  	usrmod2_domain_name:	      PWideChar;
    usrmod2_domain_id:			    PSID;
  end;

type
  PNetUserModalsInfo3 = ^TNetUserModalsInfo3;
  TNetUserModalsInfo3 = record
	 	usrmod3_lockout_duration:						DWORD;
  	usrmod3_lockout_observation_window:	DWORD;
  	usrmod3_lockout_threshold:					DWORD;
  end;

type
  TNetUseStatusEnum = (
      NET_USE_OK            ,   //  =    0;
      NET_USE_PAUSED        ,   //  =    1;
      NET_USE_DISCONN       ,   //  =    2;
      NET_USE_NETERR        ,   //  =    3;
      NET_USE_CONN          ,   //  =    4;
      NET_USE_RECONN            //  =    5;
    );

const
  NET_USE_SESSLOST          =   NET_USE_DISCONN;

type
  TNetUseKindEnum = (
      NET_USE_DISKDEV       ,   //  =    0;
      NET_USE_SPOOLDEV      ,   //  =    1;
      NET_USE_CHARDEV       ,   //  =    2;
      NET_USE_IPC               //  =    3;
    );

const
  NET_USE_WILDCARD          = TNetUseKindEnum(-1);

type
  TNetUseForceKind  = (
      NET_USE_NOFORCE        ,  //  =    0;
      NET_USE_FORCE          ,  //  =    1;
      NET_USE_LOTS_OF_FORCE     //  =    2;
    );


type
  PNetUseParamError = ^TNetUseParamError;
  TNetUseParamError = (
      NET_USE_NONE           ,  //  =    0;
      NET_USE_LOCAL          ,  //  =    1;
      NET_USE_REMOTE         ,  //  =    2;
      NET_USE_PASSWORD       ,  //  =    3;
      NET_USE_ASGTYPE        ,  //  =    4;
      NET_USE_USERNAME       ,  //  =    5;
      NET_USE_DOMAINNAME        //  =    6;
    );

type
  PNetUseInfo1 = ^TNetUseInfo1;
  TNetUseInfo1 = record
    ui1_local:              PWideChar;
    ui1_remote:             PWideChar;
    ui1_password:           PWideChar;
    ui1_status:             DWORD;
    ui1_asg_type:           DWORD;
    ui1_refcount:           DWORD;
    ui1_usecount:           DWORD;
  end;

type
  PNetUseInfo2 = ^TNetUseInfo2;
  TNetUseInfo2 = record
    ui2_local:              PWideChar;
    ui2_remote:             PWideChar;
    ui2_password:           PWideChar;
    ui2_status:             TNetUseStatusEnum;
    ui2_asg_type:           TNetUseKindEnum;
    ui2_refcount:           DWORD;
    ui2_usecount:           DWORD;
    ui2_username:           PWideChar;
    ui2_domainname:         PWideChar;
  end;

type
  TNetServerTypeEnum = (
    {00000001}  NET_SERVER_WORKSTATION,        // A LAN Manager workstation
    {00000002}  NET_SERVER_SERVER,             // A LAN Manager server
    {00000004}  NET_SERVER_SQLSERVER,          // Any server running with Microsoft SQL Server
    {00000008}  NET_SERVER_DOMAIN_CTRL,        // Primary domain controller
    {00000010}  NET_SERVER_DOMAIN_BAKCTRL,     // Backup domain controller
    {00000020}  NET_SERVER_TIME_SOURCE,        // Server running the Timesource service
    {00000040}  NET_SERVER_AFP,                // Apple File Protocol server
    {00000080}  NET_SERVER_NOVELL,             // Novell server
    {00000100}  NET_SERVER_DOMAIN_MEMBER,      // LAN Manager 2.x domain member
    {00000200}  NET_SERVER_PRINTQ_SERVER,      // Server sharing print queue
    {00000400}  NET_SERVER_DIALIN_SERVER,      // Server running dial-in service
    {00000800}  NET_SERVER_XENIX_SERVER,       // Xenix server
    {00001000}  NET_SERVER_NT,                 // Windows NT/Windows 2000/Windows XP workstation or server
    {00002000}  NET_SERVER_WFW,                // Server running Windows for Workgroups
    {00004000}  NET_SERVER_SERVER_MFPN,        // Microsoft File and Print for NetWare
    {00008000}  NET_SERVER_SERVER_NT,          // Windows NT/Windows 2000 server that is not a domain controller
    {00010000}  NET_SERVER_POTENTIAL_BROWSER,  // Server that can run the browser service
    {00020000}  NET_SERVER_BACKUP_BROWSER,     // Server running a browser service as backup
    {00040000}  NET_SERVER_MASTER_BROWSER,     // Server running the master browser service
    {00080000}  NET_SERVER_DOMAIN_MASTER,      // Server running the domain master browser
    {00100000}  NET_SERVER_SERVER_OSF,
    {00200000}  NET_SERVER_SERVER_VMS,
    {00400000}  NET_SERVER_WINDOWS,            // Windows 95 or later
    {00800000}  NET_SERVER_DFS,                // Root of a DFS tree
    {01000000}  NET_SERVER_CLUSTER_NT,         // Server clusters available in the domain
    {02000000}  NET_SERVER_TERMINALSERVER,     // Terminal Server
    {04000000}  NET_SERVER_CLUSTER_VS_NT,      // Cluster virtual servers available in the domain
    {10000000}  NET_SERVER_DCE,                // IBM DSS (Directory and Security Services) or equivalent
    {20000000}  NET_SERVER_ALTERNATE_XPORT,    // return list for alternate transport 
    {40000000}  NET_SERVER_LOCAL_LIST_ONLY,    // Servers maintained by the browser
    {80000000}  NET_SERVER_DOMAIN_ENUM         // Primary domain
  );

type
  TNetServerType = set of TNetServerTypeEnum;

const
  NET_SERVER_ALL = [Low(TNetServerTypeEnum) .. High(TNetServerTypeEnum)]; 

type
  PNetServerInfo100 = ^TNetServerInfo100;
  TNetServerInfo100 = record
    sv100_platform_id:      DWORD;
    sv100_name:             PWideChar;
  end;

  PNetServerInfo101 = ^TNetServerInfo101;
  TNetServerInfo101 = record
    sv101_platform_id:      DWORD;
    sv101_name:             PWideChar;
    sv101_version_major:    DWORD;
    sv101_version_minor:    DWORD;
    sv101_type:             TNetServerType;
    sv101_comment:          PWideChar;
  end;

  PNetServerInfo102 = ^TNetServerInfo101;
  TNetServerInfo102 = record
    sv101_platform_id:      DWORD;
    sv101_name:             PWideChar;
    sv101_version_major:    DWORD;
    sv101_version_minor:    DWORD;
    sv101_type:             TNetServerType;
    sv101_comment:          PWideChar;
    sv102_users:            DWORD;
    sv102_disc:             LongInt;
    sv102_hidden:           BOOL;
    sv102_announce:         DWORD;
    sv102_anndelta:         DWORD;
    sv102_licenses:         DWORD;
    sv102_userpath:         LPWSTR;
  end;

  PNetDisplayMachine = ^TNetDisplayMachine;
  TNetDisplayMachine = record
    usri2_name:             PWideChar;
    usri2_comment:          PWideChar;
    usri2_flags:            DWORD;
    usri2_user_id:          DWORD;
    usri2_next_index:       DWORD;
  end;

  PNetDisplayUser = ^TNetDisplayUser;
  TNetDisplayUser = record
    usri1_name:             LPWSTR;
    usri1_comment:          LPWSTR;
    usri1_flags:            DWORD;
    usri1_full_name:        LPWSTR;
    usri1_user_id:          DWORD;
    usri1_next_index:       DWORD;
  end;

  TNetJoinStatus  = (
    NET_JOIN_UNKNOWNSTATUS,
    NET_JOIN_UNJOINED,
    NET_JOIN_WORKGROUPNAME,
    NET_JOIN_DOMAINNAME
  );

type
  TNetQueryLevel = (
      NET_QUERY_USERS,
      NET_QUERY_SERVERS,
      NET_QUERY_GLOBALS
    );

type
  PNCB  = ^TNCB;   
  TNCBPostProc = procedure(P: PNCB);
  TNCB = packed record
    ncb_command: Byte;         // command code
    ncb_retcode: Byte;         // return code
    ncb_lsn: Byte;             // local session number
    ncb_num: Byte;             // number of our network name
    ncb_buffer: PChar;         // address of message buffer
    ncb_length: Word;          // size of message buffer
    ncb_callname: array[0..NCBNAMSZ - 1] of char;  // blank-padded name of remote
    ncb_name: array[0..NCBNAMSZ - 1] of char;      // our blank-padded netname
    ncb_rto: Byte;             // rcv timeout/retry count
    ncb_sto: Byte;             // send timeout/sys timeout
    ncb_post: TNCBPostProc;    // POST routine address
    ncb_lana_num: Byte;        // lana (adapter) number
    ncb_cmd_cplt: Byte;        // 0xff => commmand pending
    ncb_reserve: array[0..9] of Byte;              // reserved, used by BIOS
    ncb_event: THandle;        // HANDLE to Win32 event which
                               // will be set to the signalled
                               // state when an ASYNCH command
                               // completes
  end;

type
  PNetLanaEnum = ^TNetLanaEnum;
  TNetLanaEnum = packed record
    length: Byte;         //  Number of valid entries in lana[]
    lana: array[0..MAX_LANA] of Byte;
  end;

type
  PNetAdapterStatus = ^TNetAdapterStatus;
  TNetAdapterStatus = packed record
    adapter_address: array[0..5] of Byte;
    rev_major: Byte;
    reserved0: Byte;
    adapter_type: Byte;
    rev_minor: Byte;
    duration: Word;
    frmr_recv: Word;
    frmr_xmit: Word;
    iframe_recv_err: Word;
    xmit_aborts: Word;
    xmit_success: DWORD;
    recv_success: DWORD;
    iframe_xmit_err: Word;
    recv_buff_unavail: Word;
    t1_timeouts: Word;
    ti_timeouts: Word;
    reserved1: DWORD;
    free_ncbs: Word;
    max_cfg_ncbs: Word;
    max_ncbs: Word;
    xmit_buf_unavail: Word;
    max_dgram_size: Word;
    pending_sess: Word;
    max_cfg_sess: Word;
    max_sess: Word;
    max_sess_pkt_size: Word;
    name_count: Word;
  end;

type
  TNetDomainControllerAddressType = (
      _NET_DS_UNUSED_0        , //  0
      NET_DS_INET_ADDRESS     , //  1
      NET_DS_NETBIOS_ADDRESS    //  2
    );

type
  TNetReturnedDomainControllerFlag  = (
    {01}  NET_DS_PDC_FLAG            , //  0x00000001    // DC is PDC of Domain
    {02}  _NET_DS_RETN_RESERVED_00000002,
    {03}  NET_DS_GC_FLAG             , //  0x00000004    // DC is a GC of forest
    {04}  NET_DS_LDAP_FLAG           , //  0x00000008    // Server supports an LDAP server
    {05}  NET_DS_DS_FLAG             , //  0x00000010    // DC supports a DS and is a Domain Controller
    {06}  NET_DS_KDC_FLAG            , //  0x00000020    // DC is running KDC service
    {07}  NET_DS_TIMESERV_FLAG       , //  0x00000040    // DC is running time service
    {08}  NET_DS_CLOSEST_FLAG        , //  0x00000080    // DC is in closest site to client
    {09}  NET_DS_WRITABLE_FLAG       , //  0x00000100    // DC has a writable DS
    {10}  NET_DS_GOOD_TIMESERV_FLAG  , //  0x00000200    // DC is running time service (and has clock hardware)
    {11}  NET_DS_NDNC_FLAG           , //  0x00000400    // DomainName is non-domain NC serviced by the LDAP server
    {12}  _NET_DS_RETN_RESERVED_00000800,
    {13}  _NET_DS_RETN_RESERVED_00001000,
    {14}  _NET_DS_RETN_RESERVED_00002000,
    {15}  _NET_DS_RETN_RESERVED_00004000,
    {16}  _NET_DS_RETN_RESERVED_00008000,
    {17}  _NET_DS_RETN_RESERVED_00010000,
    {18}  _NET_DS_RETN_RESERVED_00020000,
    {19}  _NET_DS_RETN_RESERVED_00040000,
    {20}  _NET_DS_RETN_RESERVED_00080000,
    {21}  _NET_DS_RETN_RESERVED_00100000,
    {22}  _NET_DS_RETN_RESERVED_00200000,
    {23}  _NET_DS_RETN_RESERVED_00400000,
    {24}  _NET_DS_RETN_RESERVED_00800000,
    {25}  _NET_DS_RETN_RESERVED_01000000,
    {26}  _NET_DS_RETN_RESERVED_02000000,
    {27}  _NET_DS_RETN_RESERVED_04000000,
    {28}  _NET_DS_RETN_RESERVED_08000000,
    {29}  _NET_DS_RETN_RESERVED_10000000,
    {30}  NET_DS_DNS_CONTROLLER_FLAG , //  0x20000000    // DomainControllerName is a DNS name
    {31}  NET_DS_DNS_DOMAIN_FLAG     , //  0x40000000    // DomainName is a DNS name
    {32}  NET_DS_DNS_FOREST_FLAG       //  0x80000000    // DnsForestName is a DNS name
    );

type
  TNetReturnedDomainControllerFlags = set of TNetReturnedDomainControllerFlag;

//    DS_PING_FLAGS          0x0000FFFF    // Flags returned on ping


type
  PNetDomainControllerInfo = ^TNetDomainControllerInfo;
  TNetDomainControllerInfo = packed record
    DomainControllerName:   PWideChar;
    DomainControllerAddress: PWideChar;
    DomainControllerAddressType: TNetDomainControllerAddressType;
    DomainGuid: TGUID;
    DomainName: PWideChar;
    DnsForestName: PWideChar;
    Flags: TNetReturnedDomainControllerFlags;
    DcSiteName: PWideChar;
    ClientSiteName: PWideChar;
  end;

type
  TNetFindDomainControllerFlag = (
      NET_DS_FORCE_REDISCOVERY            , //  0x00000001

      _NET_DS_FIND_RESERVED_00000002,
      _NET_DS_FIND_RESERVED_00000004,
      _NET_DS_FIND_RESERVED_00000008,

      NET_DS_DIRECTORY_SERVICE_REQUIRED   , //  0x00000010
      NET_DS_DIRECTORY_SERVICE_PREFERRED  , //  0x00000020
      NET_DS_GC_SERVER_REQUIRED           , //  0x00000040
      NET_DS_PDC_REQUIRED                 , //  0x00000080
      NET_DS_BACKGROUND_ONLY              , //  0x00000100
      NET_DS_IP_REQUIRED                  , //  0x00000200
      NET_DS_KDC_REQUIRED                 , //  0x00000400
      NET_DS_TIMESERV_REQUIRED            , //  0x00000800
      NET_DS_WRITABLE_REQUIRED            , //  0x00001000
      NET_DS_GOOD_TIMESERV_PREFERRED      , //  0x00002000
      NET_DS_AVOID_SELF                   , //  0x00004000
      NET_DS_ONLY_LDAP_NEEDED             , //  0x00008000


      NET_DS_IS_FLAT_NAME                 , //  0x00010000
      NET_DS_IS_DNS_NAME                  , //  0x00020000

      _NET_DS_FIND_RESERVED_00040000,
      _NET_DS_FIND_RESERVED_00080000,
      _NET_DS_FIND_RESERVED_00100000,
      _NET_DS_FIND_RESERVED_00200000,
      _NET_DS_FIND_RESERVED_00400000,
      _NET_DS_FIND_RESERVED_00800000,
      _NET_DS_FIND_RESERVED_01000000,
      _NET_DS_FIND_RESERVED_02000000,
      _NET_DS_FIND_RESERVED_04000000,
      _NET_DS_FIND_RESERVED_08000000,
      _NET_DS_FIND_RESERVED_10000000,
      _NET_DS_FIND_RESERVED_20000000,

      NET_DS_RETURN_DNS_NAME              , //  0x40000000
      NET_DS_RETURN_FLAT_NAME               //  0x80000000
    );

type
  TNetFindDomainControllerFlags = set of TNetFindDomainControllerFlag;

type
  TNetAdapters = array of Byte;

type
  TNetUserGetInfo = function (
                      ServerName, UserName: PWideChar;
                      Level: DWORD;
                  var Buffer: Pointer): TNetapiStatus; stdcall;

  TNetUserEnum  = function (
                      servername: PWideChar;
                      level: DWORD;
                      filter: DWORD;
                  var bufptr: Pointer;
                      prefmaxlen: DWORD;
                      entriesread: PDWORD;
                      totalentries: PDWORD;
                      resume_handle: PDWORD): TNetapiStatus; stdcall;

  TNetServerEnum  = function (
                      servername: PWideChar;
                      level: DWORD;
                  var bufptr: Pointer;
                      prefmaxlen: DWORD;
                      entriesread: PDWORD;
                      totalentries: PDWORD;
                      servertype: DWORD;
                      domain: PWideChar;
                      resume_handle: PDWORD): TNetapiStatus; stdcall;

  TNetGetDCName  = function (
                      ServerName,
                      DomainName: PWideChar;
                  var Buffer: Pointer): TNetapiStatus; stdcall;

  TNetDsGetDCName  = function (
                      ServerName,
                      DomainName: PWideChar;
                      GUID: PGUID;
                      SiteName: PWideChar;
                      Flags: TNetFindDomainControllerFlags;
                  var Info: PNetDomainControllerInfo): TNetapiStatus; stdcall;

  TNetApiBufferFree = function (
                      Buffer: Pointer): TNetapiStatus; stdcall;

  TNetUserChangePassword = function (
                      DomainName,
                      UserName,
                      OldPassword,
                      NewPassword: PWideChar): TNetapiStatus; stdcall;

  TNetGetDisplayInformationIndex = function (
                      ServerName: PWideChar;
                      Level: DWORD;
                      Prefix: PWideChar;
                      Index: LPDWORD): TNetapiStatus; stdcall;

  TNetQueryDisplayInformation = function (
                      ServerName: PWideChar;
                      Level: DWORD;
                      Index: DWORD;
                      EntriesRequested: DWORD;
                      PreferredMaximumLength: DWORD;
                      ReturnedEntryCount: LPDWORD;
                  var Buffer: Pointer): TNetapiStatus; stdcall;

  TNetGetJoinInformation = function (
                      ServerName: PWideChar;
                  var DomainName: PWideChar;
                  var BufferType: TNetJoinStatus): TNetapiStatus; stdcall;


  TNetServerGetInfo = function (
                      ServerName: PWideChar;
                      Level: DWORD;
                  var Buffer: Pointer): TNetapiStatus; stdcall;

  TNetUseAdd = function (
                      UncServerName: PWideChar;
                      Level: DWORD;
                      Buf: pointer;
                      ParmError: PNetUseParamError): TNetapiStatus; stdcall;

  TNetUseDel = function (
                      UncServerName: PWideChar;
                      UseName: PWideChar;
                      ForceCond: TNetUseForceKind): TNetapiStatus; stdcall;

  TNetUseEnum = function (
                      UncServerName: PWideChar;
                      Level: DWORD;
                  var BufPtr: pointer;
                      PreferredMaximumSize: DWORD;
                      EntriesRead: PDWORD;
                      TotalEntries: PDWORD;
                      ResumeHandle: PDWORD): TNetapiStatus; stdcall;


  TNetbiosCall = function (
                  const NCB: TNCB): TNetbiosStatus; stdcall;

  TNetMessageBufferSend = function (
                      ServerName: PWideChar;
                      MsgName: PWideChar;
                      FromName: PWideChar;
                      Buf: Pointer;
                      BufLen: LongWord): TNetapiStatus; stdcall;

  TNetUserModalsGet = function (
  										ServerName: PWideChar;
                      Level: DWORD;
                      var Buffer: Pointer): TNetapiStatus; stdcall;

implementation
end.
