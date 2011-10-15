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

unit SilOgComputer;

{$I Defines.inc}

interface

uses
  SilOiComputer,
  SilOdComputer,
  SilOsWntNetapi32;

const
  GComputerKindMap: array[TNetServerTypeEnum] of TComputerKinds =
  (
    [ckWorkstation],        // NET_SERVER_WORKSTATION,        // A LAN Manager workstation
    [ckStandaloneServer],   // NET_SERVER_SERVER,             // A LAN Manager server
    [ckDatabaseServer],     // NET_SERVER_SQLSERVER,          // Any server running with Microsoft SQL Server
    [ckDomainServer],       // NET_SERVER_DOMAIN_CTRL,        // Primary domain controller
    [ckDomainServer],       // NET_SERVER_DOMAIN_BAKCTRL,     // Backup domain controller
    [ckTimeServer],         // NET_SERVER_TIME_SOURCE,        // Server running the Timesource service
    [ckStandaloneServer],   // NET_SERVER_AFP,                // Apple File Protocol server
    [ckStandaloneServer],   // NET_SERVER_NOVELL,             // Novell server
    [ckWorkstation],        // NET_SERVER_DOMAIN_MEMBER,      // LAN Manager 2.x domain member
    [],                     // NET_SERVER_PRINTQ_SERVER,      // Server sharing print queue
    [],                     // NET_SERVER_DIALIN_SERVER,      // Server running dial-in service
    [],                     // NET_SERVER_XENIX_SERVER,       // Xenix server
    [ckWorkstation],        // NET_SERVER_NT,                 // Windows NT/Windows 2000/Windows XP workstation or server
    [ckPersonal],           // NET_SERVER_WFW,                // Server running Windows for Workgroups
    [ckPersonal],           // NET_SERVER_SERVER_MFPN,        // Microsoft File and Print for NetWare
    [ckStandaloneServer],   // NET_SERVER_SERVER_NT,          // Windows NT/Windows 2000 server that is not a domain controller
    [],                     // NET_SERVER_POTENTIAL_BROWSER,  // Server that can run the browser service
    [],                     // NET_SERVER_BACKUP_BROWSER,     // Server running a browser service as backup
    [],                     // NET_SERVER_MASTER_BROWSER,     // Server running the master browser service
    [],                     // NET_SERVER_DOMAIN_MASTER,      // Server running the domain master browser
    [],                     // NET_SERVER_SERVER_OSF,
    [],                     // NET_SERVER_SERVER_VMS,
    [ckPersonal],           // NET_SERVER_WINDOWS,            // Windows 95 or later
    [],                     // NET_SERVER_DFS,                // Root of a DFS tree
    [ckServerCluster],      // NET_SERVER_CLUSTER_NT,         // Server clusters available in the domain
    [ckGraphicalTerminal],  // NET_SERVER_TERMINALSERVER,     // Terminal Server
    [ckVirtualCluster],     // NET_SERVER_CLUSTER_VS_NT,      // Cluster virtual servers available in the domain
    [],                     // NET_SERVER_DCE,                // IBM DSS (Directory and Security Services) or equivalent
    [],                     // NET_SERVER_ALTERNATE_XPORT,    // return list for alternate transport
    [],                     // NET_SERVER_LOCAL_LIST_ONLY,    // Servers maintained by the browser
    []                      // NET_SERVER_DOMAIN_ENUM         // Primary domain
  );


  GKindStr: array[TComputerKind] of string =
  (
    SckPersonal,
    SckWorkstation,
    SckStandaloneServer,
    SckDomainServer,
    SckTimeServer,
    SckDatabaseServer,
    SckServerCluster,
    SckVirtualCluster,
    SckGraphicalTerminal
  );

implementation
end.
 