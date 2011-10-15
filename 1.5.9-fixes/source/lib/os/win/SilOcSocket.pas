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

unit SilOcSocket;

{$I Defines.inc}

interface

uses
  WinSock;

const
  SO_ERROR          = WinSock.SO_ERROR;
  SO_RCVBUF         = WinSock.SO_RCVBUF;
  SO_SNDBUF         = WinSock.SO_SNDBUF;
  SO_MAX_MSG_SIZE   = $2003; // WinSock.SO_MAX_MSG_SIZE; no existe!
  SO_ACCEPTCONN     = WinSock.SO_ACCEPTCONN;

const
  FIONREAD          = WinSock.FIONREAD;

  // WinSock.getsockopt level

const
  SOL_SOCKET        = WinSock.SOL_SOCKET;
  IPPROTO_TCP       = WinSock.IPPROTO_TCP;

  // WinSock.getsockopt cmd

const
  SO_BROADCAST      = WinSock.SO_BROADCAST;
  SO_DEBUG          = WinSock.SO_DEBUG;
  SO_DONTLINGER     = WinSock.SO_DONTLINGER;
  SO_DONTROUTE      = WinSock.SO_DONTROUTE;
  SO_KEEPALIVE      = WinSock.SO_KEEPALIVE;
  SO_OOBINLINE      = WinSock.SO_OOBINLINE;
  SO_REUSEADDR      = WinSock.SO_REUSEADDR;
  SO_SNDTIMEO       = WinSock.SO_SNDTIMEO;
  SO_RCVTIMEO       = WinSock.SO_RCVTIMEO;

const
  TCP_NODELAY       = WinSock.TCP_NODELAY;

const
  ECONNRESET        = WinSock.ECONNRESET;

implementation
end.
 