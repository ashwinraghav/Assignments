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
  Libc;

const
  SO_ERROR          = Libc.SO_ERROR;
  SO_RCVBUF         = Libc.SO_RCVBUF;
  SO_SNDBUF         = Libc.SO_SNDBUF;
  SO_MAX_MSG_SIZE   = $2003; // Libc.SO_MAX_MSG_SIZE;
  SO_ACCEPTCONN     = $0002; // Libc.SO_ACCEPTCONN;

  FIONREAD          = $1234;

  // Libc.getsockopt level

  SOL_SOCKET        = Libc.SOL_SOCKET;
  IPPROTO_TCP       = Libc.IPPROTO_TCP;

  // Libc.getsockopt cmd

  SO_BROADCAST      = Libc.SO_BROADCAST;
  SO_DEBUG          = Libc.SO_DEBUG;
  SO_DONTLINGER     = $4321;
  SO_DONTROUTE      = Libc.SO_DONTROUTE;
  SO_KEEPALIVE      = Libc.SO_KEEPALIVE;
  SO_OOBINLINE      = Libc.SO_OOBINLINE;
  SO_REUSEADDR      = Libc.SO_REUSEADDR;
  SO_SNDTIMEO       = Libc.SO_SNDTIMEO;
  SO_RCVTIMEO       = Libc.SO_RCVTIMEO;

  TCP_NODELAY       = Libc.TCP_NODELAY;

implementation

end.
 