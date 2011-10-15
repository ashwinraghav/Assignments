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

unit SilOeMessenger;

{$I Defines.inc}

interface

uses
  Messages;

const
  EV_STARTUP          = Messages.WM_USER +   1;
  EV_SHUTDOWN         = Messages.WM_USER +   2;
  EV_ACTIVATE         = Messages.WM_USER +   3;
  EV_DEACTIVATE       = Messages.WM_USER +   4;
  EV_DISPATCH         = Messages.WM_USER +   5;
  EV_CREATE           = Messages.WM_USER +   6;
  EV_DESTROY          = Messages.WM_USER +   7;
  EV_EXIT             = Messages.WM_USER +   8;
  EV_FLUSH            = Messages.WM_USER +   9;
  EV_CALL             = Messages.WM_USER +  10;
  EV_INITIALIZE       = Messages.WM_USER +  11;
  EV_FINALIZE         = Messages.WM_USER +  12;
  EV_FIRST            = Messages.WM_USER + 100;

implementation
end.
 