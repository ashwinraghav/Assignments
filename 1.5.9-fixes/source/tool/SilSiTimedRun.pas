{********************************************************************************
 *                  Standard Interface Library (SIL)                            *
 *                                                                              *
 *       General purpose library whose design is based in STRONG                *
 *   use of interfaces.                                                         *
 *                                                                              *
 *                                                                              *
 *     Copyright (C) 2000 Mariano Podest�    antiriad@gmail.com                 *
 *     Copyright (C) 2000 Leandro Conde      lconde@str.com.ar                  *
 *     Copyright (C) 2000 Lisandro Podest�   lisandrop@movi.com.ar              *
 *     Copyright (C) 2002 Mariano Reggiardo   Mreggiardo@infovia.com.ar         *
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
unit SilSiTimedRun;

interface

const
  RUN_FUNCTION = 1026;
type

  ITimedRunListener = interface
  ['{A12E2D7E-6985-4A34-9728-07F2D421D5B4}']
    procedure onTime;
  end;

  ITimedRun = interface
  ['{2629B264-A22C-4BA0-B914-87CDF12A7451}']
    function GetTimeOut: Integer;
    procedure SetTimeOut(const Value: Integer);
    property TimeOut :Integer read GetTimeOut write SetTimeOut;
    procedure Finish;
    procedure Go;
  end;


implementation

end.
