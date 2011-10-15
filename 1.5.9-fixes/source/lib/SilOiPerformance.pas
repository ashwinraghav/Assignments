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

unit SilOiPerformance;

{$I Defines.inc}

interface

uses
  SilBeTypes,
  SilOsTypes;

type
  IPerformanceCounter = interface
    ['{A8D02A74-F379-4DC5-88DF-AE35EF063198}']
    function GetFrequency: LargeInt;
    function GetInitial: LargeInt;
    function GetElapsed: LargeInt;
    function GetTime(Reset: Boolean = False): LargeInt;
    function ToMicroseconds(Reset: Boolean = True): Single;
    function ToMilliseconds(Reset: Boolean = True): Single;
    function ToSeconds(Reset: Boolean = True): Single;
    function ToDateTime(Reset: Boolean = True): TDateTime;
    procedure Reset;
    property Frequency: LargeInt read GetFrequency;
    property Initial: LargeInt read GetInitial;
    property Elapsed: LargeInt read GetElapsed;
  end;

implementation
end.
