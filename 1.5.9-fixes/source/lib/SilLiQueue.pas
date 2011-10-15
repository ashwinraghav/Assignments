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

unit SilLiQueue;

{$I Defines.inc}

interface

uses
  SilBeTypes,
  SilLiLinkedList,
  SilOsTypes;

type
  IQueue = interface
    ['{E56E6E91-4A80-424B-B5A6-5DB28C2B407E}']
    procedure SetMaxSize(const Value: Integer);
    function GetMaxSize: Integer;
    procedure Cancel(Clear: Boolean = false); overload;
    procedure Cancel(out List: ILinkedList); overload;
    procedure Reset;
    property MaxSize: Integer read GetMaxSize write SetMaxSize;
  end;

  IItemQueue = interface (IQueue)
    ['{C8D00F68-9813-4AFE-8050-1A13E2866867}']
    function Put(const Item): Boolean;
    function Get(out Item; TimeOut: LongWord = INFINITE; AvoidMsgLock: Boolean = False): Boolean;
    function Top(out Item): Boolean; 
  end;

implementation

end.