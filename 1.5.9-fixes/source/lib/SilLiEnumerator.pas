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

unit SilLiEnumerator;

{$I Defines.inc}

interface

type

  { IEnumerator }

  IEnumerator = interface
    ['{C0F14942-F49E-11D3-9137-00C0261013CD}']
    function HasMore: Boolean;
    function Get(out Item): Boolean;
    function GetCurrent: Pointer;
    function GetIteration: Integer;
    function Next: Boolean;
    procedure Detach;
    procedure Reset;
    property Current: Pointer read GetCurrent;
    property Iteration: Integer read GetIteration;
  end;

  { IEnumeration }

  IEnumeration = interface
    ['{3CD8FA91-7843-11D3-9853-00104B0FA1EF}']
    function Enumerate(var Enum: IEnumerator; out Item): Boolean;
  end;

  { IEnumerable }

  IEnumerable = interface
    ['{E432E901-3B34-11D4-AD7B-00902794F778}']
    function GetEnumerator(out Enum: IEnumerator; Locked: Boolean): Boolean;
  end;

  IItemization = interface
    ['{AB14BDC1-302C-11D5-98BD-00104B0FA1EF}']
    function Enumerate(var Enum: IEnumerator; out Item): Boolean;
  end;

implementation

end.





