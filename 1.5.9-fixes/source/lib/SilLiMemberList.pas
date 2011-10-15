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

unit SilLiMemberList;

{$INCLUDE Defines.inc}

interface

uses
  SilBeTypes,
  SilBeTypeInfo,
  SilLiContainerTypes,
  SilLiEnumerator,
  SilLiParameters;

type
  PMemberHeader = ^RMemberHeader;
  PPMemberHeader = ^PMemberHeader;
  
  RMemberHeader = packed record
    Name: string;
    Handler: ITypeinfoHandler;
    Member: Pointer;
    Value: packed record end;
  end;

  PMember = ^RMember;
  RMember = packed record
    Header: RMemberHeader;
    Value: Variant;
  end;

  IMemberList = interface (IParameterList)
    ['{4E7EB3E3-250C-4712-9FED-346B1CEA8345}']
    function GetIsEmpty: Boolean;
    function GetFirstMember: PMemberHeader;
    function GetLastMember: PMemberHeader;
    function Enumerate(var Enum: IEnumerator; out Item: RMember): Boolean;
    function Find(const Name: string; Item: PMember = nil): Boolean; overload;   
    function Find(const Name: string; out Item: PMemberHeader): Boolean; overload;
    function Define(const Name: string; Member: Pointer; TypeInfo: Pointer; Default: Variant): IMemberList; overload;
    property IsEmpty: Boolean read GetIsEmpty;
    property First: PMemberHeader read GetFirstMember;
    property Last: PMemberHeader read GetLastMember;
  end;

implementation
end.
