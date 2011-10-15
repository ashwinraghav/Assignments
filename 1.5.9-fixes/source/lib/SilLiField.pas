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

unit SilLiField;

{$I Defines.inc}

interface

uses
  SilBeDataType,
  SilLiFiler;

type
  IFieldStore = interface
    ['{B4328780-AA8E-11D4-989E-00104B0FA1EF}']
    procedure Write(const Buffer; Position, Size: LongWord);
    procedure Read(var Buffer; Position, Size: LongWord);
  end;

  IFieldAccess = interface(IValueAccess)
    ['{F002A5B2-6175-11D4-988B-00104B0FA1EF}']
    function GetName: String;
    function GetDataType: TDataFieldType;
    function GetSize: LongWord;
    function GetPosition: LongWord;
    procedure SetPosition(Value: LongWord);
    function GetChanged: Boolean;
    procedure SetChanged(Value: Boolean);
    function GetIsEmpty: Boolean;
    property Name: String read GetName;
    property DataType: TDataFieldType read GetDataType;
    property Size: LongWord read GetSize;
    property Position: Cardinal read GetPosition write SetPosition;
    property IsChanged: Boolean read GetChanged write SetChanged;
    property IsEmpty: Boolean read GetIsEmpty;
  end;

  IFieldDef = interface(IFieldAccess)
    ['{F002A5B1-6175-11D4-988B-00104B0FA1EF}']
    function GetName: String;
    function GetDataType: TDataFieldType;
    function GetSize: LongWord;
    procedure Unbind;
    procedure SetName(const Value: String);
    procedure SetSize(Value: LongWord);
    property Name: String read GetName write SetName;
    property DataType: TDataFieldType read GetDataType;
    property Size: LongWord read GetSize write SetSize;
  end;

implementation

end.
