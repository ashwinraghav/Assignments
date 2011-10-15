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

unit SilViProfiler;

{$I Defines.inc}

interface

uses
  Classes,
  Controls;

type

{ IProfileSource }

  IProfileSource = interface
    ['{5973C061-E606-11D3-986E-00104B0FA1EF}']
    function SourceName: String;
    function BeginRead(const Name: String): Boolean;
    procedure EndRead(const Name: String);
    function HasMore: Boolean;
    procedure ReadNext;
    function ItemIsOwner: Boolean;
    function ItemIsObject: Boolean;
    function ItemIsValue: Boolean;
    function ItemName: String;
    function PropName: String;
    function PropValue: String;
    procedure NotifyOnChanges(Enable: Boolean);
  end;

{ IProfiler }  

  IProfiler = interface
    ['{5973C062-E606-11D3-986E-00104B0FA1EF}']
    function GetSource: IProfileSource;
    procedure SetSource(const Value: IProfileSource);
    function Update(const Owner: String = ''): Boolean; overload;
    function Value(const Name: String): String;
    function OrdValue(Obj: TPersistent; const PropName, ValueName: String): Boolean;
    procedure CreateForm(InstanceClass: TComponentClass; var Reference; Parent: TWinControl); overload;
    procedure CreateForm(InstanceClass: TComponentClass; var Reference); overload;
    procedure CreateForm(InstanceClass: TComponentClass; var Reference; const Name: String); overload;
    procedure CreateForm(InstanceClass: TComponentClass; var Reference; const Name: String; Parent: TWinControl); overload;
    procedure CreateForm(InstanceClass: TComponentClass); overload;
    property Source: IProfileSource read GetSource write SetSource;
  end;

implementation

end.
 