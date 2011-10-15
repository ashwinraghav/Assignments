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

unit SilLhAggregation;

{$I Defines.inc}

interface

uses
  SilBeTypes,
  SilLiEnumerator,
  SilLkAggregable;

type
  IAggregation = interface
    ['{7DB5E6BC-434C-45C3-8499-0E466BF631C4}']
    function QueryInterface(const IID: TGUID; out Obj): Integer;
    function Add(const Contained: IUnknown; IsPublished: Boolean = True): IUnknown; overload;
    function Add(const Contained: IUnknown; const IID: TGUID; Field: PPointer; IsPublished: Boolean = True): IUnknown; overload;
    function Add(Contained: TSilAggregableObject; IsPublished: Boolean = True): IUnknown; overload;
    function Add(Contained: TSilAggregableObject; const IID: TGUID; Field: PPointer; IsPublished: Boolean = True): IUnknown; overload;
    function Add(Contained: TSilAggregableClass; const Owner: IUnknown = nil; IsPublished: Boolean = True; const Controller: IUnknown = nil; Param: Pointer = nil): IUnknown; overload;
    function Add(Contained: TSilAggregableClass; const IID: TGUID; Field: PPointer; const Owner: IUnknown = nil; IsPublished: Boolean = True; const Controller: IUnknown = nil; Param: Pointer = nil): IUnknown; overload;
    procedure Remove(Contained: TSilAggregableObject; Field: PPointer = nil); overload;
    procedure Remove(Contained: TSilAggregableClass; Field: PPointer = nil); overload;
    procedure Remove(const Contained: IUnknown; Field: PPointer = nil); overload;
    function Enumerate(var Enum: IEnumerator; const IID: TGUID; out Obj): Boolean;
    procedure Clear;
  end;

implementation
end.
 