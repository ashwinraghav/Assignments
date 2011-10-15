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

unit SilLiParameters;

{$I Defines.inc}

interface

uses
  SilLiEnumerator;

type
  RParameter = record
    Name: string;
    Value: Variant;
  end;

  RParameters = array of RParameter;

  TMergePrecedence = (mkSource, mkDestination);

  IArguments = interface
    ['{FFD28BFA-A7B2-4ECF-A4A5-C68AAA836072}']
    function GetCount: Integer;
    function GetItem(const ID: string): Variant;
    function Find(const ID: string; out Value: Variant): Boolean;
    property Count: Integer read GetCount;
    property Items[const ID: string]: Variant read GetItem; default;
  end;

  IParameters = interface (IArguments)
    ['{5D06CBB7-C69F-4501-94B6-4343D0C32DDC}']
    function Contains(const ID: string): Boolean;
    function Enumerate(var Enum: IEnumerator; out Item: RParameter): Boolean;
    function Slice(const Name: string): IParameters;
    function Get(const Name: String; Default: Variant): Variant;
    function GetParameter(Index: Integer): RParameter;
    property Parameters[Index: Integer]: RParameter read GetParameter;
  end;

  IArgumentList = interface (IArguments)
    ['{31A625D6-BBD4-4439-A7D1-142AE2388E1E}']
    procedure PutItem(const ID: string; const Value: Variant);
    property Items[const ID: string]: Variant read GetItem write PutItem; default;
  end;

  IParameterList = interface (IParameters)
    ['{BF2491BA-5361-44A7-8F1A-4ED4DF5AEE58}']
    procedure PutItem(const ID: string; const Value: Variant);
    procedure Merge(const Source: IParameters; Precedence: TMergePrecedence = mkSource);
    function Remove(const ID: string): Integer;
    procedure Delete(Index: Integer);
    procedure Clear;
    property Items[const ID: string]: Variant read GetItem write PutItem; default;
  end;

  REvParametersItemEvent = record
    Sender: IParameterList;
    Item: RParameter;
  end;

  IParameterListEvents = interface
    ['{EBD7BF08-FD83-4C5E-8724-9189983B4192}']
    procedure OnItemAdded(const Event: REvParametersItemEvent);
    procedure OnItemChanged(const Event: REvParametersItemEvent);
  end;

implementation
end.
 