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

unit SilOmProcessList;

{$I Defines.inc}

interface

uses
  SilOiProcess,
  SilLiEnumerator,
  SilLmInterfaceList;

type
  TSilProcessList = class (
    // extends
    TSilInterfaceList,
    // implements
    IProcessList)
  protected // IProcessList
    function Get(Index: Integer): IProcess;
    procedure Put(Index: Integer; const Value: IProcess);
    function Enumerate(var Enum: IEnumerator; out Item: IProcess): Boolean; reintroduce;
    function Add(const Value: IProcess): Integer; reintroduce;
    procedure Insert(Index: Integer; const Value: IProcess); reintroduce;
  public
    constructor Create(Locked: Boolean);
  end;

implementation

{ TSilProcessList }

constructor TSilProcessList.Create(Locked: Boolean);
begin
  inherited Create(Locked);
end;

function TSilProcessList.Add(const Value: IProcess): Integer;
begin
  Result := inherited Add(Value);
end;

function TSilProcessList.Enumerate(var Enum: IEnumerator; out Item: IProcess): Boolean;
begin
  Result := inherited Enumerate(Enum, Item);
end;

function TSilProcessList.Get(Index: Integer): IProcess;
begin
  Result := IProcess(GetItem(Index));
end;

procedure TSilProcessList.Insert(Index: Integer; const Value: IProcess);
begin
  inherited Insert(Index, Value);
end;

procedure TSilProcessList.Put(Index: Integer; const Value: IProcess);
begin
  inherited SetItem(Index, Value);
end;

end.
 