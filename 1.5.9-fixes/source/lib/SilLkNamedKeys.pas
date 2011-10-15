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

unit SilLkNamedKeys;

{$I Defines.inc}

interface

uses
  SilLkNamedItems,
  SilLiKey,
  SilLiLock,
  SilLiEnumerator;

type
  TSilNamedKeys = class (
    TSilNamedItems,
    INamedKeys )
  protected //- INamedKeys
    function Add(const Name: String): INamedKey; overload;
    function Add(const Name: String; Permision: TNamedKeyPermision): INamedKey; overload; 
    function Exists(const Name: String): Boolean; 
    function Find(const Name: String; out Key: INamedKey): Boolean;
    function Get(const Name: String; CanCreate: Boolean = false): INamedKey; overload;
    function Get(const Name: String; Permision: TNamedKeyPermision; CanCreate: Boolean = false): INamedKey; overload; 
  protected
    function DoExists(const Name: String): Boolean; virtual; abstract; 
    function DoAdd(const Name: String; Permision: TNamedKeyPermision): INamedKey; virtual; abstract;
    function DoGet(const Name: String; Permision: TNamedKeyPermision; CanCreate: Boolean = false; RaiseError: Boolean = True): INamedKey; overload; virtual; abstract;
    function DoFind(const Name: String; out Key: INamedKey): Boolean; virtual; abstract;
  end;

implementation

{ TSilNamedKeys }

function TSilNamedKeys.Add(const Name: String): INamedKey;
begin
  Result := DoAdd(Name, kpReadWrite);
end;

function TSilNamedKeys.Add(const Name: String; Permision: TNamedKeyPermision): INamedKey;
begin
  Result := DoAdd(Name, Permision);
end;

function TSilNamedKeys.Get(const Name: String; CanCreate: Boolean): INamedKey;
begin
  Result := DoGet(Name, kpReadWrite, CanCreate, True);
end;

function TSilNamedKeys.Get(const Name: String; Permision: TNamedKeyPermision; CanCreate: Boolean): INamedKey;
begin
  Result := DoGet(Name, Permision, CanCreate, True);
end;

function TSilNamedKeys.Exists(const Name: String): Boolean;
begin
  Result := DoExists(Name);
end;

function TSilNamedKeys.Find(const Name: String; out Key: INamedKey): Boolean;
begin
  Result := DoFind(Name, Key);
end;

end.
