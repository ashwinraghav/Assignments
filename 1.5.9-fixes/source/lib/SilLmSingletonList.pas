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

unit SilLmSingletonList;

{$I Defines.inc}

interface

uses
  SilLkSingleton,
  SilLmStringList;
  
type
  TSingletonList = class(
    TSilStringList,
    ISingletonList )
  protected //- ISingletonList
    function Find(const ClassName: string; out Instance: TSingletonObject): Boolean;
    procedure Add(const Instance: TSingletonObject); reintroduce;
    procedure Remove(const Instance: TSingletonObject);
  end;

implementation

{ TSingletonList }

function TSingletonList.Find(const ClassName: string; out Instance: TSingletonObject): Boolean;
var
  Index: Integer;
begin
  Index := IndexOf(ClassName);
  Result := ValidIndex(Index);
  if Result then
    Instance := GetPtr(Index);
end;

procedure TSingletonList.Add(const Instance: TSingletonObject);
begin
  inherited Add(Instance.ClassName, Instance);
end;

procedure TSingletonList.Remove(const Instance: TSingletonObject);
var
  Index: Integer;
begin
  Index := IndexOf(Instance.ClassName);
  if ValidIndex(Index) then
    Delete(Index);
end;

end.
 