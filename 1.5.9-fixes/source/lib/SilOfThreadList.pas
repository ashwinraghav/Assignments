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

unit SilOfThreadList;

{$I Defines.inc}

interface

uses
  SilOiThread;

procedure AddThread(const Thread: IThread);
procedure RemoveThread(const Thread: IThread);
procedure CheckList;
function GetList: IThreadList;

implementation

uses
  SilLtGlobal,
  SilOvThreadList;

var
  MList: IThreadList = nil;

procedure CheckList;
begin
  if not Assigned(MList) then
    Global.List.Get(SilOvThreadList.Service, IThreadList, @MList, SysInit.HInstance);
end;

function GetList: IThreadList;
begin
  CheckList;
  Result := MList;
end;

procedure AddThread(const Thread: IThread);
begin
  CheckList;
  MList.Add(Thread);
end;

procedure RemoveThread(const Thread: IThread);
var
  List: IThreadList;
begin
  List := MList;
  if List <> nil then
    try
      List.Lock;
      List.Remove(Thread);
    finally
      List.Unlock;
    end;
end;

initialization
  CheckList;

finalization
  if Assigned(MList) then
    Global.List.Release(SilOvThreadList.Service, @MList, SysInit.HInstance);
  
end.
