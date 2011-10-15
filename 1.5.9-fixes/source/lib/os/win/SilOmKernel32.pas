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

unit SilOmKernel32;

{$I Defines.inc}

interface

uses
  SilLkInterfaced,
  SilOiSharedLibrary,
  SilOhKernel32,
  SilOiKernel32;

type
  TKernel32 = class (
    // extends
    TSilInterfacedObject,
    // implements
    IKernel32)
  private
    FLib: ISharedLibrary;
  protected // IKernel32
    function DoCreateToolhelp32Snapshot: TCreateToolhelp32Snapshot;
    function DoProcess32First: TProcess32First;
    function DoProcess32Next: TProcess32Next;
  public
    constructor Create(const FileName: String);
  end;

implementation

uses
  SilOtTool;

{ TKernel32 }

constructor TKernel32.Create(const FileName: String);
begin
  inherited Create;
  FLib := OS.SharedLibrary.Load(FileName);
end;

function TKernel32.DoCreateToolhelp32Snapshot: TCreateToolhelp32Snapshot;
begin
  FLib.Bind('CreateToolhelp32Snapshot', 0, Result, true);
end;

function TKernel32.DoProcess32First: TProcess32First;
begin
  FLib.Bind('Process32First', 1, Result, true);
end;

function TKernel32.DoProcess32Next: TProcess32Next;
begin
  FLib.Bind('Process32Next', 2, Result, true);
end;

end.
