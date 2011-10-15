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

unit SilLtAction;

{$I Defines.inc}

interface

uses
  SilLiAction,
  SilBkTool;

type
  IAction = SilLiAction.IAction;

type
  NestedAction = class(Tool)
    class function Create(const Action: Pointer): IAction;
  end;

  NestedTester = class(Tool)
    class function Create(const Tester: Pointer): ITester;
  end;

implementation

uses
  SilLkNestedAction,
  SilLkNestedTester;

{ NestedAction }

class function NestedAction.Create(const Action: Pointer): IAction;
begin
  Result := TNestedAction.Create(Action, TNestedAction.GetCallerFrame);
end;

{ NestedTester }

class function NestedTester.Create(const Tester: Pointer): ITester;
begin
  Result := TNestedTest.Create(Tester, TNestedTest.GetCallerFrame);
end;

end.
