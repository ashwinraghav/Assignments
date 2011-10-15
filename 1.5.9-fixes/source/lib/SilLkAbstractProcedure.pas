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

unit SilLkAbstractProcedure;

{$I Defines.inc}

interface

uses
  SilBeTypes,
  SilLkInterfaced;

type
  TAbstractProcedure = class(TSilInterfacedObject)
  protected
    FProcedure: Pointer;
  protected
    function Invoke(const Controller, Item, Args: Pointer): LongWord; virtual; register; abstract;
  protected
    constructor Create(const AProcedure: Pointer);
    function Execute(const Controller: IUnknown; const Item: Pointer; const Args: Pointer): LongWord;
    property ProcedurePtr: Pointer read FProcedure;
  end;

implementation

//////////////////////////////////////////////////////////////////////////////////////////////////

constructor TAbstractProcedure.Create(const AProcedure: Pointer);
begin
  inherited Create;
  FProcedure := AProcedure;
end;

function TAbstractProcedure.Execute(const Controller: IUnknown; const Item: Pointer; const Args: Pointer): LongWord;
begin
  Result := Invoke(Pointer(Controller), Item, Args);
end;

end.
