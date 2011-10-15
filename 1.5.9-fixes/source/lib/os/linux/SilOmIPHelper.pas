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

unit SilOmIPHelper;

{$I Defines.inc}

interface

uses
  SilLkInterfaced,
  SilOiLibraryBinding,
  //SilOhSocket,
  SilOiIPHelper;

type
  TIPHelper = class (
    // extends
    TSilInterfacedObject,
    // implements
    IIPHelper)
  private
    FBinding: ILibraryBinding;
  protected // IIPHelper
    function DoGetIpAddrTable: TGetIpAddrTable;
  public
    constructor Create(const FileName: String);
  end;

implementation

uses
  SysUtils,
  SilOmLibraryBinding,
  SilOtTool;

{ TIPHelper }

constructor TIPHelper.Create(const FileName: String);
begin
  raise Exception.CreateFmt('%s: not implemented', ['TIPHelper.Create']);
(*)
  inherited Create;
  FBinding := OS.SharedLibrary.CreateBinding(FileName);
(*)end;

function TIPHelper.DoGetIpAddrTable: TGetIpAddrTable;
begin(*)
  FBinding.GetAddress('GetIpAddrTable', 0, Result);
(*)end;

end.
 