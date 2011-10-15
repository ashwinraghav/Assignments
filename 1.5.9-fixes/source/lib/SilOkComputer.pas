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

unit SilOkComputer;

{$I Defines.inc}

interface

uses
  SilBeTypes,
  SilLiInterfaceList,
  SilLkObject,
  SilOiVersion,
  SilOiComputer;

type
  TSilComputer = class(
    TSilObject,
    IComputer )
  protected // IComputer
    function GetName: string; virtual; abstract;
    function GetDomain: IComputerDomain; virtual; abstract;     
    function GetDescription: string; virtual; abstract; 
    function GetKinds: TComputerKinds; virtual; abstract;
    function GetOS: IVersionInfo; virtual; abstract;
    function GetMACs: IInterfaceList; virtual; abstract;
  public
    constructor Create; overload; virtual;
    constructor Create(const Name: string); overload; virtual; 
  public
    property Name: string read GetName;
    property Description: string read GetDescription;
  end;

implementation

{ TSilComputer }

{ TSilComputer }

constructor TSilComputer.Create;
begin
  inherited;
end;

constructor TSilComputer.Create(const Name: string);
begin
  Create;
end;

end.
