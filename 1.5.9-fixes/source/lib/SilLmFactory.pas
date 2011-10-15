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

unit SilLmFactory;

{$I Defines.inc}

interface

uses
  SilBeTypes,
  SilLkFactory,
  SilLjFactory;

type
  TSilToolFactory = class(TSilFactory)
  private
    FTool: FactoryType;
  protected // IFactory
    function DoCreate(ClassType: TClass; const Owner: IUnknown = nil; const Controller: IUnknown = nil; Param: Pointer = nil): IUnknown; override; 
  public
    constructor Create(const Tool: FactoryType);  
    destructor Destroy; override; 
  end;

implementation

{ TSilFactory }

constructor TSilToolFactory.Create(const Tool: FactoryType);
begin
  inherited Create;
  FTool := Tool;
end;

destructor TSilToolFactory.Destroy;
begin
  FTool := nil;
  inherited;
end;

function TSilToolFactory.DoCreate(ClassType: TClass; const Owner, Controller: IInterface; Param: Pointer): IUnknown;
begin
  ASSERT(FTool <> nil, 'No tengo Factory asignado!!!');
  Result := FTool.Create(Self, ClassType, Owner, Controller, Param);
end;

end.
 