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

unit SilStFirebirdClient;

{$INCLUDE Defines.inc}

interface

uses
  Sil,
  SilShFirebird,
  SilScFirebirdClient,
  SilSeFirebirdClient,
  SilSiFirebirdClient,
  SilSjFirebirdClient;

type
  Firebird = class(FirebirdClientTool)
    class procedure initialize(ID: Pointer = nil); override;
    class procedure finalize(ID: Pointer = nil); override;
    class function status: PISC_STATUS; override;
    class function vector: PStatusVector; override;
    class function api: IFbClient; override;
    class function block(Kind: TSilFirebirdBlockClass): IFbParamBlock; override; 
  end;

implementation

uses
  SilSvFirebirdClient;

var
  MInitCount: Integer = 0;
  MInstance: IFbClient = nil;

threadvar
  MStatusVector: TStatusVector;

{ Firebird }

class procedure Firebird.initialize(ID: Pointer);
begin
  if ID = nil then ID := Self;
  if Sil.Int.Inc(MInitCount, True) = 0 then
    Sil.Global.List.Get(SilSvFirebirdClient.Service, IFbClient, @MInstance, FindHInstance(ID));
end;

class procedure Firebird.finalize(ID: Pointer);
begin
  if ID = nil then ID := Self;
  if Sil.Int.Dec(MInitCount) = 0 then
    Sil.Global.List.Release(SilSvFirebirdClient.Service, @MInstance, FindHInstance(ID));
end;

class function Firebird.status: PISC_STATUS;
begin
  Result := @MStatusVector[0]
end;

class function Firebird.vector: PStatusVector;
begin
  Result := @MStatusVector;
end;

class function Firebird.api: IFbClient;
begin
  if not Assigned(MInstance) then initialize();
  Result := MInstance;
end;

class function Firebird.block(Kind: TSilFirebirdBlockClass): IFbParamBlock;
var
  Instance: IUnknown;
begin
  Instance := Kind.Create(0);
  Error.Check(Instance.QueryInterface(IFbParamBlock, Result) = 0, 'Interface not supported');
end;

initialization

finalization
  if MInitCount > 0 then
  begin
    MInitCount := 1;
    Firebird.finalize;
  end;
end.
