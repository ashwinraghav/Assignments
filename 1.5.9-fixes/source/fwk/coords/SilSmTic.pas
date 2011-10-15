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

unit SilSmTic;

interface

uses
  Sil,
  SilMath;

type
  TTicData = class(
    TSilInterfacedObject,
    ITicData,
    ITicDataDef )
  private
    FPoint: WorldType;
    FKind: TTicKind;
    FCaption: string;
    FNext: WorldType;
  protected // ITicData
    function GetPoint: WorldType;
    function GetKind: TTicKind;
    function GetCaption: string;
  protected // ITicDataDef
    procedure SetPoint(const Value: WorldType);
    procedure SetKind(const Value: TTicKind);
    procedure SetCaption(const Value: string);
    function GetNext: WorldType;
    procedure SetNext(const Value: WorldType);
  public
    constructor Create;
  end;

implementation

{ TTicData }

constructor TTicData.Create;
begin
  inherited Create;
end;

function TTicData.GetPoint: WorldType;
begin
  Result := FPoint;
end;

function TTicData.GetKind: TTicKind;
begin
  Result := FKind;
end;

function TTicData.GetCaption: string;
begin
  Result := FCaption;
end;

function TTicData.GetNext: WorldType;
begin
  Result := FNext;
end;

procedure TTicData.SetCaption(const Value: string);
begin
  FCaption := Value;
end;

procedure TTicData.SetKind(const Value: TTicKind);
begin
  FKind := Value; 
end;

procedure TTicData.SetNext(const Value: WorldType);
begin
  FNext := Value;
end;

procedure TTicData.SetPoint(const Value: WorldType);
begin
  FPoint := Value;
end;

end.
 