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

unit SilLkSingleton;

{$I Defines.inc}

interface

uses
  SilLkObject,
  SilLkInterfaced,
  SilLiStringList,
  SilLiGlobalServices;

type
  TSingletonObject = class;

  ISingletonList = interface
    ['{5AF871F6-54E8-4783-BFC8-E8437FA6D70F}']
    function Find(const ClassName: string; out Instance: TSingletonObject): Boolean;
    procedure Add(const Instance: TSingletonObject);
    procedure Remove(const Instance: TSingletonObject);
  end;

  TSingletonObject = class(TSilInterfacedObject)
  private
    FUsageCount: Integer;
  public
    class function NewInstance: TObject; override;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure FreeInstance; override;
  end;

implementation

uses
  SilOtTool,
  SilLfSingleton;

{ TSingletonObject }

class function TSingletonObject.NewInstance: TObject;
var
  Obj: TSingletonObject;
begin
  if not List.Find(ClassName, Obj) then
  begin
    Obj := inherited NewInstance as TSingletonObject;
    List.Add(Obj);
  end;
  Os.Locked.Increment(Obj.FUsageCount);
  Result := Obj;
end;

procedure TSingletonObject.FreeInstance;
begin
  if Os.Locked.Decrement(FUsageCount) = 0 then
  begin
    List.Remove(Self);
    inherited;
  end;
end;

procedure TSingletonObject.AfterConstruction;
begin
  if FUsageCount = 1 then
    inherited;
end;

procedure TSingletonObject.BeforeDestruction;
begin
  if FUsageCount = 1 then
    inherited;
end;

end.
