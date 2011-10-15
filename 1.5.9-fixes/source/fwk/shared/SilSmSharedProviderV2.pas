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

unit SilSmSharedProviderV2;

{$INCLUDE Defines.inc}

interface

uses
  Sil,
  SilSiSharedObject,
  SilShSharedObject,
  SilSmSharedProvider;

type
  TSilObjectProviderV2 = class(
    TSilObjectProvider )
  private
    FServices: IGlobalServicesV2;
    FFactories: ISharedFactoryList;
    procedure DoSetup(const Services: IGlobalServicesV2); 
  protected
    function Get(const ID: ISharedID): ISharedFactory; override;
    function Find(const ID: ISharedID; out Factory: ISharedFactory): Boolean; override;
    function Enumerate(var Enum: IEnumerator; out Factory: ISharedFactory): Boolean; override;
  public
    constructor CreateNew(const Functions: ISharedObjectFunctions); overload; override;
    constructor CreateNew(const Services: IGlobalServicesV2); overload;
    destructor Destroy; override;
    class function Check(const Functions: ISharedObjectFunctions): Boolean; override;
  end;

implementation

{ TSilObjectProviderV2 }

class function TSilObjectProviderV2.Check(const Functions: ISharedObjectFunctions): Boolean;
begin
  Result := (Functions.Rtl.Major = 2)
        and (epLocalServices in Functions.Support);
end;

constructor TSilObjectProviderV2.CreateNew(const Services: IGlobalServicesV2); 
begin
  inherited CreateNew;
  DoSetup(Services);
end;

constructor TSilObjectProviderV2.CreateNew(const Functions: ISharedObjectFunctions);
begin
  inherited;
  DoSetup(Functions.LocalServices() as IGlobalServicesV2);
end;

destructor TSilObjectProviderV2.Destroy;
begin
  try
    FServices.Release(GsSharedFactoryList, @FFactories, System.FindHInstance(ClassType));
  finally
    FServices := nil;
    inherited;
  end;
end;

procedure TSilObjectProviderV2.DoSetup(const Services: IGlobalServicesV2);
begin
  FServices := Services;
  FServices.Get(GsSharedFactoryList, ISharedFactoryList, @FFactories, System.FindHInstance(ClassType));
end;

function TSilObjectProviderV2.Enumerate(var Enum: IEnumerator; out Factory: ISharedFactory): Boolean;
begin
  Result := Assigned(FFactories) and FFactories.Enumerate(Enum, Factory);
end;

function TSilObjectProviderV2.Find(const ID: ISharedID; out Factory: ISharedFactory): Boolean;
begin
  case ID.Kind of
    idName:   Result := Assigned(FFactories) and FFactories.Find(ID.AsName.ProgID, Factory);
    idClass:  Result := Assigned(FFactories) and FFactories.Find(ID.AsClass.ClassID, Factory);
    else      Result := False;
  end;
end;

function TSilObjectProviderV2.Get(const ID: ISharedID): ISharedFactory;
begin
  if Assigned(FFactories) then
    case ID.Kind of
      idName:   Result := FFactories.Get(ID.AsName.ProgID);
      idClass:  Result := FFactories.Get(ID.AsClass.ClassID);
      else      raise Sil.Error.Create('ID INCORRECTO O NO RECONOCIBLE');
    end
  else
    raise Sil.Error.Create('LA DLL NO EXPORTO EL GLOBAL FACTORY LIST');
end;

initialization
  TSilObjectProvider.Register(TSilObjectProviderV2);

end.
