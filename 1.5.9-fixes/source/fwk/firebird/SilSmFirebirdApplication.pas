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

unit SilSmFirebirdApplication;

{$INCLUDE Defines.inc}

interface

uses
  Sil,
  SilSiFirebird,
  SilShFirebird;

type
  TSilFirebirdApplication = class(
    TSilObject,
    IFbApplicationInternal )
  private
    FParameters: IFbParametersInternal;
  private
    function DoBuildConnectionString(const Server, Resource: string): string;
  protected // IFbApplication
    function GetProperties: IFbParameters;
    function Connect(const Database: string; const Parameters: IParameterList = nil): IFbSession; overload;
    function Connect(const HostName, Database: string; const Parameters: IParameterList = nil): IFbSession; overload;
    function ServiceManager(const HostName: string; const Parameters: IParameterList = nil): IFbServiceManager;
  protected // IFbApplicationInternal
    function DoGetProperties: IFbParametersInternal;
    function GetParameters(const List: array of PFbParameterInfo; const Block: IFbParamBlock): IFbParametersInternal;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  SilScFirebird,
  SilSfFirebirdClient,
  SilSmFirebirdSession,
  SilSmFirebirdServiceManager,
  SilSmFirebirdParameters, SilSgFirebirdParameters;

{ TSilFirebirdApplication }

constructor TSilFirebirdApplication.Create;
begin
  inherited Create;
  fb.initialize;
end;

destructor TSilFirebirdApplication.Destroy;
begin
  fb.finalize;
  inherited;
end;

function TSilFirebirdApplication.GetProperties: IFbParameters;
begin
  Result := DoGetProperties;
end;

function TSilFirebirdApplication.Connect(const Database: string; const Parameters: IParameterList): IFbSession;
begin
  Result := IFbSessionInternal(TSilFirebirdSession.Create(Self, Database, Parameters));
end;

function TSilFirebirdApplication.Connect(const HostName, Database: string; const Parameters: IParameterList): IFbSession;
begin
  Result := Connect(DoBuildConnectionString(HostName, Database), Parameters);
end;

function TSilFirebirdApplication.ServiceManager(const HostName: string; const Parameters: IParameterList): IFbServiceManager;
begin
  Result := TSilFirebirdServiceManager.Create(Self, DoBuildConnectionString(HostName, 'service_mgr'), Parameters);
end;

function TSilFirebirdApplication.DoGetProperties: IFbParametersInternal;
begin
  if not Assigned(FParameters) then FParameters := GetParameters(GFbDatabaseParameterBlock, fb.block(TSilFirebirdParameterBlock));
  Result := FParameters;
end;

function TSilFirebirdApplication.GetParameters(const List: array of PFbParameterInfo; const Block: IFbParamBlock): IFbParametersInternal;
begin
  Result := TSilFirebirdParameters.Create(List, Block);
end;

function TSilFirebirdApplication.DoBuildConnectionString(const Server, Resource: string): string;
begin
  if Sil.Str.IsAssigned(Server) then Result := Server;
  Sil.Str.Add(Result, Resource, ':');
end;

end.
