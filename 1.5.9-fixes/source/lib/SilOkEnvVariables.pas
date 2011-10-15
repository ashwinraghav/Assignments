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

unit SilOkEnvVariables;

{$I Defines.inc}

interface

uses
  SilLmStringList,

  SilLiEnumerator,
  SilLiParameters,
  
  SilOiEnvironment;

type
  TSilEnvironmentVariables = class(
    TSilStringList,
    IParameters,
    IEnvironmentList )
  protected //- IParameters
    function GetItem(const ID: string): Variant; virtual; abstract;
    function Find(const ID: string; out Value: Variant): Boolean; virtual; abstract;
    function Contains(const ID: string): Boolean; virtual;
    function Enumerate(var Enum: IEnumerator; out Item: RParameter): Boolean; reintroduce;
    function Slice(const Name: string): IParameters;
    function Get(const Name: String; Default: Variant): Variant;
    function GetParameter(Index: Integer): RParameter;
  protected //- IEnvironmentList
    procedure PutItem(const ID: string; const Value: Variant); virtual; abstract;
    function Remove(const ID: string): Integer; virtual; abstract;
  public
    constructor Create; overload; virtual;
  end;

implementation

uses
  SilBtError;

{ TSilEnvironmentVariables }

constructor TSilEnvironmentVariables.Create;
begin
  inherited Create;
end;

function TSilEnvironmentVariables.Enumerate(var Enum: IEnumerator; out Item: RParameter): Boolean;
var
  Name: string;
begin
  Result := inherited Enumerate(Enum, Name);
  if Result then
    begin
      Item.Name := Name;
      Item.Value := GetItem(Name);
    end;
end;

function TSilEnvironmentVariables.Contains(const ID: string): Boolean;
var
  Dummy: Variant;
begin
  Result := Find(ID, Dummy);
end;

function TSilEnvironmentVariables.Slice(const Name: string): IParameters;
begin
  raise Error.Create('No implementado');
end;

function TSilEnvironmentVariables.Get(const Name: String; Default: Variant): Variant;
begin
  if not Find(Name, Result) then
    Result := Default;
end;

function TSilEnvironmentVariables.GetParameter(Index: Integer): RParameter;
begin
  raise Error.Create('No implementado');
end;

end.
