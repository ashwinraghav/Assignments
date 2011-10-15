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

unit SilOmEnvVariables;

{$I Defines.inc}

interface

uses
  SilLiEnumerator,
  SilLiParameters,

  SilOiEnvironment,
  SilOkEnvVariables;

type
  TSilLinuxEnvironmentVariables = class (TSilEnvironmentVariables)
  private
    procedure DoAddItem(const Context; const Name: string); virtual;
  protected //- IParameters
    function GetItem(const ID: string): Variant; override;
  protected //- IEnvironmentList
    procedure PutItem(const ID: string; const Value: Variant); override;
  public
    constructor Create; override;
  end;

implementation

uses
  SilOfEnvironment;

{ TSilLinuxEnvironmentVariables }

constructor TSilLinuxEnvironmentVariables.Create;
var
  Buffer: PChar;
begin
  inherited Create;
  if SilOfEnvironment.GetStrings(Buffer) then
  try
    SilOfEnvironment.ParseStrings(Self, Buffer, DoAddItem);
  finally
    SilOfEnvironment.FreeStrings(Buffer);
  end;
end;

function TSilLinuxEnvironmentVariables.GetItem(const ID: string): Variant;
begin
  SilOfEnvironment.GetValue(ID, Result);
end;

procedure TSilLinuxEnvironmentVariables.PutItem(const ID: string; const Value: Variant);
begin
  SilOfEnvironment.SetValue(ID, Value);
end;

procedure TSilLinuxEnvironmentVariables.DoAddItem(const Context; const Name: string);
begin
  inherited Add(Name, nil);
end;

end.
