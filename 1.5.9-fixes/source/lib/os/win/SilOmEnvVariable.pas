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

unit SilOmEnvVariable;

{$I Defines.inc}

interface

uses
  SilLiField,
 
  SilOkEnvVariable;

type
  TSilWindowsEnvironmentVariable = class (TSilEnvironmentVariable)
  protected // IValueReader
    function GetAsString: String; override;
  protected // IValueAccess
    procedure SetAsString(const Value: String); override;
  end;

implementation

uses
  SilOfEnvironment;

{ TSilWindowsEnvironmentVariable }

function TSilWindowsEnvironmentVariable.GetAsString: String;
begin
  SilOfEnvironment.GetString(FName, Result);
end;

procedure TSilWindowsEnvironmentVariable.SetAsString(const Value: String);
begin
  SilOfEnvironment.SetString(FName, Value)
end;

end.
