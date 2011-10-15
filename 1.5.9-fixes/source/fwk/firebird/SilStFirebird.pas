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

unit SilStFirebird;

{$INCLUDE Defines.inc}

interface

uses
  Sil,
  SilSiFirebird;

type
  Firebird = class (Tool)
    class function Application: IFbApplication;
    class function Bindings(const Variables: IFbVariables): IFbBindings; overload;
    class function Bindings(const Session: IFbSession): IFbBindings; overload;
    class function Buffer(const Command: IFbCommand; const Bindings: IFbBindings): IFbBuffer; overload;
    class function Buffer(const Command: IFbCommand; const Bindings: IFbBindings; const Values: IParameterList): IFbBuffer; overload;
  end;

implementation

uses
  SilShFirebird,
  SilSfFirebird,
  SilSmFirebirdApplication,
  SilSmFirebirdBindings,
  SilSmFirebirdBuffer;

{ Firebird }

class function Firebird.Application: IFbApplication;
begin
  IFbApplicationInternal(Result) := TSilFirebirdApplication.Create();
end;

class function Firebird.Bindings(const Variables: IFbVariables): IFbBindings;
begin
  Result := SilSfFirebird.Bindings(IFbVariablesInternal(Variables));
end;

class function Firebird.Bindings(const Session: IFbSession): IFbBindings;
begin
  Result := SilSfFirebird.Bindings(IFbSessionInternal(Session));
end;

class function Firebird.Buffer(const Command: IFbCommand; const Bindings: IFbBindings): IFbBuffer;
begin
  Result := SilSfFirebird.Buffer(IFbCommandInternal(Command), IFbBindingsInternal(Bindings));
end;

class function Firebird.Buffer(const Command: IFbCommand; const Bindings: IFbBindings; const Values: IParameterList): IFbBuffer;
begin
  Result := SilSfFirebird.Buffer(IFbCommandInternal(Command), IFbBindingsInternal(Bindings), Values);
end;

end.
 