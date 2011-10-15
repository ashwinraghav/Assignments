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

unit SilBeDebug;

{$INCLUDE Defines.inc}

interface

uses
  SilBcDebug,
  SilBeTypes;

type
  PDebug = ^RDebug;
  RDebug = {packed} object
    Level: TDebugLevel;
    Flags: TDebugFlags;
    Enabled: Boolean;
    function Check(ALevel: TDebugLevel {$IFDEF USE_DEFPARAMS} = 0 {$ENDIF}; AFlags: TDebugFlags {$IFDEF USE_DEFPARAMS} = CDebugAll{$ENDIF} ): Boolean;
  end;

implementation

function RDebug.Check(ALevel: TDebugLevel; AFlags: TDebugFlags): Boolean;
begin
  Result := Enabled and (ALevel <= Level) and ((AFlags = CDebugAll) or ((Flags and AFlags) = AFlags));
end;

end.
