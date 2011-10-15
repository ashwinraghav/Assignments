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

unit SilOtModule;

{$I Defines.inc}

interface

uses
  SilOiModule,
  SilOjModule;

type
  LinuxModuleTool = class(SilModuleTool)
  protected
    class function DoCreate(ID: Pointer; const MustFree: Boolean = False): IModule; override;
    class function DoCreate(ID: LongWord; const MustFree: Boolean = False): IModule; override;
    class function DoCreate(const ModuleName: string): IModule; override;
  public
    class function Name(ID: Pointer = nil): string; override;
    class function Name(ID: LongWord): string; override;
    class function Load(const FileName: string): IModule; override;
  end;

implementation

uses
  SilOfModule,
  SilOsClasses;

{ LinuxModuleTool }

{ LinuxModuleTool }

class function LinuxModuleTool.DoCreate(ID: LongWord; const MustFree: Boolean): IModule;
begin

end;

class function LinuxModuleTool.DoCreate(const ModuleName: string): IModule;
begin

end;

class function LinuxModuleTool.DoCreate(ID: Pointer; const MustFree: Boolean): IModule;
begin

end;

class function LinuxModuleTool.Load(const FileName: string): IModule;
begin

end;

class function LinuxModuleTool.Name(ID: LongWord): string;
begin

end;

class function LinuxModuleTool.Name(ID: Pointer): string;
begin

end;

end.
