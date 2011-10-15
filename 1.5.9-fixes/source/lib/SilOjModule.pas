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

unit SilOjModule;

{$I Defines.inc}

interface

uses
  SilBkTool,
  SilOiModule;

type
  SilModuleTool = class(Tool)
  protected
    class function DoCreate(ID: Pointer; const MustFree: Boolean = False): IModule; overload; virtual; abstract;
    class function DoCreate(ID: LongWord; const MustFree: Boolean = False): IModule; overload; virtual; abstract;
    class function DoCreate(const ModuleName: string): IModule; overload; virtual; abstract;
  public
    class function Current: IModule;
    class function Get(ID: Pointer): IModule; overload;
    class function Get(ID: LongWord): IModule; overload;
    class function Get(const ModuleName: string): IModule; overload;
    class function Name(ID: Pointer = nil): string; overload; virtual; abstract;
    class function Name(ID: LongWord): string; overload; virtual; abstract;
    class function Load(const FileName: string): IModule; overload; virtual; abstract;
  end;

implementation

var
  MCurrentModule: IModule = nil;

{ Module }

class function SilModuleTool.Current: IModule;
begin
  if MCurrentModule = nil then MCurrentModule := DoCreate(SysInit.HInstance);
  Result := MCurrentModule;
end;

class function SilModuleTool.Get(ID: Pointer): IModule;
begin
  Result := DoCreate(ID);
end;

class function SilModuleTool.Get(ID: LongWord): IModule;
begin
  Result := DoCreate(ID);
end;

class function SilModuleTool.Get(const ModuleName: string): IModule;
begin
  Result := DoCreate(ModuleName);
end;

end.
