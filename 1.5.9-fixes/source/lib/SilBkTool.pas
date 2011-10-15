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

unit SilBkTool;

{$I Defines.inc}

interface

type
  Tools = class of Tool;
  Tool = class
  private
    constructor Create; reintroduce; dynamic; abstract; 
  protected
    class function InitInstance(Instance: Pointer): TObject; dynamic; abstract;
    class function InheritsFrom(AClass: TClass): Boolean; dynamic; abstract;
    class function NewInstance: TObject; reintroduce; dynamic; abstract;
    class function GetInterfaceEntry(const IID: TGUID): PInterfaceEntry; dynamic; abstract;
    class function GetInterfaceTable: PInterfaceTable; dynamic; abstract;
    class function MethodAddress(const Name: ShortString): Pointer; dynamic; abstract;
    class function MethodName(Address: Pointer): ShortString; dynamic; abstract;
    class function InstanceSize: Longint; dynamic; abstract;
    class function ClassName: ShortString; dynamic; abstract;
    class function ClassNameIs(const Name: string): Boolean; dynamic; abstract;
    class function ClassParent: TClass; dynamic; abstract;
    class function ClassInfo: Pointer; dynamic; abstract;
    class procedure HideTheFuckingWarning;
  end;

implementation

{ Tool }

class procedure Tool.HideTheFuckingWarning;
begin
  Create;
end;

end.
