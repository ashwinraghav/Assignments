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

unit SilOjEnvironment;

{$INCLUDE Defines.inc}

interface

uses
  SilBkTool,
  
  SilLiField,
  SilLiValueList,

  SilLiKey,
  SilOiEnvironment;

type
  SilEnvironment = class(Tool)
    class function Variable(const Name: String): IFieldAccess; virtual; abstract;
    class function Variables: IValueList; virtual; abstract;
    class function Expand(const Value: String; const Default: String = ''; const Name: String = ''): String; virtual; abstract;
    class function GetValue(const Name: String): Variant; virtual; abstract;
    class procedure SetValue(const Name: String; const Value: Variant); virtual; abstract;
    class function List: IEnvironmentList; virtual; abstract;
    class procedure Load(const Key: INamedKey); overload; virtual; abstract;      
    class procedure Load(const Key: string); overload;
  end;

implementation

uses
  SilOtTool,
  SilLiEnumerator;

{ SilEnvironment }

class procedure SilEnvironment.Load(const Key: string);
begin
  Load(Os.Registry.Open(Key, True));
end;

end.




