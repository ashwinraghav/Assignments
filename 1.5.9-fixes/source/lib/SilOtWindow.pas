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

unit SilOtWindow;

{$INCLUDE Defines.inc}

interface

uses
  SilBkTool,
  
  SilOiHandle,
  SilOiWindow;

type // API de manejo de ventanas
  OSWindowClass = class of Window;

  Window = class(Tool)
    class procedure Destroy(const AWindow: IHandle); reintroduce; {$IFDEF USE_DEPRECATED} deprecated; {$ENDIF}
  end;

  OSToolWindowClass = class of ToolWindow;

  ToolWindow = class(Tool)
    class function Create(Method: TWindowCallback): IHandle;
  end;

implementation

uses
  SilOsClasses;

{$IFDEF USE_DEPRECATED}
{$WARN SYMBOL_DEPRECATED OFF}
{$ENDIF}

class procedure Window.Destroy(const AWindow: IHandle);
begin
//if AWindow.IsValid then Tool.DestroyWindow(AWindow.Value);
end;

{$IFDEF USE_DEPRECATED}
{$WARN SYMBOL_DEPRECATED ON}
{$ENDIF}

{function EnumFunc(hWnd: THandle; lParam: Cardinal): Boolean; safecall;
begin
  IHandleList(lParam).Add(Handle.Create(hWnd));
end;}

{class function Window.GetList: IHandleList;
begin
  Result := Handle.CreateList;
  EnumWindows(@EnumFunc, LParam(Result));
end;}

class function ToolWindow.Create(Method: TWindowCallback): IHandle;
begin
  Result := TSilOsToolWindow.Create(Method);
end;

end.
