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

unit SilOmTimer;

{$I Defines.inc}

interface

uses
  SilOiHandle,
  SilOkTimer;

type
  TSilLinuxTimer = class (TSilTimer)
  private
    FHandle: IHandle;
    procedure DoWndCallback(var Message);
    function DoGetWnd: LongWord;
  protected //- TSilTimer
    procedure DoSetTimer; override;
    procedure DoKillTimer; override;
  public
    constructor Create(const ID, Interval: Cardinal; const AHandle: IHandle; Enabled: Boolean = true); overload;
    constructor Create(const ID, Interval: Cardinal; Enabled: Boolean = true); overload; override;
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  SilOtTool;

{ TSilLinuxTimer }

constructor TSilLinuxTimer.Create(const ID, Interval: Cardinal; Enabled: Boolean);
begin
  raise Exception.CreateFmt('%s: not implemented', ['TSilLinuxTimer.Create']);
(*)
  Create(ID, Interval, OS.ToolWindow.Create(DoWndCallback), Enabled);
(*)end;

constructor TSilLinuxTimer.Create(const ID, Interval: Cardinal; const AHandle: IHandle; Enabled: Boolean);
begin
  raise Exception.CreateFmt('%s: not implemented', ['TSilLinuxTimer.Create']);
(*)
  inherited Create(ID, Interval, Enabled);
  FHandle := AHandle;
(*)end;

destructor TSilLinuxTimer.Destroy;
begin(*)
  FHandle := nil;
  inherited;
(*)end;

procedure TSilLinuxTimer.DoWndCallback(var Message);
begin(*)
  with TMessage(Message) do
    if Msg = WM_TIMER then
      Self.Tick()
    else if FHandle <> nil then
      DefWindowProc(FHandle.Value, Msg, wParam, lParam);
(*)end;

procedure TSilLinuxTimer.DoKillTimer;
begin(*)
  Linux.KillTimer(DoGetWnd(), Self.ID);
(*)end;

procedure TSilLinuxTimer.DoSetTimer;
begin(*)
  Linux.SetTimer(DoGetWnd(), Self.ID, Self.Interval, nil);
(*)end;

function TSilLinuxTimer.DoGetWnd: LongWord;
begin(*)
  if FHandle <> nil then
    Result := FHandle.Value else
    Result := 0;
(*)end;

end.
