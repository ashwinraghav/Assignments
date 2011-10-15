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

unit SilOtComputer;

{$I Defines.inc}

interface

uses
  SilBeTypes,
  SilOiComputer,
  SilOjComputer;

type
  LinuxComputerTool = class(ComputerTool)
    class function Local: ILocalComputer; override;
    class function Remote(const Name: string): IComputer; override;
    class function KindToStr(const Kind: TComputerKinds): string; override;
    class function KindToStr(const Kind: TDomainKind): string; override;
    class function MacToStr(const Address: LargeInt): string; override;
    class function Shutdown(const Flags: TShutdownFlags): Boolean; override;
  end;

implementation

uses
  SysUtils,
  SilBtTypeInfo,
  SilOsClasses,
  SilOfComputer,
  SilOfProcess,
  SilOtProcess,
  SilOjProcess;

{ LinuxComputerTool }

class function LinuxComputerTool.Local: ILocalComputer;
begin
  Result := TSilOsLocalComputer.Create;
end;

class function LinuxComputerTool.Remote(const Name: string): IComputer;
begin
  Result := TSilOsRemoteComputer.Create(Name);
end;

class function LinuxComputerTool.KindToStr(const Kind: TComputerKinds): string;
begin
  raise Exception.CreateFmt('%s: not implemented', ['LinuxComputerTool.KindToStr']);
(*)
  Result := DoKindsToStr(Kind, ';');
(*)end;

class function LinuxComputerTool.KindToStr(const Kind: TDomainKind): string;
begin
  raise Exception.CreateFmt('%s: not implemented', ['LinuxComputerTool.KindToStr']);
//  Result := EnumTool.Name(TypeInfo(TDomainKind), Ord(Kind), 'dk');
end;

class function LinuxComputerTool.MacToStr(const Address: LargeInt): string;
begin
  raise Exception.CreateFmt('%s: not implemented', ['LinuxComputerTool.MacToStr']);
  (*)
  Result := SilOfComputer.DoMacToStr(Address);
(*)end;

class function LinuxComputerTool.Shutdown(const Flags: TShutdownFlags): Boolean;
(*)const
  WinFlags: array [TShutdownFlag] of LongWord = (EWX_LOGOFF, EWX_SHUTDOWN, EWX_REBOOT, EWX_FORCE, EWX_POWEROFF);
var
  Value: LongWord;
  i: TShutdownFlag;(*)
begin
  raise Exception.CreateFmt('%s: not implemented', ['LinuxComputerTool.Shutdown']);
(*)
  SilOfProcess.EnablePrivilegeByPid(SilProcessTool.Current.PID, 'SeShutdownPrivilege', true);

  Value := 0;

  for i := Low(TShutdownFlag) to High(TShutdownFlag) do
    if i in Flags then
      Value := Value or WinFlags[i];

  if sfForce in Flags then
    Value := Value or EWX_FORCEIFHUNG;

  Result := ExitWindowsEx(Value, 0);
(*)end;

end.
