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

unit SilOmIpcCriticalSection;

{$I Defines.inc}

interface

uses
  Windows,

  SilBtError,
  SilOiHandle,
  SilOsTypes,

  SilOkIpcCriticalSection;

type
  TSilWindowsCriticalSection = class(TSilCriticalSection)
  private
    FSection: TRTLCriticalSection;
  protected
    procedure DoInitialize; override;
    procedure DoFinalize; override;
    procedure DoEnter; override;
    function DoTryEnter: Boolean; override;
    procedure DoLeave; override;
  end;

implementation

{ TSilWindowsCriticalSection }

procedure TSilWindowsCriticalSection.DoInitialize;
begin
  Windows.InitializeCriticalSection(FSection);
end;

procedure TSilWindowsCriticalSection.DoFinalize;
begin
  Windows.DeleteCriticalSection(FSection);
end;

procedure TSilWindowsCriticalSection.DoEnter;
begin
  Windows.EnterCriticalSection(FSection);
end;

function TSilWindowsCriticalSection.DoTryEnter: Boolean;
begin
//  Result := Windows.TryEnterCriticalSection(FSection);
  raise Error.Create('TryEnterCriticalSection no esta implementada');
end;

procedure TSilWindowsCriticalSection.DoLeave;
begin
  // if not, EnterCriticalSection was not called
  if FSection.OwningThread = Windows.GetCurrentThreadId then
    Windows.LeaveCriticalSection(FSection);
end;

end.
