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
  libc,
  SysUtils,

  SilBtError,
  SilOiHandle,
  SilOsTypes,

  SilOkIpcCriticalSection;

type
  TSilLinuxCriticalSection = class(TSilCriticalSection)
  private
    FCriticalSection: TPthreadMutex;
  protected
    procedure DoInitialize; override;
    procedure DoFinalize; override;
    procedure DoEnter; override;
    function DoTryEnter: Boolean; override;
    procedure DoLeave; override;
  end;

implementation

{ TSilLinuxCriticalSection }

procedure TSilLinuxCriticalSection.DoInitialize;
var
  MAttr: TMutexAttribute;
  Stat: Integer;
begin
  Stat := libc.pthread_mutexattr_init(@MAttr);

  if Stat = 0 then
    try
      Stat := libc.pthread_mutexattr_settype(@MAttr, PTHREAD_MUTEX_RECURSIVE);

      if Stat = 0 then
        libc.pthread_mutex_init(@FCriticalSection, @MAttr);
    finally
      libc.pthread_mutexattr_destroy(@MAttr);
    end;
end;

procedure TSilLinuxCriticalSection.DoFinalize;
begin
  libc.pthread_mutex_destroy(@FCriticalSection);
end;

procedure TSilLinuxCriticalSection.DoEnter;
begin
  libc.pthread_mutex_lock(@FCriticalSection);
end;

function TSilLinuxCriticalSection.DoTryEnter: Boolean;
begin
  raise Exception.Create('TSilLinuxCriticalSection.DoTryEnter: not implemented');
  Result := false; // not implemented
end;

procedure TSilLinuxCriticalSection.DoLeave;
begin
  libc.pthread_mutex_unlock(@FCriticalSection);
end;

end.
