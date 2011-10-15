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

unit SilOtPipe;

{$I Defines.inc}

interface
{$IFNDEF FP20}
uses
  SilOsTypes,
  SilOiPipe,
  SilOjPipe;

type
  LinuxNamedPipe = class(SilNamedPipe)
    class function Create(
          const Name: String;
          const Access: TPipeAccess;
          const PipeType: TPipeMode = pmStream;
          const ReadMode: TPipeMode = pmStream;
          const MaxInstances: Integer = piUnlimited;
          const OutBufferSize: Integer = 1024;
          const InBufferSize: Integer = 1024;
          const TimeOut: LongWord = INFINITE): INamedPipeServer; override;

    class function Open(
          const Name: String;
          const Access: TPipeAccess;
          const PipeType: TPipeMode = pmStream;
          const ReadMode: TPipeMode = pmStream): INamedPipeClient; override;
  end;

{$ENDIF}
implementation
{$IFNDEF FP20}
uses
  SilOmPipeParameters,
  SilOmPipe;

{ SilNamedPipe }

class function LinuxNamedPipe.Create(const Name: String;
  const Access: TPipeAccess; const PipeType, ReadMode: TPipeMode;
  const MaxInstances, OutBufferSize, InBufferSize: Integer;
  const TimeOut: LongWord): INamedPipeServer;
begin
  Result := TLinuxNamedPipeServer.Create(TSilPipeParameters.Create(Name, Access, PipeType, ReadMode, MaxInstances, OutBufferSize, InBufferSize, TimeOut));
end;

class function LinuxNamedPipe.Open(const Name: String;
  const Access: TPipeAccess; const PipeType, ReadMode: TPipeMode): INamedPipeClient;
begin
  Result := TLinuxNamedPipeClient.Create(TSilPipeParameters.Create(Name, Access, PipeType, ReadMode));
end;
{$ENDIF}
end.
