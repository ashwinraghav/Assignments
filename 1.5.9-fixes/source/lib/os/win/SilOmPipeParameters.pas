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

unit SilOmPipeParameters;

{$I Defines.inc}

interface

uses
  SilLkInterfaced,
  SilOsTypes,
  SilOiPipe;

type
  TSilPipeParameters = class(
    TSilInterfacedObject,
    INamedPipeParameters )
  private
    FName: string;
    FAccess: TPipeAccess;
    FReadMode: TPipeMode;
    FPipeType: TPipeMode;
    FMaxInstances: Integer;
    FOutBufferSize: Integer;
    FInBufferSize: Integer;
    FDefaultTimeout: Integer;
  protected // INamedPipeParameters
    function GetName: string;
    function GetAccess: TPipeAccess;
    function GetReadMode: TPipeMode;
    function GetPipeType: TPipeMode;
    function GetMaxInstances: Integer;
    function GetOutBufferSize: Integer;
    function GetInBufferSize: Integer;
    function GetDefaultTimeout: Integer;
  public
    constructor Create(const Name: String; const Access: TPipeAccess; const PipeType: TPipeMode = pmStream;
          const ReadMode: TPipeMode = pmStream; const MaxInstances: Integer = piUnlimited;
          const OutBufferSize: Integer = 1024; const InBufferSize: Integer = 1024;
          const TimeOut: LongWord = INFINITE);
  public
    property Name: string read FName;
    property Access: TPipeAccess read FAccess;
    property ReadMode: TPipeMode read FReadMode;
    property PipeType: TPipeMode read FPipeType;
    property MaxInstances: Integer read FMaxInstances;
    property OutBufferSize: Integer read FOutBufferSize;
    property InBufferSize: Integer read FInBufferSize;
    property DefaultTimeout: Integer read FDefaultTimeout;
  end;

implementation

{ TSilPipeParameters }

constructor TSilPipeParameters.Create(const Name: String;
  const Access: TPipeAccess; const PipeType, ReadMode: TPipeMode;
  const MaxInstances, OutBufferSize, InBufferSize: Integer;
  const TimeOut: LongWord);
begin
  inherited Create;
  FName := Name;
  FAccess := Access;
  FPipeType := PipeType;
  FReadMode := ReadMode;
  FMaxInstances := MaxInstances;
  FOutBufferSize := OutBufferSize;
  FInBufferSize := InBufferSize;
  FDefaultTimeout := TimeOut;
end;

function TSilPipeParameters.GetAccess: TPipeAccess;
begin
  Result := FAccess;
end;

function TSilPipeParameters.GetDefaultTimeout: Integer;
begin
  Result := FDefaultTimeout;
end;

function TSilPipeParameters.GetInBufferSize: Integer;
begin
  Result := FInBufferSize;
end;

function TSilPipeParameters.GetMaxInstances: Integer;
begin
  Result := FMaxInstances;
end;

function TSilPipeParameters.GetName: string;
begin
  Result := FName;
end;

function TSilPipeParameters.GetOutBufferSize: Integer;
begin
  Result := FOutBufferSize;
end;

function TSilPipeParameters.GetPipeType: TPipeMode;
begin
  Result := FPipeType;
end;

function TSilPipeParameters.GetReadMode: TPipeMode;
begin
  Result := FReadMode;
end;

end.
