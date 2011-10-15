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

unit SilOkPipe;

{$I Defines.inc}

interface

uses
  SilLkInterfaced,
  SilOsHandled,
  SilLiStream,
  SilOiHandle,
  SilOiPipe;

type
  TSilPipe = class(
    TSilOsHandledObject,
    IStream,
    IPipe )
  protected // IStream
    function IStream.GetSize = DoPipeGetSize;
    function IStream.Read = DoPipeRead;
    function IStream.Write = DoPipeWrite;
  protected
    function DoPipeGetSize: LongWord; virtual; abstract;
    function DoPipeRead(var Buffer; Count: LongWord): LongWord; virtual; abstract;
    function DoPipeWrite(const Buffer; Count: LongWord): LongWord; virtual; abstract;
  protected // IPipe
    function GetStream: IStream;
  end;

  TSilNamedPipeServer = class(
    TSilInterfacedObject,
    INamedPipeServer )
  private
    FParameters: INamedPipeParameters;
  protected //- INamedPipeServer
    function GetParameters: INamedPipeParameters;
    function Connect(out Pipe: INamedPipeServerClient): Boolean; virtual; abstract;
    procedure Close; virtual; abstract; 
  public
    constructor Create(const Parameters: INamedPipeParameters); virtual;
    destructor Destroy; override;
  public
    property Parameters: INamedPipeParameters read FParameters;
  end;

implementation

{ TSilPipe }

function TSilPipe.GetStream: IStream;
begin
  Result := Self;
end;

{ TSilNamedPipeServer }

constructor TSilNamedPipeServer.Create(const Parameters: INamedPipeParameters);
begin
  inherited Create;
  FParameters := Parameters;
end;

destructor TSilNamedPipeServer.Destroy;
begin
  Close;
  FParameters := nil;
  inherited;
end;

function TSilNamedPipeServer.GetParameters: INamedPipeParameters;
begin
  Result := FParameters;
end;

end.
