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

unit SilOiPipe;

{$I Defines.inc}

interface

uses
  SilLiStream,
  SilOiHandle;

const
  piUnlimited = 0;

type
  TPipeAccess   = (paDuplex, paInbound, paOutbound);
  TPipeMode     = (pmStream, pmMessage);
  TPipeEndpoint = (peClient, peServer);

  INamedPipeParameters = interface;
  INamedPipeServer = interface;
  IPipe = interface;
  INamedPipeClient = interface;
  INamedPipeServerClient = interface;

  IPipe = interface (IHandledObject)
    ['{D9B0B339-A2BB-4D19-9832-43C33A3FED75}']
    function GetStream: IStream;
    property Stream: IStream read GetStream;
  end;

  INamedPipeParameters = interface
    ['{019F9350-B0C1-4EB9-A56C-4C74ECE15CF5}']
    function GetName: string;
    function GetAccess: TPipeAccess;
    function GetReadMode: TPipeMode;
    function GetPipeType: TPipeMode;
    function GetMaxInstances: Integer;
    function GetOutBufferSize: Integer;
    function GetInBufferSize: Integer;
    function GetDefaultTimeout: Integer;
    property Name: string read GetName;
    property Access: TPipeAccess read GetAccess;
    property ReadMode: TPipeMode read GetReadMode;
    property PipeType: TPipeMode read GetPipeType;
    property MaxInstances: Integer read GetMaxInstances;
    property OutBufferSize: Integer read GetOutBufferSize;
    property InBufferSize: Integer read GetInBufferSize;
    property DefaultTimeout: Integer read GetDefaultTimeout;
  end;

  INamedPipeServer = interface
    ['{955C421F-A02B-4DCB-BB87-EEB9729C7956}']
    function GetParameters: INamedPipeParameters;
    function Connect(out Pipe: INamedPipeServerClient): Boolean;
    procedure Close;
    property Parameters: INamedPipeParameters read GetParameters;
  end;

  INamedPipeClient = interface(IPipe)
    ['{5D3AA313-7294-4A03-AB96-42B097F27706}']
    function GetParameters: INamedPipeParameters;
    property Parameters: INamedPipeParameters read GetParameters;
  end;

  INamedPipeServerClient = interface(INamedPipeClient)
    ['{EBD5907F-3FD3-4470-ACC8-C670DC2FF46A}']
    function GetServer: INamedPipeServer;
    property Server: INamedPipeServer read GetServer;
  end;
  
implementation
end.
