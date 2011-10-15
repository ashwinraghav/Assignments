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

unit SilLayer2;

interface

{$include Sil.inc}

uses
  Sil,
  SilLayerSiGlobal,
  SilLayerSiProtocolText;

type
  TLayerLogLevel                    = SilLayerSiGlobal.TLayerLogLevel;

const
  slAlways                          = SilLayerSiGlobal.slAlways;
  slStatus                          = SilLayerSiGlobal.slStatus;
  slAction                          = SilLayerSiGlobal.slAction;

type
  TLayerLogLevelSet                 = SilLayerSiGlobal.TLayerLogLevelSet;
  TLayerKind                        = SilLayerSiGlobal.TLayerKind;

const
  lkUnspecified                     = SilLayerSiGlobal.lkUnspecified;
  lkDevice                          = SilLayerSiGlobal.lkDevice;
  lkPacker                          = SilLayerSiGlobal.lkPacker;
  lkProtocol                        = SilLayerSiGlobal.lkProtocol;
  lkStack                           = SilLayerSiGlobal.lkStack;

type
  ILayer                            = SilLayerSiGlobal.ILayer;
  ILayerStack                       = SilLayerSiGlobal.ILayerStack;
  ILayerStatus                      = SilLayerSiGlobal.ILayerStatus;
  ILayerAction                      = SilLayerSiGlobal.ILayerAction;
  ILayerHook                        = SilLayerSiGlobal.ILayerHook;
  ILayerToken                       = SilLayerSiGlobal.ILayerToken;
  ILayerWait                        = SilLayerSiGlobal.ILayerWait;
  ILayerClientHolder                = SilLayerSiGlobal.ILayerClientHolder;

type
  ITextProtocol                     = SilLayerSiProtocolText.ITextProtocol;
  ITextProtocolHook                 = SilLayerSiProtocolText.ITextProtocolHook;

type
  Layer = class (Tool)
    class function Stack: ILayerStack;
    class function Thread: ILayer;
    class function Holder(const Action: ILayerAction): ILayer; overload;
    class function Holder(const holder: ILayerClientHolder): ILayer; overload;
    class function DeviceFile: ILayer;
    class function DeviceDatagram: ILayer;
    class function PackerImate: ILayer;
    class function ProtocolText(const hook: ITextProtocolHook): ILayer;
    class function DeviceSocketClient(const Socket: ISocketClient = nil): ILayer;
    class function DeviceSocketServer: ILayer;
  end;

implementation

uses
  SilLayerSmStack,
  SilLayerSmThread,
  SilLayerSmHolder,
  SilLayerSmClientHolder,
  SilLayerSmPackerImate,
  SilLayerSmDeviceDatagram,
  SilLayerSmDeviceFile,
  SilLayerSmProtocolText,
  SilLayerSmDeviceSocketClient,
  SilLayerSmDeviceSocketServer;

class function Layer.Stack: ILayerStack;
begin
  Result := TSilLayerStack.Create;
end;

class function Layer.Thread: ILayer;
begin
  Result := TSilLayerThread.Create;
end;

class function Layer.Holder(const Action: ILayerAction): ILayer;
begin
  Result := TSilLayerHolder.Create(Action);
end;

class function Layer.Holder(const holder: ILayerClientHolder): ILayer;
begin
  Result := TSilLayerClientHolder.Create(holder);
end;

class function Layer.DeviceFile: ILayer;
begin
  Result := TSilLayerDeviceFile.Create;
end;

class function Layer.DeviceDatagram: ILayer;
begin
  Result := TSilLayerDeviceSocketDatagram.Create;
end;

class function Layer.PackerImate: ILayer;
begin
  Result := TSilLayerPackerImate.Create;
end;

class function Layer.ProtocolText(const hook: ITextProtocolHook): ILayer;
begin
  Result := TSilLayerProtocolText.Create(hook);
end;

class function Layer.DeviceSocketClient(const Socket: ISocketClient): ILayer;
begin
  result := TSilLayerDeviceSocketClient.create(Socket);
end;

class function Layer.DeviceSocketServer: ILayer;
begin
  result := TSilLayerDeviceSocketServer.create;
end;

end.
