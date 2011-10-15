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

unit SilLayer;

interface

{$include Defines.inc}

uses
  Sil,
  SilCoder,

  SilOiPipe,

  SilScLayer,
  SilSiLayer,
  SilSeLayerPacket,
  SilSeLayerProtocol,
  SilSiLayerProtocolBlind,
  SilSiLayerProtocolEblis,
  SilSiLayerProtocolFile,
  SilSiLayerConnection,

  SilSmLayerProtocolCustomImate,
  SilSmLayerConnectionImate,
  SilStLayer;

const
  NDebugLayer                                     = SilScLayer.NDebugLayer;
  CDebugLayer                                     = SilScLayer.CDebugLayer;

const
  dlException                                     = SilScLayer.dlException;
  dlActivation                                    = SilScLayer.dlActivation;
  dlWarning                                       = SilScLayer.dlWarning;
  dlCreation                                      = SilScLayer.dlCreation;
  dlConnection                                    = SilScLayer.dlConnection;
  dlPacket                                        = SilScLayer.dlPacket;
  dlInOut                                         = SilScLayer.dlInOut;

type
  ILayer                                          = SilSiLayer.ILayer;
  ILayerControl                                   = SilSiLayer.ILayerControl;
  ILayerLink                                      = SilSiLayer.ILayerLink;
  ILayerLinkControl                               = SilSiLayer.ILayerLinkControl;
  ILayerOperation                                 = SilSiLayer.ILayerOperation;
  ILayerDuplicate                                 = SilSiLayer.ILayerDuplicate;
  ILayerSelfDuplicate                             = SilSiLayer.ILayerSelfDuplicate;
  ILayerLinkList                                  = SilSiLayer.ILayerLinkList;
  ILayerChain                                     = SilSiLayer.ILayerChain;
  ILayerSlot                                      = SilSiLayer.ILayerSlot;
  ILayerActivationEvents                          = SilSiLayer.ILayerActivationEvents;

  ILayerCommand                                   = SilSiLayer.ILayerCommand;

  ILayerTerminal                                  = SilSiLayer.ILayerTerminal;
  ILayerChainSource                               = SilSiLayer.ILayerChainSource;

  ILayerStream                                    = SilSiLayer.ILayerStream;

  ILayerProtocol                                  = SilSiLayer.ILayerProtocol;
  ILayerProtocolControl                           = SilSiLayer.ILayerProtocolControl;
  ILayerProtocolStatus                            = SilSiLayer.ILayerProtocolStatus;
  ILayerWaitPacket                                = SilSiLayer.ILayerWaitPacket;

  ILayerBindings                                  = SilSiLayer.ILayerBindings;

  ILayerThreadHook                                = SilSiLayer.ILayerThreadHook;

  RLayerActivated                                 = SilSiLayer.RLayerActivated;
  RLayerDeactivated                               = SilSiLayer.RLayerDeactivated;

  PProtocolMessage                                = SilSiLayer.PProtocolMessage;
  RProtocolMessage                                = SilSiLayer.RProtocolMessage;

type
  PPacketHeader                                   = SilSeLayerPacket.PPacketHeader;
  RPacketHeader                                   = SilSeLayerPacket.RPacketHeader;

type
  PImateProtocolHeader                            = SilSeLayerProtocol.PImateProtocolHeader;
  RImateProtocolHeader                            = SilSeLayerProtocol.RImateProtocolHeader;
  PImateProtocolMessage                           = SilSeLayerProtocol.PImateProtocolMessage;
  RImateProtocolMessage                           = SilSeLayerProtocol.RImateProtocolMessage;

type
  IBlindProtocol                                  = SilSiLayerProtocolBlind.IBlindProtocol;
  RBlindTextEvent                                 = SilSiLayerProtocolBlind.RBlindTextEvent;
  RBlindRequestEvent                              = SilSiLayerProtocolBlind.RBlindRequestEvent;
  IBlindProtocolHook                              = SilSiLayerProtocolBlind.IBlindProtocolHook;

type
  IEblisProtocol                                  = SilSiLayerProtocolEblis.IEblisProtocol;
  REblisNegotiateEvent                            = SilSiLayerProtocolEblis.REblisNegotiateEvent;
  IEblisProtocolHook                              = SilSiLayerProtocolEblis.IEblisProtocolHook;

type
  IClientSideFileProtocol                         = SilSiLayerProtocolFile.IClientSideFileProtocol;
  IServerSideFileProtocol                         = SilSiLayerProtocolFile.IServerSideFileProtocol;
  IServerSideFileProtocolHook                     = SilSiLayerProtocolFile.IServerSideFileProtocolHook;

  RFPOpenFileEvent                                = SilSiLayerProtocolFile.RFPOpenFileEvent;
  RFPCreateFileEvent                              = SilSiLayerProtocolFile.RFPCreateFileEvent;
  RFPCreateDirectoryEvent                         = SilSiLayerProtocolFile.RFPCreateDirectoryEvent;
  RFPInfoEvent                                    = SilSiLayerProtocolFile.RFPInfoEvent;
  RFPMoveEvent                                    = SilSiLayerProtocolFile.RFPMoveEvent;
  RFPDeleteEvent                                  = SilSiLayerProtocolFile.RFPDeleteEvent;
  RFFileDataEvent                                 = SilSiLayerProtocolFile.RFFileDataEvent;
  RFSeekFileEvent                                 = SilSiLayerProtocolFile.RFSeekFileEvent;
  RFFlushFileEvent                                = SilSiLayerProtocolFile.RFFlushFileEvent;
  RFCloseFileEvent                                = SilSiLayerProtocolFile.RFCloseFileEvent;
  RFCreateDirectoryReaderEvent                    = SilSiLayerProtocolFile.RFCreateDirectoryReaderEvent;
  RFDirectoryReaderEvent                          = SilSiLayerProtocolFile.RFDirectoryReaderEvent;
  RFPFileSizeEvent                                = SilSiLayerProtocolFile.RFPFileSizeEvent;
  RFPFileAttributeEvent                           = SilSiLayerProtocolFile.RFPFileAttributeEvent;
  RFPFileTimeEvent                                = SilSiLayerProtocolFile.RFPFileTimeEvent;

type
  TSilCustomImateProtocol                         = SilSmLayerProtocolCustomImate.TSilCustomImateProtocol;

type
  TSilLayerImateConnectionPeer                    = SilSmLayerConnectionImate.TSilLayerImateConnectionPeer;

type
  ILayerConnection                                = SilSiLayerConnection.ILayerConnection;
  ILayerConnectionServer                          = SilSiLayerConnection.ILayerConnectionServer;
  ILayerConnections                               = SilSiLayerConnection.ILayerConnections;
  ILayerConnectionList                            = SilSiLayerConnection.ILayerConnectionList;
  ILayerConnectionEvents                          = SilSiLayerConnection.ILayerConnectionEvents;
  ILayerConnectionManager                         = SilSiLayerConnection.ILayerConnectionManager;
  ILayerClientConnectionManager                   = SilSiLayerConnection.ILayerClientConnectionManager;
  ILayerConnectionClientEvents                    = SilSiLayerConnection.ILayerConnectionClientEvents;

  TLayerConnectionMode                            = SilSiLayerConnection.TLayerConnectionMode;

const
  cmClientPeer                                    = TLayerConnectionMode(SilSiLayerConnection.cmClientPeer);
  cmServerPeer                                    = TLayerConnectionMode(SilSiLayerConnection.cmServerPeer);
  cmServer                                        = TLayerConnectionMode(SilSiLayerConnection.cmServer);

type
  RLayerConnectionEvent                           = SilSiLayerConnection.RLayerConnectionEvent;

type
  Cmd                                             = SilStLayer.Cmd;
  Tk                                              = SilStLayer.Tk;
  Layer                                           = SilStLayer.Layer;
  Packer                                          = SilStLayer.Packer;
  Device                                          = SilStLayer.Device;
  Protocol                                        = SilStLayer.Protocol;
  Connection                                      = SilStLayer.Connection;

const
  CLayerBlind: TGUID          = '{15D134D1-A69E-40CE-90F0-D482171CA749}';
  CLayerImatePacker: TGUID    = '{A4BCD546-3612-4254-BBFF-59EA6FE3DCD7}';
  CLayerImateProtocol: TGUID  = '{2DE33A95-F1B8-4FCD-9E55-6381B58043C9}';
  CLayerSocketClient: TGUID   = '{172934F8-B1D5-4522-B254-1838984C522D}';
  CLayerSocketServer: TGUID   = '{BD80DDC5-818D-4803-9083-42301695A380}';

const
  SLayerImatePacker: PChar    = 'sil.layer.packer.imate';
  SLayerBlind: PChar          = 'sil.layer.protocol.imate.blind';
  SLayerImateProtocol: PChar  = 'sil.layer.protocol.imate';
  SLayerSocketClient: PChar   = 'sil.layer.device.socketclient';
  SLayerSocketServer: PChar   = 'sil.layer.device.socketserver';

implementation

end.

