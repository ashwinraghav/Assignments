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

unit SilTool;

{$INCLUDE Defines.inc}

interface

uses
  SilScSharedSignature,
  SilSiDispatcher,
  (*)SilSiAbstractConnection,
  SilSiPacketBuilder,
  SilSiPacketCompletion,
  SilSiSerialConnection,
  SilSiSocketConnection,
  SilSiProtocolBase,
  SilSiProtocolPacket,
  SilSiCommunicationQueue,(*)
  SilSiResourcePool,
  SilSiMessageQueue,
  SilSiSharedObject,
  SilStTool,
  (*)SilStAbstractConnection,
  SilStSocketConnection,
  SilStSerialConnection,(*)
  SilStConfiguration,
  SilStThreadPool,
  SilStResourcePool,
  SilStMessageQueue,
  SilSiTimedRun,
  SilSiConfig,
  SilSkSharedObject;

type
  Sv                            = SilStTool.Sv;

type
  RDispatcherMessage            = SilSiDispatcher.RDispatcherMessage;
  IDispatcher                   = SilSiDispatcher.IDispatcher;

(*)type
  IAbstractConnection           = SilSiAbstractConnection.IAbstractConnection;
  IConnectingEvents             = SilSiAbstractConnection.IConnectingEvents;
  IConnectedEvents              = SilSiAbstractConnection.IConnectedEvents;
  TConnectionEvent              = SilSiAbstractConnection.TConnectionEvent;
  TConnectionFailedEvent        = SilSiAbstractConnection.TConnectionFailedEvent;
  TConnectionDataEvent          = SilSiAbstractConnection.TConnectionDataEvent;
  TConnectionBreakEvent         = SilSiAbstractConnection.TConnectionBreakEvent;
  RConnectionRetryEvent         = SilSiAbstractConnection.RConnectionRetryEvent;
  IConnectionTrialEvents        = SilSiAbstractConnection.IConnectionTrialEvents;
  IConnectionList               = SilSiAbstractConnection.IConnectionList;(*)

type
  ITimedRunListener             = SilSiTimedRun.ITimedRunListener;
  ITimedRun                     = SilSiTimedRun.ITimedRun;
                                
(*)type
  IPacketData                   = SilSiPacketBuilder.IPacketData;

const
  tiInteger                     = SilSiPacketBuilder.tiInteger;
  tiBoolean                     = SilSiPacketBuilder.tiBoolean;
  tiByte                        = SilSiPacketBuilder.tiByte;
  tiDouble                      = SilSiPacketBuilder.tiDouble;
//tiWideChar                    = SilSiPacketBuilder.tiWideChar;
  tiAnsiString                  = SilSiPacketBuilder.tiAnsiString;
  tiWideString                  = SilSiPacketBuilder.tiWideString;
  tiLargeInt                    = SilSiPacketBuilder.tiLargeInt;
  tiWord                        = SilSiPacketBuilder.tiWord;
  tiLongWord                    = SilSiPacketBuilder.tiLongWord;
//tiLargeWord                   = SilSiPacketBuilder.tiLargeWord;
  tiShortInt                    = SilSiPacketBuilder.tiShortInt;
  tiSmallInt                    = SilSiPacketBuilder.tiSmallInt;
  tiSingle                      = SilSiPacketBuilder.tiSingle;
  tiVariant                     = SilSiPacketBuilder.tiVariant;

type
  IPacketCompletion             = SilSiPacketCompletion.IPacketCompletion;

type
  ISerialConnection             = SilSiSerialConnection.ISerialConnection;

type
  TSocketConnectionEvent        = SilSiSocketConnection.TSocketConnectionEvent;
  ISocketServerEvents           = SilSiSocketConnection.ISocketServerEvents;
  IClientSocketConnection       = SilSiSocketConnection.IClientSocketConnection;
  IServerSocketConnection       = SilSiSocketConnection.IServerSocketConnection;

type
  IProtocol                     = SilSiProtocolBase.IProtocol;
  IFormatedProtocol             = SilSiProtocolBase.IFormatedProtocol;
  IProtocolBase                 = SilSiProtocolBase.IProtocolBase;
  IFormatedProtocolEvents       = SilSiProtocolBase.IFormatedProtocolEvents;
  TErrorEvent                   = SilSiProtocolBase.TErrorEvent;
  TUnknownPacketEvent           = SilSiProtocolBase.TUnknownPacketEvent;
  TQueuedPacketAction           = SilSiProtocolBase.TQueuedPacketAction;
  TQueuedPacketEvent            = SilSiProtocolBase.TQueuedPacketEvent;

  RDebugWaitingPacket           = SilSiProtocolBase.RDebugWaitingPacket;
  IFormatedProtocolDebugEvents  = SilSiProtocolBase.IFormatedProtocolDebugEvents;

const
  qaOk                          = SilSiProtocolBase.qaOk;
  qaCancel                      = SilSiProtocolBase.qaCancel;
  qaDiscard                     = SilSiProtocolBase.qaDiscard;

type
  ICommunicationQueue           = SilSiCommunicationQueue.ICommunicationQueue;
  TQueueOverflowMode            = SilSiCommunicationQueue.TQueueOverflowMode;

const // TQueueOverflowMode
  omDiscardOld                  = SilSiCommunicationQueue.omDiscardOld;
  omIgnoreAppend                = SilSiCommunicationQueue.omIgnoreAppend;(*)
                                
type                            
  IPoolItem                     = SilSiResourcePool.IPoolItem;
  IPoolItemManager              = SilSiResourcePool.IPoolItemManager;
  IResourcePool                 = SilSiResourcePool.IResourcePool;
                                
(*)const // TProtocolPacketFlag
  pfDelayedWrite                = SilSiProtocolPacket.pfDelayedWrite;
  pfException                   = SilSiProtocolPacket.pfException;

type
  IProtocolPacket               = SilSiProtocolPacket.IProtocolPacket;
  TProtocolPacketFlag           = SilSiProtocolPacket.TProtocolPacketFlag;
  TProtocolPacketFlags          = SilSiProtocolPacket.TProtocolPacketFlags;
  PProtocolHeader               = SilSiProtocolPacket.PProtocolHeader;
  TProtocolHeader               = SilSiProtocolPacket.TProtocolHeader;(*)

type
  IThreadMessage                = SilSiMessageQueue.IThreadMessage;
  IQueuedThreadMessage          = SilSiMessageQueue.IQueuedThreadMessage;
  IThreadMessageReply           = SilSiMessageQueue.IThreadMessageReply;
  IThreadMessageQueue           = SilSiMessageQueue.IThreadMessageQueue;
  IThreadMessageQueueSink       = SilSiMessageQueue.IThreadMessageQueueSink;
  IThreadMessageQueueClient     = SilSiMessageQueue.IThreadMessageQueueClient;
  RThreadMessage                = SilSiMessageQueue.RThreadMessage;

const
  CSilLibrarySignatureName      = SilScSharedSignature.CSilLibrarySignatureName;
  CSilLibrarySignatureData      = SilScSharedSignature.CSilLibrarySignatureData;

type
  ISharedObject                 = SilSiSharedObject.ISharedObject;
  ISharedFactory                = SilSiSharedObject.ISharedFactory;
  ISharedClassFactory           = SilSiSharedObject.ISharedClassFactory;
  ISharedFactoryList            = SilSiSharedObject.ISharedFactoryList;
  ISharedRegistry               = SilSiSharedObject.ISharedRegistry;

type
  ClassFactoryType              = SilSiSharedObject.ClassFactoryType;
  ClassFactory                  = SilSiSharedObject.ClassFactory;

type
  TSilSharedObjectClass         = SilSkSharedObject.TSilSharedObjectClass;
  TSilSharedObject              = SilSkSharedObject.TSilSharedObject;

type
  IConfiguration                = SilSiConfig.IConfiguration;
  IConfigNode                   = SilSiConfig.IConfigNode;
  IConfigData                   = SilSiConfig.IConfigData;
  RConfigData                   = SilSiConfig.RConfigData;


implementation
end.
