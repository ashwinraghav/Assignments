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

unit SilProtocol;

interface

uses
  SilSiSharedProtocol,
  SilSiBlindProtocol,
  SilSiSocks5Protocol,
  SilSiFileAccessProtocol,
  SilSiFileTransferProtocol,
  SilSiSqlProtocol,
  SilSiEblisProtocol,
  SilSiMailClient,
  SilSiAsciiProtocol,
  SilSiPop3,
  SilSiSmtp,
  SilSiPop3Command,
  SilSiSmtpCommand,
	SilStProtocol,
  SilSiAsciiCommandSink;

type
  ISharedProtocol               = SilSiSharedProtocol.ISharedProtocol;
  
type
  IBlindProtocol                = SilSiBlindProtocol.IBlindProtocol;
  IBlindProtocolEvents          = SilSiBlindProtocol.IBlindProtocolEvents;
  RBlindTextEvent               = SilSiBlindProtocol.RBlindTextEvent;
  RBlindProtocolIDEvent         = SilSiBlindProtocol.RBlindProtocolIDEvent;

type                            
  ISocks5Client                 = SilSiSocks5Protocol.ISocks5Client;
  ISocks5Server                 = SilSiSocks5Protocol.ISocks5Server;
  TAuthenticationMethodsEvent   = SilSiSocks5Protocol.TAuthenticationMethodsEvent;
  TAuthenticationEvent          = SilSiSocks5Protocol.TAuthenticationEvent;
  TRequestEvent                 = SilSiSocks5Protocol.TRequestEvent;
  TSocks5Resp                   = SilSiSocks5Protocol.TSocks5Resp;
  TSocks5UdpResp                = SilSiSocks5Protocol.TSocks5UdpResp;
  ISocks5ServerEvents           = SilSiSocks5Protocol.ISocks5ServerEvents;
                                
const // TFileAccessResult      
  frOk                          = SilSiFileAccessProtocol.frOk;
  frAccessDenied                = SilSiFileAccessProtocol.frAccessDenied;
  frNotExists                   = SilSiFileAccessProtocol.frNotExists;
  frInUse                       = SilSiFileAccessProtocol.frInUse;
                                
type                            
  IFileAccessProtocol           = SilSiFileAccessProtocol.IFileAccessProtocol;
  IFileAccessProtocolEvents     = SilSiFileAccessProtocol.IFileAccessProtocolEvents;
  TLoginEvent                   = SilSiFileAccessProtocol.TLoginEvent;
  TQueryInfoEvent               = SilSiFileAccessProtocol.TQueryInfoEvent;
  TFileAccessEvent              = SilSiFileAccessProtocol.TFileAccessEvent;
  TQueryFileListEvent           = SilSiFileAccessProtocol.TQueryFileListEvent;
  TActionEvent                  = SilSiFileAccessProtocol.TActionEvent;
  TRenameEvent                  = SilSiFileAccessProtocol.TRenameEvent;
  TOpenDirEvent                 = SilSiFileAccessProtocol.TOpenDirEvent;
  TFileAccessResult             = SilSiFileAccessProtocol.TFileAccessResult;
                                
const // TFtpTypeCode           
  tcAscii                       = SilSiFileTransferProtocol.tcAscii;
  tcEbcdic                      = SilSiFileTransferProtocol.tcEbcdic;
  tcImage                       = SilSiFileTransferProtocol.tcImage;
  tcLocal                       = SilSiFileTransferProtocol.tcLocal;
                                
  scFile                        = SilSiFileTransferProtocol.scFile;
  scRecord                      = SilSiFileTransferProtocol.scRecord;
  scPage                        = SilSiFileTransferProtocol.scPage;
                                
  tmStream                      = SilSiFileTransferProtocol.tmStream;
  tmBlock                       = SilSiFileTransferProtocol.tmBlock;
  tmCompressed                  = SilSiFileTransferProtocol.tmCompressed;
                                
type                            
  TFtpTypeCode                  = SilSiFileTransferProtocol.TFtpTypeCode;
  TFtpStructCode                = SilSiFileTransferProtocol.TFtpStructCode;
  TFtpTransferMode              = SilSiFileTransferProtocol.TFtpTransferMode;
  TFtpHostAddress               = SilSiFileTransferProtocol.TFtpHostAddress;
  IFtpServer                    = SilSiFileTransferProtocol.IFtpServer;
  IFtpClient                    = SilSiFileTransferProtocol.IFtpClient;
  TFtpListenEvent               = SilSiFileTransferProtocol.TFtpListenEvent;
  TFtpConnectEvent              = SilSiFileTransferProtocol.TFtpConnectEvent;
  TFtpListEvent                 = SilSiFileTransferProtocol.TFtpListEvent;
  TFtpFileTransferEvent         = SilSiFileTransferProtocol.TFtpFileTransferEvent;
  IFtpClientEvents              = SilSiFileTransferProtocol.IFtpClientEvents;
  IFtpClientSocketEvents        = SilSiFileTransferProtocol.IFtpClientSocketEvents;
  TFtpResultMessage             = SilSiFileTransferProtocol.TFtpResultMessage;
  TFtpServerEvent               = SilSiFileTransferProtocol.TFtpServerEvent;
  TFtpLoginEvent                = SilSiFileTransferProtocol.TFtpLoginEvent;
  TFtpChangeDirEvent            = SilSiFileTransferProtocol.TFtpChangeDirEvent;
  TFtpAddressEvent              = SilSiFileTransferProtocol.TFtpAddressEvent;
  TFtpRepresentationEvent       = SilSiFileTransferProtocol.TFtpRepresentationEvent;
  TFtpStructureEvent            = SilSiFileTransferProtocol.TFtpStructureEvent;
  TFtpTransferModeEvent         = SilSiFileTransferProtocol.TFtpTransferModeEvent;
  TFtpRestartEvent              = SilSiFileTransferProtocol.TFtpRestartEvent;
  TFtpPathEvent                 = SilSiFileTransferProtocol.TFtpPathEvent;
  TFtpAllocateEvent             = SilSiFileTransferProtocol.TFtpAllocateEvent;
  TFtpRenameFileEvent           = SilSiFileTransferProtocol.TFtpRenameFileEvent;
  IFtpServerEvents              = SilSiFileTransferProtocol.IFtpServerEvents;
                                
type                            
  RDbConnectionInfo             = SilSiSqlProtocol.RDbConnectionInfo;
  IProtSqlClient                = SilSiSqlProtocol.IProtSqlClient;
  IProtSqlServer                = SilSiSqlProtocol.IProtSqlServer;
  IProtSqlServerEvents          = SilSiSqlProtocol.IProtSqlServerEvents;
  TProtSqlServerEvent           = SilSiSqlProtocol.TProtSqlServerEvent;
  TProtSqlCloseEvent            = SilSiSqlProtocol.TProtSqlCloseEvent;
  TProtSqlQueryEvent            = SilSiSqlProtocol.TProtSqlQueryEvent;
  TProtSqlExecuteEvent          = SilSiSqlProtocol.TProtSqlExecuteEvent;
  TProtSqlStoredProcEvent       = SilSiSqlProtocol.TProtSqlStoredProcEvent;
  TProtSqlQueryFieldsEvent      = SilSiSqlProtocol.TProtSqlQueryFieldsEvent;

type
  REblisNegotiateEvent          = SilSiEblisProtocol.REblisNegotiateEvent;
  IEblisProtocol                = SilSiEblisProtocol.IEblisProtocol;
  IEblisProtocolEvents          = SilSiEblisProtocol.IEblisProtocolEvents;

type
  TProgressKind                 = SilSiMailClient.TProgressKind;
                                
const // TProgressKind          
  pkDecode                      = SilSiMailClient.pkDecode;
  pkEncode                      = SilSiMailClient.pkDecode;
                                
type                            
  IMailInterfaceList            = SilSiMailClient.IMailInterfaceList;
//  TMimeCoder                    = SilSiMailClient.TMimeCoder;
  IMimeParam                    = SilSiMailClient.IMimeParam;
  IMimeParamList                = SilSiMailClient.IMimeParamList;
  IMimeLabel                    = SilSiMailClient.IMimeLabel;
  IMimeLabelList                = SilSiMailClient.IMimeLabelList;
  IMailPart                     = SilSiMailClient.IMailPart;
  IMailPartList                 = SilSiMailClient.IMailPartList;
  IMailAttachmentList           = SilSiMailClient.IMailAttachmentList;
  IMailMessage                  = SilSiMailClient.IMailMessage;
  IMailMessageList              = SilSiMailClient.IMailMessageList;

type
  ISmtpClient                   = SilSiSmtp.ISmtpClient;

type
  IAsciiCommandSink             = SilSiAsciiCommandSink.IAsciiCommandSink;               

type
  IAsciiProtocol                = SilSiAsciiProtocol.IAsciiProtocol;
  RWriteLineEvent               = SilSiAsciiProtocol.RWriteLineEvent;
  RReadLineEvent                = SilSiAsciiProtocol.RReadLineEvent;
  IAsciiProtocolEvents          = SilSiAsciiProtocol.IAsciiProtocolEvents;

type
  IPop3Client                   = SilSiPop3.IPop3Client;

type
  IPop3ClientCommand            = SilSiPop3Command.IPop3ClientCommand;

type
  ISmtpClientCommand            = SilSiSmtpCommand.ISmtpClientCommand;

type
  Tk                            = SilStProtocol.ProtocolTool;

implementation
end.
