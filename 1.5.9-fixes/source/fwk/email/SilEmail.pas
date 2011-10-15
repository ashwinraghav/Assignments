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

unit SilEMail;

interface

uses
  SilSiAsciiProtocol,
  SilSiMailClient,
  SilSiPop3,
  SilSiSmtp,
  SilSiPop3Command,
  SilSiSmtpCommand;

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
  ISmtpClientCommand            = SilSiSmtpCommand.ISmtpClientCommand;

type
  IPop3Client                   = SilSiPop3.IPop3Client;

type
  IPop3ClientCommand            = SilSiPop3Command.IPop3ClientCommand;

function Pop3ClientCommand: IPop3ClientCommand;
function SmtpClientCommand: ISmtpClientCommand;
function Pop3Client(const Command: IPop3ClientCommand): IPop3Client; overload;
function SmtpClient(const Command: ISmtpClientCommand): ISmtpClient; overload;

implementation

uses
  SilSmAsciiProtocol,
  SilSmPop3Client,
  SilSmSmtpClient,
  SilSmPop3Command,
  SilSmSmtpCommand;

function Pop3Client(const Command: IPop3ClientCommand): IPop3Client;
begin
  Result := TPop3Client.Create(Command);
end;

function Pop3ClientCommand: IPop3ClientCommand;
begin
  Result := TPop3ClientCommand.Create;
end;

function SmtpClient(const Command: ISmtpClientCommand): ISmtpClient;
begin
  Result := TSmtpClient.Create(Command);
end;

function SmtpClientCommand: ISmtpClientCommand;
var
  Command: IAsciiProtocol;
begin
  Command := TAsciiProtocol.Create;
  Result := TSmtpClientCommand.Create(Command);
end;

end.