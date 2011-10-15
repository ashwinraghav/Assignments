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

unit SilNotify;

interface

uses
  SilBkTool,
  SilSiNotify;

type
  IEventNotifier                      = SilSiNotify.IEventNotifier;
  IEventServer                        = SilSiNotify.IEventServer;
  IEventData                          = SilSiNotify.IEventData;
  IEventNotifierListener              = SilSiNotify.IEventNotifierListener;
  IEventConfig                        = SilSiNotify.IEventConfig;
  IAttachment                         = SilSiNotify.IAttachment;
  IAttachmentList                     = SilSiNotify.IAttachmentList;

type
  Tk = class (Tool)
    class function RegistryConfig(const Root: String): IEventConfig;
    class function Notifier: IEventNotifier;
    class function MailServer: IEventServer;
  end;

implementation

uses
  SilSmMailNotify,
  SilSmNotify,
  SilSmNotifyReg;

{ Tk }

class function Tk.MailServer: IEventServer;
begin
  Result := TMailNotifier.Create;
end;

class function Tk.Notifier: IEventNotifier;
begin
  Result := TEventNotifier.Create;
end;

class function Tk.RegistryConfig(const Root: String): IEventConfig;
begin
  Result := TRegistryEventConfig.Create(Root);
end;

end.
 