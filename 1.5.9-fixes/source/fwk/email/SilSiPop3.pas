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

unit SilSiPop3;

interface

uses
  SilBeTypes,
  SilLiStringList,
  SilSiMailClient;

type
	IPop3Client = interface
		['{CAF5248D-080A-11D4-9150-00C0261013CD}']
		function GetMessages: IMailMessageList;
		function GetCoders: IStringList;
		function Login(const UserName, Password: String): Boolean;
		function Logout: Boolean;
		function RetrieveHeader(Number: Integer; MessageLines: Cardinal = 0): Boolean;
		function RetrieveMessage(Number: Integer): Boolean;
		function RetrieveUIDL(Number: Integer): Boolean;
		function DeleteMessage(Number: Integer): Boolean;
		function RetrieveAllHeaders: Boolean;
		function RetrieveAllMessages: Boolean;
		function RetrieveAllUIDLs: Boolean;
		function DeleteAllMessages: Boolean;
		function RetrieveMessageList: Boolean;
		function RetrieveInfo: Boolean;
		property Messages: IMailMessageList read GetMessages;
		property Coders: IStringList read GetCoders;
	end;

  RPop3Status = record
    Result: Boolean;
    Comment: String;
  end;

  RPop3Event = record
    Sender: IUnknown;
    Status: RPop3Status;
  end;

  RPop3LoginEvent = record
    Sender: IUnknown;
    Status: RPop3Status;
    UserName: String;
    Password: String;
  end;

  RPop3MsgEvent = record
    Sender: IUnknown;
    Status: RPop3Status;
    Msg: String;
  end;

  RPop3UidlEvent = record
    Sender: IUnknown;
    Status: RPop3Status;
    Uidl: String;
  end;

  RPop3UidlListEvent = record
    Sender: IUnknown;
    Status: RPop3Status;
    Uidls: TStringArray;
  end;

  RPop3MsgListEvent = record
    Sender: IUnknown;
    Status: RPop3Status;
    Sizes: TCardinalArray;
  end;

  RPop3InfoEvent = record
    Sender: IUnknown;
    Status: RPop3Status;
    TotalCount: LongWord;
    TotalSize: LongWord;
  end;

  IPop3ServerEvents = interface
    ['{D7ABB9C5-7301-4850-A48C-BFAA555943D8}']
    procedure OnSendGreetings(var Event: RPop3Event);
		procedure OnLogin(var Event: RPop3LoginEvent);
		procedure OnLogout(var Event: RPop3Event);
		procedure OnRetrieveHeader(var Event: RPop3MsgEvent);
		procedure OnRetrieveMessage(var Event: RPop3MsgEvent);
		procedure OnRetrieveUidl(var Event: RPop3UidlEvent);
		procedure OnDeleteMessage(var Event: RPop3Event);
		procedure OnRetrieveUidlList(var Event: RPop3UidlListEvent);
		procedure OnRetrieveMessageList(var Event: RPop3MsgListEvent);
		procedure OnRetrieveInfo(var Event: RPop3InfoEvent);
  end;

implementation

end.
 