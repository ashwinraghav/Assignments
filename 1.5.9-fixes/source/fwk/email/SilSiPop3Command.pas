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

unit SilSiPop3Command;
 
interface

uses
  SilSiAsciiProtocol;

type
  IPop3ClientCommand = interface (IAsciiProtocol)
    ['{653DCCB5-C652-42BB-BBFF-88A8BF0B1021}']
    function WaitGreetings(out Response: String): Boolean;
    function User(const Name: String; out Response: String): Boolean;
    function Password(const Value: String; out Response: String): Boolean;
    function Quit(out Response: String): Boolean;
    function Status(out Response: String): Boolean;
    function MessageList(out Response: String): Boolean;
    function MessageInfo(const MessageNumber: LongWord; out Response: String): Boolean;
    function Retrieve(const MessageNumber: LongWord; out Response: String): Boolean;
    function Delete(const MessageNumber: LongWord; out Response: String): Boolean;
    function Noop(out Response: String): Boolean;
    function Reset(out Response: String): Boolean;
    function Header(const MessageNumber, Lines: LongWord; out Response: String): Boolean;
    function Uidl(const MessageNumber: LongWord; out Response: String): Boolean;
    function Uidls(out Response: String): Boolean;
    function APop(const Mailbox, Digest: String; out Response: String): Boolean;
  end;

implementation

end.
 