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

unit SilSiSmtpCommand;

interface

uses
  SilSiAsciiProtocol;

type
  ISmtpClientCommand = interface (IAsciiProtocol) 
    ['{436B1ECF-9B13-4268-8B97-FB7021B1FA30}']
    function WaitGreetings(out Response: String): Boolean;
    function Hello(const Domain: String; out Response: String): Boolean;
    function Verify(const UserName: String; out Response: String): Boolean;
    function Quit(out Response: String): Boolean;
    function MailFrom(const Account: String; out Response: String): Boolean;
    function SendFrom(const Account: String; out Response: String): Boolean;
    function SendOrMailFrom(const Account: String; out Response: String): Boolean;
    function SendAndMailFrom(const Account: String; out Response: String): Boolean;
    function Recipient(const Account: String; out Response: String): Boolean;
    function Data(out Response: String): Boolean;
    function WaitDelivery(out Response: String): Boolean;
    function Reset(out Response: String): Boolean;
    function Turn(out Response: String): Boolean;
    function Expand(const Account: String; out Response: String): Boolean;
    function Help(out Response: String): Boolean; overload;
    function Help(const Command: String; out Response: String): Boolean; overload;
    function Noop(out Response: String): Boolean;
  end;

implementation

end.
 