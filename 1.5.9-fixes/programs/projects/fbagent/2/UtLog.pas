{********************************************************************************
 *                  Standard Interface Library (SIL)                            *
 *                                                                              *
 *       General purpose library whose design is based in STRONG                *
 *   use of interfaces.                                                         *
 *                                                                              *
 *                                                                              *
 *     Copyright (C) 2000 Mariano Podestá    marianop@intercom.com.ar           *
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

unit UtLog;

interface

uses
  Sil,
  SilLog,
  SilTool;

const
  CRootKey = '$System\SOFTWARE\Sil';

type
  Log = class (Tool)
    class procedure Initialize;
    class procedure Finalize;
  end;

implementation

{ Log }

class procedure Log.Initialize;
begin
  with Sil.OS.Process.Current.Info do
  begin
    SilLog.Logger.Initialize(CRootKey, SilLog.LogService, SilLog.StackFmt);
    Sil.OS.Environment.Load(CRootKey);
    Sil.Trace.Log('process=%s stamp=%s', [FullName, DateTime.ToStr(Time, 'yyyy-mm-dd hh:nn:ss')]);
  end;
end;

class procedure Log.Finalize;
begin
  SilLog.Logger.Finalize;
end;

end.

