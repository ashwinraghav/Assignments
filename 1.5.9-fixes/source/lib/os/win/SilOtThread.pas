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

unit SilOtThread;

{$I Defines.inc}

interface

uses
  SilBeTypes,
  SilOiThread,
  SilOjThread;

type
  WindowsThread = class (SilThread)
    class function List: IThreadList; override;
    class function ID: LongWord; override;
    class function IsMain: Boolean; override;
    class procedure SyncCall(Method: TThreadCallMethod; const Param: Pointer); override;
    class procedure SyncCall(Method: TThreadCallMethod); override;
    class procedure SyncCall(Method: TThreadCallMethod; const Obj: IUnknown; Param: Pointer = nil); override;
    class procedure AsyncCall(Method: TThreadCallMethod; const Param: Pointer); override;
    class procedure AsyncCall(Method: TThreadCallMethod); override;
    class procedure AsyncCall(Method: TThreadCallMethod; const Obj: IUnknown; Param: Pointer = nil); override;
  end;

implementation

uses
  SilOfThreadList,
  SilOfThread;

{ Thread }

class function WindowsThread.List: IThreadList;
begin
  Result := SilOfThreadList.GetList;
end;

class function WindowsThread.ID: LongWord;
begin
  Result := SilOfThread.GetCurrentId;
end;

class procedure WindowsThread.AsyncCall(Method: TThreadCallMethod; const Obj: IInterface; Param: Pointer);
begin
  SilOfThreadList.GetList.ThreadCall(Obj, tcAsync, Method, Param);
end;

class procedure WindowsThread.AsyncCall(Method: TThreadCallMethod; const Param: Pointer);
begin
  SilOfThreadList.GetList.ThreadCall(nil, tcAsync, Method, Param);
end;

class procedure WindowsThread.AsyncCall(Method: TThreadCallMethod);
begin
  SilOfThreadList.GetList.ThreadCall(nil, tcAsync, Method, nil);
end;

class procedure WindowsThread.SyncCall(Method: TThreadCallMethod; const Param: Pointer);
begin
  SilOfThreadList.GetList.ThreadCall(nil, tcSync, Method, Param);
end;

class procedure WindowsThread.SyncCall(Method: TThreadCallMethod; const Obj: IInterface; Param: Pointer);
begin
  SilOfThreadList.GetList.ThreadCall(Obj, tcSync, Method, Param);
end;

class procedure WindowsThread.SyncCall(Method: TThreadCallMethod);
begin
  SilOfThreadList.GetList.ThreadCall(nil, tcSync, Method, nil);
end;

class function WindowsThread.IsMain: Boolean;
begin
  Result := SilOfThread.GetCurrentId = System.MainThreadID;
end;

end.
