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

unit SilOiProcess;

{$I Defines.inc}

interface

uses
  SilLiEnumerator,
  SilOiFile,
  SilOeProcess,
  SilOiModule,
  SilOiHandle;

type
  IProcess = interface (IHandled)
    ['{DB0ADFD1-1FDB-11D4-916F-00C0261013CD}']
    function GetPriority: TProcessPriority;
    procedure SetPriority(Value: TProcessPriority);
    function Terminate( Timeout: Cardinal = 0 ): boolean;
    function GetInfo: IFileInfo;
    function GetPID: Cardinal;
    function GetMainThreadID: Cardinal;
    function GetModules: IModules;
    function GetCurrentPath: string;
    function GetExitCode: LongWord;
    property Priority: TProcessPriority read GetPriority write SetPriority;
    property Info: IFileInfo read GetInfo;
    property PID: Cardinal read GetPID;
    property MainThreadID: Cardinal read GetMainThreadID;
    property Modules: IModules read GetModules;
    property CurrentPath: string read GetCurrentPath;
    property ExitCode: LongWord read GetExitCode;
  end;

  IProcessList = interface
    ['{86B11592-EBA4-4859-A592-04D7ED712709}']
    function GetCount: Integer;
    function Get(Index: Integer): IProcess;
    function Enumerate(var Enum: IEnumerator; out Item: IProcess): Boolean;
    function ValidIndex(Index: Integer): Boolean;
    procedure CheckIndex(Index: Integer);
    procedure Put(Index: Integer; const Value: IProcess);
    function Add(const Value: IProcess): Integer;
    procedure Delete(Index: Integer);
    procedure Insert(Index: Integer; const Value: IProcess);
    function Remove(const Item: IUnknown): Integer;
    function IndexOf(const Item: IUnknown): Integer;
    procedure Exchange(Index1, Index2: Integer);
    procedure Clear;
    property Items[Index: Integer]: IProcess read Get write Put; default;
    property Count: Integer read GetCount;
  end;

implementation

end.
