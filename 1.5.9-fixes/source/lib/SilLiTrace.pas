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

unit SilLiTrace;

{$I Defines.inc}

interface

uses
  SilBeTypes,
  SilBeMemMgr;

type
  ITrace = interface;
  IThreadData = interface;
  
  ITraceStack = interface
    ['{2BDB33F1-AC56-11D4-ADAB-00902794F778}']
    function IsEmpty: Boolean;
    function Level: Integer;
    function Top: ITrace;
    procedure Push(const Item: ITrace);
    procedure Pop;
  end;

  ITraceData = interface
    ['{368D9D31-BCDC-11D4-ADAE-00902794F778}']
    function GetLevel: Integer;
    function GetName: string;
    function GetPrototype: string;
    function GetTime: TDateTime;
    function GetElapsed: TDateTime;
    function GetMemory: RMemoryInfo;
    function GetThread: IThreadData;
    function GetModule: string;
    function Refresh: ITraceData;
    property Level: Integer read GetLevel;
    property Name: string read GetName;
    property Prototype: string read GetPrototype;
    property Time: TDateTime read GetTime;
    property Elapsed: TDateTime read GetElapsed;
    property Memory: RMemoryInfo read GetMemory;
    property Thread: IThreadData read GetThread;
    property Module: string read GetModule;
  end;

  ITrace = interface
    ['{47F39105-5781-11D4-AD8B-00902794F778}']
    function GetData: ITraceData;
    function GetTimeInitial: Double;
    function GetTimeCurrent: Double;
    function TimeElapsed(Reset: Boolean = False): Double; overload; 
    function TimeElapsed(From: Double): Double; overload; 
    procedure Log(const Message: string; const Args: array of const; const Sender: string = ''; const Category: string = ''); overload;
    procedure Log(const Message: string; const Sender: string = ''; const Category: string = ''); overload;
    procedure Error(const Message: string; const Args: array of const; const Sender: string = ''; const Category: string = ''); overload;
    procedure Error(const Message: string; const Sender: string = ''; const Category: string = ''); overload;
    procedure Exit(const Message: string = ''); overload; 
    procedure Exit(const Message: string; const Args: array of const); overload; 
    property Data: ITraceData read GetData;
    property TimeInitial: Double read GetTimeInitial;
    property TimeCurrent: Double read GetTimeCurrent;
  end;

  IThreadData = interface
    ['{0D6068E1-ADD9-11D4-9DDA-00C0DFE46337}']
    function ID: LongWord;
    function Name: string;
  end;

implementation

end.
