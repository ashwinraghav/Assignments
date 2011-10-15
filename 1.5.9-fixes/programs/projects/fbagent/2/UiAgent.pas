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

unit UiAgent;

interface

{$include Defines.inc}

uses
  Sil,

  UiRecurrence;

type
  ISchedule = interface;
  IStepList = interface;
  IStep = interface;
  IRunningStep = interface;

  IAgent = interface
    ['{D54654EF-40DF-4928-9ADB-734E18CCD5EA}']
    procedure Start;
    procedure Stop;
  end;

  IAgentSource = interface
    ['{1E112946-7663-45C3-BDFB-C3036FD6D997}']
    function Get(out Data: ISchedule): Boolean;
    procedure History(var HistoryId: Integer; ParentId, TaskId, StepId: Integer; const StartTime, EndTime: TDateTime; const Status: String; const Text: String = '');
    procedure Update(const Data: ISchedule; CalcOnly: Boolean);
  end;

  IAgentDispatch = interface (IAgent)
    ['{E11FED09-0CE8-4C73-85D7-B402E17B9B13}']
    procedure Put(const Data: ISchedule);
  end;

  IStep = interface
    ['{34676116-AE1C-4802-A316-7F2651828B41}']
    function GetId: Integer;
    function GetName: String;
    function GetOrder: Integer;
    function GetOnSuccess: Integer;
    function GetOnFailure: Integer;
    function GetData: String;
    property Name: String read GetName;
    property Order: Integer read GetOrder;
    property OnSuccess: Integer read GetOnSuccess;
    property OnFailure: Integer read GetOnFailure;
    property Data: String read GetData;
    property Id: Integer read GetId;
  end;

  IStepList = interface
    ['{63C34775-9A9C-440D-8048-3172B2C6F8E9}']
    function GetCount: Integer;
    function GetItem(Index: Integer): IStep;
    function Enumerate(var Enum: IEnumerator; out Item: IStep): Boolean;
    function AddStep(Id: Integer; const Name, Data: String; Order, OnSuccess, OnFailure: Integer): IStep;
    function ValidIndex(Index: Integer): Boolean;
    function First: IStep;
    function Last: IStep;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: IStep read GetItem; default;
  end;

  ISchedule = interface
    ['{81645903-1B57-46F7-9F46-A7E4CEE0442E}']
    function GetId: Integer;
    procedure SetId(Value: Integer);
    procedure SetName(const Value: String);
    function GetName: String;
    function GetTaskId: Integer;
    procedure SetTaskId(Value: Integer);
    function GetLastRun: TDateTime;
    procedure SetLastRun(const Value: TDateTime);
    function GetNextRun: TDateTime;
    procedure SetNextRun(const Value: TDateTime);
    function GetRecurrence: IRecurrence;
    procedure SetRecurrence(const Value: IRecurrence);
    function GetNotification: String;
    procedure SetNotification(const Value: String);
    function GetStepList: IStepList;
    function GetSource: IAgentSource;
    function GetHistoryId: Integer;
    procedure SetHistoryId(Value: Integer);
    property Id: Integer read GetId write SetId;
    property Name: String read GetName write SetName;
    property TaskId: Integer read GetTaskId write SetTaskId;
    property LastRun: TDateTime read GetLastRun write SetLastRun;
    property NextRun: TDateTime read GetNextRun write SetNextRun;
    property Recurrence: IRecurrence read GetRecurrence write SetRecurrence;
    property StepList: IStepList read GetStepList;
    property Source: IAgentSource read GetSource;
    property HistoryId: Integer read GetHistoryId write SetHistoryId;
    property Notification: String read GetNotification write SetNotification;
  end;

  IRunningStep = interface
    ['{9667ECCA-ADF1-4EC6-BB32-2904621C761F}']
    function GetSchedule: ISchedule;
    function GetFinished: Boolean;
    function GetStartTime: TDateTime;
    procedure Start;
    procedure Stop;
    property Schedule: ISchedule read GetSchedule;
    property Finished: Boolean read GetFinished;
    property StartTime: TDateTime read GetStartTime;
  end;

implementation

end.
