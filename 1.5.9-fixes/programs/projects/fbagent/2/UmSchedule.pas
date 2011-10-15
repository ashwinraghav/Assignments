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

unit UmSchedule;

interface

{$include Defines.inc}

uses
  Sil,

  UiAgent,
  UiRecurrence;

type
  TStep = class (TSilObject, IStep)
  private
    FId: Integer;
    FName: String;
    FOrder: Integer;
    FOnSuccess: Integer;
    FOnFailure: Integer;
    FData: String;
  protected // IStep
    function GetName: String;
    function GetOrder: Integer;
    function GetOnSuccess: Integer;
    function GetOnFailure: Integer;
    function GetData: String;
    function GetId: Integer;
  public
    constructor Create(Id: Integer; const Name, Data: String; Order, OnSuccess, OnFailure: Integer);
  end;

  TStepList = class (TSilObject, IStepList)
  private
    FList: IInterfaceList;
  protected // IStepList
    function GetCount: Integer;
    function GetItem(Index: Integer): IStep;
    function Enumerate(var Enum: IEnumerator; out Item: IStep): Boolean;
    function AddStep(Id: Integer; const Name, Data: String; Order, OnSuccess, OnFailure: Integer): IStep;
    function ValidIndex(Index: Integer): Boolean;
    function First: IStep;
    function Last: IStep;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TSchedule = class (TSilObject, ISchedule)
  private
    FId: Integer;
    FName: String;
    FTaskId: Integer;
    FLastRun: TDateTime;
    FNextRun: TDateTime;
    FStepList: IStepList;
    FSource: IAgentSource;
    FHistoryId: Integer;
    FNotification: String;
    FRecurrence: IRecurrence;
  protected // ISchedule
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
    function GetStepList: IStepList;
    function GetSource: IAgentSource;
    function GetHistoryId: Integer;
    procedure SetHistoryId(Value: Integer);
    function GetNotification: String;
    procedure SetNotification(const Value: String);
  public
    constructor Create(const Source: IAgentSource; const Name: String);
    destructor Destroy; override;
  end;

implementation

uses
  UmRecurrence;

{ TSchedule }

constructor TSchedule.Create(const Source: IAgentSource; const Name: String);
begin
  inherited Create;

  FSource := Source;
  FName := Name;
  FStepList := TStepList.Create;
  FRecurrence := TRecurrence.Create;
end;

destructor TSchedule.Destroy;
begin
  FStepList := nil;
  FSource := nil;
  FRecurrence := nil;

  inherited;
end;

function TSchedule.GetHistoryId: Integer;
begin
  Result := FHistoryId;
end;

function TSchedule.GetId: Integer;
begin
  Result := FId;
end;

function TSchedule.GetLastRun: TDateTime;
begin
  Result := FLastRun;
end;

function TSchedule.GetName: String;
begin
  Result := FName;
end;

function TSchedule.GetNextRun: TDateTime;
begin
  Result := FNextRun;
end;

function TSchedule.GetNotification: String;
begin
  Result := FNotification;
end;

function TSchedule.GetRecurrence: IRecurrence;
begin
  Result := FRecurrence;
end;

function TSchedule.GetSource: IAgentSource;
begin
  Result := FSource;
end;

function TSchedule.GetStepList: IStepList;
begin
  Result := FStepList;
end;

function TSchedule.GetTaskId: Integer;
begin
  Result := FTaskId;
end;

procedure TSchedule.SetHistoryId(Value: Integer);
begin
  FHistoryId := Value;
end;

procedure TSchedule.SetId(Value: Integer);
begin
  FId := Value;
end;

procedure TSchedule.SetLastRun(const Value: TDateTime);
begin
  FLastRun := Value;
end;

procedure TSchedule.SetName(const Value: String);
begin
  FName := Value;
end;

procedure TSchedule.SetNextRun(const Value: TDateTime);
begin
  FNextRun := Value;
end;

procedure TSchedule.SetNotification(const Value: String);
begin
  FNotification := Value;
end;

procedure TSchedule.SetRecurrence(const Value: IRecurrence);
begin
  FRecurrence := Value;
end;

procedure TSchedule.SetTaskId(Value: Integer);
begin
  FTaskId := Value;
end;

{ TStep }

constructor TStep.Create(Id: Integer; const Name, Data: String; Order, OnSuccess, OnFailure: Integer);
begin
  inherited Create;

  FId := Id;
  FName := Name;
  FData := Data;
  FOrder := Order;
  FOnSuccess := OnSuccess;
  FOnFailure := OnFailure;
end;

function TStep.GetData: String;
begin
  Result := FData;
end;

function TStep.GetId: Integer;
begin
  Result := FId;
end;

function TStep.GetName: String;
begin
  Result := FName;
end;

function TStep.GetOnFailure: Integer;
begin
  Result := FOnFailure;
end;

function TStep.GetOnSuccess: Integer;
begin
  Result := FOnSuccess;
end;

function TStep.GetOrder: Integer;
begin
  Result := FOrder;
end;

{ TStepList }

function TStepList.AddStep(Id: Integer; const Name, Data: String; Order, OnSuccess, OnFailure: Integer): IStep;
begin
  Result := TStep.Create(Id, Name, Data, Order, OnSuccess, OnFailure);
  FList.Add(Result);
end;

constructor TStepList.Create;
begin
  inherited Create;
  FList := Sil.List.InterfaceList;
end;

destructor TStepList.Destroy;
begin
  FList := nil;
  inherited;
end;

function TStepList.Enumerate(var Enum: IEnumerator; out Item: IStep): Boolean;
begin
  Result := FList.Enumerate(Enum, Item);
end;

function TStepList.First: IStep;
begin
  Result := IStep(FList.First);
end;

function TStepList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TStepList.GetItem(Index: Integer): IStep;
begin
  Result := IStep(FList.Items[Index]);
end;

function TStepList.Last: IStep;
begin
  Result := IStep(FList.Last);
end;

function TStepList.ValidIndex(Index: Integer): Boolean;
begin
  Result := FList.ValidIndex(Index);
end;

end.
