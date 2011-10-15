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

unit DmAgent;

interface

{$include Defines.inc}

uses
  Windows, SysUtils, IBDatabase, DB, IBCustomDataSet, IBStoredProc, Classes, IBQuery,

  Sil,
  UiRecurrence;

const
  AActionKind: array [0..2] of String = ('sql', 'application', 'backup');

type
  RRecurrence = record
    TaskId: Integer;
    ScheduleId: Integer;
    Name: String;
    Enabled: Boolean;
    DelKind: Integer;
    RunAt: TDateTime;
  end;

type
  TdaAgent = class(TDataModule)
    dbAgent: TIBDatabase;
    taMain: TIBTransaction;
    doScheduleCheck: TIBStoredProc;
    doTaskCheck: TIBStoredProc;
    doStepCheck: TIBStoredProc;
    doStepDelete: TIBStoredProc;
    doScheduleDelete: TIBStoredProc;
    doTaskDelete: TIBStoredProc;
    doStepChangeOrder: TIBStoredProc;
  private
    procedure DoReadConfig;
  public
    procedure ReadConfig;
    procedure TaskCheck(var Id: Integer; const TaskName: String; const Category, NotValue: Variant; Enabled: Boolean; const Description: String);
    procedure TaskDelete(Id: Integer);
    procedure ScheduleDelete(ScheduleId: Integer);
    procedure StepCheck(TaskId: Integer; var StepId: Integer; const StepName, Data: String; Enabled: Boolean; Order, OnSuccess, OnFailure: Integer);
    procedure StepDelete(StepId: Integer);
    procedure RecurrenceCheck(var Info: RRecurrence; const Recurrence: IRecurrence);
    procedure Refresh(Query: TIBQuery);
    procedure StepChangeOrder(StepId, Order: Integer);
    function GetRecurrence(quQuery: TIBQuery; out Recurrence: IRecurrence): Boolean;
  end;

var
  daAgent: TdaAgent;

implementation

uses
  Forms,

  UtCoder,
  UmRecurrence,
  UfConfig,
  UtAgent, SilBeTypes;

{$R *.dfm}

procedure TdaAgent.DoReadConfig;
begin
  Sil.Trace.Enter(Self, 'DoReadConfig');

  with Config.GetTag('config/database', true) do
  begin
    if dbAgent.Connected then
      dbAgent.Close;

    dbAgent.DatabaseName := Childs.ReadString('name');
    dbAgent.Params.Values['user_name'] := Childs.ReadString('username');
    dbAgent.Params.Values['password'] := Coder.Decrypt(Childs.ReadString('password'));
  end;

  Sil.Trace.Leave;
end;

procedure TdaAgent.ReadConfig;
begin
  Sil.Trace.Enter(Self, 'DataModuleCreate');

  DoReadConfig;

  try
    dbAgent.Open;
  except
    Application.MessageBox(
      PChar(Str.Format('Unable to connect to database "%s". Please, check settings.', [dbAgent.DatabaseName])),
      PChar(FBAGENT_GUI_TITLE), MB_ICONSTOP);                                     
    raise;
  end;

  Sil.Trace.Leave;
end;

procedure TdaAgent.Refresh(Query: TIBQuery);
var
  BookMark: String;
begin
  Sil.Trace.Enter(Self, 'Refresh');

  try
    BookMark := Query.Bookmark;
    Query.DisableControls;
    Query.Close;

    if Query.Transaction.Active then
      Query.Transaction.Commit;

    Query.Open;
  finally
    if Query.BookmarkValid(PChar(BookMark)) then
      Query.Bookmark := BookMark;

    Query.EnableControls;
  end;

  Sil.Trace.Leave;
end;

procedure TdaAgent.TaskCheck(var Id: Integer; const TaskName: String; const Category, NotValue: Variant; Enabled: Boolean; const Description: String);
begin
  Sil.Trace.Enter(Self, 'TaskCheck', [Id, TaskName]);

  with doTaskCheck do
    try
      Transaction.StartTransaction;

      ParamByName('task_id').AsInteger := Id;
      ParamByName('name').AsString := TaskName;
      ParamByName('category_id').Value := Category;
      ParamByName('enabled').AsInteger := Ord(Enabled);
      ParamByName('description').AsString := Description;
      ParamByName('notification').Value := NotValue;
      ExecProc;

      Id := ParamByName('new_task_id').AsInteger;

      Transaction.Commit;
    except
      Transaction.Rollback;
      raise;
    end;

  Sil.Trace.Leave;
end;

procedure TdaAgent.TaskDelete(Id: Integer);
begin
  Sil.Trace.Enter(Self, 'TaskDelete', [Id]);

  with doTaskDelete do
    try
      Transaction.StartTransaction;

      ParamByName('task_id').AsInteger := Id;
      ExecProc;

      Transaction.Commit;
    except
      Transaction.Rollback;
      raise;
    end;

  Sil.Trace.Leave;
end;

procedure TdaAgent.ScheduleDelete(ScheduleId: Integer);
begin
  Sil.Trace.Enter(Self, 'ScheduleDelete', [ScheduleId]);

  if ScheduleId > 0 then
    with doScheduleDelete do
      try
        Transaction.StartTransaction;

        ParamByName('schedule_id').AsInteger := ScheduleId;
        ExecProc;

        Transaction.Commit;
      except
        Transaction.Rollback;
        raise;
      end;

  Sil.Trace.Leave;
end;

procedure TdaAgent.StepCheck(TaskId: Integer; var StepId: Integer; const StepName, Data: String; Enabled: Boolean; Order, OnSuccess, OnFailure: Integer);
begin
  Sil.Trace.Enter(Self, 'StepCheck', [TaskId, StepId, StepName]);

  with doStepCheck do
    try
      Transaction.StartTransaction;

      ParamByName('step_id').AsInteger := StepId;
      ParamByName('task_id').AsInteger := TaskId;
      ParamByName('name').AsString := StepName;
      ParamByName('item_order').AsInteger := Order;
      ParamByName('enabled').AsInteger := Ord(Enabled);
      ParamByName('on_success').AsInteger := OnSuccess - 3;
      ParamByName('on_failure').AsInteger := OnFailure - 3;
      ParamByName('data').AsBlob := Data;
      ExecProc;

      StepId := ParamByName('new_step_id').AsInteger;

      Transaction.Commit;
    except
      Transaction.Rollback;
      raise;
    end;

  Sil.Trace.Leave;
end;

procedure TdaAgent.StepDelete(StepId: Integer);
begin
  with doStepDelete do
    try
      Transaction.StartTransaction;

      ParamByName('step_id').AsInteger := StepId;
      ExecProc;

      Transaction.Commit;
    except
      Transaction.Rollback;
      raise;
    end;
end;

procedure TdaAgent.RecurrenceCheck(var Info: RRecurrence; const Recurrence: IRecurrence);

  function DoSetArray(Value: Integer; const List: TWordArray; Start: Integer): Integer;
  var
    i: Integer;
  begin
    Result := Value;

    for i := 0 to Length(List) - 1 do
      if List[i] > 0 then
        Result := Result or (1 shl (i + Start - 1));
  end;

var
  Increment: TDateTime;
  DayKind: SmallInt;
  DayMask, DowMask, MonthMask: Integer;
begin
  with doScheduleCheck do
    try
      Transaction.StartTransaction;

      ParamByName('schedule_id').AsInteger := Info.ScheduleId;
      ParamByName('task_id').AsInteger := Info.TaskId;
      ParamByName('name').AsString := Info.Name;
      ParamByName('enabled').AsInteger := Ord(Info.Enabled);
      ParamByName('del_kind').AsInteger := Info.DelKind;
      ParamByName('schedule_id').AsInteger := Info.ScheduleId;

      if Recurrence.OccursKind = okOnce then
      begin
        ParamByName('increment').AsDateTime := 0;
        ParamByName('day_filter_kind').AsInteger := 0;
        ParamByName('day_filter').AsInteger := 0;
        ParamByName('dow_pos_filter').AsInteger := 0;
        ParamByName('month_filter').AsInteger := 0;
        ParamByName('start_time').AsDateTime := Time.Extract(Info.RunAt);
        ParamByName('start_date').AsDateTime := Date.Extract(Info.RunAt);
        ParamByName('end_time').AsDateTime := 0;
        ParamByName('end_date').AsDateTime := 0;
        ParamByName('run_count').Value := 0;
      end else
      begin
        // unit: 0=min 1=hour 2=day
        Increment := 0;
        DateTime.Inc(Increment, 4 - Recurrence.EveryUnit, Recurrence.Every);
        ParamByName('increment').AsDateTime := Increment;

        DayKind := Recurrence.EveryUnit or (Ord(Recurrence.FilterKind) shl 8);

        DowMask := DoSetArray(0, Recurrence.DayOfWeek, 1);
        DowMask := DoSetArray(DowMask, Recurrence.DayPosition, 9);

        DayMask := DoSetArray(0, Recurrence.DayNumber, 1);
        MonthMask := DoSetArray(0, Recurrence.Months, 1);

        ParamByName('day_filter_kind').AsInteger := DayKind;
        ParamByName('dow_pos_filter').AsInteger := DowMask;
        ParamByName('day_filter').AsInteger := DayMask;
        ParamByName('month_filter').AsInteger := MonthMask;

        ParamByName('start_time').AsDateTime := Time.Extract(Recurrence.StartTime);
        ParamByName('start_date').AsDateTime := Date.Extract(Recurrence.StartDate);
        ParamByName('end_time').AsDateTime := Time.Extract(Recurrence.EndTime);
        ParamByName('end_date').AsDateTime := Date.Extract(Recurrence.EndDate);
        ParamByName('run_count').Value := Recurrence.EndCount;
      end;

      ExecProc;

      Info.ScheduleId := ParamByName('new_schedule_id').AsInteger;

      Transaction.Commit;
    except
      Transaction.Rollback;
      raise;
    end;
end;

procedure TdaAgent.StepChangeOrder(StepId, Order: Integer);
begin
  with doStepChangeOrder do
    try
      Transaction.StartTransaction;

      ParamByName('step_id').AsInteger := StepId;
      ParamByName('item_order').AsInteger := Order;
      ExecProc;

      Transaction.Commit;
    except
      Transaction.Rollback;
      raise;
    end;
end;

function TdaAgent.GetRecurrence(quQuery: TIBQuery; out Recurrence: IRecurrence): Boolean;
begin
  Sil.Trace.Enter(Self, 'GetRecurrence');

  Result := false;
  Recurrence := TRecurrence.Create;

  try
    with quQuery do
      Recurrence.Configure(
        FieldByName('start_date').AsDateTime,
        FieldByName('end_date').AsDateTime,
        FieldByName('start_time').AsDateTime,
        FieldByName('end_time').AsDateTime,
        FieldByName('increment').AsDatetime,
        FieldByName('run_count').AsInteger,
        FieldByName('day_filter_kind').AsInteger,
        FieldByName('dow_pos_filter').AsInteger,
        FieldByName('day_filter').AsInteger,
        FieldByName('month_filter').AsInteger);
  except
    Sil.Trace.Exception;
    Recurrence := nil;
  end;

  Sil.Trace.Leave;
end;

end.
