unit DmAgent;

interface

{$include Defines.inc}

uses
  SysUtils, IBDatabase, DB, IBCustomDataSet, IBStoredProc, Classes, IBQuery,

  Sil;

const
  AActionKind: array [0..1] of String = ('sql', 'application');

type
  RRecurrence = record
    TaskId: Integer;
    ScheduleId: Integer;
    RecurrenceId: Integer;
    Name: String;
    Enabled: Boolean;
    DelKind: Integer;
    StartTime: TDateTime;
    EndTime: TDateTime;
    Increment: TDateTime;
    FilterKind: Integer;
    DayFilter: Integer;
    DowPosFilter: Integer;
    MonthFilter: Integer;
    RunCount: Integer;
  end;

type
  TdaMain = class(TDataModule)
    dbAgent: TIBDatabase;
    taMain: TIBTransaction;
    doScheduleCheck: TIBStoredProc;
    doTaskCheck: TIBStoredProc;
    doStepCheck: TIBStoredProc;
    doStepDelete: TIBStoredProc;
    doScheduleDelete: TIBStoredProc;
    doRecurrenceCheck: TIBStoredProc;
    doTaskDelete: TIBStoredProc;
    procedure DataModuleCreate(Sender: TObject);
  private
    FConfigFile: String;
    procedure DoReadConfig;
  public
    procedure TaskCheck(var Id: Integer; const TaskName: String; const Category: Variant; Enabled: Boolean; const Description: String);
    procedure TaskDelete(Id: Integer);
    procedure ScheduleDelete(ScheduleId: Integer);
    procedure StepCheck(TaskId: Integer; var StepId: Integer; const StepName, Data: String; Enabled: Boolean; OnSuccess, OnFailure: Integer);
    procedure StepDelete(StepId: Integer);
    procedure RecurrenceCheck(var Info: RRecurrence);
    procedure Refresh(Query: TIBQuery);
  end;

var
  daMain: TdaMain;

implementation

uses SilLtXml, SilOiFile, SilOtFile, SilLiXml;

{$R *.dfm}

procedure TdaMain.DoReadConfig;
var
  Config: IXmlTree;
begin
  Config := Sil.Xml.ReadFile(FConfigFile, nil, fmAccessReadWrite, fmShareRead, false);

  with Config.GetTag('config/database', true) do
  begin
    dbAgent.DatabaseName := Childs.ReadString('name');
    dbAgent.Params.Values['user_name'] := Childs.ReadString('username');
    dbAgent.Params.Values['password'] := Childs.ReadString('password');
  end;
end;

procedure TdaMain.DataModuleCreate(Sender: TObject);
begin
  FConfigFile := Sil.OS.FileSystem.ChangeFileExt(Sil.OS.Process.Current.Info.FullName, '.xml');
  DoReadConfig;

  dbAgent.Open;
end;

procedure TdaMain.Refresh(Query: TIBQuery);
var
  BookMark: String;
begin
  try
    BookMark := Query.Bookmark;
    Query.DisableControls;
    Query.Close;
    Query.Transaction.Commit;
    Query.Open;
  finally
    if Query.BookmarkValid(PChar(BookMark)) then
      Query.Bookmark := BookMark;

    Query.EnableControls;
  end;
end;

procedure TdaMain.TaskCheck(var Id: Integer; const TaskName: String; const Category: Variant; Enabled: Boolean; const Description: String);
begin
  with doTaskCheck do
    try
      Transaction.StartTransaction;

      ParamByName('task_id').AsInteger := Id;
      ParamByName('name').AsString := TaskName;
      ParamByName('category_id').Value := Category;
      ParamByName('enabled').AsInteger := Ord(Enabled);
      ParamByName('description').AsString := Description;
      ExecProc;

      Id := ParamByName('new_task_id').AsInteger;

      Transaction.Commit;
    except
      Transaction.Rollback;
      raise;
    end;
end;

procedure TdaMain.TaskDelete(Id: Integer);
begin
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
end;

procedure TdaMain.ScheduleDelete(ScheduleId: Integer);
begin
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
end;

procedure TdaMain.StepCheck(TaskId: Integer; var StepId: Integer; const StepName, Data: String; Enabled: Boolean; OnSuccess, OnFailure: Integer);
begin
  with doStepCheck do
    try
      Transaction.StartTransaction;

      ParamByName('step_id').AsInteger := StepId;
      ParamByName('task_id').AsInteger := TaskId;
      ParamByName('name').AsString := StepName;
      ParamByName('item_order').AsInteger := 0;
      ParamByName('enabled').AsInteger := Ord(Enabled);
      ParamByName('on_success').AsInteger := OnSuccess;
      ParamByName('on_failure').AsInteger := OnFailure;
      ParamByName('data').AsString := Data;
      ExecProc;

      StepId := ParamByName('new_step_id').AsInteger;

      Transaction.Commit;
    except
      Transaction.Rollback;
      raise;
    end;
end;

procedure TdaMain.StepDelete(StepId: Integer);
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

procedure TdaMain.RecurrenceCheck(var Info: RRecurrence);
begin
  with doScheduleCheck do
    try
      Transaction.StartTransaction;

      ParamByName('schedule_id').AsInteger := Info.ScheduleId;
      ParamByName('task_id').AsInteger := Info.TaskId;
      ParamByName('name').AsString := Info.Name;
      ParamByName('enabled').AsInteger := Ord(Info.Enabled);
      ParamByName('del_kind').AsInteger := Info.DelKind;
      ExecProc;

      Info.ScheduleId := ParamByName('new_schedule_id').AsInteger;

      with doRecurrenceCheck do
      begin
        ParamByName('recurrence_id').AsInteger := Info.RecurrenceId;
        ParamByName('schedule_id').AsInteger := Info.ScheduleId;
        ParamByName('increment').AsDateTime := Info.Increment;
        ParamByName('day_filter_kind').AsInteger := Info.FilterKind; // 1=dow 2=dom
        ParamByName('day_filter').AsInteger := Info.DayFilter;
        ParamByName('dow_pos_filter').AsInteger := Info.DowPosFilter;
        ParamByName('month_filter').AsInteger := Info.MonthFilter;
        ParamByName('start_time').AsDateTime := Time.Extract(Info.StartTime);
        ParamByName('start_date').AsDateTime := Date.Extract(Info.StartTime);
        ParamByName('end_time').AsDateTime := Time.Extract(Info.EndTime);
        ParamByName('end_date').AsDateTime := Date.Extract(Info.EndTime);
        ParamByName('run_count').Value := Info.RunCount;
        ExecProc;

        Info.RecurrenceId := ParamByName('new_recurrence_id').AsInteger;
      end;

      Transaction.Commit;
    except
      Transaction.Rollback;
      raise;
    end;
end;

end.
