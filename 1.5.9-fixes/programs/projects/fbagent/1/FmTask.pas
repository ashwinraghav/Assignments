unit FmTask;

interface

{$include Defines.inc}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, DBGrids, StdCtrls, ComCtrls,

  Sil, DB, IBCustomDataSet, IBQuery, IBDatabase, IBStoredProc;

type
  TfoTask = class(TForm)
    pcTasks: TPageControl;
    tsTask: TTabSheet;
    TabSheet2: TTabSheet;
    Label1: TLabel;
    edTaskName: TEdit;
    cbTaskEnabled: TCheckBox;
    meTaskDescription: TMemo;
    Label2: TLabel;
    Label3: TLabel;
    edTaskCategory: TComboBox;
    btCancel: TButton;
    TabSheet3: TTabSheet;
    gtSteps: TDBGrid;
    grSchedules: TDBGrid;
    btStepAdd: TButton;
    btStepChange: TButton;
    btStepDelete: TButton;
    btScheduleAdd: TButton;
    btScheduleChange: TButton;
    btScheduleDelete: TButton;
    btDeleteCategory: TButton;
    taStep: TIBTransaction;
    qryStep: TIBQuery;
    dsStep: TDataSource;
    qrySchedule: TIBQuery;
    dsSchedule: TDataSource;
    taSchedule: TIBTransaction;
    btAccept: TButton;
    qryCategory: TIBQuery;
    doCategoryCheck: TIBStoredProc;
    taCategory: TIBTransaction;
    procedure btCancelClick(Sender: TObject);
    procedure btAcceptClick(Sender: TObject);
    procedure btStepAddClick(Sender: TObject);
    procedure btStepChangeClick(Sender: TObject);
    procedure btStepDeleteClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btScheduleAddClick(Sender: TObject);
    procedure btScheduleChangeClick(Sender: TObject);
    procedure btScheduleDeleteClick(Sender: TObject);
  private
    FTaskId: Integer;
    FTempInsert: Boolean;
    function DoCheckCategory: Integer;
    procedure DoApplyChanges;
    procedure DoCheckTemp;
  public
    procedure SetParams(TaskId: Integer; const Name: String; Enabled: Boolean; IdCategory: Integer; const Description: String);
  end;

var
  foTask: TfoTask;

implementation

uses
  DmAgent,
  FmSchedule, FmStep;

{$R *.dfm}

{ TfoTask }

procedure TfoTask.FormCreate(Sender: TObject);
begin
  pcTasks.ActivePageIndex := 0;
  FTempInsert := false;
end;

procedure TfoTask.SetParams(TaskId: Integer; const Name: String; Enabled: Boolean; IdCategory: Integer; const Description: String);
var
  Idx: Integer;
begin
  FTaskId := TaskId;
  edTaskName.Text := Name;
  cbTaskEnabled.Checked := Enabled;
  meTaskDescription.Text := Description;

  qryStep.ParamByName('task_id').Value := TaskId;
  qryStep.Open;

  Idx := -1;

  try
    taCategory.StartTransaction;
    qryCategory.Open;

    with qryCategory do
      while not Eof do
      begin
        edTaskCategory.Items.AddObject(FieldByName('name').AsString, Pointer(FieldByName('category_id').AsInteger));

        if FieldByName('category_id').AsInteger = IdCategory then
          Idx := edTaskCategory.Items.Count - 1;

        Next;
      end;
  finally
    taCategory.Commit;
  end;

  edTaskCategory.ItemIndex := Idx;

  qrySchedule.ParamByName('task_id').Value := TaskId;
  qrySchedule.Open;
end;

procedure TfoTask.btCancelClick(Sender: TObject);
begin
  if FTempInsert then
    daMain.TaskDelete(FTaskId);

  Close;
end;

function TfoTask.DoCheckCategory: Integer;
begin
  try
    taCategory.StartTransaction;

    doCategoryCheck.ParamByName('name').AsString := Str.Trim(edTaskCategory.Text);
    doCategoryCheck.ExecProc;

    Result := doCategoryCheck.ParamByName('category_id').AsInteger;
  finally
    taCategory.Commit;
  end;
end;

procedure TfoTask.btAcceptClick(Sender: TObject);
begin
  DoApplyChanges;
end;

procedure TfoTask.DoApplyChanges;
var
  Category: Variant;
begin
  if edTaskCategory.ItemIndex = -1 then
  begin
    if Str.NotEmpty(edTaskCategory.Text, true) then
      Category := DoCheckCategory else
      Category := Vart.Null;
  end else
    Category := Integer(edTaskCategory.Items.Objects[edTaskCategory.ItemIndex]);

  daMain.TaskCheck(FTaskId, edTaskName.Text, Category, cbTaskEnabled.Checked, meTaskDescription.Text);
end;

procedure TfoTask.btStepAddClick(Sender: TObject);
begin
  DoCheckTemp;

  with TfoStep.Create(Self) do
  begin
    SetParams(FTaskId, nil);
    if ShowModal = mrOk then daMain.Refresh(qryStep);
    Free;
  end;
end;

procedure TfoTask.DoCheckTemp;
begin
  if FTaskId = 0 then
  begin
    DoApplyChanges;
    FTempInsert := true;
  end;
end;

procedure TfoTask.btStepChangeClick(Sender: TObject);
begin
  with TfoStep.Create(Self) do
  begin
    SetParams(FTaskId, qryStep);
    if ShowModal = mrOk then daMain.Refresh(qryStep);
    Free;
  end;
end;

procedure TfoTask.btStepDeleteClick(Sender: TObject);
begin
  if Application.MessageBox('Confirma borrar?', PChar(Caption), MB_ICONQUESTION or MB_OKCANCEL) = ID_OK then
  begin
    daMain.StepDelete(qryStep.FieldByName('step_id').Value);
    daMain.Refresh(qryStep);
  end;
end;

procedure TfoTask.btScheduleAddClick(Sender: TObject);
begin
  DoCheckTemp;

  with TfoSchedule.Create(Self) do
  begin
    SetParams(FTaskId, nil);
    if ShowModal = mrOk then daMain.Refresh(qrySchedule);
    Free;
  end;
end;

procedure TfoTask.btScheduleChangeClick(Sender: TObject);
begin
  with TfoSchedule.Create(Self) do
  begin
    SetParams(FTaskId, qrySchedule);
    if ShowModal = mrOk then daMain.Refresh(qrySchedule);
    Free;
  end;
end;

procedure TfoTask.btScheduleDeleteClick(Sender: TObject);
begin
  if Application.MessageBox('Confirma borrar?', PChar(Caption), MB_ICONQUESTION or MB_OKCANCEL) = ID_OK then
  begin
    daMain.ScheduleDelete(qrySchedule.FieldByName('schedule_id').AsInteger);
    daMain.Refresh(qrySchedule);
  end;
end;

end.
