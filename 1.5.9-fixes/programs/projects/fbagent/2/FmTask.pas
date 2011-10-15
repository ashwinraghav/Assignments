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

unit FmTask;

interface

{$include Defines.inc}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, DBGrids, StdCtrls, ComCtrls, DB, IBCustomDataSet, IBQuery,
  IBDatabase, IBStoredProc,

  Sil,
  SilXml,
  
  DmTask, Menus;

const
  AStatus: array [0..2] of String = ('failed', 'succeeded', 'all');

type
  TfoTask = class(TForm)
    pcTasks: TPageControl;
    tsTask: TTabSheet;
    tsSteps: TTabSheet;
    Label1: TLabel;
    edTaskName: TEdit;
    cbTaskEnabled: TCheckBox;
    meTaskDescription: TMemo;
    Label2: TLabel;
    Label3: TLabel;
    cbTaskCategory: TComboBox;
    btCancel: TButton;
    tsSchedules: TTabSheet;
    gtSteps: TDBGrid;
    grSchedules: TDBGrid;
    btStepAdd: TButton;
    btStepChange: TButton;
    btStepDelete: TButton;
    btScheduleAdd: TButton;
    btScheduleChange: TButton;
    btScheduleDelete: TButton;
    btDeleteCategory: TButton;
    dsStep: TDataSource;
    dsSchedule: TDataSource;
    btAccept: TButton;
    Label4: TLabel;
    btStepUp: TButton;
    btStepDown: TButton;
    pcHistory: TTabSheet;
    grHistory: TDBGrid;
    btHisDelete: TButton;
    cbDetail: TCheckBox;
    dsHistory: TDataSource;
    tsNotifications: TTabSheet;
    Label5: TLabel;
    laJobCreated: TLabel;
    Label6: TLabel;
    laJobModified: TLabel;
    gbNotEMail: TGroupBox;
    chNotEMail: TCheckBox;
    Label7: TLabel;
    Label11: TLabel;
    edNotEMail: TEdit;
    cbNotEMail: TComboBox;
    gbNotNetSend: TGroupBox;
    chNotNetSend: TCheckBox;
    edNotNetSend: TEdit;
    cbNotNetSend: TComboBox;
    Label8: TLabel;
    Label9: TLabel;
    poTask: TPopupMenu;
    miTaskNew: TMenuItem;
    miTaskEdit: TMenuItem;
    miTaskDelete: TMenuItem;
    N1: TMenuItem;
    miTaskMoveUp: TMenuItem;
    miTaskMoveDown: TMenuItem;
    procedure btCancelClick(Sender: TObject);
    procedure btAcceptClick(Sender: TObject);
    procedure btStepAddClick(Sender: TObject);
    procedure btStepChangeClick(Sender: TObject);
    procedure btStepDeleteClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btScheduleAddClick(Sender: TObject);
    procedure btScheduleChangeClick(Sender: TObject);
    procedure btScheduleDeleteClick(Sender: TObject);
    procedure btStepUpClick(Sender: TObject);
    procedure btStepDownClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure cbDetailClick(Sender: TObject);
    procedure btHisDeleteClick(Sender: TObject);
    procedure btDeleteCategoryClick(Sender: TObject);
    procedure DoChange(Sender: TObject);
    procedure poTaskPopup(Sender: TObject);
  private
    FTaskId: Integer;
    FTempInsert: Boolean;
    FLoaded: Boolean;
    FChanged: Boolean;
    function DoCheckCategory: Integer;
    procedure DoApplyChanges;
    procedure DoCheckTemp;
    procedure DoSetupHistory;
    procedure DoReadNotification(const Buffer: String);
    function DoBuildNotification: Variant;
  public
    procedure SetParams(quTask: TIBQuery);
  end;

var
  foTask: TfoTask;

implementation

uses
  UeAgent,
  DmAgent,
  UtAgent,
  FmSchedule,
  FmStep, UtXml;

{$R *.dfm}

{ TfoTask }

procedure TfoTask.FormCreate(Sender: TObject);
begin
  Sil.Trace.Enter(Self, 'FormCreate');

  daTask := TdaTask.Create(Self);
  laJobCreated.Caption := '';
  laJobModified.Caption := '';

  pcTasks.ActivePageIndex := 0;
  FTempInsert := false;
  FChanged := false;

  with daTask do
    try
      taCategory.StartTransaction;
      quCategory.Open;

      with quCategory do
        while not Eof do
        begin
          cbTaskCategory.Items.AddObject(FieldByName('name').AsString, Pointer(FieldByName('category_id').AsInteger));
          Next;
        end;
    finally
      taCategory.Commit;
    end;

  Global.ReadForm('task', Self);
  FLoaded := true;

  Sil.Trace.Leave;
end;

procedure TfoTask.FormDestroy(Sender: TObject);
begin
  Global.WriteForm('task', Self);
  Global.WriteGrid('steps', Self.gtSteps);
  Global.WriteGrid('schedules', Self.grSchedules);
  Global.WriteGrid('history', Self.grHistory);
end;

procedure TfoTask.DoChange(Sender: TObject);
begin
  if FLoaded then FChanged := true;
end;

procedure TfoTask.SetParams(quTask: TIBQuery);
begin
  Sil.Trace.Enter(Self, 'SetParams');

  FLoaded := false;
  FTaskId := quTask.FieldByName('task_id').AsInteger;
  edTaskName.Text := quTask.FieldByName('name').AsString;
  cbTaskEnabled.Checked := quTask.FieldByName('enabled').AsInteger = 1;
  meTaskDescription.Text := quTask.FieldByName('description').AsString;
  laJobCreated.Caption := DateTime.ToStr(quTask.FieldByName('date_created').AsDateTime);
  laJobModified.Caption := Str.IIf(quTask.FieldByName('date_modified').AsDateTime > 0, DateTime.ToStr(quTask.FieldByName('date_modified').AsDateTime), '(none)');

  Caption := Str.Format('%s - %s', [Caption, edTaskName.Text]);

  with daTask do
  begin
    quStep.ParamByName('task_id').Value := FTaskId;
    quStep.Open;
    quStep.FetchAll;

    quHistory.ParamByName('task_id').Value := FTaskId;
    quHistory.ParamByName('show_step').Value := Ord(cbDetail.Checked);
    quHistory.Open;

    quSchedule.ParamByName('task_id').Value := FTaskId;
    quSchedule.Open;
  end;

  DoSetupHistory;
  DoReadNotification(quTask.FieldByName('notification').AsString);

  cbTaskCategory.ItemIndex := cbTaskCategory.Items.IndexOfObject(Pointer(quTask.FieldByName('category_id').AsInteger));

  Global.ReadGrid('steps', Self.gtSteps);
  Global.ReadGrid('schedules', Self.grSchedules);
  Global.ReadGrid('history', Self.grHistory);
  FLoaded := true;

  Sil.Trace.Leave;
end;

procedure TfoTask.btCancelClick(Sender: TObject);
begin
  Sil.Trace.Enter(Self, 'btCancelClick');

  if not FChanged or (Application.MessageBox('Cancel changes?', PChar(Caption), MB_ICONQUESTION or MB_OKCANCEL) = IDOK) then
  begin
    if FTempInsert then
      daAgent.TaskDelete(FTaskId);

    Close;
  end else
    ModalResult := mrNone;

  Sil.Trace.Leave;
end;

function TfoTask.DoCheckCategory: Integer;
begin
  Sil.Trace.Enter(Self, 'DoCheckCategory');

  with daTask do
    try
      taCategory.StartTransaction;

      doCategoryCheck.ParamByName('name').AsString := Str.Trim(cbTaskCategory.Text);
      doCategoryCheck.ExecProc;

      Result := doCategoryCheck.ParamByName('category_id').AsInteger;
    finally
      taCategory.Commit;
    end;

  Sil.Trace.Leave;
end;

procedure TfoTask.btAcceptClick(Sender: TObject);
begin
  DoApplyChanges;
end;

procedure TfoTask.DoApplyChanges;
var
  Category, Notification: Variant;
begin
  Sil.Trace.Enter(Self, 'DoApplyChanges');

  if cbTaskCategory.ItemIndex = -1 then
  begin
    if Str.NotEmpty(cbTaskCategory.Text, true) then
      Category := DoCheckCategory else
      Category := Vart.Null;
  end else
    Category := Integer(cbTaskCategory.Items.Objects[cbTaskCategory.ItemIndex]);

  Notification := DoBuildNotification;
  daAgent.TaskCheck(FTaskId, edTaskName.Text, Category, Notification, cbTaskEnabled.Checked, meTaskDescription.Text);

  Sil.Trace.Leave;
end;

procedure TfoTask.btStepAddClick(Sender: TObject);
var
  Params: IParameterList;
begin
  Sil.Trace.Enter(Self, 'btStepAddClick');

  DoCheckTemp;

  with TfoStep.Create(Self) do
  begin
    Params := Sil.List.Parameters;
    Params['step_count'] := daTask.quStep.RecordCount;
    Params['step_order'] := daTask.quStep.RecordCount;

    SetParams(FTaskId, nil, Params);

    if ShowModal = mrOk then
    begin
      daTask.quStep.ParamByName('task_id').Value := FTaskId;
      daAgent.Refresh(daTask.quStep);
    end;

    Free;
  end;

  Sil.Trace.Leave;
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
var
  Params: IParameterList;
begin
  Sil.Trace.Enter(Self, 'btStepChangeClick');

  if not daTask.quStep.IsEmpty then
    with TfoStep.Create(Self) do
    begin
      Params := Sil.List.Parameters;
      Params['step_count'] := daTask.quStep.RecordCount;
      Params['step_order'] := daTask.quStep.FieldByName('item_order').AsInteger;

      SetParams(FTaskId, daTask.quStep, Params);

      if ShowModal = mrOk then
      begin
        daTask.quStep.ParamByName('task_id').Value := FTaskId;
        daAgent.Refresh(daTask.quStep);
      end;

      Free;
    end;

  Sil.Trace.Leave;
end;

procedure TfoTask.btStepDeleteClick(Sender: TObject);
begin
  Sil.Trace.Enter(Self, 'btStepDeleteClick');

  if Application.MessageBox('Confirm deletion?', PChar(Caption), MB_ICONQUESTION or MB_OKCANCEL) = ID_OK then
  begin
    daAgent.StepDelete(daTask.quStep.FieldByName('step_id').Value);

    daTask.quStep.ParamByName('task_id').Value := FTaskId;
    daAgent.Refresh(daTask.quStep);
  end;

  Sil.Trace.Leave;
end;

procedure TfoTask.btScheduleAddClick(Sender: TObject);
begin
  Sil.Trace.Enter(Self, 'btScheduleAddClick');

  DoCheckTemp;

  with TfoSchedule.Create(Self) do
  begin
    SetParams(FTaskId, nil);

    if ShowModal = mrOk then
    begin
      daTask.quSchedule.ParamByName('task_id').Value := FTaskId;
      daAgent.Refresh(daTask.quSchedule);
    end;

    Free;
  end;

  Sil.Trace.Leave;
end;

procedure TfoTask.btScheduleChangeClick(Sender: TObject);
begin
  Sil.Trace.Enter(Self, 'btScheduleChangeClick');

  if not daTask.quSchedule.IsEmpty then
    with TfoSchedule.Create(Self) do
    begin
      SetParams(FTaskId, daTask.quSchedule);

      if ShowModal = mrOk then
      begin
        daTask.quSchedule.ParamByName('task_id').Value := FTaskId;
        daAgent.Refresh(daTask.quSchedule);
      end;

      Free;
    end;

  Sil.Trace.Leave;
end;

procedure TfoTask.btScheduleDeleteClick(Sender: TObject);
begin
  Sil.Trace.Enter(Self, 'btScheduleDeleteClick');

  if Application.MessageBox('Confirm deletion?', PChar(Caption), MB_ICONQUESTION or MB_OKCANCEL) = ID_OK then
  begin
    daAgent.ScheduleDelete(daTask.quSchedule.FieldByName('schedule_id').AsInteger);

    daTask.quSchedule.ParamByName('task_id').Value := FTaskId;
    daAgent.Refresh(daTask.quSchedule);
  end;

  Sil.Trace.Leave;
end;

procedure TfoTask.btStepUpClick(Sender: TObject);
var
  Order: Integer;
begin
  Sil.Trace.Enter(Self, 'btStepUpClick');

  Order := daTask.quStep.FieldByName('item_order').AsInteger;

  if Order > 0 then
  begin
    daAgent.StepChangeOrder(daTask.quStep.FieldByName('step_id').AsInteger, Order - 1);
    daAgent.Refresh(daTask.quStep);
    gtSteps.Perform(WM_KEYDOWN, VK_UP, 0);
    DoChange(Sender);
  end;

  Sil.Trace.Leave;
end;

procedure TfoTask.btStepDownClick(Sender: TObject);
var
  Order: Integer;
begin
  Sil.Trace.Enter(Self, 'btStepDownClick');

  Order := daTask.quStep.FieldByName('item_order').AsInteger;

  if Order < daTask.quStep.RecordCount - 1 then
  begin
    daAgent.StepChangeOrder(daTask.quStep.FieldByName('step_id').AsInteger, Order + 1);
    daAgent.Refresh(daTask.quStep);
    gtSteps.Perform(WM_KEYDOWN, VK_DOWN, 0);
    DoChange(Sender);
  end;

  Sil.Trace.Leave;
end;

procedure TfoTask.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_F5 then
    with daTask do
    begin
      daAgent.Refresh(quStep);
      daAgent.Refresh(quSchedule);
      daAgent.Refresh(quCategory);
      daAgent.Refresh(quHistory);
    end;
end;

function TfoTask.DoBuildNotification: Variant;
var
  Tree: IXmlTree;

  procedure DoWrite(const Tag: IXmlTag; const Recipients: String; Event: Integer);
  var
    Delim: String;
  begin
    if Str.Pos(';', Recipients) > 0 then
      Delim := ';'
    else
      Delim := ';';

    Tag.TagKind := tkBlock;
    Tag.Childs.WriteString('event', AStatus[Event]);
    Tag.Childs.WriteStrings('recipients', Sil.List.StringList(Recipients, Delim));
  end;

begin
  Result := Vart.Null;

  if chNotEMail.Checked or chNotNetSend.Checked then
  begin
    Tree := SilXml.Tool.Tree;

    with Tree.Root.AsTag do
    begin
      TagKind := tkBlock;
      Name := 'data';

      if chNotEMail.Checked and (edNotEMail.GetTextLen > 0) then
        DoWrite(Childs.AddTag('smtp'), edNotEMail.Text, cbNotEMail.ItemIndex);

      if chNotNetSend.Checked and (edNotNetSend.GetTextLen > 0) then
        DoWrite(Childs.AddTag('netsend'), edNotNetSend.Text, cbNotNetSend.ItemIndex);
    end;

    Result := SilXml.Tool.ToStr(Tree);
  end;
end;

procedure TfoTask.DoReadNotification(const Buffer: String);

  function DoGetEvent(const Value: String): Integer;
  var
    i: Integer;
  begin
    for i := 0 to Length(AStatus) - 1 do
      if Sil.Text.IsEqual(Value, AStatus[i]) then
      begin
        Result := i;
        Exit;
      end;

    Result := 0;
  end;

var
  Params: IParameterList;
begin
  chNotEMail.Checked := AgentXml.ReadNotification('smtp', Buffer, Params);

  if chNotEMail.Checked then
  begin
    edNotEMail.Text := Vart.ToStr(Params['recipients']);
    cbNotEMail.ItemIndex := DoGetEvent(Vart.ToStr(Params['event']));
  end;

  chNotNetSend.Checked := AgentXml.ReadNotification('netsend', Buffer, Params);

  if chNotNetSend.Checked then
  begin
    edNotNetSend.Text := Vart.ToStr(Params['recipients']);
    cbNotNetSend.ItemIndex := DoGetEvent(Vart.ToStr(Params['event']));
  end;
end;

procedure TfoTask.DoSetupHistory;
begin
  with grHistory.Columns do
    if Count >= 1 then
      begin
        Items[0].Visible := cbDetail.Checked;
        Items[1].Visible := cbDetail.Checked;
      end;
end;

procedure TfoTask.cbDetailClick(Sender: TObject);
begin
  DoSetupHistory;

  daTask.quHistory.ParamByName('show_step').Value := Ord(cbDetail.Checked);
  daAgent.Refresh(daTask.quHistory);
end;

procedure TfoTask.btHisDeleteClick(Sender: TObject);
begin
  Sil.Trace.Enter(Self, 'btHisDeleteClick');

  if Application.MessageBox('Delete all history?', PChar(Caption), MB_ICONQUESTION or MB_OKCANCEL) = ID_OK then
    with daTask do
      try
        taCategory.StartTransaction;

        doHistoryDel.ParamByName('task_id').AsInteger := FTaskId;
        doHistoryDel.ParamByName('max_size').AsInteger := 0;
        doHistoryDel.ExecProc;
      finally
        taCategory.Commit;
        daAgent.Refresh(quHistory);
      end;

  Sil.Trace.Leave;
end;

procedure TfoTask.btDeleteCategoryClick(Sender: TObject);
var
  Category: Variant;
begin
  Sil.Trace.Enter(Self, 'btDeleteCategoryClick');

  if (cbTaskCategory.ItemIndex >= 0) and (Application.MessageBox('Delete Category?', PChar(Caption), MB_ICONQUESTION or MB_OKCANCEL) = ID_OK) then
    with daTask do
      try
        taCategory.StartTransaction;

        Category := Integer(cbTaskCategory.Items.Objects[cbTaskCategory.ItemIndex]);

        doCategoryDel.ParamByName('category_id').AsInteger := Category;
        doCategoryDel.ExecProc;

        cbTaskCategory.Items.Delete(cbTaskCategory.ItemIndex);
        cbTaskCategory.ItemIndex := -1;
      finally
        taCategory.Commit;
      end;

  Sil.Trace.Leave;
end;

procedure TfoTask.poTaskPopup(Sender: TObject);
begin
  case pcTasks.ActivePageIndex of
    1:
    begin
      miTaskNew.Enabled := true;
      miTaskEdit.Enabled := true;
      miTaskNew.OnClick := btStepAdd.OnClick;
      miTaskEdit.OnClick := btStepChange.OnClick;
      miTaskDelete.OnClick := btStepDelete.OnClick;
      miTaskMoveUp.Enabled := true;
      miTaskMoveDown.Enabled := true;
    end;

    2:
    begin
      miTaskNew.Enabled := true;
      miTaskEdit.Enabled := true;
      miTaskNew.OnClick := btScheduleAdd.OnClick;
      miTaskEdit.OnClick := btScheduleChange.OnClick;
      miTaskDelete.OnClick := btScheduleDelete.OnClick;
      miTaskMoveUp.Enabled := false;
      miTaskMoveDown.Enabled := false;
    end;

    4:
    begin
      miTaskNew.Enabled := false;
      miTaskEdit.Enabled := false;
      miTaskDelete.OnClick := btHisDelete.OnClick;
      miTaskMoveUp.Enabled := false;
      miTaskMoveDown.Enabled := false;
    end;
  end;
end;

end.
