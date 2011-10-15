unit FmSchedule;

interface

{$include Defines.inc}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Mask, CheckLst, StdCtrls, ComCtrls, ExtCtrls,
  DB, IBCustomDataSet, IBQuery, IBDatabase,

  Sil,
  DmAgent,
  FmRecurrence;

type
  TfoSchedule = class(TForm)
    gbExecKind: TGroupBox;
    rbExecKindDay: TRadioButton;
    meExecTime: TMaskEdit;
    gbExec: TGroupBox;
    Label3: TLabel;
    edName: TEdit;
    cbEnabled: TCheckBox;
    dtExecDay: TDateTimePicker;
    rbExecKindRec: TRadioButton;
    btRecChange: TButton;
    Label1: TLabel;
    btAccept: TButton;
    btCancel: TButton;
    rbExecDel: TRadioGroup;
    trRecurrence: TIBTransaction;
    qryRecurrence: TIBQuery;
    procedure btRecChangeClick(Sender: TObject);
    procedure btAcceptClick(Sender: TObject);
  private
    FRec: TfoRecurrence;
    FRecurrenceId: Integer;
    FIsChange: Boolean;
    FTaskId: Integer;
    FScheduleId: Integer;
    procedure DoReadRecurrence(From: TfoRecurrence; var Rec: TfoRecurrence);
  public
    procedure SetParams(TaskId: Integer; quQuery: TIBQuery);
  end;

var
  foSchedule: TfoSchedule;

implementation

uses SilBtStr;

{$R *.dfm}

procedure TfoSchedule.SetParams(TaskId: Integer; quQuery: TIBQuery);
begin
  FIsChange := Assigned(quQuery);
  FTaskId := TaskId;

  if FIsChange then
  begin
    FRecurrenceId := 0;
    FScheduleId := quQuery.FieldByName('schedule_id').Value;
    edName.Text := quQuery.FieldByName('name').Value;
    cbEnabled.Checked := quQuery.FieldByName('enabled').AsInteger = 1;

    qryRecurrence.ParamByName('schedule_id').AsInteger := quQuery.FieldByName('schedule_id').AsInteger;
    qryRecurrence.Open;

    if not qryRecurrence.Eof then
    begin
      FRecurrenceId := qryRecurrence.FieldByName('recurrence_id').AsInteger;

      if qryRecurrence.FieldByName('increment').IsNull or (qryRecurrence.FieldByName('increment').AsDateTime = 0) then
      begin
        rbExecKindDay.Checked := true;

        dtExecDay.DateTime := qryRecurrence.FieldByName('start_date').AsDateTime;
        meExecTime.Text := Time.ToStr(qryRecurrence.FieldByName('start_time').AsDateTime, 'hh:nn:ss');
      end else
      begin
        rbExecKindRec.Checked := true;

        dtExecDay.DateTime := qryRecurrence.FieldByName('start_date').AsDateTime;
        meExecTime.Text := Time.ToStr(qryRecurrence.FieldByName('start_time').AsDateTime, 'hh:nn:ss');

        DoReadRecurrence(nil, FRec);
      end;
    end;

    rbExecDel.ItemIndex := quQuery.FieldByName('del_kind').AsInteger;
  end else
  begin
    edName.Text := 'sin nombre';
    meExecTime.Text := Time.ToStr(Time.Now);
  end;
end;

procedure TfoSchedule.DoReadRecurrence(From: TfoRecurrence; var Rec: TfoRecurrence);
begin
  qryRecurrence.ParamByName('schedule_id').AsInteger := FScheduleId;
  qryRecurrence.Open;

  Rec := TfoRecurrence.Create(Self);

  if Assigned(From) then
    Rec.SetParamsFrom(From) else
    Rec.SetParams(FScheduleId, qryRecurrence);

  qryRecurrence.Close;
end;

procedure TfoSchedule.btRecChangeClick(Sender: TObject);
var
  Recurrence: TfoRecurrence;
begin
  rbExecKindRec.Checked := true;
  DoReadRecurrence(FRec, Recurrence);

  if Recurrence.ShowModal = mrOk then
  begin
    if Assigned(FRec) then FRec.Free;
    FRec := Recurrence;
  end else
    Recurrence.Free;
end;

procedure TfoSchedule.btAcceptClick(Sender: TObject);
var
  Info: RRecurrence;
begin
  Info.TaskId := FTaskId;
  Info.ScheduleId := FScheduleId;
  Info.RecurrenceId := FRecurrenceId;
  Info.Name := edName.Text;
  Info.Enabled := cbEnabled.Checked;
  Info.DelKind := rbExecDel.ItemIndex;

  if rbExecKindRec.Checked then
  begin
    if not Assigned(FRec) then
    begin
      Application.MessageBox('Falta configurar la recurrencia.', PChar(Caption), MB_ICONSTOP);
      ModalResult := mrNone;
      Exit;
    end;

    with FRec do
    begin
      Info.StartTime := Date.Extract(dtStartDate.Date) + Time.FromStr(meStartTime.Text);

      if rbDurationEnd.Checked then
        Info.EndTime := Date.Extract(dtEndDate.Date) + Time.FromStr(meEndTime.Text) else
        Info.EndTime := Time.FromStr(meEndTime.Text);

      if rbDurationCount.Checked then
        Info.RunCount := Str.ToInt(edEndCount.Text, 1) else
        Info.RunCount := 0;

      case cbRecurrenceKind.ItemIndex of
        0:  // diario
        begin
          Info.FilterKind := 0;
          Info.DayFilter := WeeklyMap;

          case cbIncUnit.ItemIndex of
            0:  Info.Increment := Time.EncodeParts(0, 0, Str.ToInt(edIncEach.Text, 1));
            1:  Info.Increment := Time.EncodeParts(0, Str.ToInt(edIncEach.Text, 1));
            2:  Info.Increment := Time.EncodeParts(Str.ToInt(edIncEach.Text, 1));
            3:  Info.Increment := Str.ToInt(edIncEach.Text, 1);
          end;
        end;

        1:  // mensual
        begin
          Info.Increment := 1;

          if rbMonByDay.Checked then
          begin // ej: 1 de agosto
            Info.FilterKind := 1;
            Info.DayFilter := 1 shl (Str.ToInt(edMonDay.Text, 1) - 1);
          end else
          begin // ej: primer domingo de agosto
            Info.FilterKind := 2;
            Info.DayFilter := 1 shl cbMonWeekDay.ItemIndex;
            Info.DowPosFilter := 1 shl cbMonDayPos.ItemIndex;
          end;

          Info.MonthFilter := MonthlyMap;
        end;
      end;
    end;
  end else
  begin
    Info.StartTime := Date.Extract(dtExecDay.Date) + Time.FromStr(meExecTime.Text);
    Info.EndTime := 0;
    Info.Increment := 0;
    Info.FilterKind := 0;
    Info.DayFilter := 0;
    Info.DowPosFilter := 0;
    Info.MonthFilter := 0;
    Info.RunCount := 0;
  end;

  daMain.RecurrenceCheck(Info);
end;

end.
