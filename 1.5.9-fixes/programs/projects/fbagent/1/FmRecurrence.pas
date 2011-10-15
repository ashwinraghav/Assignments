unit FmRecurrence;

interface

{$include Defines.inc}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, CheckLst, ComCtrls, Mask, Buttons,
  DB, IBCustomDataSet, IBQuery, IBStoredProc, IBDatabase,

  Sil;

type
  TfoRecurrence = class(TForm)
    nbDetails: TNotebook;
    GroupBox15: TGroupBox;
    Label13: TLabel;
    edIncEach: TEdit;
    cbIncUnit: TComboBox;
    gbDetails: TGroupBox;
    rbMonByDay: TRadioButton;
    rbMonByPos: TRadioButton;
    cbMonDayPos: TComboBox;
    cbMonWeekDay: TComboBox;
    edMonDay: TEdit;
    gbDuration: TGroupBox;
    Label12: TLabel;
    meStartTime: TMaskEdit;
    meEndTime: TMaskEdit;
    btAccept: TButton;
    btCancel: TButton;
    cbRecurrenceKind: TComboBox;
    Label1: TLabel;
    Label9: TLabel;
    paDays: TPanel;
    sbDaySun: TSpeedButton;
    sbDayMon: TSpeedButton;
    sbDayTue: TSpeedButton;
    sbDayWed: TSpeedButton;
    sbDayThu: TSpeedButton;
    sbDayFri: TSpeedButton;
    sbDaySat: TSpeedButton;
    btWekAll: TButton;
    GroupBox1: TGroupBox;
    rbDurationNoEnd: TRadioButton;
    rbDurationCount: TRadioButton;
    rbDurationEnd: TRadioButton;
    dtStartDate: TDateTimePicker;
    dtEndDate: TDateTimePicker;
    Label2: TLabel;
    edEndCount: TEdit;
    Label3: TLabel;
    Label4: TLabel;
    btMonAll: TButton;
    paMonths: TPanel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SpeedButton6: TSpeedButton;
    SpeedButton7: TSpeedButton;
    SpeedButton8: TSpeedButton;
    SpeedButton9: TSpeedButton;
    SpeedButton10: TSpeedButton;
    SpeedButton11: TSpeedButton;
    SpeedButton12: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure cbRecurrenceKindChange(Sender: TObject);
    procedure btWekAllClick(Sender: TObject);
    procedure btMonAllClick(Sender: TObject);
    procedure dtEndDateChange(Sender: TObject);
    procedure edEndCountChange(Sender: TObject);
  private
    FIsChange: Boolean;
    FScheduleId: Integer;
  public
    procedure SetParams(ScheduleId: Integer; quQuery: TIBQuery);
    procedure SetParamsFrom(Form: TfoRecurrence);
    function WeeklyMap: Integer;
    function MonthlyMap: Integer;
  end;

var
  foRecurrence: TfoRecurrence;

implementation

uses
  DmAgent;

{$R *.dfm}

procedure TfoRecurrence.FormCreate(Sender: TObject);
begin
  meStartTime.Text := '00:00:00';
  meEndTime.Text := '23:59:59';
  nbDetails.PageIndex := 0;
end;

procedure TfoRecurrence.cbRecurrenceKindChange(Sender: TObject);
begin
  nbDetails.PageIndex := cbRecurrenceKind.ItemIndex;
end;

procedure TfoRecurrence.SetParamsFrom(Form: TfoRecurrence);
var
  i: Integer;
begin
  FIsChange := true;

  cbRecurrenceKind.ItemIndex := Form.cbRecurrenceKind.ItemIndex;

  case cbRecurrenceKind.ItemIndex of
    0:  // Diario
    begin
      edIncEach.Text := Form.edIncEach.Text;
      cbIncUnit.ItemIndex := Form.cbIncUnit.ItemIndex;

      for i := 0 to 6 do
        TSpeedButton(paDays.Controls[i]).Down := TSpeedButton(Form.paDays.Controls[i]).Down;
    end;

    1:  // Mensual
    begin
      rbMonByDay.Checked := Form.rbMonByDay.Checked;
      edMonDay.Text := Form.edMonDay.Text;

      rbMonByPos.Checked := Form.rbMonByPos.Checked;
      cbMonDayPos.ItemIndex := Form.cbMonDayPos.ItemIndex;
      cbMonWeekDay.ItemIndex := Form.cbMonWeekDay.ItemIndex;

      for i := 0 to 11 do
        TSpeedButton(paMonths.Controls[i]).Down := TSpeedButton(Form.paMonths.Controls[i]).Down;
    end;
  end;

  dtStartDate.DateTime := Form.dtStartDate.DateTime;
  meStartTime.Text := Form.meStartTime.Text;
  meEndTime.Text := Form.meEndTime.Text;

  if Form.rbDurationEnd.Checked then
  begin
    rbDurationEnd.Checked := true;
    dtEndDate.DateTime := Form.dtEndDate.DateTime;
  end
  else
  if Form.rbDurationCount.Checked then
  begin
    rbDurationCount.Checked := true;
    edEndCount.Text := Form.edEndCount.Text;
  end;

  cbRecurrenceKindChange(nil);
end;

procedure TfoRecurrence.SetParams(ScheduleId: Integer; quQuery: TIBQuery);

  function DoGetIncrement(const Stamp: TDateTime): Integer;
  begin
    if Stamp < Time.EncodeParts(0, 0, 1) then
      Result := 0 else
    if Stamp < Time.EncodeParts(0, 1) then
      Result := 1 else
    if Stamp < Time.EncodeParts(1) then
      Result := 2 else
    if Stamp < 1 then
      Result := 3 else
      Result := 4;
  end;

  function DoGetBit(Value: Integer): Integer;
  begin
    for Result := 1 to 8 do
      if Value and (1 shl (Result - 1)) > 0 then
        Exit;

    Result := 0;
  end;

var
  i, Map: Integer;
  Increment: TDateTime;
  List: TWordArray;
begin
  FIsChange := ScheduleId <> 0;
  FScheduleId := ScheduleId;
  List := nil;

  if FIsChange then
  begin
    meStartTime.Text := Time.ToStr(Time.Extract(quQuery.FieldByName('start_time').AsDateTime), 'hh:nn:ss');
    meEndTime.Text := Time.ToStr(Time.Extract(quQuery.FieldByName('end_time').AsDateTime), 'hh:nn:ss');
    dtStartDate.Date := Date.Extract(quQuery.FieldByName('start_date').AsDateTime);

    if quQuery.FieldByName('end_date').AsDateTime <> 0 then
    begin
      rbDurationEnd.Checked := true;
      dtEndDate.Date := Date.Extract(quQuery.FieldByName('end_date').AsDateTime);
      edEndCount.Text := '';
    end else
    if quQuery.FieldByName('run_count').AsInteger <> 0 then
    begin
      rbDurationCount.Checked := true;
      edEndCount.Text := Int.ToStr(quQuery.FieldByName('run_count').AsInteger);
    end else
      rbDurationNoEnd.Checked := true;

    if quQuery.FieldByName('day_filter_kind').AsInteger = 0 then
    begin // diario
      cbRecurrenceKind.ItemIndex := 0;

      Increment := quQuery.FieldByName('increment').AsDatetime;

      i := DoGetIncrement(Increment);
      Increment := Increment + 1; 
      List := DateTime.ToArray(Increment);

      cbIncUnit.ItemIndex := i - 1;
      edIncEach.Text := Int.ToStr(List[6 - i]);
      Map := quQuery.FieldByName('day_filter').AsInteger;

      for i := 0 to 6 do
        TSpeedButton(paDays.Controls[i]).Down := (Map and (1 shl i) > 0);
    end else
    begin // mensual
      cbRecurrenceKind.ItemIndex := 1;

      case quQuery.FieldByName('day_filter_kind').AsInteger of
        1:  // ej: 1 de agosto
        begin
          rbMonByDay.Checked := true;
          Map := quQuery.FieldByName('day_filter').AsInteger;

          for i := 0 to 31 do
            if Map and (1 shl i) > 0 then
            begin
              edMonDay.Text := Int.ToStr(i + 1);
              Break;
            end;
        end;

        2:  // ej: primer domingo de agosto
        begin
          rbMonByPos.Checked := true;
          cbMonWeekDay.ItemIndex := DoGetBit(quQuery.FieldByName('day_filter').AsInteger) - 1;
          cbMonDayPos.ItemIndex := DoGetBit(quQuery.FieldByName('dow_pos_filter').AsInteger) - 1;
        end;
      end;

      Map := quQuery.FieldByName('month_filter').AsInteger;

      for i := 0 to 11 do
        TSpeedButton(paMonths.Controls[i]).Down := (Map and (1 shl i) > 0);
    end;
  end;

  cbRecurrenceKindChange(nil);
end;

procedure TfoRecurrence.btWekAllClick(Sender: TObject);
var
  i: Integer;
  Value: Boolean;
begin
  Value := not TSpeedButton(paDays.Controls[0]).Down;

  for i := 0 to 6 do
    TSpeedButton(paDays.Controls[i]).Down := Value;
end;

procedure TfoRecurrence.btMonAllClick(Sender: TObject);
var
  i: Integer;
  Value: Boolean;
begin
  Value := not TSpeedButton(paMonths.Controls[0]).Down;

  for i := 0 to 11 do
    TSpeedButton(paMonths.Controls[i]).Down := Value;
end;

function TfoRecurrence.WeeklyMap: Integer;
var
  i: Integer;
begin
  Result := 0;

  for i := 0 to 6 do
    if TSpeedButton(paDays.Controls[i]).Down then
      Result := Result or (1 shl i);
end;

function TfoRecurrence.MonthlyMap: Integer;
var
  i: Integer;
begin
  Result := 0;

  for i := 0 to 11 do
    if TSpeedButton(paMonths.Controls[i]).Down then
      Result := Result or (1 shl i);
end;

procedure TfoRecurrence.dtEndDateChange(Sender: TObject);
begin
  rbDurationEnd.Checked := true;
end;

procedure TfoRecurrence.edEndCountChange(Sender: TObject);
begin
  rbDurationCount.Checked := true;
end;

end.

