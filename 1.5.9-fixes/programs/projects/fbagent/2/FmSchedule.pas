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

unit FmSchedule;

interface

{$include Defines.inc}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Mask, CheckLst, StdCtrls, ComCtrls, ExtCtrls,
  DB, IBCustomDataSet, IBQuery, IBDatabase,

  Sil,
  
  UeAgent,
  DmAgent,
  UiRecurrence,
  FmRecurrence;

type
  TfoSchedule = class(TForm)
    gbExecKind: TGroupBox;
    rbExecKindDay: TRadioButton;
    dtExecDay: TDateTimePicker;
    rbExecKindRec: TRadioButton;
    btRecChange: TButton;
    Label1: TLabel;
    btAccept: TButton;
    btCancel: TButton;
    rbExecDel: TRadioGroup;
    Label3: TLabel;
    edName: TEdit;
    cbEnabled: TCheckBox;
    Label2: TLabel;
    meExecTime: TDateTimePicker;
    meRecurring: TMemo;
    procedure btRecChangeClick(Sender: TObject);
    procedure btAcceptClick(Sender: TObject);
    procedure DoChangeType(Sender: TObject);
  private
    FRecurrence: IRecurrence;
    FIsChange: Boolean;
    FTaskId: Integer;
    FScheduleId: Integer;
    FQuery: TIBQuery;
    procedure DoBuildRecurrenceHint;
  public
    procedure SetParams(TaskId: Integer; quQuery: TIBQuery);
  end;

var
  foSchedule: TfoSchedule;

implementation

uses SilBtStr, UmRecurrence;

{$R *.dfm}

procedure TfoSchedule.SetParams(TaskId: Integer; quQuery: TIBQuery);
begin
  Sil.Trace.Enter(Self, 'SetParams');

  FIsChange := Assigned(quQuery);
  FTaskId := TaskId;

  if FIsChange then
  begin
    FQuery := quQuery;

    FScheduleId := FQuery.FieldByName('schedule_id').Value;
    edName.Text := FQuery.FieldByName('name').Value;
    cbEnabled.Checked := FQuery.FieldByName('enabled').AsInteger = 1;

    daAgent.GetRecurrence(FQuery, FRecurrence);

    dtExecDay.DateTime := FRecurrence.StartDate;
    meExecTime.Time := FRecurrence.StartTime;

    rbExecKindDay.Checked := FRecurrence.OccursKind = okOnce;
    rbExecKindRec.Checked := FRecurrence.OccursKind = okEvery;

    rbExecDel.ItemIndex := FQuery.FieldByName('del_kind').AsInteger;
  end else
  begin
    FRecurrence := TRecurrence.Create;
    edName.Text := 'Unnamed';

    dtExecDay.Date := Date.Now;
    meExecTime.Time := Time.Now;
  end;

  DoBuildRecurrenceHint;

  Sil.Trace.Leave;
end;

procedure TfoSchedule.btRecChangeClick(Sender: TObject);
var
  Rec: TfoRecurrence;
begin
  Sil.Trace.Enter(Self, 'btRecChangeClick');

  rbExecKindRec.Checked := true;
  Rec := TfoRecurrence.Create(Self);

  try
    if Assigned(FRecurrence) then
    begin
      Rec.SetParams(FRecurrence);

      if Rec.ShowModal = mrOk then
        DoBuildRecurrenceHint;
    end;
  finally
    Rec.Free;
  end;

  Sil.Trace.Leave;
end;

procedure TfoSchedule.btAcceptClick(Sender: TObject);
var
  Info: RRecurrence;
begin
  Sil.Trace.Enter(Self, 'btAcceptClick');

  Info.TaskId := FTaskId;
  Info.ScheduleId := FScheduleId;
  Info.Name := edName.Text;
  Info.Enabled := cbEnabled.Checked;
  Info.DelKind := rbExecDel.ItemIndex;
  Info.RunAt := Date.Extract(dtExecDay.Date) + Time.Extract(meExecTime.Time);

  if rbExecKindDay.Checked then
    FRecurrence.OccursKind := okOnce;

  daAgent.RecurrenceCheck(Info, FRecurrence);

  Sil.Trace.Leave;
end;

procedure TfoSchedule.DoBuildRecurrenceHint;
var
  Sub, Text: String;
  i: Integer;
begin
  if rbExecKindRec.Checked and Assigned(FRecurrence) then
  begin
    Text := 'Occurs';
    Str.Add(Text, 'every %d %s', [FRecurrence.Every, CUnits[FRecurrence.EveryUnit]], ' ');

    if FRecurrence.EndTime = 0 then
      Str.Add(Text, 'at %s', [Time.ToStr(FRecurrence.StartTime, ShortTimeFormat)], ' ');

    case FRecurrence.FilterKind of
      fkWeek:
      begin
        Sub := '';

        for i := 0 to Length(FRecurrence.DayPosition) - 1 do
          if FRecurrence.DayPosition[i] > 0 then
            Str.Add(Sub, Str.ToLower(CPositions[i]), ', ');

        for i := 0 to Length(FRecurrence.DayOfWeek) - 1 do
          if FRecurrence.DayOfWeek[i] > 0 then
            Str.Add(Sub, LongDayNames[i + 1], ', ');

        if Str.NotEmpty(Sub) then
          Str.Add(Text, Sub, ' on ');
      end;

      fkMonth:
      begin
        Sub := '';

        for i := 0 to Length(FRecurrence.DayNumber) - 1 do
          if FRecurrence.DayNumber[i] > 0 then
            Str.Add(Sub, Int.ToStr(i + 1), ', ');

        if Str.NotEmpty(Sub) then
          Str.Add(Text, 'on day(s) ' + Sub, ' ');
      end;
    end;

    Sub := '';

    for i := 0 to Length(FRecurrence.Months) - 1 do
      if FRecurrence.Months[i] > 0 then
        Str.Add(Sub, LongMonthNames[i + 1], ', ');

    if Str.NotEmpty(Sub) then
      Str.Add(Text, 'on month(s) ' + Sub, ' ');

    if FRecurrence.EndTime > 0 then
      Str.Add(Text, 'between %s and %s', [Time.ToStr(FRecurrence.StartTime, ShortTimeFormat), Time.ToStr(FRecurrence.EndTime, ShortTimeFormat)], ', ');

  end else
    Text := '';

  meRecurring.Text := Text;
end;

procedure TfoSchedule.DoChangeType(Sender: TObject);
begin
  DoBuildRecurrenceHint;
end;

end.
