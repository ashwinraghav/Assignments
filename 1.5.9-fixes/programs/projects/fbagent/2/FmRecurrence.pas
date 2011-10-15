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

unit FmRecurrence;

interface

{$include Defines.inc}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, CheckLst, ComCtrls, Mask, Buttons,
  DB, IBCustomDataSet, IBQuery, IBStoredProc, IBDatabase,

  Sil,

  UeAgent,
  UiRecurrence;

type
  TfoRecurrence = class(TForm)
    btAccept: TButton;
    btCancel: TButton;
    gbDuration: TGroupBox;
    rbDurationNoEnd: TRadioButton;
    rbDurationCount: TRadioButton;
    rbDurationEnd: TRadioButton;
    dpStartDate: TDateTimePicker;
    dpEndDate: TDateTimePicker;
    edEndCount: TEdit;
    Label3: TLabel;
    gbOccurs: TGroupBox;
    edEvery: TEdit;
    coEveryUnit: TComboBox;
    laInitTime: TLabel;
    laEndTime: TLabel;
    gbFilters: TGroupBox;
    clDayPosition: TCheckListBox;
    clDayOfWeek: TCheckListBox;
    clMonths: TCheckListBox;
    clDayNumber: TCheckListBox;
    rbFilterWeek: TRadioButton;
    rbFilterMonth: TRadioButton;
    dpEveryEnd: TDateTimePicker;
    udEvery: TUpDown;
    udDurationCount: TUpDown;
    rbFilterNone: TRadioButton;
    dpEveryIni: TDateTimePicker;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure btAcceptClick(Sender: TObject);
    procedure rbDurationEndClick(Sender: TObject);
    procedure rbFilterNoneClick(Sender: TObject);
    procedure coEveryUnitClick(Sender: TObject);
  private
    FIsChange: Boolean;
    FRecurrence: IRecurrence;
    procedure DoUpdateDuration(Radio: TRadioButton);
    procedure DoUpdateFilters(Radio: TRadioButton);
    procedure DoUpdateChanges;
    procedure DoInitLists;
  public
    procedure SetParams(const Recurrence: IRecurrence);
  end;

var
  foRecurrence: TfoRecurrence;

implementation

uses
  DmAgent, SilBtInt, SilBtStr;

{$R *.dfm}

procedure TfoRecurrence.FormCreate(Sender: TObject);
begin
  Sil.Trace.Enter(Self, 'FormCreate');

  DoInitLists;

  dpEveryIni.Time := 0;
  dpEveryEnd.Time := Time.EncodeParts(23, 59);

  dpStartDate.Date := Date.Now;
  dpEndDate.Date := dpStartDate.Date;

  Sil.Trace.Leave;
end;

procedure TfoRecurrence.DoInitLists;
var
  i: Integer;
begin
  for i := 1 to 31 do
  begin
    if i < 8 then clDayOfWeek.Items.Add(LongDayNames[i]);
    if i < 13 then clMonths.Items.Add(LongMonthNames[i]);
    if i < Length(CPositions) + 1 then clDayPosition.Items.Add(CPositions[i - 1]);

    clDayNumber.Items.Add(Int.ToStr(i));
  end;
end;

procedure TfoRecurrence.SetParams(const Recurrence: IRecurrence);

  procedure DoSetArray(Check: TCheckListBox; const List: TWordArray);
  var
    i: Integer;
  begin
    for i := 0 to Check.Count - 1 do
      if i < Length(List) then
        Check.Checked[i] := List[i] > 0 else
        Check.Checked[i] := false;
  end;

begin
  Sil.Trace.Enter(Self, 'SetParams');

  FIsChange := true;
  FRecurrence := Recurrence;

  if Recurrence.StartDate > 0 then
    dpStartDate.Date := Recurrence.StartDate;

  case Recurrence.DurationKind of
    dkEnd:
    begin
      dpEndDate.Date := Recurrence.EndDate;
      DoUpdateDuration(rbDurationEnd);
    end;

    dkCount:
    begin
      udDurationCount.Position := Recurrence.EndCount;
      DoUpdateDuration(rbDurationCount);
    end;

    dkUnspecified:
      DoUpdateDuration(rbDurationNoEnd);
  end;

  udEvery.Position := Recurrence.Every;
  coEveryUnit.ItemIndex := Recurrence.EveryUnit;
  dpEveryIni.Time := Recurrence.StartTime;
  dpEveryEnd.Time := Recurrence.EndTime;

  case Recurrence.FilterKind of
    fkNone:   DoUpdateFilters(rbFilterNone);
    fkWeek:
    begin
      DoSetArray(clDayPosition, Recurrence.DayPosition);
      DoSetArray(clDayOfWeek, Recurrence.DayOfWeek);
      DoUpdateFilters(rbFilterWeek);
    end;

    fkMonth:
    begin
      DoSetArray(clDayNumber, Recurrence.DayNumber);
      DoUpdateFilters(rbFilterMonth);
    end;
  end;

  DoSetArray(clMonths, Recurrence.Months);
  coEveryUnitClick(nil);

  Sil.Trace.Leave;
end;

procedure TfoRecurrence.DoUpdateDuration(Radio: TRadioButton);
begin
  Radio.Checked := true;

  dpEndDate.Enabled := rbDurationEnd.Checked;
  edEndCount.Enabled := rbDurationCount.Checked;
  udDurationCount.Enabled := rbDurationCount.Checked;
end;

procedure TfoRecurrence.DoUpdateFilters(Radio: TRadioButton);
begin
  Radio.Checked := true;

  clDayPosition.Enabled := rbFilterWeek.Checked;
  clDayOfWeek.Enabled := rbFilterWeek.Checked;
  clDayNumber.Enabled := rbFilterMonth.Checked;
  clMonths.Enabled := rbFilterWeek.Checked or rbFilterMonth.Checked;
end;

procedure TfoRecurrence.btAcceptClick(Sender: TObject);
begin
  if dpEveryEnd.Visible and (dpEveryIni.Time > dpEveryEnd.Time) then
  begin
    dpEveryIni.SetFocus;
    Application.MessageBox('End time must be greater than Start time.', PChar(Caption), MB_ICONSTOP);
    ModalResult := mrNone;
  end;

  if rbDurationEnd.Checked and (dpEndDate.Date < dpStartDate.Date) then
  begin
    dpStartDate.SetFocus;
    Application.MessageBox('End date must be greater than Start date.', PChar(Caption), MB_ICONSTOP);
    ModalResult := mrNone;
  end;

  if ModalResult = mrOk then
    DoUpdateChanges;
end;

procedure TfoRecurrence.rbDurationEndClick(Sender: TObject);
begin
  DoUpdateDuration(TRadioButton(Sender));
end;

procedure TfoRecurrence.rbFilterNoneClick(Sender: TObject);
begin
  DoUpdateFilters(TRadioButton(Sender));
end;

procedure TfoRecurrence.DoUpdateChanges;

  function DoGetArray(List: TCheckListBox): TWordArray;
  var
    i: Integer;
  begin
    SetLength(Result, List.Count);

    for i := 0 to List.Count - 1 do
      Result[i] := Ord(List.Checked[i]);
  end;

begin
  FRecurrence.StartDate := dpStartDate.Date;

  if rbDurationEnd.Checked then
  begin
    FRecurrence.DurationKind := dkEnd;
    FRecurrence.EndCount := 0;
    FRecurrence.EndDate := dpEndDate.Date;
  end else
  if rbDurationCount.Checked then
  begin
    FRecurrence.DurationKind := dkCount;
    FRecurrence.EndCount := udDurationCount.Position;
    FRecurrence.EndDate := 0;
  end else
  begin
    FRecurrence.DurationKind := dkUnspecified;
    FRecurrence.EndCount := 0;
    FRecurrence.EndDate := 0;
  end;

  FRecurrence.OccursKind := okEvery;
  FRecurrence.Every := udEvery.Position;
  FRecurrence.EveryUnit := coEveryUnit.ItemIndex;
  FRecurrence.StartTime := dpEveryIni.Time;

  if dpEveryEnd.Visible then
    FRecurrence.EndTime := dpEveryEnd.Time else
    FRecurrence.EndTime := 0;

  if rbFilterWeek.Checked then
  begin
    FRecurrence.FilterKind := fkWeek;
    FRecurrence.DayPosition := DoGetArray(clDayPosition);
    FRecurrence.DayOfWeek := DoGetArray(clDayOfWeek);
    FRecurrence.DayNumber := nil;
    FRecurrence.Months := DoGetArray(clMonths);
  end else
  if rbFilterMonth.Checked then
  begin
    FRecurrence.FilterKind := fkMonth;
    FRecurrence.DayPosition := nil;
    FRecurrence.DayOfWeek := nil;
    FRecurrence.DayNumber := DoGetArray(clDayNumber);
    FRecurrence.Months := DoGetArray(clMonths);
  end else
  begin
    FRecurrence.FilterKind := fkNone;
    FRecurrence.DayPosition := nil;
    FRecurrence.DayOfWeek := nil;
    FRecurrence.DayNumber := nil;
    FRecurrence.Months := nil;
  end;
end;

procedure TfoRecurrence.coEveryUnitClick(Sender: TObject);
begin
  if coEveryUnit.ItemIndex = 2 then
  begin
    laInitTime.Caption := 'Occurs at';
    laEndTime.Visible := false;
    dpEveryEnd.Visible := false;
  end else
  begin
    laInitTime.Caption := 'Occurs between';
    laEndTime.Visible := true;
    dpEveryEnd.Visible := true;
  end;
end;

end.

