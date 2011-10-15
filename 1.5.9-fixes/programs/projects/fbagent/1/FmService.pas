unit FmService;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DB, StdCtrls,

  IBDatabase, IBCustomDataSet, IBStoredProc, IBQuery,

  Sil, 
  SilSiFirebird,
  SilStFirebird;

type
  TMonthFilter = set of 1..12;
  TDayNumberFilter = set of 1..31;
  TDayKind = (dkDayOfWeek, dkDayOfMonth);

  RDayFilter = record
  case DayKind: TDayKind of
    dkDayOfWeek:
      (
        Days: TDaysOfWeek;
        Positions: TDayPositions
      );
    dkDayOfMonth:
      (
        Numbers: TDayNumberFilter
      );
  end;

  RSchedule = record
    LastRun: TDateTime;
    NextRun: TDateTime;
    Increment: TDateTime;
    DayFilter: RDayFilter;
    MonthFilter: TMonthFilter;
    StartTime: TDateTime;
    EndTime: TDateTime;
    StartDate: TDateTime;
    EndDate: TDateTime;
  end;

  TForm1 = class(TForm)
    Memo1: TMemo;
    Button2: TButton;
    Edit1: TEdit;
    Button7: TButton;
    CheckBox1: TCheckBox;
    Edit2: TEdit;
    Button4: TButton;
    coAgent: TIBDatabase;
    doRunStatUpd: TIBStoredProc;
    trRunStat: TIBTransaction;
    qryRunStat: TIBQuery;
    procedure FormCreate(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FService: IFbApplication;
    FSession: IFbSession;
    FStmt: IFbStatement;
  end;

var
  Form1: TForm1;

implementation

uses SilBtTime, SilBtDate, SilBtDateTime, SilBtBin, DateUtils, SilBtInt;

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  FService := FireBird.Application;
  FSession := FService.Connect('hcc2616:fbagent', Params);

  coAgent.Open;

  edit1.text := datetime.tostr(datetime.now, 'yyyy/mm/dd hh:nn:ss');
  edit2.text := datetime.tostr(datetime.now, 'yyyy/mm/dd hh:nn:ss');
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FSession := nil;
  FService := nil;
end;

procedure CalcNext(var Data: RSchedule; FirstRun: Boolean);
var
  StartDate: TDateTime;
  DayOfWeek: TDayOfWeek;
  DayPos: TDayPosition;
  Found: Boolean;

  procedure DoInc(Pos: Integer);
  begin
    DateTime.Inc(Data.NextRun, Pos, 1);
    Data.NextRun := Date.Extract(Data.NextRun) + Data.StartTime;
  end;

begin
  if Data.LastRun = 0 then Data.LastRun := DateTime.Now;

  StartDate := Data.StartDate + Data.StartTime;
  Data.NextRun := Data.LastRun + Data.Increment;

  if FirstRun or (Data.NextRun < StartDate) then
    Data.NextRun := StartDate;

  while true do
  begin
    if (Data.EndDate > 0) and (Data.NextRun > Data.EndDate) then
    begin
      Data.NextRun := 0;
      Break;
    end;

    if (Data.MonthFilter <> []) and not (Date.Decode(Data.NextRun).Month in Data.MonthFilter) then
    begin
      DoInc(1);
      Continue;
    end;

    case Data.DayFilter.DayKind of
      dkDayOfWeek:
        if Data.DayFilter.Days <> [] then
        begin
          DayOfWeek := Date.DayOfWeek(Data.NextRun);

          if not (DayOfWeek in Data.DayFilter.Days) then
          begin
            DoInc(2);
            Continue;
          end;

          if Data.DayFilter.Positions <> [] then
          begin
            Found := false;

            for DayPos := Low(TDayPosition) + 1 to High(TDayPosition) do
              if DayPos in Data.DayFilter.Positions then
              begin
                Found := Date.Compare(Data.NextRun, DateTime.Find(Data.NextRun, DayPos, DayOfWeek)) = 0;
                if Found then Break;
              end;

            if not Found then
            begin
              DoInc(2);
              Continue;
            end;
          end;
        end;

      dkDayOfMonth:
        if (Data.DayFilter.Numbers <> []) and not (Date.Decode(Data.NextRun).Day in Data.DayFilter.Numbers) then
        begin
          DoInc(2);
          Continue;
        end;
    end;

    if (Data.EndTime <> 0) and (Time.Extract(Data.NextRun) > Data.EndTime) then
    begin
      DoInc(2);
      Continue;
    end;

    Break;
  end;
end;

procedure TForm1.Button7Click(Sender: TObject);
var
  data: RSchedule;
  t: TDateTime;
begin
  data.LastRun := DateTime.FromStr(edit1.text, 'yyyy/mm/dd hh:nn:ss');
  data.Increment := Time.EncodeParts(4);

  {data.DayFilter.DayKind := dkDayOfWeek;
  data.DayFilter.Days := [dwMonday, dwWednesday, dwFriday];
  data.DayFilter.Positions := [1, 5];}

  data.DayFilter.DayKind := dkDayOfMonth;
  data.DayFilter.Numbers := [5, 15];

  data.MonthFilter := [3, 12];
  data.StartTime := Time.FromStr('16:00');
  data.EndTime := Time.FromStr('23:00');
  data.StartDate := Date.Extract(DateTime.FromStr(edit2.text, 'yyyy/mm/dd hh:nn:ss'));
  data.EndDate := DateTime.FromStr('2004/01/01 00:00:00', 'yyyy/mm/dd hh:nn:ss');

  t := DateTime.Now;
  CalcNext(data, CheckBox1.checked);
  t := DateTime.Now - t;

  if data.NextRun > 0 then
    memo1.lines.add(datetime.tostr(data.NextRun, 'ddd dd mmm yyyy hh:nn:ss') + datetime.tostr(t, ' (zzz)')) else
    memo1.lines.add('fin de tarea' + datetime.tostr(t, ' (zzz)'));

  edit1.text := datetime.tostr(data.NextRun, 'yyyy/mm/dd hh:nn:ss');
end;

procedure TForm1.Button4Click(Sender: TObject);
var
  Data: RSchedule;
  RunCount, Filter, DowFilter, PosFilter: Integer;
  OnlyCalc: Boolean;
begin
  trRunStat.StartTransaction;
  qryRunStat.Open;

  while not qryRunStat.Eof do
  begin
    memo1.lines.add(qryRunStat.FieldByName('last_run').AsString + ' ' + qryRunStat.FieldByName('next_run').AsString);

    RunCount := qryRunStat.FieldByName('repeat').AsInteger;
    Data.LastRun := qryRunStat.FieldByName('last_run').AsDateTime;
    Data.NextRun := qryRunStat.FieldByName('next_run').AsDateTime;
    Data.Increment := qryRunStat.FieldByName('increment').AsDateTime;

    OnlyCalc := Data.LastRun = 0;

    case qryRunStat.FieldByName('day_filter_kind').AsInteger of
      0:
      begin
        Data.DayFilter.DayKind := dkDayOfWeek;
        Data.DayFilter.Positions := [];

        DowFilter := qryRunStat.FieldByName('day_filter').AsInteger shl 1;
        Move(DowFilter, Data.DayFilter.Days, SizeOf(Data.DayFilter.Days));
      end;

      1:
      begin
        Data.DayFilter.DayKind := dkDayOfMonth;
        Filter := qryRunStat.FieldByName('day_filter').AsInteger shl 1;
        Move(Filter, Data.DayFilter.Numbers, SizeOf(Data.DayFilter.Numbers));
      end;

      2:
      begin
        Data.DayFilter.DayKind := dkDayOfWeek;
        DowFilter := qryRunStat.FieldByName('day_filter').AsInteger shl 1;
        PosFilter := qryRunStat.FieldByName('dow_pos_filter').AsInteger shl 1;

        Move(DowFilter, Data.DayFilter.Days, SizeOf(Data.DayFilter.Days));
        Move(PosFilter, Data.DayFilter.Positions, SizeOf(Data.DayFilter.Positions));
      end;
    end;

    Filter := qryRunStat.FieldByName('month_filter').AsInteger shl 1;
    Move(Filter, Data.MonthFilter, SizeOf(Data.MonthFilter));

    Data.StartTime := qryRunStat.FieldByName('start_time').AsDateTime;
    Data.EndTime := qryRunStat.FieldByName('end_time').AsDateTime;
    Data.StartDate := qryRunStat.FieldByName('start_date').AsDateTime;
    Data.EndDate := qryRunStat.FieldByName('end_date').AsDateTime;

    CalcNext(Data, OnlyCalc);

    doRunStatUpd.ParamByName('run_id').AsInteger := qryRunStat.FieldByName('run_id').AsInteger;
    doRunStatUpd.ParamByName('last_run').AsDateTime := DateTime.Now;
    doRunStatUpd.ParamByName('next_run').AsDateTime := Data.NextRun;
    doRunStatUpd.ParamByName('repeat').AsInteger := Int.IIf(RunCount > 0, RunCount - 1, -1);
    doRunStatUpd.ExecProc;

    qryRunStat.Next;
  end;

  qryRunStat.Close;
  trRunStat.Commit;
end;

end.
