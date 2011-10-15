unit UmRecurrence;

{$include Defines.inc}

interface

uses
  Sil,

  UiRecurrence;

type
  TRecurrence = class (TSilObject, IRecurrence)
  private
    FStartDate: TDateTime;
    FEndDate: TDateTime;
    FDurationKind: TDurationKind;
    FEndCount: Integer;
    FOccursKind: TOccursKind;
    FStartTime: TDateTime;
    FEndTime: TDateTime;
    FEvery: Integer;
    FEveryUnit: Integer;
    FFilterKind: TFilterKind;
    FDayPosition: TWordArray;
    FDayOfWeek: TWordArray;
    FDayNumber: TWordArray;
    FMonths: TWordArray;
    FIncrement: TDateTime;
  protected
    function GetStartDate: TDateTime;
    function GetEndDate: TDateTime;
    function GetDurationKind: TDurationKind;
    function GetEndCount: Integer;
    function GetOccursKind: TOccursKind;
    function GetStartTime: TDateTime;
    function GetEndTime: TDateTime;
    function GetEvery: Integer;
    function GetEveryUnit: Integer;
    function GetFilterKind: TFilterKind;
    function GetDayPosition: TWordArray;
    function GetDayOfWeek: TWordArray;
    function GetDayNumber: TWordArray;
    function GetMonths: TWordArray;
    function GetIncrement: TDateTime;
    procedure SetStartDate(const Value: TDateTime);
    procedure SetEndDate(const Value: TDateTime);
    procedure SetDurationKind(Value: TDurationKind);
    procedure SetEndCount(Value: Integer);
    procedure SetOccursKind(Value: TOccursKind);
    procedure SetStartTime(const Value: TDateTime);
    procedure SetEndTime(const Value: TDateTime);
    procedure SetEvery(Value: Integer);
    procedure SetEveryUnit(Value: Integer);
    procedure SetFilterKind(Value: TFilterKind);
    procedure SetDayPosition(const Value: TWordArray);
    procedure SetDayOfWeek(const Value: TWordArray);
    procedure SetDayNumber(const Value: TWordArray);
    procedure SetMonths(const Value: TWordArray);
    procedure SetIncrement(const Value: TDateTime);
    procedure Configure(const StartDate, EndDate, StartTime, EndTime, Increment: TDateTime; RunCount, DayKind, DowMask, DayMask, MonthMask: Integer);
    function GetMonthFilter: TMonthFilter;
    function GetDayOfWeekFilter: TDaysOfWeek;
    function GetDayPositionFilter: TDayPositions;
    function GetDayNumberFilter: TDayNumberFilter;
  public
    constructor Create;
  end;

implementation

{ TRecurrence }

constructor TRecurrence.Create;
begin
  inherited;

  FStartDate := Date.Now;
  FEndTime := Time.EncodeParts(23, 59);
end;

procedure TRecurrence.Configure(const StartDate, EndDate, StartTime, EndTime, Increment: TDateTime; RunCount, DayKind, DowMask, DayMask, MonthMask: Integer);

  function DoGetArray(Value, Start, Count: Integer): TWordArray;
  var
    i, x: Integer;
  begin
    SetLength(Result, Count);
    x := 1 shl (Start - 1);

    for i := 0 to Count - 1 do
    begin
      Result[i] := Ord(Value and x > 0);
      x := x shl 1;
    end;
  end;

begin
  FStartDate := Date.Extract(StartDate);
  FEndDate := Date.Extract(EndDate);
  FEndCount := RunCount;

  if FEndDate > 0 then
    FDurationKind := dkEnd
  else
  if FEndCount > 0 then
    FDurationKind := dkCount
  else
    FDurationKind := dkUnspecified;

  FStartTime := Time.Extract(StartTime);
  FEndTime := Time.Extract(EndTime);

  FIncrement := Increment;

  if Increment > 0 then
  begin
    FOccursKind := okEvery;

    FDayOfWeek := DoGetArray(DowMask, 1, 7);
    FDayPosition := DoGetArray(DowMask, 9, 5);
    FDayNumber := DoGetArray(DayMask, 1, 31);
    FMonths := DoGetArray(MonthMask, 1, 12);

    FEveryUnit := Lo(DayKind);

    case FEveryUnit of
      0:  FEvery := Time.Decode(Increment).Minutes;
      1:  FEvery := Time.Decode(Increment).Hour;
      2:  FEvery := Trunc(Increment);
    end;
  end else
  begin
    FOccursKind := okOnce;
    FEvery := 0;
    FEveryUnit := 0;
  end;

  FFilterKind := TFilterKind(Hi(DayKind));
end;

function TRecurrence.GetDayNumber: TWordArray;
begin
  Result := FDayNumber;
end;

function TRecurrence.GetDayOfWeek: TWordArray;
begin
  Result := FDayOfWeek;
end;

function TRecurrence.GetDayPosition: TWordArray;
begin
  Result := FDayPosition;
end;

function TRecurrence.GetDurationKind: TDurationKind;
begin
  Result := FDurationKind;
end;

function TRecurrence.GetEndCount: Integer;
begin
  Result := FEndCount;
end;

function TRecurrence.GetEndDate: TDateTime;
begin
  Result := FEndDate;
end;

function TRecurrence.GetEndTime: TDateTime;
begin
  Result := FEndTime;
end;

function TRecurrence.GetEvery: Integer;
begin
  Result := FEvery;
end;

function TRecurrence.GetEveryUnit: Integer;
begin
  Result := FEveryUnit;
end;

function TRecurrence.GetFilterKind: TFilterKind;
begin
  Result := FFilterKind;
end;

function TRecurrence.GetIncrement: TDateTime;
begin
  Result := FIncrement;
end;

function TRecurrence.GetMonths: TWordArray;
begin
  Result := FMonths;
end;

function TRecurrence.GetOccursKind: TOccursKind;
begin
  Result := FOccursKind;
end;

function TRecurrence.GetStartDate: TDateTime;
begin
  Result := FStartDate;
end;

function TRecurrence.GetStartTime: TDateTime;
begin
  Result := FStartTime;
end;

procedure TRecurrence.SetDayNumber(const Value: TWordArray);
begin
  FDayNumber := Value;
end;

procedure TRecurrence.SetDayOfWeek(const Value: TWordArray);
begin
  FDayOfWeek := Value;
end;

procedure TRecurrence.SetDayPosition(const Value: TWordArray);
begin
  FDayPosition := Value;
end;

procedure TRecurrence.SetDurationKind(Value: TDurationKind);
begin
  FDurationKind := Value;
end;

procedure TRecurrence.SetEndCount(Value: Integer);
begin
  FEndCount := Value;
end;

procedure TRecurrence.SetEndDate(const Value: TDateTime);
begin
  FEndDate := Value;
end;

procedure TRecurrence.SetEndTime(const Value: TDateTime);
begin
  FEndTime := Value;
end;

procedure TRecurrence.SetEvery(Value: Integer);
begin
  FEvery := Value;
end;

procedure TRecurrence.SetEveryUnit(Value: Integer);
begin
  FEveryUnit := Value;
end;

procedure TRecurrence.SetFilterKind(Value: TFilterKind);
begin
  FFilterKind := Value;
end;

procedure TRecurrence.SetIncrement(const Value: TDateTime);
begin
  FIncrement := Value;
end;

procedure TRecurrence.SetMonths(const Value: TWordArray);
begin
  FMonths := Value;
end;

procedure TRecurrence.SetOccursKind(Value: TOccursKind);
begin
  FOccursKind := Value;
end;

procedure TRecurrence.SetStartDate(const Value: TDateTime);
begin
  FStartDate := Value;
end;

procedure TRecurrence.SetStartTime(const Value: TDateTime);
begin
  FStartTime := Value;
end;

function TRecurrence.GetMonthFilter: TMonthFilter;
var
  i: Integer;
begin
  Result := [];

  for i := 0 to Length(FMonths) - 1 do
    if FMonths[i] > 0 then Include(Result, TMonthFilterItem(i));
end;

function TRecurrence.GetDayNumberFilter: TDayNumberFilter;
var
  i: Integer;
begin
  Result := [];

  for i := 0 to Length(FDayNumber) - 1 do
    if FDayNumber[i] > 0 then Include(Result, TDayNumberFilterItem(i));
end;

function TRecurrence.GetDayOfWeekFilter: TDaysOfWeek;
var
  i: Integer;
begin
  Result := [];

  for i := 0 to Length(FDayOfWeek) - 1 do
    if FDayOfWeek[i] > 0 then Include(Result, TDayOfWeek(i + 1));
end;

function TRecurrence.GetDayPositionFilter: TDayPositions;
var
  i: Integer;
begin
  Result := [];

  for i := 0 to Length(FDayPosition) - 1 do
    if FDayPosition[i] > 0 then Include(Result, TDayPosition(i + 1));
end;

end.
