unit UiRecurrence;

{$include Defines.inc}

interface

uses
  Sil;

type
  TDurationKind = (dkUnspecified, dkEnd, dkCount);
  TOccursKind = (okOnce, okEvery);
  TFilterKind = (fkNone, fkWeek, fkMonth);

  TMonthFilterItem = 1..12;
  TMonthFilter = set of TMonthFilterItem;
  TDayNumberFilterItem = 1..31;
  TDayNumberFilter = set of TDayNumberFilterItem;

  IRecurrence = interface
    ['{B3C1774E-ADE8-4CB6-BA79-F0EDBCBD39D0}']
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
    property StartDate: TDateTime read GetStartDate write SetStartDate;
    property EndDate: TDateTime read GetEndDate write SetEndDate;
    property DurationKind: TDurationKind read GetDurationKind write SetDurationKind;
    property EndCount: Integer read GetEndCount write SetEndCount;
    property OccursKind: TOccursKind read GetOccursKind write SetOccursKind;
    property StartTime: TDateTime read GetStartTime write SetStartTime;
    property EndTime: TDateTime read GetEndTime write SetEndTime;
    property Every: Integer read GetEvery write SetEvery;
    property EveryUnit: Integer read GetEveryUnit write SetEveryUnit;
    property FilterKind: TFilterKind read GetFilterKind write SetFilterKind;
    property DayPosition: TWordArray read GetDayPosition write SetDayPosition;
    property DayOfWeek: TWordArray read GetDayOfWeek write SetDayOfWeek;
    property DayNumber: TWordArray read GetDayNumber write SetDayNumber;
    property Months: TWordArray read GetMonths write SetMonths;
    property Increment: TDateTime read GetIncrement write SetIncrement;
  end;

implementation

end.
 