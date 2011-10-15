{********************************************************************************
 *                  Standard Interface Library (SIL)                            *
 *                                                                              *
 *       General purpose library whose design is based in STRONG                *
 *   use of interfaces.                                                         *
 *                                                                              *
 *                                                                              *
 *     Copyright (C) 2000 Mariano Podestá    antiriad@gmail.com                 *
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

unit SilLmScheduler;

{$I Defines.inc}

interface

uses
  SilBeTypes,
  SilLkInterfaced,
  SilLiScheduler,
  SilLiKey,
  SilOsTypes;

type
  TSchedRej = ( rbMon, rbWeek, rbDay, rbTime, rbNever );

  // Implementación de un Scheduler
  TScheduling = class( TSilInterfacedObject, IUnknown, IScheduling )
  private
    fIsModified: boolean;
    fNextApply: TDateTime;
    //
    fName: string;
    fStart: TDateTime;
    fStop: TDateTime;
    fAfterHour: TDateTime;
    fBeforeHour: TDateTime;
    fLastApply: TDateTime;
    fInterval: TDateTime;    // intervalo en segundos
    fDuration: TDateTime;    // duración en segundos
    fDaysOfWeek: TDaysOfWeek;
    fDaysOfMonth: TDaysOfMonth;
    fWeeksOfMonth: TWeeksOfMonth;
    fMonthsOfYear: TMonthsOfYear;
  private
    procedure Changed;
    procedure CalcNextApply; overload;
    function CalcNextApply( AFrom: TDateTime; Inclusive: boolean ): TDateTime; overload;
    function CalcEventsBetween( AFrom, ATo: TDateTime; FromIncl: boolean ): integer;
    function IntAppliesAt(ATime: TDateTime; out RejectBy: TSchedRej ): boolean;
    function TruncTime(ATime: TDateTime): TDateTime;
  protected // de IScheduling
    function GetIsModified: boolean;
    procedure SetIsModified(const Value: boolean);
    function GetName: string;
    function GetDaysOfMonth: TDaysOfMonth;
    function GetDaysOfWeek: TDaysOfWeek;
    function GetInterval: TDateTime;
    function GetDuration: TDateTime;
    function GetMissed: integer;
    function GetMonthsOfYear: TMonthsOfYear;
    function GetStart: TDateTime;
    function GetStop: TDateTime;
    function GetAfterHour: TDateTime;
    function GetBeforeHour: TDateTime;
    function GetLastApply: TDateTime;
    function GetWeeksOfMonth: TWeeksOfMonth;
    procedure SetName(const Value: string);
    procedure SetDaysOfMonth(const Value: TDaysOfMonth);
    procedure SetDaysOfWeek(const Value: TDaysOfWeek);
    procedure SetInterval(const Value: TDateTime);
    procedure SetDuration(const Value: TDateTime);
    procedure SetMonthsOfYear(const Value: TMonthsOfYear);
    procedure SetStart(const Value: TDateTime);
    procedure SetStop(const Value: TDateTime);
    procedure SetAfterHour(const Value: TDateTime);
    procedure SetBeforeHour(const Value: TDateTime);
    procedure SetLastApply(const Value: TDateTime);
    procedure SetWeeksOfMonth(const Value: TWeeksOfMonth);
    function NextApply: TDateTime;
    function AppliesAt( ATime: TDateTime ): boolean;
    function LoadFrom( AConfig: INamedValues ): boolean; virtual;
    function SaveTo( AConfig: INamedValues ): boolean; virtual;
    // propiedades
    property Name: string read GetName write SetName;
    property Start: TDateTime read GetStart write SetStart;
    property Stop: TDateTime read GetStop write SetStop;
    property AfterHour: TDateTime read GetAfterHour write SetAfterHour;
    property BeforeHour: TDateTime read GetBeforeHour write SetBeforeHour;
    property Interval: TDateTime read GetInterval write SetInterval;
    property Duration: TDateTime read GetDuration write SetDuration;
    property DaysOfWeek: TDaysOfWeek read GetDaysOfWeek write SetDaysOfWeek;
    property DaysOfMonth: TDaysOfMonth read GetDaysOfMonth write SetDaysOfMonth;
    property WeeksOfMonth: TWeeksOfMonth read GetWeeksOfMonth write SetWeeksOfMonth;
    property MonthsOfYear: TMonthsOfYear read GetMonthsOfYear write SetMonthsOfYear;
    property Missed: integer read GetMissed;
    property LastApply: TDateTime read GetLastApply write SetLastApply;
    property IsModified: boolean read GetIsModified write SetIsModified;
  public
    constructor Create; reintroduce;
  public
    class function StrToDaysOfWeek(AText: string): TDaysOfWeek;
    class function StrToDaysOfMonth(AText: string): TDaysOfMonth;
    class function StrToMonthsOfYear(AText: string): TMonthsOfYear;
    class function StrToWeeksOfMonth(AText: string): TWeeksOfMonth;
    class function DaysOfMonthToStr(ADaysOfMonth: TDaysOfMonth): string;
    class function DaysOfWeekToStr(ADaysOfWeek: TDaysOfWeek): string;
    class function WeeksOfMonthToStr(AWeeksOfMonth: TWeeksOfMonth): string;
    class function MonthsOfYearToStr(AMonthsOfYear: TMonthsOfYear): string;
    class function WeekOfMonth(ATime: TDateTime): integer;
  end;

implementation

uses
  Sysutils,  // por Dayofweek
  SilBtDateTime,
  SilBtDate,
  SilBtTime,
  SilBtStr;

{ TScheduling }

constructor TScheduling.Create;
begin
  inherited Create();

  fDaysOfWeek := kAllDaysOfWeek;
  fDaysOfMonth := kAllDaysOfMonth;
  fWeeksOfMonth := kAllWeeksOfMonth;
  fMonthsOfYear := kAllMonthsOfYear;
  fStart := 0;
  fStop := INFINITE;
  fAfterHour := 0;
  fBeforeHour := 1 - 1 / 86400; // 23:59:59
end;

function TScheduling.LoadFrom(AConfig: INamedValues): boolean;
begin
  if ( AConfig <> nil ) then
  begin
    with AConfig do
    begin
      fName := ReadString( 'Name', '' );
      fStart := ReadFloat( 'Start', 0 );
      fStop := ReadFloat( 'Stop', 0 );
      fAfterHour := ReadFloat( 'AfterHour', 0 );
      fBeforeHour := ReadFloat( 'BeforeHour', 0 );
      fInterval := ReadFloat( 'Interval', 0 );
      fDuration := ReadFloat( 'Duration', 0 );
      fDaysOfWeek := StrToDaysOfWeek( ReadString( 'DaysOfWeek', '' ) );
      fDaysOfMonth := StrToDaysOfMonth( ReadString( 'DaysOfMonth', '' ) );
      fWeeksOfMonth := StrToWeeksOfMonth( ReadString( 'WeeksOfMonth', '' ) );
      fMonthsOfYear := StrToMonthsOfYear( ReadString( 'MonthsOfYear', '' ) );
    end;
    fIsModified := false;
  end;
  result := true;
end;

function TScheduling.SaveTo(AConfig: INamedValues): boolean;
begin
  if ( AConfig <> nil ) then
  begin
    with AConfig do
    begin
      WriteString( 'Name', fName );
      WriteFloat( 'Start', fStart );
      WriteFloat( 'Stop', fStop );
      WriteFloat( 'AfterHour', fAfterHour );
      WriteFloat( 'BeforeHour', fBeforeHour );
      WriteFloat( 'Interval', fInterval );
      WriteFloat( 'Duration', fDuration );
      WriteString( 'DaysOfWeek', DaysOfWeekToStr( fDaysOfWeek ) );
      WriteString( 'DaysOfMonth', DaysOfMonthToStr( fDaysOfMonth ) );
      WriteString( 'WeeksOfMonth', WeeksOfMonthToStr( fWeeksOfMonth ) );
      WriteString( 'MonthsOfYear', MonthsOfYearToStr( MonthsOfYear ) );
    end;
    fIsModified := false;
  end;
  result := true;
end;

function TScheduling.GetIsModified: boolean;
begin
  result := fIsModified;
end;

procedure TScheduling.SetIsModified(const Value: boolean);
begin
  fIsModified := Value;
end;

function TScheduling.GetName: string;
begin
  result := fName;
end;

function TScheduling.GetDaysOfMonth: TDaysOfMonth;
begin
  result := fDaysOfMonth;
end;

function TScheduling.GetDaysOfWeek: TDaysOfWeek;
begin
  result := fDaysOfWeek;
end;

function TScheduling.GetInterval: TDateTime;
begin
  result := fInterval;
end;

function TScheduling.GetDuration: TDateTime;
begin
  result := fDuration;
end;

function TScheduling.GetMissed: integer;
begin
  result := CalcEventsBetween( fLastApply, DateTime.Now(), false );
end;

function TScheduling.GetMonthsOfYear: TMonthsOfYear;
begin
  result := fMonthsOfYear;
end;

function TScheduling.GetStart: TDateTime;
begin
  result := fStart;
end;

function TScheduling.GetStop: TDateTime;
begin
  result := fStop;
end;

function TScheduling.GetAfterHour: TDateTime;
begin
  result := fAfterHour;
end;

function TScheduling.GetBeforeHour: TDateTime;
begin
  result := fBeforeHour;
end;

function TScheduling.GetLastApply: TDateTime;
begin
  result := fLastApply;
end;

function TScheduling.GetWeeksOfMonth: TWeeksOfMonth;
begin
  result := WeeksOfMonth;
end;

procedure TScheduling.SetName(const Value: string);
begin
  if ( fName = Value ) then exit;
  fName := Value;
  Changed;
end;

procedure TScheduling.SetDaysOfMonth(const Value: TDaysOfMonth);
begin
  if ( fDaysOfMonth = Value ) then exit;
  fDaysOfMonth := Value;
  Changed;
end;

procedure TScheduling.SetDaysOfWeek(const Value: TDaysOfWeek);
begin
  if ( fDaysOfWeek = Value ) then exit;
  fDaysOfWeek := Value;
  Changed;
end;

procedure TScheduling.SetInterval(const Value: TDateTime);
begin
  if ( fInterval = Value ) then exit;
  fInterval := Value;
  Changed;
end;

procedure TScheduling.SetDuration(const Value: TDateTime);
begin
  if ( fDuration = Value ) then exit;
  fDuration := Value;
  Changed;
end;

procedure TScheduling.SetMonthsOfYear(const Value: TMonthsOfYear);
begin
  if ( fMonthsOfYear = Value ) then exit;
  fMonthsOfYear := Value;
  Changed;
end;

procedure TScheduling.SetStart(const Value: TDateTime);
begin
  if ( fStart = Value ) then exit;
  fStart := Value;
  Changed;
end;

procedure TScheduling.SetStop(const Value: TDateTime);
begin
  if ( fStop = Value ) then exit;
  fStop := Value;
  Changed;
end;

procedure TScheduling.SetAfterHour(const Value: TDateTime);
begin
  if ( fAfterHour = Value ) then exit;
  fAfterHour := Value;
  Changed;
end;

procedure TScheduling.SetBeforeHour(const Value: TDateTime);
begin
  if ( fBeforeHour = Value ) then exit;
  fBeforeHour := Value;
  Changed;
end;

procedure TScheduling.SetLastApply(const Value: TDateTime);
begin
  if ( fLastApply = Value ) then exit;
  fLastApply := Value;
  CalcNextApply;
end;

procedure TScheduling.SetWeeksOfMonth(const Value: TWeeksOfMonth);
begin
  if ( fWeeksOfMonth = Value ) then exit;
  fWeeksOfMonth := Value;
  Changed;
end;

procedure TScheduling.Changed;
begin
  fIsModified := true;
  CalcNextApply;
end;

procedure TScheduling.CalcNextApply;
begin
  fNextApply := CalcNextApply( DateTime.Now(), true );
end;

function TScheduling.NextApply: TDateTime;
begin
  result := fNextApply;
end;

function TScheduling.CalcEventsBetween( AFrom, ATo: TDateTime; FromIncl: boolean ): integer;
var
  t1: TDateTime;
begin
  result := 0;
  t1 := AFrom;
  while ( t1 > 0 ) do
  begin
    t1 := CalcNextApply( t1, FromIncl );
    if ( t1 > ATo ) then break;
    if ( t1 > 0 ) then Inc( result );
    FromIncl := false;
  end;
end;

//
// Obtiene la próxima aplicación desde un momento determinado
//
function TScheduling.CalcNextApply( AFrom: TDateTime; Inclusive: boolean ): TDateTime;
var
  t1, t2: TDateTime;
  rejby: TSchedRej;
begin
  if ( AFrom > fStart ) then
  begin
    result := 0;
    if ( fInterval > 0 ) then
    begin
      t1 := TruncTime( AFrom );
      if ( t1 = fLastApply ) or ( t1 < AFrom ) or
        not Inclusive and ( t1 = AFrom ) then
        t1 := t1 + fInterval;

      while ( t1 <= fStop ) do
      begin
        if IntAppliesAt( t1, rejby ) then
        begin
          result := t1;
          break;
        end;

        // Incrementa según la causa de rechazo
        case rejby of
        rbNever: break;
        rbTime: t1 := t1 + fInterval;
        else
          // Pasa al siguiente día
          begin
            t2 := Trunc( t1 ) + 1;
            t1 := TruncTime( t2 + fInterval );
            if ( t1 - t2 >= fInterval ) then
              t1 := t1 - fInterval;
          end;
        end;
      end;
    end;
  end
  else
    result := fStart;
end;

//
// Determina si hay aplicación en un determinado momento
//
function TScheduling.AppliesAt(ATime: TDateTime): boolean;
var
  rejby: TSchedRej;
begin
  result := ( fStart <= ATime ) and ( ATime <= fStop ) and
    IntAppliesAt( ATime, rejby );
end;

function TScheduling.IntAppliesAt( ATime: TDateTime; out RejectBy: TSchedRej ): boolean;
var
  fecha: TDateParts;
  t1: TDateTime;
begin
  result := false;
  if ( fAfterHour > fBeforeHour ) then
  begin
    RejectBy := rbNever;
    exit;
  end;

  // Filtra por duración
  t1 := TruncTime( ATime );
  if ( ATime - t1 > fDuration + 0.1 / 86400 ) then
  begin
    RejectBy := rbTime;
    exit;
  end;

  fecha := Date.Decode( t1 );
  result := true;

  // Filtra por mes del año
  if result then
  begin
    case fecha.Month of
    1: result := ( moyJua in fMonthsOfYear );
    2: result := ( moyFeb in fMonthsOfYear );
    3: result := ( moyMar in fMonthsOfYear );
    4: result := ( moyApr in fMonthsOfYear );
    5: result := ( moyMay in fMonthsOfYear );
    6: result := ( moyJun in fMonthsOfYear );
    7: result := ( moyJul in fMonthsOfYear );
    8: result := ( moyAgo in fMonthsOfYear );
    9: result := ( moySep in fMonthsOfYear );
    10: result := ( moyOct in fMonthsOfYear );
    11: result := ( moyNov in fMonthsOfYear );
    12: result := ( moyDec in fMonthsOfYear );
    end;
    if not result then
      RejectBy := rbMon;
  end;

  // Filtra por semana del mes
  if result then
  begin
    case WeekOfMonth( t1 ) of
    1: result := ( womFirst in fWeeksOfMonth );
    2: result := ( womSecond in fWeeksOfMonth );
    3: result := ( womThird in fWeeksOfMonth );
    4: result := ( womFourth in fWeeksOfMonth );
    5: result := ( womFifth in fWeeksOfMonth );
    end;
    if not result then
      RejectBy := rbWeek;
  end;

  // Filtra por día de la semana
  if result then
  begin
    case DayOfWeek( t1 ) of
    1: result := ( dowSun in fDaysOfWeek );
    2: result := ( dowMon in fDaysOfWeek );
    3: result := ( dowTue in fDaysOfWeek );
    4: result := ( dowWen in fDaysOfWeek );
    5: result := ( dowThu in fDaysOfWeek );
    6: result := ( dowFri in fDaysOfWeek );
    7: result := ( dowSat in fDaysOfWeek );
    end;
    if not result then
      RejectBy := rbDay;
  end;

  // Filtra por día del mes
  if result then
  begin
    case fecha.Day of
    1: result := ( dom1 in fDaysOfMonth );
    2: result := ( dom2 in fDaysOfMonth );
    3: result := ( dom3 in fDaysOfMonth );
    4: result := ( dom4 in fDaysOfMonth );
    5: result := ( dom5 in fDaysOfMonth );
    6: result := ( dom6 in fDaysOfMonth );
    7: result := ( dom7 in fDaysOfMonth );
    8: result := ( dom8 in fDaysOfMonth );
    9: result := ( dom9 in fDaysOfMonth );
    10: result := ( dom10 in fDaysOfMonth );
    11: result := ( dom11 in fDaysOfMonth );
    12: result := ( dom12 in fDaysOfMonth );
    13: result := ( dom13 in fDaysOfMonth );
    14: result := ( dom14 in fDaysOfMonth );
    15: result := ( dom15 in fDaysOfMonth );
    16: result := ( dom16 in fDaysOfMonth );
    17: result := ( dom17 in fDaysOfMonth );
    18: result := ( dom18 in fDaysOfMonth );
    19: result := ( dom19 in fDaysOfMonth );
    20: result := ( dom20 in fDaysOfMonth );
    21: result := ( dom21 in fDaysOfMonth );
    22: result := ( dom22 in fDaysOfMonth );
    23: result := ( dom23 in fDaysOfMonth );
    24: result := ( dom24 in fDaysOfMonth );
    25: result := ( dom25 in fDaysOfMonth );
    26: result := ( dom26 in fDaysOfMonth );
    27: result := ( dom27 in fDaysOfMonth );
    28: result := ( dom28 in fDaysOfMonth );
    29: result := ( dom29 in fDaysOfMonth );
    30: result := ( dom30 in fDaysOfMonth );
    31: result := ( dom31 in fDaysOfMonth );
    end;
    if not result then
      RejectBy := rbDay;
  end;

  // Filtra por el rango horario
  if result then
  begin
    t1 := Time.FromDateTime( t1 );
    result := ( fAfterHour <= t1 ) and ( t1 <= fBeforeHour );
    if not result then
      RejectBy := rbTime;
  end;
end;

function TScheduling.TruncTime( ATime: TDateTime ): TDateTime;
begin
  result := fStart + fInterval *
    Date.FromDateTime( ( ATime - fStart ) / fInterval + 0.0001 );
end;

class function TScheduling.StrToDaysOfWeek( AText: string ): TDaysOfWeek;
var
  i1: integer;
begin
  i1 := Str.ToInt( AText, 0 );
  Move( i1, result, sizeof( result ) );
end;

class function TScheduling.StrToDaysOfMonth( AText: string ): TDaysOfMonth;
var
  i1: integer;
begin
  i1 := Str.ToInt( AText, 0 );
  Move( i1, result, sizeof( result ) );
end;

class function TScheduling.StrToWeeksOfMonth( AText: string ): TWeeksOfMonth;
var
  i1: integer;
begin
  i1 := Str.ToInt( AText, 0 );
  Move( i1, result, sizeof( result ) );
end;

class function TScheduling.StrToMonthsOfYear( AText: string ): TMonthsOfYear;
var
  i1: integer;
begin
  i1 := Str.ToInt( AText, 0 );
  Move( i1, result, sizeof( result ) );
end;

class function TScheduling.DaysOfWeekToStr( ADaysOfWeek: TDaysOfWeek ): string;
var
  i1: integer;
begin
  Move( ADaysOfWeek, i1, sizeof( TDaysOfWeek ) );
  result := IntToStr( i1 );
end;

class function TScheduling.DaysOfMonthToStr( ADaysOfMonth: TDaysOfMonth ): string;
var
  i1: integer;
begin
  Move( ADaysOfMonth, i1, sizeof( TDaysOfMonth ) );
  result := IntToStr( i1 );
end;

class function TScheduling.WeeksOfMonthToStr( AWeeksOfMonth: TWeeksOfMonth ): string;
var
  i1: integer;
begin
  Move( AWeeksOfMonth, i1, sizeof( TWeeksOfMonth ) );
  result := IntToStr( i1 );
end;

class function TScheduling.MonthsOfYearToStr( AMonthsOfYear: TMonthsOfYear ): string;
var
  i1: integer;
begin
  Move( AMonthsOfYear, i1, sizeof( TMonthsOfYear ) );
  result := IntToStr( i1 );
end;

class function TScheduling.WeekOfMonth( ATime: TDateTime ): integer;
begin
  // FUNCION NO IMPLEMENTADA
  result := 0;
end;

end.

