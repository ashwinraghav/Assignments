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

unit SilLiScheduler;

{$I Defines.inc}

interface

uses
  SilLiKey;
  
type
  IScheduling = interface;

  TDayOfWeek = ( dowMon, dowTue, dowWen, dowThu, dowFri, dowSat, dowSun );
  TDayOfMonth = ( dom1, dom2, dom3, dom4, dom5, dom6, dom7, dom8, dom9,
    dom10, dom11, dom12, dom13, dom14, dom15, dom16, dom17, dom18, dom19,
    dom20, dom21, dom22, dom23, dom24, dom25, dom26, dom27, dom28, dom29,
    dom30, dom31 );
  TWeekOfMonth = ( womFirst, womSecond, womThird, womFourth, womFifth );
  TMonthOfYear = ( moyJua, moyFeb, moyMar, moyApr, moyMay, moyJun,
    moyJul, moyAgo, moySep, moyOct, moyNov, moyDec );
  TDaysOfWeek = set of TDayOfWeek;
  TDaysOfMonth = set of TDayOfMonth;
  TWeeksOfMonth = set of TWeekOfMonth;
  TMonthsOfYear = set of TMonthOfYear;

  // Scheduling genérico
  IScheduling = interface
  ['{137F0702-BEDF-11D4-B9DD-006008AE4EDF}']
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
    function LoadFrom( AConfig: INamedValues ): boolean;
    function SaveTo( AConfig: INamedValues ): boolean; 
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
  end;

const
  kAllDaysOfWeek = [ dowMon, dowTue, dowWen, dowThu, dowFri, dowSat, dowSun ];
  kAllDaysOfMonth = [ dom1, dom2, dom3, dom4, dom5, dom6, dom7, dom8, dom9,
    dom10, dom11, dom12, dom13, dom14, dom15, dom16, dom17, dom18, dom19,
    dom20, dom21, dom22, dom23, dom24, dom25, dom26, dom27, dom28, dom29,
    dom30, dom31 ];
  kAllWeeksOfMonth = [ womFirst, womSecond, womThird, womFourth, womFifth ];
  kAllMonthsOfYear = [ moyJua, moyFeb, moyMar, moyApr, moyMay, moyJun,
    moyJul, moyAgo, moySep, moyOct, moyNov, moyDec ];

implementation

end.

