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

unit SilOkPerformance;

{$I Defines.inc}

interface

uses
  SilBeTypes,
  SilOsTypes,
  SilLkInterfaced,
  SilOiPerformance;
                  
type
  TSilPerformance = class(
    TSilInterfacedObject,
    IPerformanceCounter )
  protected 
    FFrequency: LargeInt;
    FInitial: LargeInt;
  protected
    function GetFrequency: LargeInt;       
    function GetInitial: LargeInt; 
    function GetElapsed: LargeInt; virtual; 
    function GetTime(DoReset: Boolean): LargeInt; virtual; 
    function ToMicroseconds(DoReset: Boolean): Single;
    function ToMilliseconds(DoReset: Boolean): Single;
    function ToSeconds(DoReset: Boolean): Single;
    function ToDateTime(DoReset: Boolean): TDateTime;
    procedure Reset;
  protected
    function DoQueryTime: LargeInt; virtual; abstract;      
    function DoQueryFrequency: LargeInt; virtual; abstract;
  public
    constructor Create; virtual;
    destructor Destroy; override; 
  end;                                                                    

implementation

uses
  SilBtDateTime;
  
{ TSilPerformance }

constructor TSilPerformance.Create;
begin
  inherited Create;
  FFrequency := DoQueryFrequency();
  FInitial := DoQueryTime();
end;

destructor TSilPerformance.Destroy;
begin
  inherited;
end;

function TSilPerformance.GetFrequency: LargeInt;
begin
  Result := FFrequency;
end;

function TSilPerformance.GetInitial: LargeInt;
begin
  Result := FInitial;
end;

function TSilPerformance.GetElapsed: LargeInt;
begin
  Result := GetTime(False);
end;

function TSilPerformance.GetTime(DoReset: Boolean): LargeInt;
var
  Current: LargeInt;
begin
  Current := DoQueryTime();
  Result := Current - FInitial;
  if DoReset then
    FInitial := Current;
end;

function TSilPerformance.ToMicroseconds(DoReset: Boolean): Single;
begin
  Result := GetTime(DoReset) / FFrequency * 1e6;
end;

function TSilPerformance.ToMilliseconds(DoReset: Boolean): Single;
begin
  Result := GetTime(DoReset) / FFrequency * 1e3;
end;

function TSilPerformance.ToSeconds(DoReset: Boolean): Single;
begin
  Result := GetTime(DoReset) / FFrequency;
end;

function TSilPerformance.ToDateTime(DoReset: Boolean): TDateTime;
begin
  Result := DateTime.FromSecs(ToSeconds(DoReset));
end;

procedure TSilPerformance.Reset;
begin
  FInitial := DoQueryTime();
end;

end.
