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
 *     Copyright (C) 2002 Mariano Reggiardo   Mreggiardo@infovia.com.ar         *
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
unit SilSmTimedRun;

interface

uses Sil,  SilSiTimedRun , SilLiReference , SilOiThread;

type

  TTimedRun = Class( TSilInterfacedObject , iTimedRun, iDispatchable )
  private
    FTimeOut: Integer;
    procedure SetTimeOut(const Value: Integer);
    Function GetTimeOut: Integer;
  private
    FListener : iTimedRunListener;
    FFinish: IEvent;
    FWaitThread : iThread;
    procedure AsyncRun(var Msg: RThreadRunMessage); message RUN_FUNCTION ;
    procedure Finish;
  public
    procedure Go;
    property TimeOut :Integer read GetTimeOut write SetTimeOut;
    constructor create( ATimeOut : Integer ; AListener : iTimedRunListener );
    destructor destroy;override;
  end;


implementation

{ TTimedRun }
procedure TTimedRun.AsyncRun(var Msg: RThreadRunMessage);
begin
  if not Sil.Os.Wait.Single( Msg.Thread.Termination , TimeOut ) then
    begin
    FListener.onTime;
    end;
end;

constructor TTimedRun.create(ATimeOut : Integer ; AListener : iTimedRunListener);
begin
  FTimeOut := ATimeOut;
  FListener := AListener;
end;

destructor TTimedRun.destroy;
begin
  Finish;
  inherited;
end;

procedure TTimedRun.Finish;
begin
  if Assigned(FWaitThread) then
    if not FWaitThread.IsTerminated then
      begin
        FFinish.Signal;
        FWaitThread.Termination.WaitFor(INFINITE, true);
      end
end;

procedure TTimedRun.Go;
begin
  Finish;
  FFinish := Sil.OS.Ipc.Event;
  FWaitThread := Sil.Os.Thread.Spawn( RUN_FUNCTION , Self );
end;

procedure TTimedRun.SetTimeOut(const Value: Integer);
begin
  FTimeOut := Value;
end;

function TTimedRun.GetTimeOut: Integer;
begin
  result := FTimeOut;
end;

end.
