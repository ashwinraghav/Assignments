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

unit SvAgentMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, SvcMgr,

  Sil,
  UiAgent,
  UmAgent,
  DmAgentSource;

type
  TSvAgent = class(TService)
    procedure ServiceStart(Sender: TService; var Started: Boolean);
    procedure ServiceStop(Sender: TService; var Stopped: Boolean);
  private
    FAgent: IAgent;
    FSource: TdaAgentSource;
  public
    function GetServiceController: TServiceController; override;
  end;

var
  SvAgent: TSvAgent;

implementation

{$R *.DFM}

uses
  UtLog;

procedure ServiceController(CtrlCode: DWord); stdcall;
begin
  SvAgent.Controller(CtrlCode);
end;

function TSvAgent.GetServiceController: TServiceController;
begin
  Result := ServiceController;
end;

procedure TSvAgent.ServiceStart(Sender: TService; var Started: Boolean);
begin
  Log.Initialize;

  FSource := TdaAgentSource.Create(Self);
  FAgent := TAgent.Create(FSource);

  FAgent.Start;
end;

procedure TSvAgent.ServiceStop(Sender: TService; var Stopped: Boolean);
begin
  if Assigned(FAgent) then
  begin
    FAgent.Stop;
    FAgent := nil;
  end;

  Log.Finalize;
end;

end.
