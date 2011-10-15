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

unit SilVmDrag;

interface

uses
  Sil,
  SilVcl,
  Controls;

type
  TDragState = class(TSilInterfacedObject, IDragState)
  private
    FOwner: TWinControl;
    FSender: TWinControl;
    FOrigin: TPoint;
    FCurrent: TPoint;
    FSink: IDragEvents;
    FMoved: Boolean;
    function Convert(const Value: TPoint): TPoint;
  protected
    function Owner: TControl;
    function Sender: TControl;
    function Origin: TPoint;
    function Current: TPoint;
    function Zone: TRect;
    procedure Update(const Point: TPoint);
    function EndDrag(const Point: TPoint): TRect;
  public
    constructor Create(const Owner, Sender: TWinControl; const Origin: TPoint; const Sink: IDragEvents);
    destructor Destroy; override;
  end;

implementation

{ TDragState }

constructor TDragState.Create(const Owner, Sender: TWinControl; const Origin: TPoint; const Sink: IDragEvents);
begin
  inherited Create;
  FOwner := Owner;
  FSender := Sender;
  FOrigin := Convert(Origin);
  FCurrent := FOrigin;
  FSink := Sink;
  FSink.OnDragBegin(Self);
end;

destructor TDragState.Destroy;
begin
  FSink := nil;
  FSender := nil;
  FOwner := nil;
  inherited;
end;

function TDragState.Current: TPoint;
begin
  Result := FCurrent;
end;

procedure TDragState.Update(const Point: TPoint);
begin
  if FMoved then FSink.OnDragUpdate(Self);
  FCurrent := Convert(Point);
  FSink.OnDragUpdate(Self);
  FMoved := True;
end;

function TDragState.EndDrag(const Point: TPoint): TRect;
begin
  if FMoved then FSink.OnDragUpdate(Self);
  FCurrent := Convert(Point);
  FSink.OnDragEnd(Self);
  Result := Zone;
end;

function TDragState.Origin: TPoint;
begin
  Result := FOrigin;
end;

function TDragState.Sender: TControl;
begin
  Result := FSender;
end;

function TDragState.Zone: TRect;
begin
  Result := Rect.Normalize(Rect.Make(FOrigin.X, FOrigin.Y, FCurrent.X, FCurrent.Y));  
end;

function TDragState.Owner: TControl;
begin
  Result := FOwner;
end;

function TDragState.Convert(const Value: TPoint): TPoint;
begin
  Result := FOwner.ScreenToClient(FSender.ClientToScreen(Value));
end;

end.
 