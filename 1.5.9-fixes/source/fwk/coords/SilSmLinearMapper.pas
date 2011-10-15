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

unit SilSmLinearMapper;

interface

uses
  Sil,
  SilMath,
  SilSkCoordsMapper;

type
  TLinearMapper = class(TCoordsMapper)
  private
    FLongStep: WorldType;
    FShortStep: WorldType;
    FStepCount: ClientType;
    FScale: WorldType;
    FOffset: WorldType;
  private
    function CalcBestStep(const Mapping: ICoordsMapping): WorldType;
  protected
    function SetupMapping(const Mapping: ICoordsMapping): ICoordsMapping; override;
    function UpdateScale(const Mapping: ICoordsMapping): Boolean;
    procedure Update(const Mapping: ICoordsMapping); override;
  public
    constructor Create(const Mapping: ICoordsMapping); override;
    function WorldToClient(const Point: WorldType): ClientType; override;
    function ClientToWorld(const Point: ClientType): WorldType; override;
    function CalcNextTic(var Data: ITicDataDef): Boolean; override;
  end;

implementation

uses
  SysUtils,
  SilSmLinearTic;

constructor TLinearMapper.Create(const Mapping: ICoordsMapping);
begin
  inherited;
  FStepCount := 5;
end;

function TLinearMapper.WorldToClient(const Point: WorldType): ClientType; 
begin
  Result := Round(Point * FScale + FOffset);
end;

function TLinearMapper.ClientToWorld(const Point: ClientType): WorldType;
begin
  Result := (Point  - FOffset) / FScale;
end;

procedure TLinearMapper.Update(const Mapping: ICoordsMapping);
begin
  if UpdateScale(Mapping) then
  begin
    FLongStep := CalcBestStep(Mapping);
    FShortStep := FLongStep / FStepCount;
  end;
end;

function TLinearMapper.UpdateScale(const Mapping: ICoordsMapping): Boolean;
var
  Size: WorldType;
begin
  Size := Mapping.WorldSize;
  Result := Size <> 0;
  if Result then
    begin
      FScale  := + Mapping.ClientSize / Size;
      FOffset := - FScale * Mapping.WorldMin + Mapping.ClientMin;
    end;
end;

function TLinearMapper.CalcNextTic(var Data: ITicDataDef): Boolean;
var
  Map: ICoordsMapping;
  LongPos, ShortPos: WorldType;
  Tic: ILinearTicDef;
begin
  Map := Mapping.Normalize;

  if Data = nil then
    begin
      Tic := TLinearTic.Create();
      Data := Tic;
      Tic.CurrTic := 0;
      Tic.Next := Map.WorldMin;

      ShortPos := FShortStep * Trunc( Tic.Point / FShortStep );
      LongPos  := FLongStep  * Trunc( Tic.Point / FLongStep );

      if Tic.Next > 0 then
        begin
          // World tiene orientación positiva.
          // Hay que encontrar el menor multiplo de FShortStep mayor que World.

          if Tic.Next <> ShortPos then
            Tic.Next := ShortPos + FShortStep else
            Tic.Next := ShortPos;

          Tic.CurrTic := Round( (LongPos + FLongStep - Tic.Next) / FShortStep );
        end
      else if Tic.Next < 0 then
        begin
          // World tiene orientación negativa.
          // Hay que encontrar el menor multiplo de FShortStep menor que World.

          Tic.Next := ShortPos;

          Tic.CurrTic := Abs(Round( (ShortPos - LongPos) / FShortStep ));
        end;
    end
  else
    begin
      Tic := Data as ILinearTicDef;
      if Tic.Next > Map.WorldMax then
        begin

          Result := False;
          Data := nil;

          Exit;
        end;
    end;
    
  Tic.Point := Tic.Next;
  Tic.Next := Tic.Next + FShortStep;

  if (Tic.CurrTic mod FStepCount) = 0 then
    begin
      Tic.Kind := tkLong;
      Tic.Caption := Float.ToStr(Tic.Point, 3, 2);
    end
  else
    begin
      Tic.Kind := tkShort;
      Tic.Caption := '';
    end;

  Tic.CurrTic := Tic.CurrTic - 1;

  if Tic.CurrTic = 0 then
    Tic.CurrTic := FStepCount;

   Result := True;
end;

function TLinearMapper.SetupMapping(const Mapping: ICoordsMapping): ICoordsMapping;
begin
  Mapping.WorldMin  :=   0;
  Mapping.WorldMin  := 100;
  Mapping.ClientMin :=   0;
  Mapping.ClientMin := 100;
  Result := Mapping;
end;

function TLinearMapper.CalcBestStep(const Mapping: ICoordsMapping): WorldType;
var
  Size, Step: WorldType;
  N: Longint;
begin
  Result := 0;

  Size := Mapping.Normalize.WorldSize;
  
  if Size = 0 then Exit;

  Step := 10;
  
  if ( Frac( Size / 5 ) <> 0 ) and ( Size > 5 ) then
    begin
      { Adopto un Size igual al mayor multiplo de 5, menor al Size dado.
        Esto impide que la rutina escoja un paso extraño. }
      Size := Trunc( Size / 5 ) * 5;
    end;

  N := Trunc(Size / Step);

  while (N > 10) or (N < 6) do
  begin
     if N > 10 then
     begin
        Step := Step * 2.5;
        N := Trunc(Size / Step);
        if N > 10 then
        begin
           Step := Step * 2.0;
           N := Trunc(Size / Step);
           if N > 10 then
           begin
              Step := Step * 2.0;
              N := Trunc(Size / Step);
           end
        end
     end else if N < 6 then
     begin
        Step := Step / 2.0;
        N := Trunc(Size / Step);
        if N < 6 then
        begin
           Step := Step / 2.0;
           N := Trunc(Size / Step);
           if N < 6 then
           begin
              Step := Step / 2.5;
              N := Trunc(Size / Step);
           end;
        end;
     end;

  end;

  Result := Step;
end;

initialization
begin
  //Sil.Coords.Define('Linear', TLinearMapper);
end;

end.
