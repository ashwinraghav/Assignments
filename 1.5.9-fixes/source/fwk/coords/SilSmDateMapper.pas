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

unit SilSmDateMapper;

interface

uses
  Sil,
  SilMath,

  SilSeDateMapper,
  SilSmLinearMapper,
  SilSmTic;

type
  TDateMapper = class(TLinearMapper)
  private
    FEntry: PTableEntry;
    procedure CalcFlags(const Mapping: ICoordsMapping);
  protected
    function SetupMapping(const Mapping: ICoordsMapping): ICoordsMapping; override;
    procedure Update(const Mapping: ICoordsMapping); override;
  public
    function CalcNextTic(var Data: ITicDataDef): Boolean; override;
  end;

implementation

uses
  SysUtils,
  SilScDateMapper,
  SilSfDateMapper;

{ TDateMapper }

function TDateMapper.CalcNextTic(var Data: ITicDataDef): Boolean;
var
  Map: ICoordsMapping;
  Parts: TDateTimeParts;
  Found, First: Boolean;
begin
  Map := Mapping.Normalize();
  First := False;
  
  if Data = nil then
    begin
      Data := TTicData.Create();
      Data.Next := Map.WorldMin;
      Parts := Sil.DateTime.Decode(Data.Next);
      First := True;
      if FEntry.Delta = 2 then
        begin
          if (Parts.Date.Day mod 2) = 0 then
            Data.Next := Data.Next + 1;
        end
      else if Frac(Data.Next / FEntry.Delta) <> 0 then
        Data.Next := FEntry.Delta + System.Int(Data.Next / FEntry.Delta) * FEntry.Delta;
    end
  else if Data.Next > Map.WorldMax then
      Data := nil;

  Found := False;

  if Data <> nil then
    repeat
      Data.Caption := '';
      Data.Kind := tkShort;
      Parts := Sil.DateTime.Decode(Data.Next);

      if FEntry.Delta >= 2 then
        begin

          if dfDosDias in FEntry.Flags  then
            begin { Se colocan tics cortos c/dos días }
              Data.Kind := tkShort;
              Found := True;
            end;

          if Parts.Date.Day = 1 then

            begin { Se coloca el tic del mes siempre }

              if Parts.Date.Month = 1 then

                begin { Se coloca el tic mas grande }
                  Found := True;
                  Data.Kind := tkLong;
                  Data.Caption := Sil.Date.ToStr(Data.Next, 'dd/mm/yyyy');
                end

              else if Parts.Date.Month = 7 then

                begin
                  if not (dfAnual in FEntry.Flags) then
                    begin { Se coloca el tic mas grande y devuelvo solo día y mes }
                      Found := True;
                      Data.Kind := tkLong;
                      Data.Caption := Sil.Date.ToStr(Data.Next, 'dd/mm/yyyy');
                    end
                  else
                      Found := True;
                end

              else if dfMes in FEntry.Flags then

                begin { Se coloca el tic mas grande }
                  Found := True;
                  if not (dfSeisMeses in FEntry.Flags) then
                    begin
                      Data.Kind := tkLong;
                      Data.Caption := Sil.Date.ToStr(Data.Next, 'dd/mm/yyyy');
                    end;
                end;
            end

          else if Parts.Date.Day in [11, 21] then

            begin
              if( dfDiezDias in FEntry.Flags ) then
                begin
                  Found := True;
                  Data.Kind := tkLong;
                  Data.Caption := '';
                end;
            end;
        end

      else // FEntry.Delta < 2

        begin
          if First and (dfExtremos in FEntry.Flags) then
            Data.Caption := Sil.Date.ToStr(Data.Next, 'ddd dd/mm/yyyy');

          if dfUnDia in FEntry.Flags then
            begin
              if Parts.Date.Day in [1, 7, 14, 21] then
                begin
                  Data.Kind := tkLong;
                  if Parts.Date.Day <> 1 then
                    Data.Caption := Sil.Date.ToStr(Data.Next, 'ddd dd/mm') else
                    Data.Caption := Sil.Date.ToStr(Data.Next, 'ddd dd/mm/yyyy');
                end
            end
          else if dfSeisHoras in FEntry.Flags then
            begin
              if (Parts.Time.Hour mod 6) = 0 then
              begin
                if Parts.Time.Hour <> 0 then
                  Data.Kind := tkShort else
                  Data.Kind := tkLong;
                if First then
                  Data.Caption := Sil.Date.ToStr(Data.Next, 'dd/mm/yyyy hh:nn')
                else if Data.Kind = tkLong then
                  Data.Caption := Sil.Date.ToStr(Data.Next, 'ddd dd hh:nn')
                else
                  Data.Caption := Sil.Date.ToStr(Data.Next, 'hh:nn')
              end;
            end
          else if dfTresHoras in FEntry.Flags then
            begin
              if Parts.Time.Minutes = 0 then
                Data.Kind := tkLong else
                Data.Kind := tkShort;

              if (Data.Kind = tkLong) and ((Parts.Time.Hour mod 3) = 0) then
                Data.Caption := Sil.Date.ToStr(Data.Next, 'hh:nn')
            end
          else if dfUnaHora in FEntry.Flags then
            begin
              Data.Kind := tkShort;
              if Parts.Time.Minutes = 0 then
                begin
                  Data.Kind := tkLong;
                  Data.Caption := Sil.Date.ToStr(Data.Next, 'hh:nn');
                end;
              if (dfMediaHora in FEntry.Flags) and (Parts.Time.Minutes = 30) then
                begin
                  Data.Caption := Sil.Date.ToStr(Data.Next, 'hh:nn');
                end;
            end
          else if dfMediaHora in FEntry.Flags then
            begin
              Data.Kind := tkShort;
              if (Parts.Time.Minutes mod 15) = 0 then
                begin
                  Data.Kind := tkLong;
                  if (Parts.Time.Minutes mod 30) = 0 then
                    Data.Caption := Sil.Date.ToStr(Data.Next, 'hh:nn');
                end;
            end;

          Found := True;
        end;

      Data.Point := Data.Next;
      Data.Next := Data.Next + FEntry.Delta;

      if Data.Next > Map.WorldMax then
         Break;

      if (FEntry.Delta = 2) and (Sil.DateTime.Decode(Data.Next).Date.Day = 2) then
         Data.Next := Data.Next - 1;

    until Found; 

  Result := Data <> nil;
end;

function TDateMapper.SetupMapping(const Mapping: ICoordsMapping): ICoordsMapping;
begin
  Result := inherited SetupMapping(Mapping);
  Mapping.WorldMax := DateTime.Now;
  Mapping.WorldMin := Mapping.WorldMax - 365;
end;

procedure TDateMapper.Update(const Mapping: ICoordsMapping);
begin
  if UpdateScale(Mapping) then
    CalcFlags(Mapping);
end;

procedure TDateMapper.CalcFlags(const Mapping: ICoordsMapping);
begin
  FEntry := SilSfDateMapper.LookupEntry(Abs(Mapping.WorldSize));
end;

end.
