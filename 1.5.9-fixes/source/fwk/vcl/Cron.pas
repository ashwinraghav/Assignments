(****
autor......:  mariano d. podesta - antiriad@gmail.com      
empresa....:
fecha......:
descripcion:
notas......:
****)
unit Cron;

interface

{$include Defines.inc}

uses
  Windows, Messages, SysUtils,

  Sil;

const
  WM_CRONNOTIFY = WM_USER + 1;
  WM_CRONADDTASK = WM_USER + 2;

  CRON_EMPTY = '----/--/-- --:--:--';
  CRON_LEN = Length(CRON_EMPTY);

  MS_DELAY = 100;
  MS_SECOND = 1000;
  MS_MINUTE = 60000;

  CronPos: array[0..5] of Byte = (1, 6, 9, 12, 15, 18);
  CronLen: array[0..5] of Byte = (4, 2, 2, 2, 2, 2);

type
  TACron = array[0..5] of String;

function f_RangeExpand(const strRange: String): String;
function f_CronItems(var Adv: TACron; strCom: String): String;
function f_CronFormat(const dtStamp: TDateTime): String;
function f_CronDateTime(const strStamp: String): TDateTime;
function f_CronMsgToDateTime(const Msg: TMessage): TDateTime;
function f_PadZero(const S: String; const L: Byte): String;

implementation

const
  Ini: array[0..5] of Byte = (0, 1, 1, 0, 0, 0);
  Fin: array[0..5] of ShortInt = (-1, 12, -2, 23, 59, 59);

function f_PadZero(const S: String; const L: Byte): String;
begin
  Result := Str.Replicate('0', L);
  Move(S[1], Result[L - Length(S) + 1], Length(S));
end;

function f_CronMsgToDateTime(const Msg: TMessage): TDateTime;
begin
  Result := Msg.WParam + Msg.LParam / 1000000;
end;

function f_CronFormat(const dtStamp: TDateTime): String;
var
  wYear, wMonth, wDay, wHour, wMin, wSec, wMSec: Word;
begin
  DecodeDate(dtStamp, wYear, wMonth, wDay);
  DecodeTime(dtStamp, wHour, wMin, wSec, wMSec);
  Result :=
    f_PadZero(Int.ToStr(wYear), CronLen[0]) + '/' +
    f_PadZero(Int.ToStr(wMonth), CronLen[1]) + '/' +
    f_PadZero(Int.ToStr(wDay), CronLen[2]) + ' ' +
    f_PadZero(Int.ToStr(wHour), CronLen[3]) + ':' +
    f_PadZero(Int.ToStr(wMin), CronLen[4]) + ':' +
    f_PadZero(Int.ToStr(wSec), CronLen[5]);
end;

function f_CronDateTime(const strStamp: String): TDateTime;
var
  i: Integer;
  dt: array[0..5] of Word;
begin
  for i := 0 to 5 do dt[i] := StrToInt(Str.Copy(strStamp, CronPos[i], CronLen[i]));
  Result := EncodeDate(dt[0], dt[1], dt[2]) + EncodeTime(dt[3], dt[4], dt[5], 0);
end;

function f_CronItems(var Adv: TACron; strCom: String): String;
  // rutinas internas
  function _Toma(const intPos: Integer): String;
  begin
    Result := Str.Copy(strCom, CronPos[intPos], CronLen[intPos]);
  end;

  procedure _MenorValor(var strCom: String; const j: Integer);
  var
    strSub: String;
  begin
    if (Length(Adv[j]) = 0) or (Adv[j][1] = '-') then  // si es comodin se toma el menor valor
    begin
      strSub := f_PadZero(Int.ToStr(Ini[j]), CronLen[j]);
      Move(strSub[1], strCom[CronPos[j]], CronLen[j]);
    end else
      Move(Adv[j][1], strCom[CronPos[j]], CronLen[j]);
  end;

  function _Suma(var strChg: String; const intPos: Integer): Boolean;
  var
    j, intSum: Integer;
    strSub: String;
  begin
    Result := false;
    intSum := StrToInt(_Toma(intPos)) + 1;  // suma uno

    if (intPos = 2) then  // cantidad de dias segun el mes y el año
      Fin[2] := MonthDays[IsLeapYear(StrToInt(_Toma(0)))][StrToInt(_Toma(1))];

    if (Fin[intPos] > -1) and (intSum > Fin[intPos]) then    // verifica que no se pase del maximo
      if (intPos > 0) then
      begin
        Result := _Suma(strChg, intPos - 1);  // hace una llamada recursiva para modificar
        Exit;                                 // el valor anterior
      end else
        Exit;

    strSub := f_PadZero(Int.ToStr(intSum), CronLen[intPos]);
    Move(strSub[1], strCom[CronPos[intPos]], CronLen[intPos]);

    for j := intPos + 1 to 5 do _MenorValor(strCom, j);
    Result := true;
  end;

var
  i, intPos: Integer;
  S, strInc, strSub: String;
  blnMay, blnComp: Boolean;
begin
  Result := CRON_EMPTY;
  //_Suma(strCom, 5);    // suma 1 segundo para forzarlo a buscar uno mayor
  blnMay := false;

  for i := 0 to 5 do
    if blnMay then  // si ya hay un mayor el resto son todos "los minimos posibles"
      _MenorValor(Result, i) else
    begin
      strInc := '';
      intPos := 1;

      repeat
        S := Str.Token(Adv[i], ',', intPos);    // toma el valor siguiente
        strSub := _Toma(i);                      // extrae el valor a comparar

        if (Length(S) = 0) or (S[1] = '-') then S := strSub;        // si es comodin se iguala al valor a comparar

        case StrComp(PChar(S), PChar(strSub)) of
          0:
          begin                                 // son iguales
            strInc := S;
            blnComp := false;
            Break;
          end;
          1..MaxInt:                            // es mayor
          begin
            blnMay := true;
            strInc := S;
            blnComp := false;
            Break;
          end;
          else                                   // es menor
            blnComp := true;
        end;
      until (intPos = 0);                        // mientras haya algun valor mas

      if blnComp then                           // si no tuvo exito ninguna comparacion
      begin
        if (i > 0) then                         // controla que sea "mes" o menor (dia, hora,...)
        begin
          if _Suma(strCom, i - 1) then          // le suma 1 al anterior (año en el caso de mes, etc)
            Result := f_CronItems(Adv, strCom) else    // hace la comparacion de nuevo
            Result := CRON_EMPTY;
        end else
          Result := CRON_EMPTY;                 // si falla devuelve blanco
        Break;
      end;

      if (strInc = '') then                      // construye el resultado
        Move(Adv[i][1], Result[CronPos[i]], CronLen[i]) else
        Move(strInc[1], Result[CronPos[i]], CronLen[i]);
    end;
end;

function f_RangeExpand(const strRange: String): String;
var
  i, j, intLast: Integer;
  blnIni, blnRng: Boolean;
  S, strNum: string;
begin
  blnIni := true;
  blnRng := false;

  intLast := 0;
  strNum := '';
  Result := '';

  if (strRange[Length(strRange)] <> ',') then
    S := strRange + ',' else
    S := strRange;

  for i := 0 to Length(S) do
  begin
    case S[i] of
      '0'..'9':
        if blnIni then strNum := strNum + S[i];
      '-':
      begin
        Result := Result + strNum + ',';
        intLast := StrToInt(strNum);
        blnRng := true;
        strNum := '';
      end;
      ',':
      begin
        if blnRng then
        begin
          blnRng := false;
          for j := intLast + 1 to StrToInt(strNum) - 1 do
            Result := Result + Int.ToStr(j) + ',';
        end;

        Result := Result + strNum + ',';
        blnIni := true;
        strNum := '';
      end;
    end;
  end;

  if (Result = '') then
    Result := strRange else
    Delete(Result, Length(Result), 1);
end;

end.
