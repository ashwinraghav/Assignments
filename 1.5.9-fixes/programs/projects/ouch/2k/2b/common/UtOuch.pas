unit UtOuch;

interface

uses
  Sil,
  UiOuch;

type
  Ouch = class(Tool)
    class function Engine(const Application: IOuchApplication): IOuchEngine;
    class function StateToStr(Value: TOuchEngineState): string;
    class function StageToStr(Value: TOuchUpdateStage): string;
  end;

implementation

uses
  UmOuchEngine, UdOuch;

const
  MStateStr: array[TOuchEngineState] of PResStringRec =
    (
      @SEngineUnknown,
      @SEngineDisconnected,
      @SEngineConnected,
      @SEngineLoggedOn
    );

const
  MStageStr: array[TOuchUpdateStage] of PResStringRec =
    (
      @SUpdateStageStart,
      @SUpdateStageDownload,
      @SUpdateStageChange,
      @SUpdateStageEnd,
      @SUpdateStageError    
    );

{ Ouch }

class function Ouch.Engine(const Application: IOuchApplication): IOuchEngine;
begin
  Result := TOuchEngine.Create(Application);
end;

class function Ouch.StateToStr(Value: TOuchEngineState): string;
begin
  Result := LoadResString(MStateStr[Value]);
end;

class function Ouch.StageToStr(Value: TOuchUpdateStage): string;
begin
  Result := LoadResString(MStageStr[Value]);
end;

end.
