unit FmTester;

{$INCLUDE Defines.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, SilSiModel, SilSiMotor, SilSiHolors, StdCtrls, Sil, SilMath, SilSmTags, SilSmEntities,
  ComCtrls, ExtCtrls;

type
  TEntityCustom = class;
  
  T2DVector = record
    X: Extended;
    Y: Extended;
  end;
  
  P2DVector = ^T2DVector;
  
  TFormMotor = class(TForm)
    Reset: TButton;
    Run: TButton;
    lvGlobals: TListView;
    Stepper: TTimer;
    Painter: TPaintBox;
    Step: TButton;
    Update: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure RunClick(Sender: TObject);
    procedure PainterPaint(Sender: TObject);
    procedure StepClick(Sender: TObject);
    procedure UpdateTimer(Sender: TObject);
    procedure ResetClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    FCounter: IPerformanceCounter;
    FModel: IModel;
    FMotor: IMotor;
    FTime: Extended;
    FMapperX: ICoordsMapper;
    FMapperY: ICoordsMapper;
    procedure DoRefresh(View: TListView; const Model: IModel); overload;
    procedure DoRefresh(View: TListView; const Name: string; First, Last: PITag); overload;
    procedure DoRefresh(View: TListView; First, Last: PIEntity); overload;
    procedure DoPaintList(Canvas: TCanvas; First, Last: PIEntity); 
    procedure DoPaint(Canvas: TCanvas; Instance: TEntityCustom); 
  published
    now: TSilTagFloat;
    tiempo: TSilTagFloat;
    dtiempo: TSilTagFloat;
    iteraciones: TSilTagInteger;
    lapse: TSilTagFloat;
    refresh: TSilTagFloat;
    Sun: TEntityCustom;
    Earth: TEntityCustom;
    procedure DoNow(const Model: TFormMotor; const Rule: IRule);
    procedure DoDtiempo(const Model: TFormMotor; const Rule: IRule);
    procedure DoTiempo(const Model: TFormMotor; const Rule: IRule);
    procedure DoStep(const Model: TFormMotor; const Rule: IRule);
  end;

  TLayerGravity = class(TSilEntityLayer)
  published
    procedure GravityField(const Model: TFormMotor; const Rule: IRule; Entity: TLayerGravity);
  end;

  TEntityCustom = class(TSilEntityObject)
  published
    masa: TSilTagFloat;
    radio: TSilTagFloat;
    posicion: TSilTagHolor;
    velocidad: TSilTagHolor;
    fuerza: TSilTagHolor;
    aceleracion: TSilTagHolor;
    procedure Movimiento(const Model: TFormMotor; const Rule: IRule; const Entity: TEntityCustom);
  end;

var
  FormMotor: TFormMotor;

implementation

uses
  SilSmModel, SilSmMotor, SilStCoords, SilSiCoords;

{$R *.dfm}

procedure TFormMotor.FormCreate(Sender: TObject);
var
  Layers, Objects: IEntity;
begin
  FCounter := Sil.Os.Performance.Create();
  FMotor := TSilMotor.Create(Self);
  FModel := TSilModel.Create(Self);

  FMapperX := SilMath.Coords.NewMapper('Linear', SilMath.Coords.NewMapping());
  FMapperX.Mapping.WorldMin := - 3500000000.0;
  FMapperX.Mapping.WorldMax := + 3500000000.0;
  FMapperY := SilMath.Coords.NewMapper('Linear', SilMath.Coords.NewMapping());
  FMapperY.Mapping.WorldMin := - 3500000000.0;
  FMapperY.Mapping.WorldMax := + 3500000000.0;

  FTime := 0;
  
  with FModel do
  begin
    Tags.Add('now',         FTime, Rules.Add('DoNow'));
    Tags.Add('tiempo',      now.Value^);
    Tags.Add('dtiempo',                0.0, Rules.Add('DoDtiempo'));
    Tags.Add('iteraciones',              0, Rules.Add('DoStep'));
    Tags.Add('lapse', 0.0);
    Tags.Add('refresh', 0.0);
    Rules.Add('DoTiempo');
  end;

  Layers := FModel.Entities.Add('layers', ekGroup);
  
  Objects := FModel.Entities.Add('objects', TLayerGravity, Layers);
  Objects.Rules.Add('GravityField');

  FModel.Entities.Add('floors', ekLayer, Layers);
  FModel.Entities.Add('walls', ekLayer, Layers);

  with FModel.Entities.Add('Sun', TEntityCustom, Objects, Self) do
  begin
    Tags. Add('masa',        1.989e+30);
    Tags. Add('radio',        695000.0);
    Tags. Add('posicion',    Vector.Create([       0,     0 ]));
    Tags. Add('velocidad',   Vector.Create([       0,     0 ]));
    Tags. Add('fuerza',      Vector.Create([       0,     0 ]));
    Tags. Add('aceleracion', Vector.Create([       0,     0 ]));
    Rules.Add('Movimiento');
  end;

  with FModel.Entities.Add('Earth', TEntityCustom, Objects, Self) do
  begin
    Tags. Add('masa',         5.97e+24);
    Tags. Add('radio',        6378.14);
    Tags. Add('posicion',    Vector.Create([  0   , - 149600000.00 ]));
    Tags. Add('velocidad',   Vector.Create([  0,                 0 ]));
    Tags. Add('fuerza',      Vector.Create([  0,                 0 ]));
    Tags. Add('aceleracion', Vector.Create([  0,                 0 ]));
    Rules.Add('Movimiento');
  end;

  with FModel.Entities.Add('Jupiter', TEntityCustom, Objects, Self) do
  begin
    Tags. Add('masa',        1.900e+27);
    Tags. Add('radio',        71492.0);
    Tags. Add('posicion',    Vector.Create([   0,  +778330000.00 ]));
    Tags. Add('velocidad',   Vector.Create([   0,              0 ]));
    Tags. Add('fuerza',      Vector.Create([   0,              0 ]));
    Tags. Add('aceleracion', Vector.Create([   0,              0 ]));
    Rules.Add('Movimiento');
  end;

  (*)with FModel.Entities.Add('Ganymede', TEntityCustom, Objects, Self) do
  begin
    Tags. Add('masa',         1.48e+23);
    Tags. Add('radio',        2631.0);
    Tags. Add('posicion',    Vector.Create([ center.value.data^[0]   , center.value.data^[1] +1070000.00 ]));
    Tags. Add('velocidad',   Vector.Create([                     + 0.0,                         0 ]));
    Tags. Add('fuerza',      Vector.Create([                         0,                         0 ]));
    Tags. Add('aceleracion', Vector.Create([                         0,                         0 ]));
    Rules.Add('Movimiento');
  end;(*)

  Update.Enabled := True;
end;

procedure TFormMotor.FormDestroy(Sender: TObject);
begin
  Update.Enabled := False;
  FModel := nil;
  FMotor := nil;
  FCounter := nil;
end;

procedure TFormMotor.DoRefresh(View: TListView; const Model: IModel);
begin
  View.Items.BeginUpdate;
  try
    with Model do
    begin
      with Tags do DoRefresh(View, Name, First, Last);
      with Entities do DoRefresh(View, First, Last);
    end;
  finally
    View.Items.EndUpdate;
  end;
end;

procedure TFormMotor.DoRefresh(View: TListView; const Name: string; First, Last: PITag);
var
  Item: TListItem;
begin
  while First <> Last do
  begin
    if not Assigned(First.Data) then
    begin
      Item := View.Items.Add();
      Item.Caption := Name + '.' + First.Name;
      Item.SubItems.Add('');
      First.Data := Item;
    end else
      Item := First.Data;

    Item.SubItems[0] := First.Text;
    Inc(First);
  end;
end;

procedure TFormMotor.DoRefresh(View: TListView; First, Last: PIEntity);
begin
  while First <> Last do
  begin
    DoRefresh(View, First.Name, First.Tags.First, First.Tags.Last);
    Inc(First);
  end;
end;

procedure TFormMotor.DoPaintList(Canvas: TCanvas; First, Last: PIEntity);
begin
  while First <> Last do
  begin
    if First.Kind = ekObject then DoPaint(Canvas, First.Instance);
    Inc(First);
  end;
end;

procedure TFormMotor.DoPaint(Canvas: TCanvas; Instance: TEntityCustom);
var
  Vec: T2DVector;
  Vel: P2DVector;
  Ace: P2DVector;
  Radio: Extended;
  Rect: TRect;
begin
  Vec.X := FMapperX.WorldToClient(0);
  Vec.Y := FMapperY.WorldToClient(0);
  
  Radio := 3;

  Canvas.Pen.Color := clWhite;
  Rect.Left   := Round(Vec.X - Radio);
  Rect.Top    := Round(Vec.Y - Radio);
  Rect.Right  := Round(Vec.X + Radio);
  Rect.Bottom := Round(Vec.Y + Radio);
  Canvas.Ellipse(Rect);

  Vec := Pointer(Instance.posicion.Value.Data);
  Vel := Pointer(Instance.velocidad.Value.Data);
  Ace := Pointer(Instance.aceleracion.Value.Data);
  Radio := Instance.radio.Value^;

  Canvas.Pen.Color := clWhite;
  Rect.Left   := Round(Vec.X - Radio);
  Rect.Top    := Round(Vec.Y - Radio);
  Rect.Right  := Round(Vec.X + Radio);
  Rect.Bottom := Round(Vec.Y + Radio);
  Canvas.Ellipse(Rect);

  Canvas.Pen.Color := clGreen;
  Canvas.MoveTo(Round(Vec.X), Round(Vec.Y));
  Canvas.LineTo(Round(Vec.X) + Round(Vel.X), Round(Vec.Y) + Round(Vel.Y));

  Canvas.Pen.Color := clLime;
  Canvas.MoveTo(Round(Vec.X), Round(Vec.Y));
  Canvas.LineTo(Round(Vec.X) + Round(Ace.X), Round(Vec.Y) + Round(Ace.Y));
end;

procedure TFormMotor.DoNow(const Model: TFormMotor; const Rule: IRule);
begin
  Now.Value^ := FTime;
end;

procedure TFormMotor.DoDtiempo(const Model: TFormMotor; const Rule: IRule);
begin
  dtiempo.Value^ := Now.Value^ - tiempo.Value^;
end;

procedure TFormMotor.DoTiempo(const Model: TFormMotor; const Rule: IRule);
begin
  tiempo.Value^ := Now.Value^;
end;

procedure TFormMotor.DoStep(const Model: TFormMotor; const Rule: IRule);
begin
  Inc(iteraciones.Value^);
end;

procedure TFormMotor.ResetClick(Sender: TObject);
begin
  FTime := 0;
end;

procedure TFormMotor.UpdateTimer(Sender: TObject);
begin
  FCounter.Reset();
  FMotor.Step(FModel);
  lapse.Value^ := FCounter.ToSeconds();
  DoRefresh(lvGlobals, FModel);
  refresh.Value^ := FCounter.ToSeconds();
  Painter.Invalidate;
end;

procedure TFormMotor.RunClick(Sender: TObject);
begin
  Stepper.Enabled := not Stepper.Enabled;
end;

procedure TFormMotor.StepClick(Sender: TObject);
begin
  FTime := FTime + 0.1;
end;

procedure TFormMotor.PainterPaint(Sender: TObject);
begin
  with FModel do DoPaintList(Painter.Canvas, Entities.First, Entities.Last);
end;

{ TEntityCustom }

procedure TEntityCustom.Movimiento(const Model: TFormMotor; const Rule: IRule; const Entity: TEntityCustom);
begin
  with Entity do
  begin
  //Ai = Fi / m;
    Mul(aceleracion.Value.Limits, fuerza.Value.First, 1 / masa.Value^);
  //Vi += Ai * dT
    MulAdd(velocidad.Value.Limits, aceleracion.Value.First, Model.dtiempo.Value^);
  //Xi += Vi * dT
    MulAdd(posicion.Value.Limits, velocidad.Value.First, Model.dtiempo.Value^);
  end;
end;

{ TLayerGravity }

procedure TLayerGravity.GravityField(const Model: TFormMotor; const Rule: IRule; Entity: TLayerGravity);
var
  Top, Bottom: PIEntity;
  Temp: IHolor;


  procedure DoZero(Mass: TEntityCustom);
  begin
    Mass.fuerza.Value.Data^[0] := 0;
    Mass.fuerza.Value.Data^[1] := 0;
  end;

  procedure DoInteract(Mass1, Mass2: TEntityCustom);
  var
    dist: Extended;
  begin
    Sub(Temp.Limits, Mass1.posicion.Value.First, Mass2.posicion.Value.First);
    dist := Norm(Temp.Limits);
    Mul(Temp.Limits, 1 / Sqr(dist));
    MulAdd(Mass1.fuerza.Value.Limits, Temp.First, - Mass2.masa.Value^ / dist);
  end;

begin
  Temp := Vector.Create([0, 0]);
  with Entity.Children do
  begin
    Top := First;
    while Top <> Last do
    begin
      DoZero(Top.Instance);
      Bottom := Last;
      while Bottom <> First do
      begin
        Dec(Bottom);
        if Top <> Bottom then DoInteract(Top.Instance, Bottom.Instance);
      end;
      Inc(Top);
    end;
  end;
end;

procedure TFormMotor.FormResize(Sender: TObject);
var
  Aspect: Extended;
begin
  FMapperX.Mapping.ClientMin := 0;
  FMapperX.Mapping.ClientMax := Painter.ClientWidth;
  FMapperY.Mapping.ClientMin := 0;
  FMapperY.Mapping.ClientMax := Painter.ClientHeight;


  Aspect := Extended(Painter.ClientWidth) / Extended(Painter.ClientHeight);   
  FMapperY.Mapping.WorldMin := 0;
  FMapperY.Mapping.WorldMax := Round(Extended(Painter.ClientHeight) * FMapperX.Mapping.WorldSize / ));
end;

end.
