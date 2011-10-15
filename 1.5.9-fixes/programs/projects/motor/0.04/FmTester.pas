unit FmTester;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, SilSiMotor, SilSiHolors, StdCtrls, Sil;

type
  TEntityCustom = class;

  TForm1 = class(TForm)
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    FM1: IEntity;
    FFreq, FTime1, FTime2: LargeInt;
    FModel: IModel;
  public
    { Public declarations }
  published
    tiempo: ITagFloat;
    dtiempo: ITagFloat;
    Entity1: TEntityCustom;
  end;

  TEntityCustom = class(TEntityObject)
    masa: ITagFloat;
    posicion: ITagHolor;
    velocidad: ITagHolor;
    fuerza: ITagHolor;
    aceleracion: ITagHolor;
    procedure Movimiento(const Model: TForm1; const Rule: IRule; const Entity: IEntity);
  end;

var
  Form1: TForm1;

implementation

uses
  TypInfo;

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  QueryPerformanceFrequency(FFreq);
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  Layers, Objects: IEntity;
begin
  with FModel do
  begin
    Tags.Add('tiempo', 0.0, Self);
    Tags.Add('dtiempo', 0.0, Self);
  end;

  Layers := FModel.Entities.Add('layers', ekGroup);
  Objects := FModel.Entities.Add('objects', ekLayer, Layers);
  FModel.Entities.Add('floors', ekLayer, Layers);
  FModel.Entities.Add('walls', ekLayer, Layers);

  with FModel.Entities.Add('Entity1', ekObject, Objects, Self) do
  begin
    Tags.Add('masa', 34.0);
    Tags.Add('posicion', Vector.Create([0, 0, 0, 1]));
    Tags.Add('velocidad',Vector.Create([0, 0, 0, 1]));
    Tags.Add('fuerza', Vector.Create([0, 0, 0, 1]));
    Tags.Add('aceleracion', tkHolor);
    Rules.Add('Movimiento', rkMethod);
  end;
end;

{ TEntityCustom }

procedure TEntityCustom.Movimiento(const Model: TForm1; const Rule: IRule; const Entity: IEntity);
begin
  Mul(aceleracion.Value^.Limits,  fuerza.Value^.First,        1 / masa.Value^); // A := F / M
  Mul(velocidad.Value^.Limits,    aceleracion.Value^.First,   Model.tiempo.Value^);
  Mul(posicion.Value^.Limits,     velocidad.Value^.First,     Model.tiempo.Value^ * Model.tiempo.Value^);
end;

end.
