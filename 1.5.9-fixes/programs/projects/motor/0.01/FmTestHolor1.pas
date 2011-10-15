unit FmTestHolor1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, StdCtrls,
  Dialogs, Sil, SilSeHolors;

type
  TFormHolors = class(TForm)
    Generico: TButton;
    Vectores: TButton;
    procedure VectoresClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FFreq, FTime1, FTime2: LargeInt;
  end;

var
  FormHolors: TFormHolors;

implementation

{$R *.dfm}

procedure TFormHolors.VectoresClick(Sender: TObject);
var
  I: Integer;
  Vector1, Vector3: HolorType;
begin
  QueryPerformanceCounter(FTime1);
  Vector1 := Vector.Create([1, 2, 3]);
  Vector3 := Holor.Create(Vector1);
  for I := 0 to 1000000 do
    Vector.Sum(Vector3, Vector1);
  QueryPerformanceCounter(FTime2);
  Caption := Sil.Float.ToStr((FTime2 - FTime1) / FFreq);
end;

procedure TFormHolors.FormCreate(Sender: TObject);
begin
  QueryPerformanceFrequency(FFreq);
end;

end.
