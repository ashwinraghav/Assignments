unit FmTestHolor2;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, StdCtrls,
  Dialogs, Sil, SilSiHolors;

type
  TFormHolors = class(TForm)
    btAsg0: TButton;
    Button1: TButton;
    btAsg1: TButton;
    btAsg2: TButton;
    procedure btAsg0Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SystemMove(Sender: TObject);
    procedure btAsg1Click(Sender: TObject);
    procedure btAsg2Click(Sender: TObject);
  private
    Vector0, Vector1, Vector2, Vector3: IHolor;
    FFreq, FTime1, FTime2: LargeInt;
  end;

var
  FormHolors: TFormHolors;

implementation

{$R *.dfm}

const
  CIterations = 10000;

procedure TFormHolors.FormCreate(Sender: TObject);
var
  Pedorro: array of Double;
begin
  QueryPerformanceFrequency(FFreq);
  SetLength(Pedorro, 10000);
  Vector0 := Vector.Create(Pedorro);
  Vector1 := Vector.Create(Pedorro);
  Vector2 := Vector.Create(Pedorro);
  Vector3 := Vector.Create(Vector1);
end;

procedure TFormHolors.btAsg0Click(Sender: TObject);
var
  I: Integer;
  D: Double;
  Fr, Lr, F2, F1, F0: PMerate;
begin
  Fr := Vector3.First;
  Lr := Vector3.Last;
  F0 := Vector0.First;
  F1 := Vector1.First;
  F2 := Vector2.First;
  D := 0;
  QueryPerformanceCounter(FTime1);
  for I := 0 to CIterations do
  begin
    //Asg1(Fr, Lr, F0);
    Asg0(Fr, Lr, F0);
    //Sum(Fr, Lr, F1);
    //Mul(Fr, Lr, 1.0);
    //D := Mul(Fr, Lr, F2) + D;
  end;
  QueryPerformanceCounter(FTime2);
  Caption := Sil.Float.ToStr((FTime2 - FTime1) / FFreq);
end;

procedure TFormHolors.SystemMove(Sender: TObject);
var
  I: Integer;
  D: Double;
  Fr, Lr, F2, F1, F0: PMerate;
begin
  Fr := Vector3.First;
  Lr := Vector3.Last;
  F0 := Vector0.First;
  F1 := Vector1.First;
  F2 := Vector2.First;
  D := 0;
  QueryPerformanceCounter(FTime1);
  for I := 0 to CIterations do
  begin
    //Asg1(Fr, Lr, F0);
    AsgM(Fr, Lr, F0);
    //Sum(Fr, Lr, F1);
    //Mul(Fr, Lr, 1.0);
    //D := Mul(Fr, Lr, F2) + D;
  end;
  QueryPerformanceCounter(FTime2);
  Caption := Sil.Float.ToStr((FTime2 - FTime1) / FFreq);
end;

procedure TFormHolors.btAsg1Click(Sender: TObject);
var
  I: Integer;
  D: Double;
  Fr, Lr, F2, F1, F0: PMerate;
begin
  Fr := Vector3.First;
  Lr := Vector3.Last;
  F0 := Vector0.First;
  F1 := Vector1.First;
  F2 := Vector2.First;
  D := 0;
  QueryPerformanceCounter(FTime1);
  for I := 0 to CIterations do
  begin
    //Asg1(Fr, Lr, F0);
    Asg1(Fr, Lr, F0);
    //Sum(Fr, Lr, F1);
    //Mul(Fr, Lr, 1.0);
    //D := Mul(Fr, Lr, F2) + D;
  end;
  QueryPerformanceCounter(FTime2);
  Caption := Sil.Float.ToStr((FTime2 - FTime1) / FFreq);
end;

procedure TFormHolors.btAsg2Click(Sender: TObject);
var
  I: Integer;
  D: Double;
  Fr, Lr, F2, F1, F0: PMerate;
begin
  Fr := Vector3.First;
  Lr := Vector3.Last;
  F0 := Vector0.First;
  F1 := Vector1.First;
  F2 := Vector2.First;
  D := 0;
  QueryPerformanceCounter(FTime1);
  for I := 0 to CIterations do
  begin
    //Asg1(Fr, Lr, F0);
    Asg2(Fr, Lr, F0);
    //Sum(Fr, Lr, F1);
    //Mul(Fr, Lr, 1.0);
    //D := Mul(Fr, Lr, F2) + D;
  end;
  QueryPerformanceCounter(FTime2);
  Caption := Sil.Float.ToStr((FTime2 - FTime1) / FFreq);
end;

end.
