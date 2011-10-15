unit FmTestValues;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TFormTestValues = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    FA: PDouble;
    FB: PDouble;
    FC: PDouble;
    FD: PDouble;
    FI: PInteger;
    FX: Pointer;
    FV: PVariant;
  public
    { Public declarations }
  end;

var
  FormTestValues: TFormTestValues;

implementation

uses
  SilSfValues;

{$R *.dfm}

procedure TFormTestValues.Button1Click(Sender: TObject);
begin
  FA := SilSfValues.New(SizeOf(Double));
  FB := SilSfValues.New(SizeOf(Double));
  FV := SilSfValues.New(SizeOf(Variant));
  FI := SilSfValues.New(SizeOf(Integer));
  FX := SilSfValues.New(0);
  FC := SilSfValues.New(FA);
  FD := SilSfValues.New(FC);


  SilSfValues.Invalidate(FV);
  SilSfValues.Invalidate(FC);

  SilSfValues.Release(FV);
  SilSfValues.Release(FC);
  
  SilSfValues.Release(FA);
  FA := SilSfValues.New(SizeOf(Double));

  SilSfValues.Release(FI);
  FV := SilSfValues.New(SizeOf(Variant));
  
  SilSfValues.Release(FB);
  SilSfValues.Release(FX);
  SilSfValues.Release(FA);
  SilSfValues.Release(FD);
  SilSfValues.Release(FV);
end;

end.
