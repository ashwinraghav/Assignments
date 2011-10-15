unit FmTestBaseStrScan;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,

  Sil;

type
  TFormTestScan1 = class(TForm)
    Scan1: TButton;
    Scan2: TButton;
    Scan3: TButton;
    Scan4: TButton;
    Input1: TEdit;
    Result1: TEdit;
    Format1: TEdit;
    Input2: TEdit;
    Result2: TEdit;
    Format2: TEdit;
    Input3: TEdit;
    Result3: TEdit;
    Format3: TEdit;
    Input4: TEdit;
    Result4: TEdit;
    Format4: TEdit;                                             
    Scan5: TButton;
    Input5: TEdit;
    Result5: TEdit;
    Format5: TEdit;
    Scan6: TButton;
    Input6: TEdit;
    Result6: TEdit;
    Format6: TEdit;
    Scan7: TButton;
    Input7: TEdit;
    Result7: TEdit;
    Format7: TEdit;
    procedure Scan1Click(Sender: TObject);
    procedure Scan2Click(Sender: TObject);
    procedure Scan3Click(Sender: TObject);
    procedure Scan4Click(Sender: TObject);
    procedure Scan5Click(Sender: TObject);
    procedure Scan6Click(Sender: TObject);
    procedure Scan7Click(Sender: TObject);
  end;

var
  FormTestScan1: TFormTestScan1;

implementation

uses SilBtStr;

{$R *.dfm}

procedure TFormTestScan1.Scan1Click(Sender: TObject);
var
  a: String;
  d, m, y: Integer;
begin
  Sil.Str.Scan(Input1.Text, Format1.Text, [@y, @m, @d, @a]);
  Result1.Text := Sil.Str.Format('y = %d; m = %d; d = %d; a = %s', [y, m, d, a]);
end;

procedure TFormTestScan1.Scan2Click(Sender: TObject);
var
  d, m, y: Integer;
begin
  Sil.Str.Scan(Input2.Text, Format2.Text, [@y, @m, @d]);
  Result2.Text := Sil.Str.Format('y = %d; m = %d; d = %d', [y, m, d]);
end;

procedure TFormTestScan1.Scan3Click(Sender: TObject);
var
  d, m, y: Integer;
begin
  Sil.Str.Scan(Input3.Text, Format3.Text, [@y, @m, @d]);
  Result3.Text := Sil.Str.Format('y = %d; m = %d; d = %d', [y, m, d]);
end;

procedure TFormTestScan1.Scan4Click(Sender: TObject);
var
  a, b: String;
  d, m, y: Integer;
begin
  Sil.Str.Scan(Input4.Text, Format4.Text, [@y, @m, @d, @a, @b]);
  Result4.Text := Sil.Str.Format('y = %d; m = %d; d = %d; a = %s; b = %s', [y, m, d, a, b]);
end;

procedure TFormTestScan1.Scan5Click(Sender: TObject);
var
  Units: string;
  Value: Double;
begin
  Str.Scan(Input5.Text, Format5.Text, [@Value, @Units]);
  Result5.Text := Sil.Str.Format('value = %f units = %s', [Value, Units]);;
end;

procedure TFormTestScan1.Scan6Click(Sender: TObject);
var
  s1, s2, s3: String;
begin
  Str.Scan(Input6.Text, Format6.Text, [@s1, @s2, @s3]);
  Result6.Text := Sil.Str.Format('s1 = %s, s2 = %s, s3 = %s', [s1, s2, s3]);;
end;

procedure TFormTestScan1.Scan7Click(Sender: TObject);
type
  RDate = record
    Year: Integer;
    Month: Integer;
    Day: Integer;
  end;
var
  dr: RDate;
begin
  Str.Scan(Input7.Text, Format7.Text, [@dr.Year, @dr.Month, @dr.Day]);
  Result7.Text := Sil.Str.Format('dr.year = %d, dr.month = %d, dr.day = %d', [dr.Year, dr.Month, dr.Day]);
end;

end.
