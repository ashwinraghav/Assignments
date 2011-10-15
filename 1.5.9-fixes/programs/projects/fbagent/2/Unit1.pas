unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Mask, CheckLst, StdCtrls, ComCtrls,

  DmMain;

type
  TForm1 = class(TForm)
    GroupBox6: TGroupBox;
    RadioButton12: TRadioButton;
    MaskEdit6: TMaskEdit;
    GroupBox7: TGroupBox;
    RadioButton13: TRadioButton;
    RadioButton14: TRadioButton;
    RadioButton15: TRadioButton;
    GroupBox8: TGroupBox;
    Label3: TLabel;
    Edit3: TEdit;
    CheckBox1: TCheckBox;
    RadioButton16: TRadioButton;
    DateTimePicker1: TDateTimePicker;
    RadioButton11: TRadioButton;
    Button2: TButton;
    procedure Button2Click(Sender: TObject);
  private
  public
    { Public declarations }
  end;

var
  Form1: TForm1;                                                    

implementation

uses Unit2;

{$R *.dfm}

procedure TForm1.Button2Click(Sender: TObject);
begin
  with TForm2.Create(Self) do
  begin
    ShowModal;
    Free;
  end;
end;

end.
