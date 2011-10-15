unit Unit2;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, CheckLst, ComCtrls, Mask;

type
  TfoSchedule = class(TForm)
    Notebook1: TNotebook;
    GroupBox9: TGroupBox;
    Label4: TLabel;
    MaskEdit3: TMaskEdit;
    DateTimePicker3: TDateTimePicker;
    GroupBox15: TGroupBox;
    Label13: TLabel;
    Edit9: TEdit;
    ComboBox7: TComboBox;
    GroupBox10: TGroupBox;
    Label5: TLabel;
    Label6: TLabel;
    Label10: TLabel;
    Edit4: TEdit;
    MaskEdit5: TMaskEdit;
    GroupBox11: TGroupBox;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Edit5: TEdit;
    CheckListBox3: TCheckListBox;
    GroupBox12: TGroupBox;
    RadioButton19: TRadioButton;
    RadioButton20: TRadioButton;
    ComboBox5: TComboBox;
    ComboBox6: TComboBox;
    Edit7: TEdit;
    GroupBox13: TGroupBox;
    Label11: TLabel;
    RadioButton21: TRadioButton;
    Edit6: TEdit;
    RadioButton22: TRadioButton;
    CheckListBox5: TCheckListBox;
    GroupBox14: TGroupBox;
    Label12: TLabel;
    DateTimePicker4: TDateTimePicker;
    MaskEdit7: TMaskEdit;
    RadioButton23: TRadioButton;
    MaskEdit8: TMaskEdit;
    DateTimePicker5: TDateTimePicker;
    RadioButton24: TRadioButton;
    RadioButton25: TRadioButton;
    Edit8: TEdit;
    RadioGroup1: TRadioGroup;
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    procedure RadioGroup1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  foSchedule: TfoSchedule;

implementation

{$R *.dfm}

procedure TfoSchedule.RadioGroup1Click(Sender: TObject);
begin
  Notebook1.PageIndex := RadioGroup1.ItemIndex;
end;

end.
