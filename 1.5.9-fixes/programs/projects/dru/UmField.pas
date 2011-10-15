unit UmField;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  button, StdCtrls,

  Sil,
  SilData;

type
  TfoField = class(TForm)
    Label1: TLabel;
    edName: TEdit;
    Label2: TLabel;
    cbType: TComboBox;
    Label3: TLabel;
    edSize: TEdit;
    btOk: TButtonEx;
    btCancel: TButtonEx;
    procedure cbTypeClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  foField: TfoField;

const
  ATypes: array[0..11] of TDataFieldType = (
    ftString, ftInteger, ftLongWord, ftByte, ftWord, ftLongWord, ftBoolean, ftFloat,
    ftCurrency, ftDateTime, ftMemo, ftGuid);

implementation

{$R *.DFM}

procedure TfoField.cbTypeClick(Sender: TObject);
begin
  edSize.Enabled := ATypes[cbType.ItemIndex] = ftString;
end;

end.
