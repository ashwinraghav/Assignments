unit FmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,

  Sil;

type
  TfoMain = class(TForm)
    Label1: TLabel;
    laName: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    edDate: TEdit;
    edTime: TEdit;
    btOk: TButton;
    btCancel: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btCancelClick(Sender: TObject);
    procedure btOkClick(Sender: TObject);
  private
    FFile: IFile;
  end;

var
  foMain: TfoMain;

implementation

{$R *.dfm}

procedure TfoMain.FormCreate(Sender: TObject);
begin
  FFile := Sil.OS.FileSystem.OpenFile(ParamStr(1), fmAccessReadWrite, fmShareReadWrite);
  laName.Caption := FFile.Info.Name;
  edDate.Text := Date.ToStr(FFile.Info.Time);
  edTime.Text := Time.ToStr(FFile.Info.Time);
end;

procedure TfoMain.btCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TfoMain.btOkClick(Sender: TObject);
begin
  FFile.Info.Time := Date.FromStr(edDate.Text) + Time.FromStr(edTime.Text);
  Close;
end;

end.
