unit FmMain;

interface

{$include Defines.inc}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DB, IBCustomDataSet, IBQuery, Grids, DBGrids, ComCtrls,
  StdCtrls, DBCtrls, IBDatabase,

  DmAgent;

type
  TfoMain = class(TForm)
    PageControl1: TPageControl;
    Tareas: TTabSheet;
    grTask: TDBGrid;
    dsTask: TDataSource;
    quTask: TIBQuery;
    gbDescription: TGroupBox;
    meDescription: TDBMemo;
    btAdd: TButton;
    btChange: TButton;
    btDelete: TButton;
    btClose: TButton;
    taTask: TIBTransaction;
    procedure FormCreate(Sender: TObject);
    procedure btAddClick(Sender: TObject);
    procedure btChangeClick(Sender: TObject);
    procedure btCloseClick(Sender: TObject);
    procedure btDeleteClick(Sender: TObject);
  end;

var
  foMain: TfoMain;

implementation

uses
  FmTask;

{$R *.dfm}

procedure TfoMain.FormCreate(Sender: TObject);
begin
  daMain := TdaMain.Create(Self);
  quTask.Open;
end;

procedure TfoMain.btAddClick(Sender: TObject);
begin
  with TfoTask.Create(Self) do
  begin
    if ShowModal = mrOk then
      daMain.Refresh(quTask);

    Free;
  end;
end;

procedure TfoMain.btChangeClick(Sender: TObject);
begin
  with TfoTask.Create(Self) do
  begin
    SetParams(
      quTask.FieldByName('task_id').AsInteger,
      quTask.FieldByName('name').AsString,
      quTask.FieldByName('enabled').AsInteger = 1,
      quTask.FieldByName('category_id').AsInteger,
      quTask.FieldByName('description').AsString);

    if ShowModal = mrOk then
      daMain.Refresh(quTask);
      
    Free;
  end;
end;

procedure TfoMain.btCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfoMain.btDeleteClick(Sender: TObject);
begin
  if Application.MessageBox('Confirma borrar?', PChar(Caption), MB_ICONQUESTION or MB_OKCANCEL) = ID_OK then
  begin
    daMain.TaskDelete(quTask.FieldByName('task_id').AsInteger);
    daMain.Refresh(quTask);
  end;
end;

end.
