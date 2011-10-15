unit FmSteps;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DB, IBCustomDataSet, IBQuery, StdCtrls, DBCtrls, Grids, DBGrids,
  ComCtrls;

type
  TfoSteps = class(TForm)
    PageControl1: TPageControl;
    Tareas: TTabSheet;
    grTask: TDBGrid;
    gbDescription: TGroupBox;
    meDescription: TDBMemo;
    btAdd: TButton;
    btChange: TButton;
    btDelete: TButton;
    btClose: TButton;
    dsTask: TDataSource;
    quTask: TIBQuery;
  private
    { Private declarations }
  public
    procedure SetParams(TaskId: Integer);
  end;

var
  foSteps: TfoSteps;

implementation

{$R *.dfm}

{ TfoSteps }

procedure TfoSteps.SetParams(TaskId: Integer);
begin
  quTask.ParamByName('task_id').Value := TaskId;
  quTask.Open;
end;

end.
