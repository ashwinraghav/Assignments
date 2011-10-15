unit FmTestOsProcessList;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,

  Sil, ExtCtrls;

type
  TFormTestOsProcessList = class(TForm)
    Memo: TMemo;
    Panel1: TPanel;
    btRefresh: TButton;
    edProcessName: TEdit;
    procedure btRefreshClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormTestOsProcessList: TFormTestOsProcessList;

implementation

{$R *.dfm}

procedure TFormTestOsProcessList.btRefreshClick(Sender: TObject);
var
  List: IProcessList;
  Item: IProcess;
  Enum: IEnumerator;
begin
  List := Sil.OS.Process.GetList(edProcessName.Text);

  memo.lines.clear;
  while List.Enumerate(Enum, Item) do
    memo.lines.add(Str.Format('pid: %d file: %s', [Item.PID, Item.Info.FullName]));
end;

end.
