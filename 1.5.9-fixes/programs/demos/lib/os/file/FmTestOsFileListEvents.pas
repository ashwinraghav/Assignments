unit FmTestOsFileListEvents;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls,
  SIL, ExtCtrls, ComCtrls;

type
  TFormTestFileListEvents = class(TForm, IListEvents)
    Panel1: TPanel;
    btStartSearch: TButton;
    edFileMask: TEdit;
    ckRecursive: TCheckBox;
    lvFiles: TListView;
    procedure btStartSearchClick(Sender: TObject);
  private
    procedure OnListAdd(const AList: IList; Index: Integer);
    procedure OnListInsert(const AList: IList; Index: Integer);
    procedure OnListDelete(const AList: IList; Index: Integer);
  public
    { Public declarations }
  end;

var
  FormTestFileListEvents: TFormTestFileListEvents;

implementation

{$R *.DFM}

procedure TFormTestFileListEvents.btStartSearchClick(Sender: TObject);
var
  l: IFileInfoList;
begin
  l := Sil.Os.FileSystem.GetList(Sil.Os.Environment.Expand(edFileMask.Text), ckRecursive.Checked, Self);
end;

procedure TFormTestFileListEvents.OnListAdd(const AList: IList; Index: Integer);
var
  Info: IFileInfo;
  List: IFileInfoList;
begin
  List := AList as IFileInfoList;
  Info := List[Index];
  with lvFiles.Items.Add do
  begin
    Caption := Info.Name;
    Subitems.Add(Info.Path);
    Subitems.Add(Sil.DateTime.ToStr(Info.Time));
    Subitems.Add(Sil.Int.ToStr(Info.Size));
  end;
end;

procedure TFormTestFileListEvents.OnListInsert(const AList: IList; Index: Integer);
begin
  OnListAdd(AList, Index);
end;

procedure TFormTestFileListEvents.OnListDelete(const AList: IList; Index: Integer);
begin
end;

end.
