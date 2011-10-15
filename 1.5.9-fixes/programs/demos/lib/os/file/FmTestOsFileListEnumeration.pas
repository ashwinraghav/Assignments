unit FmTestOsFileListEnumeration;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls,
  Sil;

type
  TFormTestFileListEnumeration = class(TForm)
    ListBox1: TListBox;
    Panel1: TPanel;
    Button1: TButton;
    Edit1: TEdit;
    CheckBox1: TCheckBox;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormTestFileListEnumeration: TFormTestFileListEnumeration;

implementation

{$R *.dfm}

procedure TFormTestFileListEnumeration.Button1Click(Sender: TObject);
var
  list: IFileInfoList;
  enum: IEnumerator;
  item: IFileInfo;
begin
  ListBox1.Items.Clear;
  list := Sil.OS.FileSystem.GetList(Sil.OS.Environment.Expand(edit1.text), CheckBox1.Checked);

  while list.Enumerate(enum, item) do
    ListBox1.Items.Add(item.FullName);
end;

end.
