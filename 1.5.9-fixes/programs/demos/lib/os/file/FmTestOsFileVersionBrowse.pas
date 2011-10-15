unit FmTestOsFileVersionBrowse;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, ExtCtrls,

  Sil,
  SilVCL;

type
  TFormTestFileVersion = class(TForm)
    Panel1: TPanel;
    lvFiles: TListView;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormTestFileVersion: TFormTestFileVersion;

implementation

{$R *.DFM}

procedure TFormTestFileVersion.FormCreate(Sender: TObject);
var
  Mask: string;
  List: IFileInfoList;
  Enum: IEnumerator;
  Item: IFileInfo;
  Info: IFileInfo;
  Version: IVersionInfo;
begin
  Info := OS.Process.Current.Info;
   
  Caption := Info.FullName + ' ' + OS.Version.ToStr(Info.Version.Number, 'Version #%m.%0.2n.%0.4r.%0.4b');

  with Lock do Take(AsLocker(lvFiles));

  Mask := ParamStr(1);

  if Sil.Str.IsEmpty(Mask) then
    Mask := Info.Path;

  if not Sil.Str.IsWildCard(Mask) then
    Mask := Sil.Os.FileSystem.AddSlash(Mask);

  List := OS.FileSystem.GetList(Mask, True);

  while List.Enumerate(Enum, Item) do
  begin
    if not (faDirectory in Item.Attributes) then
      with lvFiles.Items.Add do
      begin
        Caption := Item.Path;
        Subitems.Add(Item.Name);
        Subitems.Add(DateTime.ToStr(Item.Time));

        Version := Item.Version;
        if Version <> nil then
        begin
          Subitems.Add(OS.Version.ToStr(Version.Number));
          Subitems.Add(Version.Tags.Std.FileDescription);
          Subitems.Add(Version.Tags.Std.CompanyName);
        end;
      end;
  end;
end;

end.
