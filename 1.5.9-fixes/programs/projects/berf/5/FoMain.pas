unit FoMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,

  Sil,
  list, Menus, WTaskBar;

type
  TForm1 = class(TForm)
    Taskbar: TWinTaskbar;
    pmMain: TPopupMenu;
    miCerrar: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure miCerrarClick(Sender: TObject);
  private
    FBerf: IBerfList;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.FormCreate(Sender: TObject);
begin
  Taskbar.ShowIcon := true;

  FBerf := TBerfList.Create;
  FBerf.ReadFile(Sil.OS.Process.Current.Info.Path + 'berf.xml');
  FBerf.Start;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FBerf.Stop;
  FBerf := nil;
end;

procedure TForm1.miCerrarClick(Sender: TObject);
begin
  Close;
end;

end.
