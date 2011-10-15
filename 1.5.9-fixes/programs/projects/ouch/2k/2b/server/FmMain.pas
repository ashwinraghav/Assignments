unit FmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Menus, WTaskBar,

  UiServer,
  UmServer;

type
  TfoMain = class(TForm)
    PopupMenu: TPopupMenu;
    Task: TWinTaskbar;
    miClose: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure miCloseClick(Sender: TObject);
  private
    FServer: IServer;
  public
    { Public declarations }
  end;

var
  foMain: TfoMain;

implementation

{$R *.dfm}

procedure TfoMain.FormCreate(Sender: TObject);
begin
  Task.ShowIcon := true;
  FServer := TServer.Create;
  FServer.Start;
end;

procedure TfoMain.FormDestroy(Sender: TObject);
begin
  if Assigned(FServer) then
  begin
    FServer.Stop;
    FServer := nil;
  end;
end;

procedure TfoMain.miCloseClick(Sender: TObject);
begin
  Close;
end;

end.
