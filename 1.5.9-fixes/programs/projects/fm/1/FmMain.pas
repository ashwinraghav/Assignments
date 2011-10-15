unit FmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, ShellApi,
  Dialogs, FrViewPanel, ExtCtrls;

type
  TFormMain = class(TForm)
    pnLeft: TPanel;
    pnRight: TPanel;
    spCenter: TSplitter;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FLeft: TFrameView;
    FRight: TFrameView;
  public
  end;

var
  FormMain: TFormMain;

implementation

uses
  SilLog;

{$R *.dfm}

procedure TFormMain.FormCreate(Sender: TObject);
begin
  SilLog.Logger.Initialize('$SYSTEM\SOFTWARE\SIL', SilLog.LogService, SilLog.StackFmt);
  FLeft := TFrameView.Create(nil);
  FLeft.Name := 'frLeft';
  FLeft.Parent := pnLeft;
  InsertComponent(FLeft);
  FRight := TFrameView.Create(nil);
  FRight.Name := 'frRight';
  FRight.Parent := pnRight;
  InsertComponent(FRight);
end;

procedure TFormMain.FormResize(Sender: TObject);
begin
  pnLeft.Width := (Width div 2) - spCenter.Width div 2;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  SilLog.Logger.Finalize;
end;

end.

