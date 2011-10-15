unit FrViewPanel;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, Sil, FrTreePanel, FrFilePanel;

type
  TFrameView = class(TFrame)
    pnTree: TPanel;
    spMiddle: TSplitter;
    pnFiles: TPanel;
    procedure FrameResize(Sender: TObject);
  private
    FTree: TFrameTree;
    FFiles: TFrameFiles;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.dfm}

{ TFrameView }

constructor TFrameView.Create(Owner: TComponent);
begin
  inherited;
  FTree := TFrameTree.Create(Self);
  FTree.Parent := pnTree;
  FFiles := TFrameFiles.Create(Self);
  FFiles.Parent := pnFiles;
  Sil.Sink.Connect(FTree, FFiles);
end;

destructor TFrameView.Destroy;
begin
  Sil.Sink.Disconnect(FTree, FFiles);
  inherited;
end;

procedure TFrameView.FrameResize(Sender: TObject);
begin
  pnTree.Height := (Self.Height div 2) - spMiddle.Height div 2;
end;

end.
