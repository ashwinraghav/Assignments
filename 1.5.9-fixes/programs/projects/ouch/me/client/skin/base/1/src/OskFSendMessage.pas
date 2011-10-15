unit OskFSendMessage;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Buttons;

type
  TSendMessageForm = class(TForm)
    pnHeader: TPanel;
    pnHistory: TPanel;
    pnMessage: TPanel;
    pnFooter: TPanel;
    spHistory: TSplitter;
    meMessage: TMemo;
    Label1: TLabel;
    ComboBox1: TComboBox;
    Memo2: TMemo;
    btDialogo: TSpeedButton;
    btSend: TButton;
    btClear: TButton;
    btCancel: TButton;
    procedure btDialogoClick(Sender: TObject);
    procedure btClearClick(Sender: TObject);
	private
	public
	end;

implementation

{$R *.DFM}

procedure TSendMessageForm.btDialogoClick(Sender: TObject);
begin
	pnHistory.Visible := btDialogo.Down;
	spHistory.Visible := btDialogo.Down;
end;

procedure TSendMessageForm.btClearClick(Sender: TObject);
begin
	meMessage.Clear;
	meMessage.SetFocus;
end;

end.
