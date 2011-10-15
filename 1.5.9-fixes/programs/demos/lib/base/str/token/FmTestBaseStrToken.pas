unit FmTestBaseStrToken;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls,

  Sil;

type
  TFormTestToken = class(TForm)
    edValue: TEdit;
    Tokens: TListBox;
    DoEnumerate: TButton;
    edSeparator: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    DoTokenize: TButton;
    DoTokenArray: TButton;
    DoStringList: TButton;
    procedure DoEnumerateClick(Sender: TObject);
    procedure DoTokenizeClick(Sender: TObject);
    procedure DoTokenArrayClick(Sender: TObject);
    procedure DoStringListClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormTestToken: TFormTestToken;

implementation

uses SilLiStringList;

{$R *.DFM}

procedure TFormTestToken.DoEnumerateClick(Sender: TObject);
var
  I: Integer;
  S: string;
begin
  Tokens.Items.Clear;

  I := 0; // This is a enumeration context variable, so it must be initialized before use.
  while Sil.Str.Enumerate(edValue.Text, edSeparator.Text, S, I) do
    Tokens.Items.Add(S);
end;

procedure TFormTestToken.DoTokenizeClick(Sender: TObject);
var
  I: Integer;
  S: string;
begin
  Tokens.Items.Clear;

  I := 0; // This is a enumeration context variable, so it must be initialized before use.
  repeat
    S := Sil.Str.Token(edValue.Text, edSeparator.Text, I);
    // Str.Token signals the end of iteration by zeroing out I
    Tokens.Items.Add(S);
  until I = 0;
end;

procedure TFormTestToken.DoTokenArrayClick(Sender: TObject);
var
  Values: TStringArray;
  I: Integer;
begin
  Tokens.Items.Clear;

  Values := Sil.Str.TokenToArray(edValue.Text, edSeparator.Text);

  for I := Low(Values) to High(Values) do
    Tokens.Items.Add(Values[I]);
end;

procedure TFormTestToken.DoStringListClick(Sender: TObject);
var
  I: IEnumerator;
  S: string;
begin
  Tokens.Items.Clear;

  with Sil.List.StringList(edValue.Text, edSeparator.Text) do
    while Enumerate(I, S) do
      Tokens.Items.Add(S);      
end;

end.
