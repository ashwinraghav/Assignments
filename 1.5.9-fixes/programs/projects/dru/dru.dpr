program dru;

uses
  Forms,
  UmMain in 'UmMain.pas' {foMain},
  UmField in 'UmField.pas' {foField};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TfoMain, foMain);
  Application.Run;
end.
