program TestBaseBlockSparseList;

uses
  Forms,
  FmTestBaseBlockSparseList in 'FmTestBaseBlockSparseList.pas' {FormTestSparseList};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormTestSparseList, FormTestSparseList);
  Application.Run;
end.
