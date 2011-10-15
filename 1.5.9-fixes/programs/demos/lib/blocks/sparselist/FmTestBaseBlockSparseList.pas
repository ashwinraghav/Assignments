unit FmTestBaseBlockSparseList;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
  Sil, SilClasses, SilVcl, ExtCtrls;

type
  TFormTestSparseList = class(TForm)
    Panel1: TPanel;
    TR: TListBox;
    Panel2: TPanel;
    LB: TListBox;
    Panel3: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Velocidad: TButton;
    edElems: TEdit;
    edIterac: TEdit;
    chkPerturb: TCheckBox;
    Operaciones: TButton;
    chkOpExchange: TCheckBox;
    chkOpExtract: TCheckBox;
    chkOpMove: TCheckBox;
    Splitter1: TSplitter;
    Random: TButton;
    procedure VelocidadClick(Sender: TObject);
    procedure OperacionesClick(Sender: TObject);
    procedure RandomClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FList: IPointerList;
    FCounter: IPerformanceCounter;
  private
    function SpeedTest( Random: boolean; Elems, Iter: integer ): Single;
    procedure OpTest(Elems: integer);
    procedure ShowList( const AList: IPointerList; AsRandom: boolean = false );
    procedure ShowTitle(const AText: string);
    procedure OpRandom(Elems: integer);
  end;

var
  FormTestSparseList: TFormTestSparseList;

implementation

{$R *.DFM}

procedure TFormTestSparseList.FormCreate(Sender: TObject);
begin
  FCounter := Sil.Os.Performance.Create();
end;

procedure TFormTestSparseList.VelocidadClick(Sender: TObject);
var
  v1, v2: single;
  elems, iter: integer;
begin
  LB.Items.Clear;
  
  elems := StrToIntDef( edElems.Text, 0 );
  iter := StrToIntDef( edIterac.Text, 0 );

  v1 := SpeedTest( false, elems, iter );
  v2 := SpeedTest( true, elems, iter );

  TR.Items.Add( 'Elems=' + IntToStr( elems ) + ' Iter=' + IntToStr( iter ) + ' Relacion=' + FloatToStr( v2 / v1 ) );
end;

procedure TFormTestSparseList.OperacionesClick(Sender: TObject);
begin
  OpTest( StrToIntDef( edElems.Text, 0 ) );
end;

procedure TFormTestSparseList.RandomClick(Sender: TObject);
begin
  OpRandom( StrToIntDef( edElems.Text, 0 ) );
end;

procedure TFormTestSparseList.OpRandom( Elems: integer );
var
  i1: integer;
begin
  LB.Items.Clear;
  LB.Items.Add( 'Prueba de operaciones random con TRandomList' );

  FList := Sil.List.RandomPointerList();

  // Crea los elementos
  for i1 := 1 to Elems do
    FList.Items [ ( i1 - 1 ) * 3 ] := Pointer( i1 );

  ShowList( FList );
  ShowList( FList, true );

end;

procedure TFormTestSparseList.OpTest( Elems: integer );
var
  i1: integer;
begin
  LB.Items.Clear;
  LB.Items.Add( 'Prueba de operaciones básicas con TRandomList' );

  FList := Sil.List.PointerList();

  // Crea los elementos
  for i1 := 1 to Elems do
    FList.Add( Pointer( i1 ) );
    
  ShowList( FList );

  // Intercambio de elementos
  if chkOpExchange.Checked then
  begin
    ShowTitle( 'Exchange' );
    for i1 := 1 to Elems div 2 do
      FList.Exchange( i1 - 1, Elems - i1 );
    ShowList( FList );
  end;

  // Extracción de elementos
  (*)if chkOpExtract.Checked then
  begin
    ShowTitle( 'Extract' );
    for i1 := 1 to Elems div 2 do
      LB.Items.Add( 'Extracción elemento ' + IntToStr( i1 ) + '->' +
        IntToStr( integer( FList.Extract( Pointer( i1 ) ) ) ) );
    ShowList( FList );
  end;(*)

  // Movimiento de elementos
  (*)if chkOpMove.Checked then
  begin
    ShowTitle( 'Move' );
    for i1 := 1 to Elems div 2 do
    begin
      FList.Move( i1 - 1, i1 - 1 + Elems div 2 );
      LB.Items.Add( 'Movimiento de posición ' + IntToStr( i1 - 1 ) + ' a ' + IntToStr( i1 + Elems div 2 ) );
    end;
    ShowList( FList );
  end;(*)

end;

function TFormTestSparseList.SpeedTest( Random: boolean; Elems, Iter: integer ): Single;
var
  list: IPointerList;
  i1, i2: integer;
begin
  if Random then
    LB.Items.Add( 'Prueba de TRandomList' ) else
    LB.Items.Add( 'Prueba de TBaseList' );
  if Random then
    list := Sil.List.RandomPointerList() else
    list := Sil.List.PointerList();

  FCounter.Reset();

  // Agrega elementos a la lista
  for i1 := 0 to elems - 1 do
    list.Add( Pointer( i1 + 1 ) );

  // Hace ineficiente la búsqueda
  if Random and chkPerturb.Checked then
    list.Items[ elems * 10 ] := Pointer( elems * 10 );

  // Modifica los elementos
  for i2 := 1 to iter do
    for i1 := 0 to elems - 1 do
      list.Items[ i1 ] := Pointer( i1 + 1 );

  Result := FCounter.ToMilliseconds();

  ShowList( list );

  LB.Items.Add( 'Tiempo empleado: ' + FloatToStr( Result ) + ' ms' );
end;

procedure TFormTestSparseList.ShowTitle( const AText: string );
begin
  LB.Items.Add( '' );
  LB.Items.Add( AText );
end;

procedure TFormTestSparseList.ShowList( const AList: IPointerList; AsRandom: boolean );
var
  i1, elems: integer;
begin
  if AsRandom then
    elems := IRandomPointerList(AList).UsedCount else
    elems := AList.Count;

  LB.Items.Add( '' );
  if AsRandom then
    LB.Items.Add( 'Resultado como Random: (count=' + IntToStr( elems ) + ')' ) else
    LB.Items.Add( 'Resultado: (count=' + IntToStr( elems ) + ')' );

  if ( elems > 10 ) then
  begin
    for i1 := 0 to 5 do
      if AsRandom then
        LB.Items.Add( IntToStr( integer( IRandomPointerList(AList).UsedItems[ i1 ] ) ) ) else
        LB.Items.Add( IntToStr( integer( AList.Items[ i1 ] ) ) );

    LB.Items.Add( '...' );
    for i1 := elems - 6 to elems - 1 do
      if AsRandom then
        LB.Items.Add( IntToStr( integer( IRandomPointerList(AList).UsedItems[ i1 ] ) ) ) else
        LB.Items.Add( IntToStr( integer( AList.Items[ i1 ] ) ) );
  end
  else
    for i1 := 0 to elems - 1 do
      if AsRandom then
        LB.Items.Add( IntToStr( integer( IRandomPointerList(AList).UsedItems[ i1 ] ) ) ) else
        LB.Items.Add( IntToStr( integer( AList.Items[ i1 ] ) ) );
end;

end.

