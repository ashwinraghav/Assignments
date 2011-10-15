unit FHint;

interface

uses
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
	StdCtrls, ExtCtrls;

type
	TAppHint = class;

	// Función de notificación de fin de edición en hint
	//
	TAppHintEvent = procedure ( AHint: TAppHint; AParam: Pointer = nil ) of object;

	TAppHint = class(TForm)
		Text: TMemo;
		HideTimer: TTimer;
		procedure FormDeactivate(Sender: TObject);
		procedure HideTimerTimer(Sender: TObject);
		procedure FormCreate(Sender: TObject);
		procedure FormDestroy(Sender: TObject);
		procedure FormKeyDown(Sender: TObject; var Key: Word;
			Shift: TShiftState);
	private
		fVisible: boolean;
		fEditing: boolean;
		fPosition: TPoint;
		fMousePos: TPoint;
		fHintWindow: THintWindow;
		fOnHide: TAppHintEvent;
		fOnHideParam: Pointer;
		fOnEdit: TAppHintEvent;
		fOnEditPtr: Pointer;
    procedure SetFont(const Value: TFont);
    function GetFont: TFont;
	protected
		procedure ShowEdit( AText: string; AEdit: boolean; const APosition: TPoint;
			AOnHide: TAppHintEvent; AOnHideParam: Pointer );
		procedure Accept;
	public
		procedure Show( const AText: string ); reintroduce; overload;
		procedure Show(const AText: string; AOnHide: TAppHintEvent; AOnHideParam: Pointer = nil ); reintroduce; overload;
		procedure Show( const AText: string; const APosition: TPoint; AOnHide: TAppHintEvent = nil; AOnHideParam: Pointer = nil ); reintroduce; overload;
		procedure Edit( const AText: string; AOnEdit: TAppHintEvent = nil; AParam: Pointer = nil ); overload;
		procedure Edit( const AText: string; const APosition: TPoint; AOnEdit: TAppHintEvent = nil; AParam: Pointer = nil ); overload;
		procedure HintHide;
		procedure Cancel;
	public
		property Editing: boolean read fEditing;
		property Pos: TPoint read fPosition;
    property Font: TFont read GetFont write SetFont;
    property Wnd: THintWindow read fHintWindow;
	end;

	function AppHint: TAppHint;

implementation

{$R *.DFM}

var
	_AppHint: TAppHint;



type
  TCustomHintWindow = class(Controls.THintWindow)
  protected
    procedure Paint; override; 
  end;


function AppHint: TAppHint;
begin
	if ( _AppHint = nil ) then
		_AppHint := TAppHint.Create( nil );

	result := _AppHint;
end;

{ TAppHint }

procedure TAppHint.FormCreate(Sender: TObject);
begin
	fHintWindow := TCustomHintWindow.CreateParented( Application.Handle );
end;

procedure TAppHint.FormDestroy(Sender: TObject);
begin
	FreeAndNil( fHintWindow );
end;

procedure TAppHint.Show(const AText: string);
begin
	Show( AText, Point( Mouse.CursorPos.x, Mouse.CursorPos.y + 20 ) );
end;

procedure TAppHint.Show(const AText: string; AOnHide: TAppHintEvent; AOnHideParam: Pointer );
begin
	Show( AText, Point( Mouse.CursorPos.x, Mouse.CursorPos.y + 20 ), AOnHide, AOnHideParam );
end;

procedure TAppHint.Show(const AText: string; const APosition: TPoint;
	AOnHide: TAppHintEvent; AOnHideParam: Pointer );
begin
	ShowEdit( AText, false, APosition, AOnHide, AOnHideParam );
	HideTimer.Enabled := true;
end;

procedure TAppHint.Edit(const AText: string; AOnEdit: TAppHintEvent; AParam: Pointer );
begin
	Edit( AText, Mouse.CursorPos, AOnEdit, AParam );
end;

procedure TAppHint.Edit(const AText: string; const APosition: TPoint; AOnEdit: TAppHintEvent; AParam: Pointer );
begin
	fOnEdit := AOnEdit;
	fOnEditPtr := AParam;

	HideTimer.Enabled := false;
	ShowEdit( AText, true, APosition, nil, nil );
end;

procedure TAppHint.FormDeactivate(Sender: TObject);
begin
	Accept();
end;

procedure TAppHint.HideTimerTimer(Sender: TObject);
begin
	if not fEditing and
		( ( Mouse.CursorPos.x <> fMousePos.x ) or
		( Mouse.CursorPos.y <> fMousePos.y ) ) then
	begin
		HintHide;
	end;
end;

procedure TAppHint.ShowEdit(AText: string; AEdit: boolean;
	const APosition: TPoint; AOnHide: TAppHintEvent; AOnHideParam: Pointer );
var
	i1: integer;
	s1: string;
	ts: TSize;
	ra: TRect;
begin
	fPosition := APosition;
	fMousePos := Mouse.CursorPos;

	// Reemplaza los #13 por #13 #10
	s1 := '';
	for i1 := 1 to Length( AText ) do
	begin
		if ( AText[ i1 ] = #13 ) and ( ( i1 = Length( AText ) ) or ( AText[ i1 + 1 ] <> #10 ) ) then
			s1 := s1 + #13 + #10 else
			s1 := s1 + AText[ i1 ];
	end;
	AText := s1;

	// Determina si señala el apagado del anterior
	if Assigned( fOnHide ) and
		( ( @fOnHide <> @AOnHide ) or ( fOnHideParam <> AOnHideParam ) ) then
			fOnHide( self, fOnHideParam );
	fOnHide := AOnHide;
	fOnHideParam := fOnHideParam;

	// Asigna la posición y el tamaño
	if AEdit then
	begin
		Left := fPosition.x;
		Top := fPosition.y;
		ts := Canvas.TextExtent( '1' );
		Width := 300;
		Height := 3 * ts.cy + 4;
	end
	else
	begin
		ra := fHintWindow.CalcHintRect( Screen.Width, AText, nil );
		Left := fPosition.x;
		Top := fPosition.y;
		Width := ra.Right - ra.Left;
		Height := ra.Bottom - ra.Top;
	end;

	// Asigna el texto
	Text.Clear;
	Text.Lines[ 0 ] := AText;
	Text.ReadOnly := not AEdit;

	// Determina si debe apagarlo
	if fVisible and ( fEditing <> AEdit ) then HintHide;

	if not AEdit then
	begin
		fHintWindow.Hide;
		fHintWindow.Color := Color;
    fHintWindow.Font := Self.Font;
		fHintWindow.ActivateHint( Rect( Left, Top, Left + Width, Top + Height ),
			AText );
	end
	// Muestra el hint como form si está siendo editado
	else if not fVisible then
		inherited Show();

	if AEdit then
		ShowCaret( Text.Handle ) else
		HideCaret( Text.Handle );

	fVisible := true;
	fEditing := AEdit;
end;

procedure TAppHint.Accept;
begin
	HintHide;

	if Assigned( fOnEdit ) then
	begin
		fOnEdit( self, fOnEditPtr );
		fOnEdit := nil;		// Previene rebotes
	end;
end;

procedure TAppHint.Cancel;
begin
	fOnEdit := nil;
	Text.Clear;
	HintHide;
end;

procedure TAppHint.HintHide;
begin
	HideTimer.Enabled := false;

	if Assigned( fOnHide ) then
	begin
		fOnHide( self, fOnHideParam );
		fOnHide := nil;
	end;
		
	if not fEditing then
		fHintWindow.ReleaseHandle else
		Hide;

	fVisible := false;
	fEditing := false;
end;

procedure TAppHint.FormKeyDown(Sender: TObject; var Key: Word;
	Shift: TShiftState);
begin
	if ( Key = vk_Return ) and ( ssCtrl in Shift ) then
	begin
		Accept();
	end
	else if ( Key = vk_Escape ) then
	begin
		Cancel();
	end;
end;

function TAppHint.GetFont: TFont;
begin
  Result := Text.Font;
end;

procedure TAppHint.SetFont(const Value: TFont);
begin
  Text.Font := Value;
end;

{ TCustomHintWindow }

procedure TCustomHintWindow.Paint;
begin
  Canvas.Font := Self.Font;
  inherited;
end;

initialization

finalization
	_AppHint.Free();

end.

