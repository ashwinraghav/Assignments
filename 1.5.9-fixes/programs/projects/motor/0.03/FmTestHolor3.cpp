//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "FmTestHolor3.h"
#include "Holors.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
  : TForm(Owner)
{
    QueryPerformanceFrequency((LARGE_INTEGER*)&FFreq);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::VectoresClick(TObject *Sender)
{

    Holor*    v1  = new Holor(3);
    Holor*    v2  = new Holor(3);
    TMerate*  p   = v1->First();
    TMerate*  q   = v1->Last();
    TMerate*  r   = v2->First();

    QueryPerformanceCounter((LARGE_INTEGER*)&FTime1);
    for(register int i = 1000000; i != 0; i--)
      Sum(p, q, r);
    QueryPerformanceCounter((LARGE_INTEGER*)&FTime2);

    Caption = FloatToStr( ( (double) FTime2 - (double) FTime1) / (double) FFreq );
    
    delete v1;
    delete v2;
}
//---------------------------------------------------------------------------
