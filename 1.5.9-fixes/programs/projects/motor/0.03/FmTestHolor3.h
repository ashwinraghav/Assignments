//---------------------------------------------------------------------------

#ifndef FmTestHolor3H
#define FmTestHolor3H
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
  TButton *Vectores;
  void __fastcall VectoresClick(TObject *Sender);
private:
    __int64   FFreq, FTime1, FTime2;
public:		// User declarations
  __fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
