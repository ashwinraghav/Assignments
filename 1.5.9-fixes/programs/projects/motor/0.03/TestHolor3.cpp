//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("TestHolor3.res");
USEFORM("FmTestHolor3.cpp", Form1);
USEUNIT("Holors.cpp");
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
  try
  {
     Application->Initialize();
     Application->CreateForm(__classid(TForm1), &Form1);
     Application->Run();
  }
  catch (Exception &exception)
  {
     Application->ShowException(&exception);
  }
  return 0;
}
//---------------------------------------------------------------------------
