#include <iostream>

extern void use_cbrt_functions();

int main()
{
    std::cout << "Illustration of AMD LibM functions" << std::endl;
    std::cout << "With C++ compiler" << std::endl << std::endl;
    use_cbrt_functions();
    std::cout <<"\n\n";
    return 0;
}

