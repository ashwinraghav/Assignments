{********************************************************************************

 *                  Standard Interface Library (SIL)                            *

 *                                                                              *

 *       General purpose library whose design is based in STRONG                *

 *   use of interfaces.                                                         *

 *                                                                              *

 *                                                                              *

 *     Copyright (C) 2000 Mariano Podest�    antiriad@gmail.com                 *

 *     Copyright (C) 2000 Leandro Conde      lconde@str.com.ar                  *

 *     Copyright (C) 2000 Lisandro Podest�   lisandrop@movi.com.ar              *

 *                                                                              *

 *     See License.txt for details.                                             *

 *                                                                              *

 *   This library is free software; you can redistribute it and/or              *

 *   modify it under the terms of the GNU Lesser General Public                 *

 *   License as published by the Free Software Foundation; either               *

 *   version 2.1 of the License, or (at your option) any later version.         *

 *                                                                              *

 *   This library is distributed in the hope that it will be useful,            *

 *   but WITHOUT ANY WARRANTY; without even the implied warranty of             *

 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU          *

 *   Lesser General Public License for more details.                            *

 *                                                                              *

 *   You should have received a copy of the GNU Lesser General Public           *

 *   License along with this library; if not, write to the Free Software        *

 *   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA  *

 *                                                                              *

 ********************************************************************************}



unit SilOtPerformance;



{$I Defines.inc}



interface



uses

  SilBeTypes,

  SilOiPerformance,

  SilOjPerformance;



type

  SilLinuxPerformanceTool = class(SilPerformanceTool)

    class function Create: IPerformanceCounter; override;

    class function ToDateTime(const Counter: IPerformanceCounter; Reset: Boolean = False; const Offset: TDateTime = 0): TDateTime; override;

    class function ToSeconds(const Counter: IPerformanceCounter; Reset: Boolean = False): Double; override;

    class function ToMSeconds(const Counter: IPerformanceCounter; Reset: Boolean = False): Double; override;

  end;



implementation



uses

  SilOsClasses,

  SilBtDateTime;



{ SilLinuxPerformanceTool }



class function SilLinuxPerformanceTool.Create: IPerformanceCounter;

begin

  Result := TSilOsPerformance.Create();  

end;



class function SilLinuxPerformanceTool.ToDateTime(const Counter: IPerformanceCounter; Reset: Boolean; const Offset: TDateTime): TDateTime;

begin

  Result := DateTime.FromSecs(ToSeconds(Counter, Reset), Offset);

end;



class function SilLinuxPerformanceTool.ToMSeconds(const Counter: IPerformanceCounter; Reset: Boolean): Double;

begin

  Result := 1000 * Counter.GetTime(Reset) / Counter.Frequency;

end;



class function SilLinuxPerformanceTool.ToSeconds(const Counter: IPerformanceCounter; Reset: Boolean): Double;

begin

  Result := Counter.GetTime(Reset) / Counter.Frequency;

end;



end.

 