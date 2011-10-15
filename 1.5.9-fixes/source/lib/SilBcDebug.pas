{********************************************************************************
 *                  Standard Interface Library (SIL)                            *
 *                                                                              *
 *       General purpose library whose design is based in STRONG                *
 *   use of interfaces.                                                         *
 *                                                                              *
 *                                                                              *
 *     Copyright (C) 2000 Mariano Podestá    antiriad@gmail.com                 *
 *     Copyright (C) 2000 Leandro Conde      lconde@str.com.ar                  *
 *     Copyright (C) 2000 Lisandro Podestá   lisandrop@movi.com.ar              *
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

unit SilBcDebug;

{$I Defines.inc}

interface

uses
  SilBeTypes;

type
  TDebugFlag                            = 0 .. 31;
  TDebugLevel                           = SilBeTypes.LongWord;
  TDebugFlags                           = SilBeTypes.LongWord;


const
  NDebugDb                              =  Low(TDebugFlag);           //  0
  NDebugMemory                          =  Succ(NDebugDb);            //  1
  NDebugTracing                         =  Succ(NDebugMemory);        //  2
  NDebugLibrary                         =  Succ(NDebugTracing);       //  3
  NDebugCalls                           =  Succ(NDebugLibrary);       //  4
  NDebugOnErrorForce                    =  Succ(NDebugCalls);         //  5
  NDebugOnTraceForce                    =  Succ(NDebugOnErrorForce);  //  6

  //reserved                            = 7

  NDebugFirst                           =  Low(TDebugFlag) + 8;

  NDebugLast                            = Pred(High(TDebugFlag));

  NDebugError                           = High(TDebugFlag);

type
  TDebugUserRange                       = NDebugFirst .. NDebugLast;

const
  CDebugNone                            = TDebugFlags($00000000);
  CDebugAll                             = TDebugFlags($FFFFFFFF);
  CDebugMemory                          = TDebugFlags( 1 shl NDebugDb     );        //  00000000 00000001
  CDebugDb                              = TDebugFlags( 1 shl NDebugMemory );        //  00000000 00000010
  CDebugTracing                         = TDebugFlags( 1 shl NDebugTracing);        //  00000000 00000100
  CDebugLibrary                         = TDebugFlags( 1 shl NDebugLibrary);        //  00000000 00001000
  CDebugCalls                           = TDebugFlags( 1 shl NDebugCalls  );        //  00000000 00010000
  CDebugOnErrorForce                    = TDebugFlags( 1 shl NDebugOnErrorForce);   //  00000000 00100000
  CDebugOnTraceForce                    = TDebugFlags( 1 shl NDebugOnTraceForce);   //  00000000 01000000
  CDebugError                           = TDebugFlags( 1 shl NDebugError  );        //  10000000 00000000
  CDebugReserved                        = NDebugFirst - 1;

implementation
end.
