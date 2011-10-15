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

unit SilContainer;

{$INCLUDE Defines.inc}

interface

uses
  SilLiContainerTypes,
  SilLiContainerBase,
  SilLiContainerList,
  SilLiContainerVector,
  SilLiContainer,
  SilLiContainerStrings,
  SilLtContainer;

const
  HNull                                       = SilLiContainerTypes.HNull;

type
  HItem                                       = SilLiContainerTypes.HItem;
  PItem                                       = SilLiContainerTypes.PItem;
  HData                                       = SilLiContainerTypes.HData;
  PData                                       = SilLiContainerTypes.PData;

type
  TListDuplicates                             = SilLiContainerTypes.TListDuplicates;

const
  duIgnore                                    = TListDuplicates(SilLiContainerTypes.duIgnore);
  duAccept                                    = TListDuplicates(SilLiContainerTypes.duAccept);
  duError                                     = TListDuplicates(SilLiContainerTypes.duError );

type
  ESilListError                               = SilLiContainerTypes.ESilListError;

type
  RSortOptions                                = SilLiContainerTypes.RSortOptions;

type
  TTypeCompare                                = SilLiContainerTypes.TTypeCompare;

type
  ITypeComparator                             = SilLiContainerTypes.ITypeComparator;
  ITypeHandler                                = SilLiContainerTypes.ITypeHandler   ;
  ITypeAllocator                              = SilLiContainerTypes.ITypeAllocator ;

type
  TSilBaseContainerClass                      = SilLiContainerTypes.TSilBaseContainerClass;
  TSilCursorClass                             = SilLiContainerTypes.TSilCursorClass       ;
  TSilContainerClass                          = SilLiContainerTypes.TSilContainerClass    ;

type
  IBaseContainer                              = SilLiContainerBase.IBaseContainer;

const
  CListMagic                                  = SilLiContainerList.CListMagic;

type
  PPListItem                                  = SilLiContainerList.PPListItem;
  PListItem                                   = SilLiContainerList.PListItem ;
  RListItem                                   = SilLiContainerList.RListItem ;

type
  TListLink                                   = SilLiContainerList.TListLink ;

const
  llNext                                      = TListLink(SilLiContainerList.llNext);
  llPrev                                      = TListLink(SilLiContainerList.llPrev);

type
  IBaseList                                   = SilLiContainerList.IBaseList;

type
  IBaseVector                                 = SilLiContainerVector.IBaseVector;  

type
  IContainerTypes                             = SilLiContainer.IContainerTypes;
  IContainerItemsStatic                       = SilLiContainer.IContainerItemsStatic;
  IContainerItemsDynamic                      = SilLiContainer.IContainerItemsDynamic;
  IContainerCursors                           = SilLiContainer.IContainerCursors;
  IContainerPointers                          = SilLiContainer.IContainerPointers;
  IContainerPointersStatic                    = SilLiContainer.IContainerPointersStatic;
  IContainerPointersDynamic                   = SilLiContainer.IContainerPointersDynamic;
  IContainerLookupStatic                      = SilLiContainer.IContainerLookupStatic;
  IContainerLookupDynamic                     = SilLiContainer.IContainerLookupDynamic;
  IContainerStatic                            = SilLiContainer.IContainerStatic;
  IContainerDynamic                           = SilLiContainer.IContainerDynamic;
  IContainerCursor                            = SilLiContainer.IContainerCursor;
  IContainerCursorStatic                      = SilLiContainer.IContainerCursorStatic;
  IContainerCursorDynamic                     = SilLiContainer.IContainerCursorDynamic;
  IContainerPointerStatic                     = SilLiContainer.IContainerPointerStatic;
  IContainerPointerDynamic                    = SilLiContainer.IContainerPointerDynamic;
  IContainerCursorOrdered                     = SilLiContainer.IContainerCursorOrdered;
  IContainerCursorOrderedStatic               = SilLiContainer.IContainerCursorOrderedStatic;
  IContainerCursorOrderedDynamic              = SilLiContainer.IContainerCursorOrderedDynamic;
  IContainerSequence                          = SilLiContainer.IContainerSequence;
  IContainerSequenceStatic                    = SilLiContainer.IContainerSequenceStatic;
  IContainerSequenceDynamic                   = SilLiContainer.IContainerSequenceDynamic;
  IVectorStatic                               = SilLiContainer.IVectorStatic;
  IVectorDynamic                              = SilLiContainer.IVectorDynamic;

type
  IStringsStatic                              = SilLiContainerStrings.IStringsStatic       ;
  IStringsDynamic                             = SilLiContainerStrings.IStringsDynamic      ;
  IStringCursorsStatic                        = SilLiContainerStrings.IStringCursorsStatic ;
  IStringCursorsDynamic                       = SilLiContainerStrings.IStringCursorsDynamic;
  IStringPointerStatic                        = SilLiContainerStrings.IStringPointerStatic ;
  IStringPointerDynamic                       = SilLiContainerStrings.IStringPointerDynamic;

type
  PStringData                                 = SilLiContainerStrings.PStringData;
  RStringData                                 = SilLiContainerStrings.RStringData;

type
  ContainerClass                              = SilLtContainer.ContainerClass;

type
  ckList                                      = SilLtContainer.ckList;
  ckVector                                    = SilLtContainer.ckVector;

type
  List                                        = SilLtContainer.List;
  Vector                                      = SilLtContainer.Vector;

type
  Container                                   = SilLtContainer.Container;
  Handler                                     = SilLtContainer.Handler;
  Compare                                     = SilLtContainer.Compare;
  Sequence                                    = SilLtContainer.Sequence;

type
  Tool                                        = Container;
  Tk                                          = Container;

implementation
end.
