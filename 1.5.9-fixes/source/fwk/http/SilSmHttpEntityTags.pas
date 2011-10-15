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

unit SilSmHttpEntityTags;

{$I Defines.inc}

interface

uses
  Sil,
  SilLexer,
  SilSiHttpTypes,
  SilSiHttpTags,
  SilSkHttpObjectTags;

type
  TSilHttpEntityTags = class(
    TSilHttpObjectTags,
    IHttpEntityTags )
  private
    FAllow: IHttpTagAllow;
    FContentEncoding: IHttpTagContentEncoding;
    FContentLanguage: IHttpTagContentLanguage;
    FContentLength: IHttpTagContentLength;
    FContentLocation: IHttpTagContentLocation;
    FContentMD5: IHttpTagContentMD5;
    FContentRange: IHttpTagContentRange;
    FContentType: IHttpTagContentType;
    FExpires: IHttpTagExpires;
    FLastModified: IHttpTagLastModified;
  protected // IHttpEntityTags
    function GetAllow: IHttpTagAllow;
    function GetContentEncoding: IHttpTagContentEncoding;
    function GetContentLanguage: IHttpTagContentLanguage;
    function GetContentLength: IHttpTagContentLength;
    function GetContentLocation: IHttpTagContentLocation;
    function GetContentMD5: IHttpTagContentMD5;
    function GetContentRange: IHttpTagContentRange;
    function GetContentType: IHttpTagContentType;
    function GetExpires: IHttpTagExpires;
    function GetLastModified: IHttpTagLastModified;
  end; 
    
implementation

{ TSilHttpEntityTags }

function TSilHttpEntityTags.GetAllow: IHttpTagAllow;
begin
  DoCheck(@FAllow, @Result, httAllow, IHttpTagAllow);
end;

function TSilHttpEntityTags.GetContentEncoding: IHttpTagContentEncoding;
begin
  DoCheck(@FContentEncoding, @Result, httContentEncoding, IHttpTagContentEncoding);
end;

function TSilHttpEntityTags.GetContentLanguage: IHttpTagContentLanguage;
begin
  DoCheck(@FContentLanguage, @Result, httContentLanguage, IHttpTagContentLanguage);
end;

function TSilHttpEntityTags.GetContentLength: IHttpTagContentLength;
begin
  DoCheck(@FContentLength, @Result, httContentLength, IHttpTagContentLength);
end;

function TSilHttpEntityTags.GetContentLocation: IHttpTagContentLocation;
begin
  DoCheck(@FContentLocation, @Result, httContentLocation, IHttpTagContentLocation);
end;

function TSilHttpEntityTags.GetContentMD5: IHttpTagContentMD5;
begin
  DoCheck(@FContentMD5, @Result, httContentMD5, IHttpTagContentMD5);
end;

function TSilHttpEntityTags.GetContentRange: IHttpTagContentRange;
begin
  DoCheck(@FContentRange, @Result, httContentRange, IHttpTagContentRange);
end;

function TSilHttpEntityTags.GetContentType: IHttpTagContentType;
begin
  DoCheck(@FContentType, @Result, httContentType, IHttpTagContentType);
end;

function TSilHttpEntityTags.GetExpires: IHttpTagExpires;
begin
  DoCheck(@FExpires, @Result, httExpires, IHttpTagExpires);
end;

function TSilHttpEntityTags.GetLastModified: IHttpTagLastModified;
begin
  DoCheck(@FLastModified, @Result, httLastModified, IHttpTagLastModified);
end;

end.
