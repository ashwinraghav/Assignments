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

unit SilSfHttpTags;

interface

uses
  SilSiHttpTypes,
  SilSmHttpTag;

function Lookup(const Name: string): TSilHttpTagClass; overload;
function Lookup(ID: THttpTag): TSilHttpTagClass; overload;

implementation

uses
  SilSfHttpParser;

const
  GHttpTag: array[THttpTag] of TSilHttpTagClass =
    (
      TSilHttpTagVariant,            //httUnknown,
      //General
      TSilHttpTagVariant,            //httCacheControl,
      TSilHttpTagVariant,            //httConnection,
      TSilHttpTagVariant,            //httDate,
      TSilHttpTagVariant,            //httPragma,
      TSilHttpTagVariant,            //httTrailer,
      TSilHttpTagVariant,            //httTransferEncoding,
      TSilHttpTagVariant,            //httUpgrade,
      TSilHttpTagVariant,            //httVia,
      TSilHttpTagVariant,            //httWarning,
      //Request
      TSilHttpTagVariant,            //httAccept,
      TSilHttpTagVariant,            //httAcceptCharset,
      TSilHttpTagVariant,            //httAcceptEncoding,
      TSilHttpTagVariant,            //httAcceptLanguage,
      TSilHttpTagVariant,            //httAuthorization,
      TSilHttpTagVariant,            //httExpect,
      TSilHttpTagVariant,            //httFrom,
      TSilHttpTagVariant,            //httHost,
      TSilHttpTagVariant,            //httIfMatch,
      TSilHttpTagVariant,            //httIfModifiedSince,
      TSilHttpTagVariant,            //httIfNoneMatch,
      TSilHttpTagVariant,            //httIfRange,
      TSilHttpTagVariant,            //httIfUnmodifiedSince,
      TSilHttpTagVariant,            //httMaxForwards,
      TSilHttpTagVariant,            //httProxyAuthorization,
      TSilHttpTagVariant,            //httRange,
      TSilHttpTagVariant,            //httReferer,
      TSilHttpTagVariant,            //httTE,
      TSilHttpTagVariant,            //httUserAgent,
      // Response
      TSilHttpTagVariant,            //httAcceptRanges,
      TSilHttpTagVariant,            //httAge,
      TSilHttpTagVariant,            //httETag,
      TSilHttpTagVariant,            //httLocation,
      TSilHttpTagVariant,            //httProxyConnection,
      TSilHttpTagVariant,            //httProxyAuthenticate,
      TSilHttpTagVariant,            //httRetryAfter,
      TSilHttpTagVariant,            //httServer,
      TSilHttpTagVariant,            //httVary,
      TSilHttpTagVariant,            //httWWWAuthenticate,
      //Entity
      TSilHttpTagVariant,            //httAllow,
      TSilHttpTagVariant,            //httContentEncoding,
      TSilHttpTagVariant,            //httContentLanguage,
      TSilHttpTagVariant,            //httContentLength,
      TSilHttpTagVariant,            //httContentLocation,
      TSilHttpTagVariant,            //httContentMD5,
      TSilHttpTagVariant,            //httContentRange,
      TSilHttpTagVariant,            //httContentType,
      TSilHttpTagVariant,            //httExpires,
      TSilHttpTagVariant,            //httLastModified,
      //extension-header
      TSilHttpTagVariant             //httExtension
    );

function Lookup(const Name: string): TSilHttpTagClass;
begin
  Result := Lookup(HttpTagID(Name));  
end;

function Lookup(ID: THttpTag): TSilHttpTagClass; 
begin
  Result := GHttpTag[ID];  
end;

end.
 