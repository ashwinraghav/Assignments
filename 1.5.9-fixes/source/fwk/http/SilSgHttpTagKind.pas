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

unit SilSgHttpTagKind;

interface

uses
  SilSiHttpTypes;

const
  GHttpTagKind: array[THttpTag] of THttpTagKind =
    (
      htkUnknown,           //httUnknown,
      //General
      htkGeneral,           //httCacheControl,
      htkGeneral,           //httConnection,
      htkGeneral,           //httDate,
      htkGeneral,           //httPragma,
      htkGeneral,           //httTrailer,
      htkGeneral,           //httTransferEncoding,
      htkGeneral,           //httUpgrade,
      htkGeneral,           //httVia,
      htkGeneral,           //httWarning,
      //Request
      htkRequest,           //httAccept,
      htkRequest,           //httAcceptCharset,           
      htkRequest,           //httAcceptEncoding,          
      htkRequest,           //httAcceptLanguage,          
      htkRequest,           //httAuthorization,            
      htkRequest,           //httExpect,                   
      htkRequest,           //httFrom,                     
      htkRequest,           //httHost,                     
      htkRequest,           //httIfMatch,                 
      htkRequest,           //httIfModifiedSince,        
      htkRequest,           //httIfNoneMatch,            
      htkRequest,           //httIfRange,                 
      htkRequest,           //httIfUnmodifiedSince,      
      htkRequest,           //httMaxForwards,             
      htkRequest,           //httProxyAuthorization,      
      htkRequest,           //httRange,                    
      htkRequest,           //httReferer,                  
      htkRequest,           //httTE,                       
      htkRequest,           //httUserAgent,                   
      // Response
      htkResponse,          //httAcceptRanges,
      htkResponse,          //httAge,
      htkResponse,          //httETag,                    
      htkResponse,          //httLocation,
      htkResponse,          //httProxyConnection,                
      htkResponse,          //httProxyAuthenticate,      
      htkResponse,          //httRetryAfter,             
      htkResponse,          //httServer,                  
      htkResponse,          //httVary,                    
      htkResponse,          //httWWWAuthenticate,
      //Entity
      htkEntity,            //httAllow,
      htkEntity,            //httContentEncoding,
      htkEntity,            //httContentLanguage,
      htkEntity,            //httContentLength,
      htkEntity,            //httContentLocation,
      htkEntity,            //httContentMD5,
      htkEntity,            //httContentRange,
      htkEntity,            //httContentType,
      htkEntity,            //httExpires,
      htkEntity,            //httLastModified,
      //extension-header
      htkExtension          //httExtension
    );

implementation
end.
 