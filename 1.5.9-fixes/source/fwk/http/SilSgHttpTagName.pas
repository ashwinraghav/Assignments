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

unit SilSgHttpTagName;

interface

uses
  SilSiHttpTypes;

const
  GHttpTagName: array[THttpTag] of string =
    (
      '',                          //httUnknown,

      'Cache-Control',             //httCacheControl,
      'Connection',                //httConnection,
      'Date',                      //httDate,
      'Pragma',                    //httPragma,
      'Trailer',                   //httTrailer,
      'Transfer-Encoding',         //httTransferEncoding,
      'Upgrade',                   //httUpgrade,
      'Via',                       //httVia,
      'Warning',                   //httWarning,

      'Accept',                    //httAccept,
      'Accept-Charset',            //httAcceptCharset,           
      'Accept-Encoding',           //httAcceptEncoding,          
      'Accept-Language',           //httAcceptLanguage,          
      'Authorization',             //httAuthorization,            
      'Expect',                    //httExpect,                   
      'From',                      //httFrom,                     
      'Host',                      //httHost,                     
      'If-Match',                  //httIfMatch,                 
      'If-Modified-Since',         //httIfModifiedSince,        
      'If-None-Match',             //httIfNoneMatch,            
      'If-Range',                  //httIfRange,                 
      'If-Unmodified-Since',       //httIfUnmodifiedSince,      
      'Max-Forwards',              //httMaxForwards,
      'Proxy-Connection',              
      'Proxy-Authorization',       //httProxyAuthorization,      
      'Range',                     //httRange,                    
      'Referer',                   //httReferer,                  
      'TE',                        //httTE,                       
      'User-Agent',                //httUserAgent,                   


      'Accept-Ranges',             //httAcceptRanges,

      'Age',                       //httAge,
      'ETag',                      //httETag,                    
      'Location',                  //httLocation,                
      'Proxy-Authenticate',        //httProxyAuthenticate,      
      'Retry-After',               //httRetryAfter,             
      'Server',                    //httServer,                  
      'Vary',                      //httVary,                    
      'WWW-Authenticate',          //httWWWAuthenticate,


      'Allow',                     //httAllow,
      'Content-Encoding',          //httContentEncoding,
      'Content-Language',          //httContentLanguage,
      'Content-Length',            //httContentLength,
      'Content-Location',          //httContentLocation,
      'Content-MD5',               //httContentMD5,
      'Content-Range',             //httContentRange,
      'Content-Type',              //httContentType,
      'Expires',                   //httExpires,
      'Last-Modified',             //httLastModified,

      ''                           //httExtension
    );


implementation

end.
