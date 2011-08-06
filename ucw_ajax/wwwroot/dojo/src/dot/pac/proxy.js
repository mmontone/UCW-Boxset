/*
	Copyright (c) 2004-2006, The Dojo Foundation
	All Rights Reserved.

	Licensed under the Academic Free License version 2.1 or above OR the
	modified BSD license. For more information on Dojo licensing, see:

		http://dojotoolkit.org/community/licensing.shtml
*/

function FindProxyForURL(url, host){
	if(shExpMatch(url, "http://www.codinginparadise.org*")
		|| shExpMatch(url, "http://codinginparadise.org*")){
		return "PROXY 127.0.0.1:8123";
	}else{
		return "DIRECT";
	}
}
