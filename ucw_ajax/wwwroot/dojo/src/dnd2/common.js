/*
	Copyright (c) 2004-2006, The Dojo Foundation
	All Rights Reserved.

	Licensed under the Academic Free License version 2.1 or above OR the
	modified BSD license. For more information on Dojo licensing, see:

		http://dojotoolkit.org/community/licensing.shtml
*/

dojo.provide("dojo.dnd2.common");

dojo.dnd2.multiSelectKey = function(e) {
	// summary: abstracts away the difference between selection on Mac and PC
	// e: Event: mouse event
	return dojo.render.os.mac ? e.metaKey : e.ctrlKey;	// Boolean
};
