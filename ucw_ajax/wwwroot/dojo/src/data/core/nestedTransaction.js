/*
	Copyright (c) 2004-2006, The Dojo Foundation
	All Rights Reserved.

	Licensed under the Academic Free License version 2.1 or above OR the
	modified BSD license. For more information on Dojo licensing, see:

		http://dojotoolkit.org/community/licensing.shtml
*/

dojo.provide("dojo.data.core.nestedTransaction");
dojo.require("dojo.data.core.Write");
dojo.require("dojo.experimental");

dojo.experimental("dojo.data.core.nestedTransaction");

dojo.data.core.nestedTransaction.beginTransaction = function() {
	if (!this._countOfNestedTransactions) {
		this._countOfNestedTransactions = 0;
	}
	this._countOfNestedTransactions += 1;
};

dojo.data.core.nestedTransaction.endTransaction = function() {
	this._countOfNestedTransactions -= 1;
	dojo.lang.assert(this._countOfNestedTransactions >= 0);
	if (this._countOfNestedTransactions === 0) {
		return this.save(); // save() is defined on the dojo.data.core.Write API
	}
};

dojo.data.core.nestedTransaction.rollbackTransaction = function() {
	this._countOfNestedTransactions = 0;
	this.revert(); // revert() is defined on the dojo.data.core.Write API
};


