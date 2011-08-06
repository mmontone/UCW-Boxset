/*
	Copyright (c) 2004-2006, The Dojo Foundation
	All Rights Reserved.

	Licensed under the Academic Free License version 2.1 or above OR the
	modified BSD license. For more information on Dojo licensing, see:

		http://dojotoolkit.org/community/licensing.shtml
*/

dojo.provide("dojo.widget.Editor2Plugin.CreateLinkDialog");
dojo.require("dojo.i18n.common");
dojo.requireLocalization("dojo.widget", "Editor2", null, "ROOT,it,de");

dojo.widget.defineWidget(
	"dojo.widget.Editor2CreateLinkDialog",
	dojo.widget.Editor2DialogContent,
{
	templateString:"<table>\n<tr><td>${this.editorStrings.linkUrl}</td><td> <input type=\"text\" dojoAttachPoint=\"link_href\" name=\"dojo_createLink_href\"/></td></tr>\n<tr><td>${this.editorStrings.linkTarget} </td><td><select dojoAttachPoint=\"link_target\">\n\t<option value=\"\">${this.editorStrings.linkWindowSelf}</option>\n\t<option value=\"_blank\">${this.editorStrings.linkWindowNew}</option>\n\t<option value=\"_top\">${this.editorStrings.linkWindowTop}</option>\n\t</select></td></tr>\n<tr><td>${this.editorStrings.linkClass} </td><td><input type=\"text\" dojoAttachPoint=\"link_class\" /></td></tr>\n<tr><td colspan=\"2\">\n\t<table><tr>\n\t<td><button dojoType='Button' dojoAttachEvent='onClick:ok'>${this.commonStrings.buttonOk}</button></td>\n\t<td><button dojoType='Button' dojoAttachEvent='onClick:cancel'>${this.commonStrings.buttonCancel}</button></td>\n\t</tr></table>\n\t</td></tr>\n</table>\n",

	editableAttributes: ['href', 'target', 'class'],
	loadContent: function(){
		var curInst = dojo.widget.Editor2Manager.getCurrentInstance();

		curInst.saveSelection(); //save selection (none-activeX IE)

		this.linkNode = dojo.withGlobal(curInst.window, "getAncestorElement", dojo.html.selection, ['a']);
		var linkAttributes = {};
		this.extraAttribText = "";
		if(this.linkNode){
			var attrs = this.linkNode.attributes;
			for(var i=0; i<attrs.length; i++) {
				if(dojo.lang.find(this.editableAttributes, attrs[i].name.toLowerCase())>-1){
					linkAttributes[attrs[i].name] = attrs[i].value;
				}else{
					//IE lists all attributes, even default ones, filter them
					if(attrs[i].specified == undefined || attrs[i].specified){
						this.extraAttribText += attrs[i].name + '="'+attrs[i].value+'" ';
					}
				}
			}
		}else{
			var html = dojo.withGlobal(curInst.window, "getSelectedText", dojo.html.selection);
			if(html == null || html.length == 0){
				var resource = dojo.i18n.getLocalization("dojo.widget", "Editor2", this.lang);
				alert(resource.createLinkDialogSelectError);
				return false;//do not show the dialog
			}
		}

		for(var i=0; i<this.editableAttributes.length; ++i){
			name = this.editableAttributes[i];
			this["link_"+name].value = (linkAttributes[name] == undefined) ? "" : linkAttributes[name] ;
		}
		return true;
	},
	ok: function(){
		var curInst = dojo.widget.Editor2Manager.getCurrentInstance();
		curInst.restoreSelection(); //restore previous selection, required for none-activeX IE

		if(!this.linkNode){
			var html = dojo.withGlobal(curInst.window, "getSelectedHtml", dojo.html.selection);
		}else{
			var html = this.linkNode.innerHTML;
			dojo.withGlobal(curInst.window, "selectElement", dojo.html.selection, [this.linkNode]);
		}

		var attstr='';
		for(var i=0; i<this.editableAttributes.length; ++i){
			name = this.editableAttributes[i];
			var value = this["link_"+name].value;
			if(value.length > 0){
				attstr += name + '="'+value+'" ';
			}
		}

		curInst.execCommand('inserthtml', '<a '+attstr+this.extraAttribText+'>'+html+'</a>');

		this.cancel();
	}
});
