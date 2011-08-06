/*
	Copyright (c) 2004-2006, The Dojo Foundation
	All Rights Reserved.

	Licensed under the Academic Free License version 2.1 or above OR the
	modified BSD license. For more information on Dojo licensing, see:

		http://dojotoolkit.org/community/licensing.shtml
*/

dojo.provide("dojo.widget.Editor2Plugin.InsertImageDialog");

dojo.require("dojo.i18n.common"); 
dojo.requireLocalization("dojo.widget", "common", null, "ROOT,it,de"); 
dojo.requireLocalization("dojo.widget", "Editor2", null, "ROOT,it,de"); 

dojo.widget.defineWidget(
	"dojo.widget.Editor2InsertImageDialog",
	dojo.widget.Editor2DialogContent,
{
	templateString:"<table cellspacing=\"1\" cellpadding=\"1\" border=\"0\" width=\"100%\" height=\"100%\">\n\t<tr>\n\t\t<td>\n\t\t\t<table cellspacing=\"0\" cellpadding=\"0\" width=\"100%\" border=\"0\">\n\t\t\t\t<tr>\n\t\t\t\t\t<td width=\"100%\">\n\t\t\t\t\t\t<span>${this.editorStrings.linkUrl}</span>\n\t\t\t\t\t</td>\n\t\t\t\t\t<td style=\"display: none\" nowrap=\"nowrap\" rowspan=\"2\">\n\t\t\t\t\t\t<!--input id=\"btnBrowse\" onclick=\"BrowseServer();\" type=\"button\" value=\"Browse Server\"/-->\n\t\t\t\t\t</td>\n\t\t\t\t</tr>\n\t\t\t\t<tr>\n\t\t\t\t\t<td valign=\"top\">\n\t\t\t\t\t\t<input dojoAttachPoint=\"image_src\" style=\"width: 100%\" type=\"text\" />\n\t\t\t\t\t</td>\n\t\t\t\t</tr>\n\t\t\t</table>\n\t\t</td>\n\t</tr>\n\t<tr>\n\t\t<td>\n\t\t\t<span>${this.editorStrings.imageAlt}</span><br />\n\t\t\t<input dojoAttachPoint=\"image_alt\" style=\"width: 100%\" type=\"text\" /><br />\n\t\t</td>\n\t</tr>\n\t<tr>\n\t\t<td valign=\"top\">\n\t\t\t<table><tr><td>\n\t\t\t\t\t\t<table cellspacing=\"0\" cellpadding=\"0\" border=\"0\">\n\t\t\t\t\t\t\t<tr>\n\t\t\t\t\t\t\t\t<td nowrap=\"nowrap\">\n\t\t\t\t\t\t\t\t\t<span>${this.commonStrings.width}</span>&nbsp;</td>\n\t\t\t\t\t\t\t\t<td>\n\t\t\t\t\t\t\t\t\t<input type=\"text\" size=\"3\" dojoAttachPoint=\"image_width\" /></td>\n\n\t\t\t\t\t\t\t\t<td rowspan=\"2\">\n\t\t\t\t\t\t\t\t\t<!--div id=\"btnLockSizes\" class=\"BtnLocked\" onmouseover=\"this.className = (bLockRatio ? 'BtnLocked' : 'BtnUnlocked' ) + ' BtnOver';\"\n\t\t\t\t\t\t\t\t\t\tonmouseout=\"this.className = (bLockRatio ? 'BtnLocked' : 'BtnUnlocked' );\" title=\"Lock Sizes\"\n\t\t\t\t\t\t\t\t\t\tonclick=\"SwitchLock(this);\">\n\t\t\t\t\t\t\t\t\t</div-->\n\t\t\t\t\t\t\t\t</td>\n\t\t\t\t\t\t\t\t<td rowspan=\"2\">\n\t\t\t\t\t\t\t\t\t<!--div id=\"btnResetSize\" class=\"BtnReset\" onmouseover=\"this.className='BtnReset BtnOver';\"\n\t\t\t\t\t\t\t\t\t\tonmouseout=\"this.className='BtnReset';\" title=\"Reset Size\" onclick=\"ResetSizes();\">\n\t\t\t\t\t\t\t\t\t</div-->\n\t\t\t\t\t\t\t\t</td>\n\t\t\t\t\t\t\t</tr>\n\n\t\t\t\t\t\t\t<tr>\n\t\t\t\t\t\t\t\t<td nowrap=\"nowrap\">\n\t\t\t\t\t\t\t\t\t<span>${this.commonStrings.height}</span>&nbsp;</td>\n\t\t\t\t\t\t\t\t<td>\n\t\t\t\t\t\t\t\t\t<input type=\"text\" size=\"3\" dojoAttachPoint=\"image_height\" /></td>\n\t\t\t\t\t\t\t</tr>\n\t\t\t\t\t\t</table>\n\t\t\t\t\t</td><td>\n\n\t\t\t\t\t\t<table cellspacing=\"0\" cellpadding=\"0\" border=\"0\">\n\t\t\t\t\t\t\t<tr>\n\n\t\t\t\t\t\t\t\t<td nowrap=\"nowrap\">\n\t\t\t\t\t\t\t\t\t<span >${this.editorStrings.imageHSpace}</span>&nbsp;</td>\n\t\t\t\t\t\t\t\t<td>\n\t\t\t\t\t\t\t\t\t<input type=\"text\" size=\"2\" dojoAttachPoint=\"image_hspace\"/></td>\n\t\t\t\t\t\t\t</tr>\n\t\t\t\t\t\t\t<tr>\n\t\t\t\t\t\t\t\t<td nowrap=\"nowrap\">\n\t\t\t\t\t\t\t\t\t<span >${this.editorStrings.imageVSpace}</span>&nbsp;</td>\n\n\t\t\t\t\t\t\t\t<td>\n\t\t\t\t\t\t\t\t\t<input type=\"text\" size=\"2\" dojoAttachPoint=\"image_vspace\" /></td>\n\t\t\t\t\t\t\t</tr>\n\t\t\t\t\t\t</table>\n\t\t\t\t\t</td></tr>\n\t\t\t\t\t<tr><td colspan=\"2\">\n\t\t\t\t\t\t<table cellspacing=\"0\" cellpadding=\"0\" border=\"0\">\n\t\t\t\t\t\t\t<tr>\n\t\t\t\t\t\t\t\t<td nowrap=\"nowrap\">\n\t\t\t\t\t\t\t\t\t<span>${this.editorStrings.imageBorder}</span>&nbsp;</td>\n\t\t\t\t\t\t\t\t<td>\n\t\t\t\t\t\t\t\t\t<input type=\"text\" size=\"2\" value=\"\" dojoAttachPoint=\"image_border\" /></td>\n\t\t\t\t\t\t\t\t<td>&nbsp;&nbsp;&nbsp;</td>\n\t\t\t\t\t\t\t\t<td nowrap=\"nowrap\">\n\t\t\t\t\t\t\t\t\t<span >${this.editorStrings.imageAlign}</span>&nbsp;</td>\n\t\t\t\t\t\t\t\t<td>\n\t\t\t\t\t\t\t\t\t<select dojoAttachPoint=\"image_align\">\n\t\t\t\t\t\t\t\t\t\t<option value=\"\" selected=\"selected\"></option>\n\t\t\t\t\t\t\t\t\t\t<option value=\"left\">${this.commonStrings.left}</option>\n\t\t\t\t\t\t\t\t\t\t<option value=\"absBottom\">${this.editorStrings.imageAbsBot}</option>\n\t\t\t\t\t\t\t\t\t\t<option value=\"absMiddle\">${this.editorStrings.imageAbsMid}</option>\n\t\t\t\t\t\t\t\t\t\t<option value=\"baseline\">${this.editorStrings.imageBaseline}</option>\n\t\t\t\t\t\t\t\t\t\t<option value=\"bottom\">${this.commonStrings.bottom}</option>\n\t\t\t\t\t\t\t\t\t\t<option value=\"middle\">${this.commonStrings.middle}</option>\n\t\t\t\t\t\t\t\t\t\t<option value=\"right\">${this.commonStrings.right}</option>\n\t\t\t\t\t\t\t\t\t\t<option value=\"textTop\">${this.editorStrings.imageTextTop}</option>\n\t\t\t\t\t\t\t\t\t\t<option value=\"top\">${this.commonStrings.top}</option>\n\t\t\t\t\t\t\t\t\t</select>\n\t\t\t\t\t\t\t\t</td>\n\t\t\t\t\t\t\t</tr>\n\t\t\t\t\t\t</table>\n\t\t\t\t\t</td>\n\t\t\t\t</tr></table>\n\t\t</td>\n\t</tr>\n\t<tr><td>\n\t\t<table><tr>\n\t\t<td><button dojoType='Button' dojoAttachEvent='onClick:ok'>${this.commonStrings.buttonOk}</button></td>\n\t\t<td><button dojoType='Button' dojoAttachEvent='onClick:cancel'>${this.commonStrings.buttonCancel}</button></td>\n\t\t</tr></table>\n\t</td></tr>\n</table>\n",

	postMixInProperties: function(){ 
		dojo.widget.Editor2InsertImageDialog.superclass.postMixInProperties.apply(this, arguments); 
		this.editorStrings = dojo.i18n.getLocalization("dojo.widget", "Editor2", this.lang); 
		this.commonStrings = dojo.i18n.getLocalization("dojo.widget", "common", this.lang); 
	}, 

	editableAttributes: ['src', 'alt', 'width', 'height', 'hspace', 'vspace', 'border', 'align'],
	loadContent: function(){
		var curInst = dojo.widget.Editor2Manager.getCurrentInstance();
		this.imageNode = dojo.withGlobal(curInst.window, "getSelectedElement", dojo.html.selection);
		if(!this.imageNode){
			this.imageNode = dojo.withGlobal(curInst.window, "getAncestorElement", dojo.html.selection, ['img']);
		}
		var imageAttributes = {};
		this.extraAttribText = "";
		if(this.imageNode){
			var attrs = this.imageNode.attributes;
			for(var i=0; i<attrs.length; i++) {
				if(dojo.lang.find(this.editableAttributes, attrs[i].name.toLowerCase())>-1){
					imageAttributes[attrs[i].name] = attrs[i].value;
				}else{
					this.extraAttribText += attrs[i].name + '="'+attrs[i].value+'" ';
				}
			}
		}
		for(var i=0; i<this.editableAttributes.length; ++i){
			name = this.editableAttributes[i];
			this["image_"+name].value = (imageAttributes[name] == undefined) ? "" : imageAttributes[name] ;
		}
		return true;
	},
	ok: function(){
		var curInst = dojo.widget.Editor2Manager.getCurrentInstance();
		var insertcmd = curInst.getCommand('inserthtml');
		var option = 0;

		var attstr='';
		for(var i=0; i<this.editableAttributes.length; ++i){
			name = this.editableAttributes[i];
			var value = this["image_"+name].value;
			if(value.length > 0){
				attstr += name + '="'+value+'" ';
			}
		}
		if(this.imageNode){
			dojo.withGlobal(curInst.window, "selectElement", dojo.html.selection, [this.imageNode]);
		}
		insertcmd.execute('<img '+attstr+this.extraAttribText+'/>');

		this.cancel();
	}
});
