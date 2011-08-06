/*
	Copyright (c) 2004-2006, The Dojo Foundation
	All Rights Reserved.

	Licensed under the Academic Free License version 2.1 or above OR the
	modified BSD license. For more information on Dojo licensing, see:

		http://dojotoolkit.org/community/licensing.shtml
*/

dojo.provide("dojo.widget.Editor2Plugin.DialogCommands");
dojo.require("dojo.i18n.common");
dojo.requireLocalization("dojo.widget", "common", null, "ROOT,it,de");
dojo.requireLocalization("dojo.widget", "Editor2", null, "ROOT,it,de");

dojo.require("dojo.widget.FloatingPane");
dojo.widget.defineWidget(
	"dojo.widget.Editor2Dialog",
	[dojo.widget.HtmlWidget, dojo.widget.FloatingPaneBase, dojo.widget.ModalDialogBase],
	{
		// summary:
		//		Provides a Dialog which can be modal or normal for the Editor2.

		templateString:"<div id=\"${this.widgetId}\" class=\"dojoFloatingPane\">\n\t<span dojoattachpoint=\"tabStartOuter\" dojoonfocus=\"trapTabs\" dojoonblur=\"clearTrap\"\ttabindex=\"0\"></span>\n\t<span dojoattachpoint=\"tabStart\" dojoonfocus=\"trapTabs\" dojoonblur=\"clearTrap\" tabindex=\"0\"></span>\n\t<div dojoAttachPoint=\"titleBar\" class=\"dojoFloatingPaneTitleBar\"  style=\"display:none\">\n\t  \t<img dojoAttachPoint=\"titleBarIcon\"  class=\"dojoFloatingPaneTitleBarIcon\">\n\t\t<div dojoAttachPoint=\"closeAction\" dojoAttachEvent=\"onClick:hide\"\n   \t  \t\tclass=\"dojoFloatingPaneCloseIcon\"></div>\n\t\t<div dojoAttachPoint=\"restoreAction\" dojoAttachEvent=\"onClick:restoreWindow\"\n   \t  \t\tclass=\"dojoFloatingPaneRestoreIcon\"></div>\n\t\t<div dojoAttachPoint=\"maximizeAction\" dojoAttachEvent=\"onClick:maximizeWindow\"\n   \t  \t\tclass=\"dojoFloatingPaneMaximizeIcon\"></div>\n\t\t<div dojoAttachPoint=\"minimizeAction\" dojoAttachEvent=\"onClick:minimizeWindow\"\n   \t  \t\tclass=\"dojoFloatingPaneMinimizeIcon\"></div>\n\t  \t<div dojoAttachPoint=\"titleBarText\" class=\"dojoFloatingPaneTitleText\">${this.title}</div>\n\t</div>\n\n\t<div id=\"${this.widgetId}_container\" dojoAttachPoint=\"containerNode\" class=\"dojoFloatingPaneClient\"></div>\n\t<span dojoattachpoint=\"tabEnd\" dojoonfocus=\"trapTabs\" dojoonblur=\"clearTrap\" tabindex=\"0\"></span>\n\t<span dojoattachpoint=\"tabEndOuter\" dojoonfocus=\"trapTabs\" dojoonblur=\"clearTrap\" tabindex=\"0\"></span>\n\t<div dojoAttachPoint=\"resizeBar\" class=\"dojoFloatingPaneResizebar\" style=\"display:none\"></div>\n</div>\n",

		// modal: Boolean: Whether this is a modal dialog. True by default.
		modal: true,

//		refreshOnShow: true, //for debug for now

		// width: String: Width of the dialog. None by default.
		width: "",

		// height: String: Height of the dialog. None by default.
		height: "",

		// windowState: String: startup state of the dialog
		windowState: "minimized",

		displayCloseAction: true,

		// contentFile: String
		//	TODO
		contentFile: "",
		
		// contentClass: String
		//	TODO
		contentClass: "",

		fillInTemplate: function(args, frag){
			this.fillInFloatingPaneTemplate(args, frag);
			dojo.widget.Editor2Dialog.superclass.fillInTemplate.call(this, args, frag);
		},
		postCreate: function(){
			if(this.contentFile){
				dojo.require(this.contentFile);
			}

			with(this.domNode.style) {
				zIndex = 999;
				display = "none";
			}

			dojo.widget.FloatingPaneBase.prototype.postCreate.apply(this, arguments);
			dojo.widget.Editor2Dialog.superclass.postCreate.call(this);
			if(this.width && this.height){
				with(this.domNode.style){
					width = this.width;
					height = this.height;
				}
			}
		},
		createContent: function(){
			if(!this.contentWidget && this.contentClass){
				this.contentWidget = dojo.widget.createWidget(this.contentClass,{parent:this});
				//buggy IE: if the dialog is hidden, the button widgets
				//in the dialog can not be shown, so append this to
				//body first (off screen of course), and then add it back
				this.contentWidget.domNode.style.top="-10000px";
				this.contentWidget.domNode.style.position="absolute";
				dojo.body().appendChild(this.contentWidget.domNode);
				this.addChild(this.contentWidget);
				this.contentWidget.domNode.style.top="";
				this.contentWidget.domNode.style.position="";
			}
		},
		show: function(){
			if(!this.contentWidget){
				this.createContent();
			}

			if(!this.contentWidget || !this.contentWidget.loadContent()){
				return;
			}
			this.showFloatingPane();
			
			if(this.modal){
				this.showModalDialog();
				//place the background div under this modal pane
				this.bg.style.zIndex = this.domNode.style.zIndex-1;
			}
			dojo.widget.Editor2Dialog.superclass.show.apply(this, arguments);
		},
		onShow: function(){
			dojo.widget.Editor2Dialog.superclass.onShow.call(this);
			this.onFloatingPaneShow();
		},
		closeWindow: function(){
			this.hide();
			dojo.widget.Editor2Dialog.superclass.closeWindow.apply(this, arguments);
		},
		hide: function(){
			if(this.modal){
				this.hideModalDialog();
			}
			dojo.widget.Editor2Dialog.superclass.hide.call(this);
		},
		//modified from ModalDialogBase.checkSize to call _sizeBackground conditionally
		checkSize: function(){
			if(this.isShowing()){
				if(this.modal){
					this._sizeBackground();
				}
				this.placeModalDialog();
				this.onResized();
			}
		}
	}
);

dojo.widget.defineWidget(
	"dojo.widget.Editor2DialogContent",
	dojo.widget.HtmlWidget,
{
	// summary:
	//		dojo.widget.Editor2DialogContent is the actual content of a Editor2Dialog.
	//		This class should be subclassed to provide the content.

	widgetsInTemplate: true,

	postMixInProperties: function(){
		dojo.widget.HtmlWidget.superclass.postMixInProperties.apply(this, arguments);
		this.editorStrings = dojo.i18n.getLocalization("dojo.widget", "Editor2", this.lang);
		this.commonStrings = dojo.i18n.getLocalization("dojo.widget", "common", this.lang);
	},

	loadContent:function(){
		// summary: Load the content. Called by Editor2Dialog when first shown
		return true;
	},
	cancel: function(){
		// summary: Default handler when cancel button is clicked.
		this.parent.hide();
	}
});

dojo.lang.declare("dojo.widget.Editor2DialogCommand", dojo.widget.Editor2BrowserCommand,
	function(editor, name, dialogParas){
		// summary:
		//		Provides an easy way to popup a dialog when
		//		the command is executed.
		this.dialogParas = dialogParas;
	},
{
	execute: function(){
		if(!this.dialog){
			if(!this.dialogParas.contentFile || !this.dialogParas.contentClass){
				alert("contentFile and contentClass should be set for dojo.widget.Editor2DialogCommand.dialogParas!");
				return;
			}
			this.dialog = dojo.widget.createWidget(this.dialogParas.dialogClass?this.dialogParas.dialogClass:"Editor2Dialog", this.dialogParas);

			dojo.body().appendChild(this.dialog.domNode);

			dojo.event.connect(this, "destroy", this.dialog, "destroy");
		}
		this.dialog.show();
	},
	getText: function(){
		return this.dialogParas.title || dojo.widget.Editor2DialogCommand.superclass.getText.call(this);
	}
});
