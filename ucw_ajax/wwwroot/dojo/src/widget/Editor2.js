/*
	Copyright (c) 2004-2006, The Dojo Foundation
	All Rights Reserved.

	Licensed under the Academic Free License version 2.1 or above OR the
	modified BSD license. For more information on Dojo licensing, see:

		http://dojotoolkit.org/community/licensing.shtml
*/

dojo.provide("dojo.widget.Editor2");

dojo.require("dojo.io.*");
dojo.require("dojo.widget.RichText");
dojo.require("dojo.widget.Editor2Toolbar");
dojo.require("dojo.i18n.common");
dojo.require("dojo.uri.cache");
dojo.requireLocalization("dojo.widget", "Editor2", null, "ROOT,it,de");
dojo.requireLocalization("dojo.widget", "Editor2BrowserCommand", null, "it,de,ROOT");

dojo.widget.Editor2Manager = new dojo.widget.HandlerManager();

dojo.lang.mixin(dojo.widget.Editor2Manager,
{
	// summary: Manager of current focused Editor2 Instance and available editor2 commands

	_currentInstance: null,

	// commandState: Object: state a command may be in
	commandState: {Disabled: 0, Latched: 1, Enabled: 2},

	getCurrentInstance: function(){
		// summary: Return the current focused Editor2 instance
		
		//TODO: make use of the focusmanager in dijit, so we can
		//get rid of the setFocus logic for FF too
		var iframe=document.activeElement; //IE only
		if(iframe && iframe.tagName=='IFRAME'){
			var f=dojo.widget.byId(iframe.id);
			if(f){
				this._currentInstance = f;
			}
		}
		return this._currentInstance;
	},
	setCurrentInstance: function(/*Widget*/inst){
//		dojo.debug('setCurrentInstance',inst);
		this._currentInstance = inst;
	},
	getCommand: function(/*dojo.widget.Editor2*/editor,/*String*/name){
		// summary: Return Editor2 command with the given name
		// name: name of the command (case insensitive)
		var oCommand;
		name = name.toLowerCase();
		var i=0, handle, handers=this._registeredHandlers;
		while(handle=handers[i++]){
			oCommand = handle(editor, name);
			if(oCommand){
				return oCommand;
			}
		}
		if(name == 'createlink' || name == 'insertimage'){
			if(!dojo.widget['Editor2Plugin'] || !dojo.widget.Editor2Plugin['DialogCommands']){
				dojo.deprecated('Command '+name+" is now defined in plugin dojo.widget.Editor2Plugin.DialogCommands. It shall be required explicitly", "0.6");
				dojo['require']("dojo.widget.Editor2Plugin.DialogCommands"); //avoid loading by the build
			}
		}
		var resources = dojo.i18n.getLocalization("dojo.widget", "Editor2", this.lang);
		switch(name){
			case 'htmltoggle':
				//Editor2 natively provide the htmltoggle functionalitity
				//and it is treated as a builtin command
				oCommand = new dojo.widget.Editor2BrowserCommand(editor, name);
				break;
//			case 'formatblock':
//				oCommand = new dojo.widget.Editor2FormatBlockCommand(editor, name);
//				break;
			case 'anchor':
				oCommand = new dojo.widget.Editor2Command(editor, name);
				break;

			//dialog command
			case 'createlink':
				oCommand = new dojo.widget.Editor2DialogCommand(editor, name,
						{contentFile: "dojo.widget.Editor2Plugin.CreateLinkDialog",
							contentClass: "Editor2CreateLinkDialog",
							title: resources.createLinkDialogTitle, width: "300px", height: "200px",
							lang: this.lang});
				break;
			case 'insertimage':
				oCommand = new dojo.widget.Editor2DialogCommand(editor, name,
						{contentFile: "dojo.widget.Editor2Plugin.InsertImageDialog",
							contentClass: "Editor2InsertImageDialog",
							title: resources.insertImageDialogTitle, width: "400px", height: "270px",
							lang: this.lang});
				break;
			// By default we assume that it is a builtin simple command.
			default:
				var curtInst = this.getCurrentInstance();
				if((curtInst && curtInst.queryCommandAvailable(name)) ||
					(!curtInst && dojo.widget.Editor2.prototype.queryCommandAvailable(name))){
					oCommand = new dojo.widget.Editor2BrowserCommand(editor, name);
				}else{
					dojo.debug("dojo.widget.Editor2Manager.getCommand: Unknown command "+name);
					return;
				}
		}
		return oCommand;
	},
	destroy: function(){
		// summary: Cleaning up. This is called automatically on page unload.
		this._currentInstance = null;
		dojo.widget.HandlerManager.prototype.destroy.call(this);
	}
});

dojo.addOnUnload(dojo.widget.Editor2Manager, "destroy");

dojo.lang.declare("dojo.widget.Editor2Command",null,
	function(editor,name){
		// summary:
		//		dojo.widget.Editor2Command is the base class for all commands in Editor2

		this._editor = editor;
		this._updateTime = 0;
		this._name = name;
	},
{
		_text: 'Unknown',
		execute: function(para){
			// summary: Execute the command. should be implemented in subclass
			// description: this function should be re-implemented in subclass
			dojo.unimplemented("dojo.widget.Editor2Command.execute");
		},
		getText: function(){
			// summary: return the text name of this command
			return this._text;
		},
		getState: function(){
			// summary:
			//		Return the state of the command. The default behavior is
			//		to only dependant on whether is in source mode: if not, 
			//		enabled, otherwise disabled.
			return this._editor._inSourceMode ? dojo.widget.Editor2Manager.commandState.Disabled : dojo.widget.Editor2Manager.commandState.Enabled;
		},
		destroy: function(){
			// summary: Destructor
		}
	}
);

dojo.lang.declare("dojo.widget.Editor2BrowserCommand", dojo.widget.Editor2Command, 
	function(editor,name){
		// summary:
		//		dojo.widget.Editor2BrowserCommand is the base class for all the browser built
		//		in commands
		var browserCommandNames = dojo.i18n.getLocalization("dojo.widget", "Editor2BrowserCommand", editor.lang);
		var text = browserCommandNames[name.toLowerCase()];
		if(text){
			this._text = text;
		}
	},
{
		execute: function(para){
			this._editor.execCommand(this._name, para);
		},
		getState: function(){
			if(this._editor._lastStateTimestamp > this._updateTime || this._state == undefined){
				this._updateTime = this._editor._lastStateTimestamp;
				try{
					if(this._editor.queryCommandEnabled(this._name)){
						if(this._editor.queryCommandState(this._name)){
							this._state = dojo.widget.Editor2Manager.commandState.Latched;
						}else{
							this._state = dojo.widget.Editor2Manager.commandState.Enabled;
						}
					}else{
						this._state = dojo.widget.Editor2Manager.commandState.Disabled;
					}
				}catch (e) {
					//dojo.debug("exception when getting state for command "+this._name+": "+e);
					this._state = dojo.widget.Editor2Manager.commandState.Enabled;
				}
			}
			return this._state;
		},
		getValue: function(){
			try{
				return this._editor.queryCommandValue(this._name);
			}catch(e){}
		}
	}
);

dojo.widget.Editor2ToolbarGroups = {
	// summary: keeping track of all available share toolbar groups
};

dojo.widget.defineWidget(
	"dojo.widget.Editor2",
	dojo.widget.RichText,
	function(){
		this._loadedCommands={};
	},
	{
		// summary:
		//		dojo.widget.Editor2 is the WYSIWYG editor in dojo with toolbar. It supports a plugin
		//		framework which can be used to extend the functionalities of the editor, such as
		//		adding a context menu, table operation etc.
		// description:
		//		Plugins are available using dojo's require syntax. Please find available built-in plugins
		//		under src/widget/Editor2Plugin.

//		// saveUrl: String: url to which save action should send content to
//		saveUrl: "",
//		// saveMethod: String: HTTP method for save (post or get)
//		saveMethod: "post",
//		saveArgName: "editorContent",
//		closeOnSave: false,

		toolbarConfig: {},
		// toolbarAlwaysVisible: Boolean: Whether the toolbar should scroll to keep it in the view
		toolbarAlwaysVisible: false,

//		htmlEditing: false,

		toolbarWidget: null,
		scrollInterval: null,

		// toolbarTemplatePath: dojo.uri.Uri
		//		to specify the template file for the toolbar
		toolbarTemplatePath: dojo.uri.cache.set(dojo.uri.moduleUri("dojo.widget", "templates/EditorToolbarOneline.html"), "<div class=\"EditorToolbarDomNode EditorToolbarSmallBg\">\n\t<table cellpadding=\"1\" cellspacing=\"0\" border=\"0\">\n\t\t<tbody>\n\t\t\t<tr valign=\"top\" align=\"left\">\n\t\t\t\t<td>\n\t\t\t\t\t<span class=\"iconContainer dojoEditorToolbarItem\" dojoETItemName=\"htmltoggle\">\n\t\t\t\t\t\t<span class=\"dojoE2TBIcon\" \n\t\t\t\t\t\tstyle=\"background-image: none; width: 30px;\" >&lt;h&gt;</span>\n\t\t\t\t\t</span>\n\t\t\t\t</td>\n\t\t\t\t<td>\n\t\t\t\t\t<span class=\"iconContainer dojoEditorToolbarItem\" dojoETItemName=\"copy\">\n\t\t\t\t\t\t<span class=\"dojoE2TBIcon dojoE2TBIcon_Copy\">&nbsp;</span>\n\t\t\t\t\t</span>\n\t\t\t\t</td>\n\t\t\t\t<td>\n\t\t\t\t\t<span class=\"iconContainer dojoEditorToolbarItem\" dojoETItemName=\"paste\">\n\t\t\t\t\t\t<span class=\"dojoE2TBIcon dojoE2TBIcon_Paste\">&nbsp;</span>\n\t\t\t\t\t</span>\n\t\t\t\t</td>\n\t\t\t\t<td>\n\t\t\t\t\t<span class=\"iconContainer dojoEditorToolbarItem\" dojoETItemName=\"undo\">\n\t\t\t\t\t\t<!-- FIXME: should we have the text \"undo\" here? -->\n\t\t\t\t\t\t<span class=\"dojoE2TBIcon dojoE2TBIcon_Undo\">&nbsp;</span>\n\t\t\t\t\t</span>\n\t\t\t\t</td>\n\t\t\t\t<td>\n\t\t\t\t\t<span class=\"iconContainer dojoEditorToolbarItem\" dojoETItemName=\"redo\">\n\t\t\t\t\t\t<span class=\"dojoE2TBIcon dojoE2TBIcon_Redo\">&nbsp;</span>\n\t\t\t\t\t</span>\n\t\t\t\t</td>\n\t\t\t\t<td isSpacer=\"true\">\n\t\t\t\t\t<span class=\"iconContainer\">\n\t\t\t\t\t\t<span class=\"dojoE2TBIcon dojoE2TBIcon_Sep\"\tstyle=\"width: 5px; min-width: 5px;\"></span>\n\t\t\t\t\t</span>\n\t\t\t\t</td>\n\t\t\t\t<td>\n\t\t\t\t\t<span class=\"iconContainer dojoEditorToolbarItem\" dojoETItemName=\"createlink\">\n\t\t\t\t\t\t<span class=\"dojoE2TBIcon dojoE2TBIcon_Link\">&nbsp;</span>\n\t\t\t\t\t</span>\n\t\t\t\t</td>\n\t\t\t\t<td>\n\t\t\t\t\t<span class=\"iconContainer dojoEditorToolbarItem\" dojoETItemName=\"insertimage\">\n\t\t\t\t\t\t<span class=\"dojoE2TBIcon dojoE2TBIcon_Image\">&nbsp;</span>\n\t\t\t\t\t</span>\n\t\t\t\t</td>\n\t\t\t\t<td>\n\t\t\t\t\t<span class=\"iconContainer dojoEditorToolbarItem\" dojoETItemName=\"inserthorizontalrule\">\n\t\t\t\t\t\t<span class=\"dojoE2TBIcon dojoE2TBIcon_HorizontalLine \">&nbsp;</span>\n\t\t\t\t\t</span>\n\t\t\t\t</td>\n\t\t\t\t<td>\n\t\t\t\t\t<span class=\"iconContainer dojoEditorToolbarItem\" dojoETItemName=\"bold\">\n\t\t\t\t\t\t<span class=\"dojoE2TBIcon dojoE2TBIcon_Bold\">&nbsp;</span>\n\t\t\t\t\t</span>\n\t\t\t\t</td>\n\t\t\t\t<td>\n\t\t\t\t\t<span class=\"iconContainer dojoEditorToolbarItem\" dojoETItemName=\"italic\">\n\t\t\t\t\t\t<span class=\"dojoE2TBIcon dojoE2TBIcon_Italic\">&nbsp;</span>\n\t\t\t\t\t</span>\n\t\t\t\t</td>\n\t\t\t\t<td>\n\t\t\t\t\t<span class=\"iconContainer dojoEditorToolbarItem\" dojoETItemName=\"underline\">\n\t\t\t\t\t\t<span class=\"dojoE2TBIcon dojoE2TBIcon_Underline\">&nbsp;</span>\n\t\t\t\t\t</span>\n\t\t\t\t</td>\n\t\t\t\t<td>\n\t\t\t\t\t<span class=\"iconContainer dojoEditorToolbarItem\" dojoETItemName=\"strikethrough\">\n\t\t\t\t\t\t<span \n\t\t\t\t\t\t\tclass=\"dojoE2TBIcon dojoE2TBIcon_StrikeThrough\">&nbsp;</span>\n\t\t\t\t\t</span>\n\t\t\t\t</td>\n\t\t\t\t<td isSpacer=\"true\">\n\t\t\t\t\t<span class=\"iconContainer\">\n\t\t\t\t\t\t<span class=\"dojoE2TBIcon dojoE2TBIcon_Sep\" \n\t\t\t\t\t\t\tstyle=\"width: 5px; min-width: 5px;\"></span>\n\t\t\t\t\t</span>\n\t\t\t\t</td>\n\t\t\t\t<td>\n\t\t\t\t\t<span class=\"iconContainer dojoEditorToolbarItem\" dojoETItemName=\"insertunorderedlist\">\n\t\t\t\t\t\t<span \n\t\t\t\t\t\t\tclass=\"dojoE2TBIcon dojoE2TBIcon_BulletedList\">&nbsp;</span>\n\t\t\t\t\t</span>\n\t\t\t\t</td>\n\t\t\t\t<td>\n\t\t\t\t\t<span class=\"iconContainer dojoEditorToolbarItem\" dojoETItemName=\"insertorderedlist\">\n\t\t\t\t\t\t<span \n\t\t\t\t\t\t\tclass=\"dojoE2TBIcon dojoE2TBIcon_NumberedList\">&nbsp;</span>\n\t\t\t\t\t</span>\n\t\t\t\t</td>\n\t\t\t\t<td isSpacer=\"true\">\n\t\t\t\t\t<span class=\"iconContainer\">\n\t\t\t\t\t\t<span class=\"dojoE2TBIcon dojoE2TBIcon_Sep\" style=\"width: 5px; min-width: 5px;\"></span>\n\t\t\t\t\t</span>\n\t\t\t\t</td>\n\t\t\t\t<td>\n\t\t\t\t\t<span class=\"iconContainer dojoEditorToolbarItem\" dojoETItemName=\"indent\">\n\t\t\t\t\t\t<span class=\"dojoE2TBIcon dojoE2TBIcon_Indent\" \n\t\t\t\t\t\t\tunselectable=\"on\">&nbsp;</span>\n\t\t\t\t\t</span>\n\t\t\t\t</td>\n\t\t\t\t<td>\n\t\t\t\t\t<span class=\"iconContainer dojoEditorToolbarItem\" dojoETItemName=\"outdent\">\n\t\t\t\t\t\t<span class=\"dojoE2TBIcon dojoE2TBIcon_Outdent\" \n\t\t\t\t\t\t\tunselectable=\"on\">&nbsp;</span>\n\t\t\t\t\t</span>\n\t\t\t\t</td>\n\t\t\t\t<td isSpacer=\"true\">\n\t\t\t\t\t<span class=\"iconContainer\">\n\t\t\t\t\t\t<span class=\"dojoE2TBIcon dojoE2TBIcon_Sep\" style=\"width: 5px; min-width: 5px;\"></span>\n\t\t\t\t\t</span>\n\t\t\t\t</td>\n\t\t\t\t<td>\n\t\t\t\t\t<span class=\"iconContainer dojoEditorToolbarItem\" dojoETItemName=\"forecolor\">\n\t\t\t\t\t\t<span class=\"dojoE2TBIcon dojoE2TBIcon_TextColor\" \n\t\t\t\t\t\t\tunselectable=\"on\">&nbsp;</span>\n\t\t\t\t\t</span>\n\t\t\t\t</td>\n\t\t\t\t<td>\n\t\t\t\t\t<span class=\"iconContainer dojoEditorToolbarItem\" dojoETItemName=\"hilitecolor\">\n\t\t\t\t\t\t<span class=\"dojoE2TBIcon dojoE2TBIcon_BackgroundColor\" \n\t\t\t\t\t\t\tunselectable=\"on\">&nbsp;</span>\n\t\t\t\t\t</span>\n\t\t\t\t</td>\n\t\t\t\t<td isSpacer=\"true\">\n\t\t\t\t\t<span class=\"iconContainer\">\n\t\t\t\t\t\t<span class=\"dojoE2TBIcon dojoE2TBIcon_Sep\" style=\"width: 5px; min-width: 5px;\"></span>\n\t\t\t\t\t</span>\n\t\t\t\t</td>\n\t\t\t\t<td>\n\t\t\t\t\t<span class=\"iconContainer dojoEditorToolbarItem\" dojoETItemName=\"justifyleft\">\n\t\t\t\t\t\t<span class=\"dojoE2TBIcon dojoE2TBIcon_LeftJustify\">&nbsp;</span>\n\t\t\t\t\t</span>\n\t\t\t\t</td>\n\t\t\t\t<td>\n\t\t\t\t\t<span class=\"iconContainer dojoEditorToolbarItem\" dojoETItemName=\"justifycenter\">\n\t\t\t\t\t\t<span class=\"dojoE2TBIcon dojoE2TBIcon_CenterJustify\">&nbsp;</span>\n\t\t\t\t\t</span>\n\t\t\t\t</td>\n\t\t\t\t<td>\n\t\t\t\t\t<span class=\"iconContainer dojoEditorToolbarItem\" dojoETItemName=\"justifyright\">\n\t\t\t\t\t\t<span class=\"dojoE2TBIcon dojoE2TBIcon_RightJustify\">&nbsp;</span>\n\t\t\t\t\t</span>\n\t\t\t\t</td>\n\t\t\t\t<td>\n\t\t\t\t\t<span class=\"iconContainer dojoEditorToolbarItem\" dojoETItemName=\"justifyfull\">\n\t\t\t\t\t\t<span class=\"dojoE2TBIcon dojoE2TBIcon_BlockJustify\">&nbsp;</span>\n\t\t\t\t\t</span>\n\t\t\t\t</td>\t\n\t\t\t\t<td>\n\t\t\t\t\t<select class=\"dojoEditorToolbarItem\" dojoETItemName=\"plainformatblock\">\n\t\t\t\t\t\t<!-- FIXME: using \"p\" here inserts a paragraph in most cases! -->\n\t\t\t\t\t\t<option value=\"\">-- format --</option>\n\t\t\t\t\t\t<option value=\"p\">Normal</option>\n\t\t\t\t\t\t<option value=\"pre\">Fixed Font</option>\n\t\t\t\t\t\t<option value=\"h1\">Main Heading</option>\n\t\t\t\t\t\t<option value=\"h2\">Section Heading</option>\n\t\t\t\t\t\t<option value=\"h3\">Sub-Heading</option>\n\t\t\t\t\t\t<!-- <option value=\"blockquote\">Block Quote</option> -->\n\t\t\t\t\t</select>\n\t\t\t\t</td>\n\t\t\t\t<td><!-- uncomment to enable save button -->\n\t\t\t\t\t<!-- save -->\n\t\t\t\t\t<!--span class=\"iconContainer dojoEditorToolbarItem\" dojoETItemName=\"save\">\n\t\t\t\t\t\t<span class=\"dojoE2TBIcon dojoE2TBIcon_Save\">&nbsp;</span>\n\t\t\t\t\t</span-->\n\t\t\t\t</td>\n\t\t\t\t<td width=\"*\">&nbsp;</td>\n\t\t\t</tr>\n\t\t</tbody>\n\t</table>\n</div>\n"),

		// toolbarTemplateCssPath: dojo.uri.Uri
		//		to specify the css file for the toolbar
		toolbarTemplateCssPath: dojo.uri.cache.set(dojo.uri.moduleUri("dojo.widget", "templates/EditorToolbar.css"), ".StyleDropdownContainer {\n\tposition: absolute;\n\tz-index: 1000;\n\toverflow: auto;\n\tcursor: default;\n\twidth: 250px;\n\theight: 250px;\n\tbackground-color: white;\n\tborder: 1px solid black;\n}\n\n.ColorDropdownContainer {\n\tposition: absolute;\n\tz-index: 1000;\n\toverflow: auto;\n\tcursor: default;\n\twidth: 250px;\n\theight: 150px;\n\tbackground-color: white;\n\tborder: 1px solid black;\n}\n\n.EditorToolbarDomNode {\n\tbackground-image: url(buttons/bg-fade.png);\n\tbackground-repeat: repeat-x;\n\tbackground-position: 0px -50px;\n}\n\n.EditorToolbarSmallBg {\n\tbackground-image: url(images/toolbar-bg.gif);\n\tbackground-repeat: repeat-x;\n\tbackground-position: 0px 0px;\n}\n\n/*\nbody {\n\tbackground:url(images/blank.gif) fixed;\n}*/\n\n.IEFixedToolbar {\n\tposition:absolute;\n\t/* top:0; */\n\ttop: expression(eval((document.documentElement||document.body).scrollTop));\n}\n\ndiv.bigIcon {\n\twidth: 40px;\n\theight: 40px; \n\t/* background-color: white; */\n\t/* border: 1px solid #a6a7a3; */\n\tfont-family: Verdana, Trebuchet, Tahoma, Arial;\n}\n\n.iconContainer {\n\tfont-family: Verdana, Trebuchet, Tahoma, Arial;\n\tfont-size: 13px;\n\tfloat: left;\n\theight: 18px;\n\tdisplay: block;\n\t/* background-color: white; */\n\tcursor: pointer;\n\tpadding: 1px 4px 1px 1px; /* almost the same as a transparent border */\n\tborder: 0px;\n}\n\n.dojoE2TBIcon {\n\tdisplay: block;\n\ttext-align: center;\n\tmin-width: 18px;\n\twidth: 18px;\n\theight: 18px;\n\t/* background-color: #a6a7a3; */\n\tbackground-repeat: no-repeat;\n\tbackground-image: url(buttons/aggregate.gif);\n}\n\n\n.dojoE2TBIcon[class~=dojoE2TBIcon] {\n}\n\n.ToolbarButtonLatched {\n    border: #316ac5 1px solid; !important;\n    padding: 0px 3px 0px 0px; !important; /* make room for border */\n    background-color: #c1d2ee;\n}\n\n.ToolbarButtonHighlighted {\n    border: #316ac5 1px solid; !important;\n    padding: 0px 3px 0px 0px; !important; /* make room for border */\n    background-color: #dff1ff;\n}\n\n.ToolbarButtonDisabled{\n    filter: gray() alpha(opacity=30); /* IE */\n    opacity: 0.30; /* Safari, Opera and Mozilla */\n}\n\n.headingContainer {\n\twidth: 150px;\n\theight: 30px;\n\tmargin: 0px;\n\t/* padding-left: 5px; */\n\toverflow: hidden;\n\tline-height: 25px;\n\tborder-bottom: 1px solid black;\n\tborder-top: 1px solid white;\n}\n\n.EditorToolbarDomNode select {\n\tfont-size: 14px;\n}\n \n.dojoE2TBIcon_Sep { width: 5px; min-width: 5px; max-width: 5px; background-position: 0px 0px}\n.dojoE2TBIcon_Backcolor { background-position: -18px 0px}\n.dojoE2TBIcon_Bold { background-position: -36px 0px}\n.dojoE2TBIcon_Cancel { background-position: -54px 0px}\n.dojoE2TBIcon_Copy { background-position: -72px 0px}\n.dojoE2TBIcon_Link { background-position: -90px 0px}\n.dojoE2TBIcon_Cut { background-position: -108px 0px}\n.dojoE2TBIcon_Delete { background-position: -126px 0px}\n.dojoE2TBIcon_TextColor { background-position: -144px 0px}\n.dojoE2TBIcon_BackgroundColor { background-position: -162px 0px}\n.dojoE2TBIcon_Indent { background-position: -180px 0px}\n.dojoE2TBIcon_HorizontalLine { background-position: -198px 0px}\n.dojoE2TBIcon_Image { background-position: -216px 0px}\n.dojoE2TBIcon_NumberedList { background-position: -234px 0px}\n.dojoE2TBIcon_Table { background-position: -252px 0px}\n.dojoE2TBIcon_BulletedList { background-position: -270px 0px}\n.dojoE2TBIcon_Italic { background-position: -288px 0px}\n.dojoE2TBIcon_CenterJustify { background-position: -306px 0px}\n.dojoE2TBIcon_BlockJustify { background-position: -324px 0px}\n.dojoE2TBIcon_LeftJustify { background-position: -342px 0px}\n.dojoE2TBIcon_RightJustify { background-position: -360px 0px}\n.dojoE2TBIcon_left_to_right { background-position: -378px 0px}\n.dojoE2TBIcon_list_bullet_indent { background-position: -396px 0px}\n.dojoE2TBIcon_list_bullet_outdent { background-position: -414px 0px}\n.dojoE2TBIcon_list_num_indent { background-position: -432px 0px}\n.dojoE2TBIcon_list_num_outdent { background-position: -450px 0px}\n.dojoE2TBIcon_Outdent { background-position: -468px 0px}\n.dojoE2TBIcon_Paste { background-position: -486px 0px}\n.dojoE2TBIcon_Redo { background-position: -504px 0px}\ndojoE2TBIcon_RemoveFormat { background-position: -522px 0px}\n.dojoE2TBIcon_right_to_left { background-position: -540px 0px}\n.dojoE2TBIcon_Save { background-position: -558px 0px}\n.dojoE2TBIcon_Space { background-position: -576px 0px}\n.dojoE2TBIcon_StrikeThrough { background-position: -594px 0px}\n.dojoE2TBIcon_Subscript { background-position: -612px 0px}\n.dojoE2TBIcon_Superscript { background-position: -630px 0px}\n.dojoE2TBIcon_Underline { background-position: -648px 0px}\n.dojoE2TBIcon_Undo { background-position: -666px 0px}\n.dojoE2TBIcon_WikiWord { background-position: -684px 0px}\n\n"),

		// toolbarPlaceHolder: String
		//		element id to specify where to attach the toolbar
		toolbarPlaceHolder: '',

		_inSourceMode: false,
		_htmlEditNode: null,

		// toolbarGroup: String
		//		This instance of editor will share the same toolbar with other editor with the same toolbarGroup. 
		//		By default, toolbarGroup is empty and standalone toolbar is used for this instance.
		toolbarGroup: '',

		// contextMenuGroupSet: String: specify which context menu set should be used for this instance. Include ContextMenu plugin to use this
		contextMenuGroupSet: '',

		editorOnLoad: function(){
			// summary:
			//		Create toolbar and other initialization routines. This is called after
			//		the finish of the loading of document in the editing element

//			dojo.profile.start("dojo.widget.Editor2::editorOnLoad");
			this.iframe.id=this.widgetId;
			if(dojo.render.html.ie){
				//IE always focus into the created new instance, try to workaround that by
				//logging the real instance which should be focused, and in setFocus, always
				//try to focus in the correct instance
				if(this.focusOnLoad){
					this._shared.focusOnLoadInstance=this;
				}else{
					this._fixIEFocus=true;
				}
			}
			dojo.event.topic.publish("dojo.widget.Editor2::preLoadingToolbar", this);
			if(this.toolbarAlwaysVisible){
				dojo.require("dojo.widget.Editor2Plugin.AlwaysShowToolbar");
			}

			if(this.toolbarWidget){
				this.toolbarWidget.show();
				//re-add the toolbar to the new domNode (caused by open() on another element)
				dojo.html.insertBefore(this.toolbarWidget.domNode, this.domNode.firstChild);
			}else{
				if(this.toolbarGroup){
					if(dojo.widget.Editor2ToolbarGroups[this.toolbarGroup]){
						this.toolbarWidget = dojo.widget.Editor2ToolbarGroups[this.toolbarGroup];
					}
				}
				if(!this.toolbarWidget){
						var tbOpts = {shareGroup: this.toolbarGroup, parent: this, lang: this.lang, config:this.toolbarConfig};
						tbOpts.templateString = dojo.uri.cache.get(this.toolbarTemplatePath);
						if(this.toolbarTemplateCssPath){
							tbOpts.templateCssPath = this.toolbarTemplateCssPath;
							tbOpts.templateCssString = dojo.uri.cache.get(this.toolbarTemplateCssPath);
						}
						if(this.toolbarPlaceHolder){
							this.toolbarWidget = dojo.widget.createWidget("Editor2Toolbar", tbOpts, dojo.byId(this.toolbarPlaceHolder), "after");
						}else{
							this.toolbarWidget = dojo.widget.createWidget("Editor2Toolbar", tbOpts, this.domNode.firstChild, "before");
						}
						if(this.toolbarGroup){
							dojo.widget.Editor2ToolbarGroups[this.toolbarGroup] = this.toolbarWidget;
						}
						dojo.event.connect(this, "close", this.toolbarWidget, "hide");
	
						this.toolbarLoaded();
				}
			}

			dojo.event.topic.registerPublisher("Editor2.clobberFocus", this, "clobberFocus");
			dojo.event.topic.subscribe("Editor2.clobberFocus", this, "setBlur");

			dojo.event.topic.publish("dojo.widget.Editor2::onLoad", this);
//			dojo.profile.end("dojo.widget.Editor2::editorOnLoad");
		},

		//event for plugins to use
		toolbarLoaded: function(){
			// summary:
			//		Fired when the toolbar for this editor is created.
			//		This event is for plugins to use
//			this.updateToolbar();
		},

		//TODO: provide a query mechanism about loaded plugins?
		registerLoadedPlugin: function(/*Object*/obj){
			// summary: Register a plugin which is loaded for this instance
			if(!this.loadedPlugins){
				this.loadedPlugins = [];
			}
			this.loadedPlugins.push(obj);
		},
		unregisterLoadedPlugin: function(/*Object*/obj){
			// summary: Delete a loaded plugin for this instance
			for(var i in this.loadedPlugins){
				if(this.loadedPlugins[i] === obj){
					delete this.loadedPlugins[i];
					return;
				}
			}
			dojo.debug("dojo.widget.Editor2.unregisterLoadedPlugin: unknown plugin object: "+obj);
		},

		//overload the original ones to provide extra commands
		execCommand: function(/*String*/command, argument){
			switch(command.toLowerCase()){
				case 'htmltoggle':
					this.toggleHtmlEditing();
					break;
				default:
					dojo.widget.Editor2.superclass.execCommand.apply(this, arguments);
			}
		},
		queryCommandEnabled: function(/*String*/command, argument){
			switch(command.toLowerCase()){
				case 'htmltoggle':
					return true;
				default:
					if(this._inSourceMode){ return false;}
					return dojo.widget.Editor2.superclass.queryCommandEnabled.apply(this, arguments);
			}
		},
		queryCommandState: function(/*String*/command, argument){
			switch(command.toLowerCase()){
				case 'htmltoggle':
					return this._inSourceMode;
				default:
					return dojo.widget.Editor2.superclass.queryCommandState.apply(this, arguments);
			}
		},

		onClick: function(/*Event*/e){
			dojo.widget.Editor2.superclass.onClick.call(this, e);
			//if Popup is used, call dojo.widget.PopupManager.onClick
			//manually when click in the editing area to close all
			//open popups (dropdowns)
			if(dojo.widget.PopupManager){
				if(!e){ //IE
					e = this.window.event;
				}
				dojo.widget.PopupManager.onClick(e);
			}
		},

		clobberFocus: function(){
			// summary: stub to signal other instances to clobber focus
		},
		toggleHtmlEditing: function(){
			// summary: toggle between WYSIWYG mode and HTML source mode
			if(this===dojo.widget.Editor2Manager.getCurrentInstance()){
				if(!this._inSourceMode){
					var html = this.getValue();
					this._inSourceMode = true;

					if(!this._htmlEditNode){
						this._htmlEditNode = dojo.doc().createElement("textarea");
						dojo.html.insertAfter(this._htmlEditNode, this.editorObject);
						dojo.event.connect(this._htmlEditNode,'onfocus',this,'onFocus');
					}

					this._htmlEditNode.style.display = "";
					this._htmlEditNode.style.width = "100%";
					this._htmlEditNode.style.height = dojo.html.getBorderBox(this.editNode).height+"px";
					this._htmlEditNode.value = html;

					//activeX object (IE) doesn't like to be hidden, so move it outside of screen instead
					with(this.editorObject.style){
						position = "absolute";
						left = "-2000px";
						top = "-2000px";
					}
				}else{
					this._inSourceMode = false;

					//In IE activeX mode, if _htmlEditNode is focused,
					//when toggling, an error would occur, so unfocus it
					this._htmlEditNode.blur();

					with(this.editorObject.style){
						position = "";
						left = "";
						top = "";
					}
					var html = this._htmlEditNode.value;

					dojo.lang.setTimeout(this, "replaceValue", 1, html);
					this._htmlEditNode.style.display = "none";
					this.focus();
				}
				this.onDisplayChanged(null, true);
			}
		},

		setFocus: function(){
			// summary: focus is set on this instance
//			dojo.debug("setFocus: start ",this);
			if(dojo.widget.Editor2Manager.getCurrentInstance() === this){ return; }

			this.clobberFocus();
//			dojo.debug("setFocus:", this);
			dojo.widget.Editor2Manager.setCurrentInstance(this);
		},

		setBlur: function(){
			// summary: focus on this instance is lost
//			 dojo.debug("setBlur:", this);
			//dojo.event.disconnect(this.toolbarWidget, "exec", this, "execCommand");
		},

		saveSelection: function(){
			// summary: save the current selection for restoring it
			this._bookmark = null;
			this._bookmark = dojo.withGlobal(this.window, dojo.html.selection.getBookmark);
		},
		restoreSelection: function(/*Boolean*/keepbookmark){
			// summary: restore the last saved selection
			if(this._bookmark){
				this.focus(); //require for none-activeX IE
				dojo.withGlobal(this.window, "moveToBookmark", dojo.html.selection, [this._bookmark]);
				if(!keepbookmark){
					this._bookmark = null;
				}
			}else{
				dojo.debug("restoreSelection: no saved selection is found!");
			}
		},

		_updateToolbarLastRan: null,
		_updateToolbarTimer: null,
		_updateToolbarFrequency: 500,

		updateToolbar: function(/*Boolean*/force){
			// summary: update the associated toolbar of this Editor2
			if((!this.isLoaded)||(!this.toolbarWidget) || (!this.enabled)){ return; }

			// keeps the toolbar from updating too frequently
			// TODO: generalize this functionality?
			var diff = new Date() - this._updateToolbarLastRan;
			if( (!force)&&(this._updateToolbarLastRan)&&
				((diff < this._updateToolbarFrequency)) ){

				clearTimeout(this._updateToolbarTimer);
				var _this = this;
				this._updateToolbarTimer = setTimeout(function() {
					_this.updateToolbar();
				}, this._updateToolbarFrequency/2);
				return;

			}else{
				this._updateToolbarLastRan = new Date();
			}
			// end frequency checker

//			//IE has the habit of generating events even when this editor is blurred, prevent this
//			if(dojo.widget.Editor2Manager.getCurrentInstance() !== this){ return; }

			this.toolbarWidget.update();
		},

		destroy: function(/*Boolean*/finalize){
			this._htmlEditNode = null;
			dojo.event.disconnect(this, "close", this.toolbarWidget, "hide");
			if(!finalize){
				this.toolbarWidget.destroy();
			}
			dojo.widget.Editor2.superclass.destroy.call(this);
		},

		_lastStateTimestamp: 0,
		onDisplayChanged: function(/*Object*/e, /*Boolean*/forceUpdate){
			this._lastStateTimestamp = (new Date()).getTime();
			dojo.widget.Editor2.superclass.onDisplayChanged.call(this,e);
			this.updateToolbar(forceUpdate);
		},

		onLoad: function(){
			try{
				dojo.widget.Editor2.superclass.onLoad.call(this);
			}catch(e){ // FIXME: debug why this is throwing errors in IE!
				dojo.debug(e);
			}
			this.editorOnLoad();
		},
		_shared: {focusOnLoadInstance:null},
		onFocus: function(){
			if(this._fixIEFocus){
				delete this._fixIEFocus;
				var f=this._shared.focusOnLoadInstance;
				if(f && f!==this){
					f.focus();
					return;
				}
			}
			dojo.widget.Editor2.superclass.onFocus.apply(this,arguments);
			//no need to track focus for IE us onFocus event, see comment in dojo.widget.Editor2Manager.getCurrentInstance()
			if(!dojo.render.html.ie){
				this.setFocus();
			}
		},

		//overload to support source editing mode
		getValue: function(/*Boolean?*/nondistructive){
			if(this._inSourceMode){
				return this._htmlEditNode.value;
			}
			return dojo.widget.Editor2.superclass.getValue.apply(this,arguments);
		},

		replaceValue: function(html){
			if(this._inSourceMode){
				this._htmlEditNode.value = html;
				return;
			}
			dojo.widget.Editor2.superclass.replaceValue.apply(this,arguments);
		},
		getCommand: function(/*String*/name){
			// summary: return a command associated with this instance of editor
			if(this._loadedCommands[name]){
				return this._loadedCommands[name];
			}
			var cmd = dojo.widget.Editor2Manager.getCommand(this, name);
			this._loadedCommands[name] = cmd;
			return cmd;
		},
		// Array: Commands shortcuts. Each element can has up to 3 fields:
		//		1. String: the name of the command
		//		2. String Optional: the char for shortcut key, by default the first char from the command name is used
		//		3. Int Optional: specify the modifier of the shortcut, by default ctrl is used
		shortcuts: [['bold'],['italic'],['underline'],['selectall','a'],['insertunorderedlist','\\']],
		setupDefaultShortcuts: function(){
			// summary: setup default shortcuts using Editor2 commands
			var exec = function(cmd){ return function(){ cmd.execute(); } };
//			if(!dojo.render.html.ie){
//				this.shortcuts.push(['redo','Z']);
//			}
			var self = this;
			dojo.lang.forEach(this.shortcuts, function(item){
				var cmd = self.getCommand(item[0]);
				if(cmd){
					self.addKeyHandler(item[1]?item[1]:item[0].charAt(0), item[2]==undefined?self.KEY_CTRL:item[2], exec(cmd));
				}
			});
//			this.addKeyHandler("s", ctrl, function () { this.save(true); });
		}
	}
);
