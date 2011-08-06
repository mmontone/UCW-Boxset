/*
	Copyright (c) 2004-2006, The Dojo Foundation
	All Rights Reserved.

	Licensed under the Academic Free License version 2.1 or above OR the
	modified BSD license. For more information on Dojo licensing, see:

		http://dojotoolkit.org/community/licensing.shtml
*/

dojo.provide("dojo.widget.Editor2Toolbar");

dojo.require("dojo.lang.*");
dojo.require("dojo.widget.*");
dojo.require("dojo.event.*");
dojo.require("dojo.html.layout");
dojo.require("dojo.html.display");
dojo.require("dojo.widget.RichText");

dojo.lang.declare("dojo.widget.HandlerManager", null,
	function(){
		this._registeredHandlers=[];
	},
{
	// summary: internal base class for handler function management
	registerHandler: function(/*Object*/obj, /*String*/func){
		// summary: register a handler
		// obj: object which has the function to call
		// func: the function in the object
		if(arguments.length == 2){
			this._registeredHandlers.push(function(){return obj[func].apply(obj, arguments);});
		}else{
			/* obj: Function
			    func: null
			    pId: f */
			this._registeredHandlers.push(obj);
		}
	},
	removeHandler: function(func){
		// summary: remove a registered handler
		var i=0,handle,handles=this._registeredHandlers;
		while(handle=handles[i++]){
			if(func === handle){
				delete this._registeredHandlers[--i];
				return;
			}
		}
		dojo.debug("HandlerManager handler "+func+" is not registered, can not remove.");
	},
	destroy: function(){
		var i=this._registeredHandlers.length-1,handles=this._registeredHandlers;
		while(handles[i--]){
			delete this._registeredHandlers[i+1];
		}
	}
});

dojo.widget.Editor2ToolbarItemManager = new dojo.widget.HandlerManager;
dojo.lang.mixin(dojo.widget.Editor2ToolbarItemManager,
{
	getToolbarItem: function(/*String*/name){
		// summary: return a toobar item with the given name
		var item;
		name = name.toLowerCase();
		var i=0, handle, handles=this._registeredHandlers;
		while(handle=handles[i++]){
			item = handle(name);
			if(item){
				return item;
			}
		}

		_deprecated = function(cmd, plugin){
			if(!dojo.widget['Editor2Plugin'] || !dojo.widget.Editor2Plugin[plugin]){
				dojo.deprecated('Toolbar item '+name+" is now defined in plugin dojo.widget.Editor2Plugin."+plugin+". It shall be required explicitly", "0.6");
				dojo['require']("dojo.widget.Editor2Plugin."+plugin); //avoid loading by the build
			}
		}
		if(name == 'forecolor' || name == 'hilitecolor'){
			_deprecated(name, 'ColorPicker')
		}

		switch(name){
			case 'forecolor':
			case 'hilitecolor':
				item = new dojo.widget.Editor2ToolbarColorPaletteButton(name);
				break;
			case 'plainformatblock':
				item = new dojo.widget.Editor2ToolbarFormatBlockPlainSelect("formatblock");
				break;
			//TODO:
//			case 'inserthtml':
			case 'blockdirltr':
			case 'blockdirrtl':
			case 'dirltr':
			case 'dirrtl':
			case 'inlinedirltr':
			case 'inlinedirrtl':
				dojo.debug("Not yet implemented toolbar item: "+name);
				break;
			default:
				//simple button, which just call a command with name
				item = new dojo.widget.Editor2ToolbarButton(name);
//				dojo.debug("dojo.widget.Editor2ToolbarItemManager.getToolbarItem: Unknown toolbar item: "+name);
		}
		return item;
	}
});

dojo.addOnUnload(dojo.widget.Editor2ToolbarItemManager, "destroy");

dojo.declare("dojo.widget.Editor2ToolbarButton", null,
	function(name){
		this._name = name;
//		this._command = editor.getCommand(name);
	},
{
	// summary:
	//		dojo.widget.Editor2ToolbarButton is the base class for all toolbar item in Editor2Toolbar
	create: function(/*DomNode*/node, /*dojo.widget.Editor2Toolbar*/toolbar, /*Boolean*/nohover){
		// summary: create the item
		// node: the dom node which is the root of this toolbar item
		// toolbar: the Editor2Toolbar widget this toolbar item belonging to
		// nohover: whether this item in charge of highlight this item
		this._domNode = node;
		var cmd = toolbar.parent.getCommand(this._name); //FIXME: maybe an issue if different instance has different language
		if(cmd){
			this._domNode.title = cmd.getText();
		}
		//make this unselectable: different browsers
		//use different properties for this, so use
		//js do it automatically
		this.disableSelection(this._domNode);

		this._parentToolbar = toolbar;
		dojo.event.connect(this._domNode, 'onclick', this, 'onClick');
		if(!nohover){
			dojo.event.connect(this._domNode, 'onmouseover', this, 'onMouseOver');
			dojo.event.connect(this._domNode, 'onmouseout', this, 'onMouseOut');
		}
	},
	disableSelection: function(/*DomNode*/rootnode){
		// summary: disable selection on the passed node and all its children
		dojo.html.disableSelection(rootnode);
		var i=0,node,nodes = rootnode.all || rootnode.getElementsByTagName("*");
		while(node=nodes[i++]){
			dojo.html.disableSelection(node);
		}
	},
	getMainComponent: function(){
		return dojo.widget.Editor2Manager.getCurrentInstance();
	},
	onMouseOver: function(){
		var curInst = this.getMainComponent();
		if(curInst){
			var _command = curInst.getCommand(this._name);
			if(_command && _command.getState() != dojo.widget.Editor2Manager.commandState.Disabled){
				this.highlightToolbarItem();
			}
		}
	},
	onMouseOut: function(){
		this.unhighlightToolbarItem();
	},
	destroy: function(){
		// summary: destructor
		this._domNode = null;
//		delete this._command;
		this._parentToolbar = null;
	},
	onClick: function(e){
		if(this._domNode && !this._domNode.disabled && this._parentToolbar.checkAvailability()){
			e.preventDefault();
			e.stopPropagation();
			var curInst = this.getMainComponent();
			if(curInst){
				var _command = curInst.getCommand(this._name);
				if(_command){
					_command.execute();
				}
			}
		}
	},
	refreshState: function(){
		// summary: update the state of the toolbar item
		var curInst = this.getMainComponent();
		var em = dojo.widget.Editor2Manager;
		if(curInst){
			var _command = curInst.getCommand(this._name);
			if(_command){
				var state = _command.getState();
				if(state != this._lastState){
					switch(state){
						case em.commandState.Latched:
							this.latchToolbarItem();
							break;
						case em.commandState.Enabled:
							this.enableToolbarItem();
							break;
						case em.commandState.Disabled:
						default:
							this.disableToolbarItem();
					}
					this._lastState = state;
				}
			}
		}
		return em.commandState.Enabled;
	},

	latchToolbarItem: function(){
		this._domNode.disabled = false;
		this.removeToolbarItemStyle(this._domNode);
		dojo.html.addClass(this._domNode, 'ToolbarButtonLatched');
	},

	enableToolbarItem: function(){
		this._domNode.disabled = false;
		this.removeToolbarItemStyle(this._domNode);
		dojo.html.addClass(this._domNode, 'ToolbarButtonEnabled');
	},

	disableToolbarItem: function(){
		this._domNode.disabled = true;
		this.removeToolbarItemStyle(this._domNode);
		dojo.html.addClass(this._domNode, 'ToolbarButtonDisabled');
	},

	highlightToolbarItem: function(){
		dojo.html.addClass(this._domNode, 'ToolbarButtonHighlighted');
	},

	unhighlightToolbarItem: function(){
		dojo.html.removeClass(this._domNode, 'ToolbarButtonHighlighted');
	},

	removeToolbarItemStyle: function(){
		dojo.html.removeClass(this._domNode, 'ToolbarButtonEnabled');
		dojo.html.removeClass(this._domNode, 'ToolbarButtonLatched');
		dojo.html.removeClass(this._domNode, 'ToolbarButtonDisabled');
		this.unhighlightToolbarItem();
	}
});

dojo.declare("dojo.widget.Editor2ToolbarFormatBlockPlainSelect", dojo.widget.Editor2ToolbarButton, {
	// summary: dojo.widget.Editor2ToolbarFormatBlockPlainSelect provides a simple select for setting block format

	create: function(node, toolbar){
//		dojo.widget.Editor2ToolbarFormatBlockPlainSelect.superclass.create.apply(this, arguments);
		this._domNode = node;
		this._parentToolbar = toolbar;
		//TODO: check node is a select
		this._domNode = node;
		this.disableSelection(this._domNode);
		dojo.event.connect(this._domNode, 'onchange', this, 'onChange');
	},

	destroy: function(){
		this._domNode = null;
	},

	onChange: function(){
		if(this._parentToolbar.checkAvailability()){
			var sv = this._domNode.value.toLowerCase();
			var curInst = this.getMainComponent();
			if(curInst){
				var _command = curInst.getCommand(this._name);
				if(_command){
					_command.execute(sv);
				}
			}
		}
	},

	refreshState: function(){
		if(this._domNode){
			dojo.widget.Editor2ToolbarFormatBlockPlainSelect.superclass.refreshState.call(this);
			var curInst = this.getMainComponent();
			if(curInst){
				var _command = curInst.getCommand(this._name);
				if(_command){
					var format = _command.getValue();
					if(!format){ format = ""; }
					dojo.lang.forEach(this._domNode.options, function(item){
						if(item.value.toLowerCase() == format.toLowerCase()){
							item.selected = true;
						}
					});
				}
			}
		}
	}
});

dojo.widget.defineWidget(
	"dojo.widget.Editor2Toolbar",
	dojo.widget.HtmlWidget,
	function(){
		dojo.event.connect(this, "fillInTemplate", dojo.lang.hitch(this, function(){
			if(dojo.render.html.ie){
				this.domNode.style.zoom = 1.0;
			}
		}));
	},
	{
		// summary:
		//		dojo.widget.Editor2Toolbar is the main widget for the toolbar associated with an Editor2

		templateString:"<div dojoAttachPoint=\"domNode\" class=\"EditorToolbarDomNode\" unselectable=\"on\">\n\t<table cellpadding=\"3\" cellspacing=\"0\" border=\"0\">\n\t\t<!--\n\t\t\tour toolbar should look something like:\n\n\t\t\t+=======+=======+=======+=============================================+\n\t\t\t| w   w | style | copy  | bo | it | un | le | ce | ri |\n\t\t\t| w w w | style |=======|==============|==============|\n\t\t\t|  w w  | style | paste |  undo | redo | change style |\n\t\t\t+=======+=======+=======+=============================================+\n\t\t-->\n\t\t<tbody>\n\t\t\t<tr valign=\"top\">\n\t\t\t\t<td rowspan=\"2\">\n\t\t\t\t\t<div class=\"bigIcon\" dojoAttachPoint=\"wikiWordButton\"\n\t\t\t\t\t\tdojoOnClick=\"wikiWordClick; buttonClick;\">\n\t\t\t\t\t\t<span style=\"font-size: 30px; margin-left: 5px;\">\n\t\t\t\t\t\t\tW\n\t\t\t\t\t\t</span>\n\t\t\t\t\t</div>\n\t\t\t\t</td>\n\t\t\t\t<td rowspan=\"2\">\n\t\t\t\t\t<div class=\"bigIcon\" dojoAttachPoint=\"styleDropdownButton\"\n\t\t\t\t\t\tdojoOnClick=\"styleDropdownClick; buttonClick;\">\n\t\t\t\t\t\t<span unselectable=\"on\"\n\t\t\t\t\t\t\tstyle=\"font-size: 30px; margin-left: 5px;\">\n\t\t\t\t\t\t\tS\n\t\t\t\t\t\t</span>\n\t\t\t\t\t</div>\n\t\t\t\t\t<div class=\"StyleDropdownContainer\" style=\"display: none;\"\n\t\t\t\t\t\tdojoAttachPoint=\"styleDropdownContainer\">\n\t\t\t\t\t\t<table cellpadding=\"0\" cellspacing=\"0\" border=\"0\"\n\t\t\t\t\t\t\theight=\"100%\" width=\"100%\">\n\t\t\t\t\t\t\t<tr valign=\"top\">\n\t\t\t\t\t\t\t\t<td rowspan=\"2\">\n\t\t\t\t\t\t\t\t\t<div style=\"height: 245px; overflow: auto;\">\n\t\t\t\t\t\t\t\t\t\t<div class=\"headingContainer\"\n\t\t\t\t\t\t\t\t\t\t\tunselectable=\"on\"\n\t\t\t\t\t\t\t\t\t\t\tdojoOnClick=\"normalTextClick\">normal</div>\n\t\t\t\t\t\t\t\t\t\t<h1 class=\"headingContainer\"\n\t\t\t\t\t\t\t\t\t\t\tunselectable=\"on\"\n\t\t\t\t\t\t\t\t\t\t\tdojoOnClick=\"h1TextClick\">Heading 1</h1>\n\t\t\t\t\t\t\t\t\t\t<h2 class=\"headingContainer\"\n\t\t\t\t\t\t\t\t\t\t\tunselectable=\"on\"\n\t\t\t\t\t\t\t\t\t\t\tdojoOnClick=\"h2TextClick\">Heading 2</h2>\n\t\t\t\t\t\t\t\t\t\t<h3 class=\"headingContainer\"\n\t\t\t\t\t\t\t\t\t\t\tunselectable=\"on\"\n\t\t\t\t\t\t\t\t\t\t\tdojoOnClick=\"h3TextClick\">Heading 3</h3>\n\t\t\t\t\t\t\t\t\t\t<h4 class=\"headingContainer\"\n\t\t\t\t\t\t\t\t\t\t\tunselectable=\"on\"\n\t\t\t\t\t\t\t\t\t\t\tdojoOnClick=\"h4TextClick\">Heading 4</h4>\n\t\t\t\t\t\t\t\t\t\t<div class=\"headingContainer\"\n\t\t\t\t\t\t\t\t\t\t\tunselectable=\"on\"\n\t\t\t\t\t\t\t\t\t\t\tdojoOnClick=\"blahTextClick\">blah</div>\n\t\t\t\t\t\t\t\t\t\t<div class=\"headingContainer\"\n\t\t\t\t\t\t\t\t\t\t\tunselectable=\"on\"\n\t\t\t\t\t\t\t\t\t\t\tdojoOnClick=\"blahTextClick\">blah</div>\n\t\t\t\t\t\t\t\t\t\t<div class=\"headingContainer\"\n\t\t\t\t\t\t\t\t\t\t\tunselectable=\"on\"\n\t\t\t\t\t\t\t\t\t\t\tdojoOnClick=\"blahTextClick\">blah</div>\n\t\t\t\t\t\t\t\t\t\t<div class=\"headingContainer\">blah</div>\n\t\t\t\t\t\t\t\t\t\t<div class=\"headingContainer\">blah</div>\n\t\t\t\t\t\t\t\t\t\t<div class=\"headingContainer\">blah</div>\n\t\t\t\t\t\t\t\t\t\t<div class=\"headingContainer\">blah</div>\n\t\t\t\t\t\t\t\t\t</div>\n\t\t\t\t\t\t\t\t</td>\n\t\t\t\t\t\t\t\t<!--\n\t\t\t\t\t\t\t\t<td>\n\t\t\t\t\t\t\t\t\t<span class=\"iconContainer\" dojoOnClick=\"buttonClick;\">\n\t\t\t\t\t\t\t\t\t\t<span class=\"icon justifyleft\" \n\t\t\t\t\t\t\t\t\t\t\tstyle=\"float: left;\">&nbsp;</span>\n\t\t\t\t\t\t\t\t\t</span>\n\t\t\t\t\t\t\t\t\t<span class=\"iconContainer\" dojoOnClick=\"buttonClick;\">\n\t\t\t\t\t\t\t\t\t\t<span class=\"icon justifycenter\" \n\t\t\t\t\t\t\t\t\t\t\tstyle=\"float: left;\">&nbsp;</span>\n\t\t\t\t\t\t\t\t\t</span>\n\t\t\t\t\t\t\t\t\t<span class=\"iconContainer\" dojoOnClick=\"buttonClick;\">\n\t\t\t\t\t\t\t\t\t\t<span class=\"icon justifyright\" \n\t\t\t\t\t\t\t\t\t\t\tstyle=\"float: left;\">&nbsp;</span>\n\t\t\t\t\t\t\t\t\t</span>\n\t\t\t\t\t\t\t\t\t<span class=\"iconContainer\" dojoOnClick=\"buttonClick;\">\n\t\t\t\t\t\t\t\t\t\t<span class=\"icon justifyfull\" \n\t\t\t\t\t\t\t\t\t\t\tstyle=\"float: left;\">&nbsp;</span>\n\t\t\t\t\t\t\t\t\t</span>\n\t\t\t\t\t\t\t\t</td>\n\t\t\t\t\t\t\t\t-->\n\t\t\t\t\t\t\t</tr>\n\t\t\t\t\t\t\t<tr valign=\"top\">\n\t\t\t\t\t\t\t\t<td>\n\t\t\t\t\t\t\t\t\tthud\n\t\t\t\t\t\t\t\t</td>\n\t\t\t\t\t\t\t</tr>\n\t\t\t\t\t\t</table>\n\t\t\t\t\t</div>\n\t\t\t\t</td>\n\t\t\t\t<td>\n\t\t\t\t\t<!-- copy -->\n\t\t\t\t\t<span class=\"iconContainer\" dojoAttachPoint=\"copyButton\"\n\t\t\t\t\t\tunselectable=\"on\"\n\t\t\t\t\t\tdojoOnClick=\"copyClick; buttonClick;\">\n\t\t\t\t\t\t<span class=\"icon copy\" \n\t\t\t\t\t\t\tunselectable=\"on\"\n\t\t\t\t\t\t\tstyle=\"float: left;\">&nbsp;</span> copy\n\t\t\t\t\t</span>\n\t\t\t\t\t<!-- \"droppable\" options -->\n\t\t\t\t\t<span class=\"iconContainer\" dojoAttachPoint=\"boldButton\"\n\t\t\t\t\t\tunselectable=\"on\"\n\t\t\t\t\t\tdojoOnClick=\"boldClick; buttonClick;\">\n\t\t\t\t\t\t<span class=\"icon bold\" unselectable=\"on\">&nbsp;</span>\n\t\t\t\t\t</span>\n\t\t\t\t\t<span class=\"iconContainer\" dojoAttachPoint=\"italicButton\"\n\t\t\t\t\t\tdojoOnClick=\"italicClick; buttonClick;\">\n\t\t\t\t\t\t<span class=\"icon italic\" unselectable=\"on\">&nbsp;</span>\n\t\t\t\t\t</span>\n\t\t\t\t\t<span class=\"iconContainer\" dojoAttachPoint=\"underlineButton\"\n\t\t\t\t\t\tdojoOnClick=\"underlineClick; buttonClick;\">\n\t\t\t\t\t\t<span class=\"icon underline\" unselectable=\"on\">&nbsp;</span>\n\t\t\t\t\t</span>\n\t\t\t\t\t<span class=\"iconContainer\" dojoAttachPoint=\"leftButton\"\n\t\t\t\t\t\tdojoOnClick=\"leftClick; buttonClick;\">\n\t\t\t\t\t\t<span class=\"icon justifyleft\" unselectable=\"on\">&nbsp;</span>\n\t\t\t\t\t</span>\n\t\t\t\t\t<span class=\"iconContainer\" dojoAttachPoint=\"fullButton\"\n\t\t\t\t\t\tdojoOnClick=\"fullClick; buttonClick;\">\n\t\t\t\t\t\t<span class=\"icon justifyfull\" unselectable=\"on\">&nbsp;</span>\n\t\t\t\t\t</span>\n\t\t\t\t\t<span class=\"iconContainer\" dojoAttachPoint=\"rightButton\"\n\t\t\t\t\t\tdojoOnClick=\"rightClick; buttonClick;\">\n\t\t\t\t\t\t<span class=\"icon justifyright\" unselectable=\"on\">&nbsp;</span>\n\t\t\t\t\t</span>\n\t\t\t\t</td>\n\t\t\t</tr>\n\t\t\t<tr>\n\t\t\t\t<td>\n\t\t\t\t\t<!-- paste -->\n\t\t\t\t\t<span class=\"iconContainer\" dojoAttachPoint=\"pasteButton\"\n\t\t\t\t\t\tdojoOnClick=\"pasteClick; buttonClick;\" unselectable=\"on\">\n\t\t\t\t\t\t<span class=\"icon paste\" style=\"float: left;\" unselectable=\"on\">&nbsp;</span> paste\n\t\t\t\t\t</span>\n\t\t\t\t\t<!-- \"droppable\" options -->\n\t\t\t\t\t<span class=\"iconContainer\" dojoAttachPoint=\"undoButton\"\n\t\t\t\t\t\tdojoOnClick=\"undoClick; buttonClick;\" unselectable=\"on\">\n\t\t\t\t\t\t<span class=\"icon undo\" style=\"float: left;\" unselectable=\"on\">&nbsp;</span> undo\n\t\t\t\t\t</span>\n\t\t\t\t\t<span class=\"iconContainer\" dojoAttachPoint=\"redoButton\"\n\t\t\t\t\t\tdojoOnClick=\"redoClick; buttonClick;\" unselectable=\"on\">\n\t\t\t\t\t\t<span class=\"icon redo\" style=\"float: left;\" unselectable=\"on\">&nbsp;</span> redo\n\t\t\t\t\t</span>\n\t\t\t\t</td>\t\n\t\t\t</tr>\n\t\t</tbody>\n\t</table>\n</div>\n",
		templateCssString:".StyleDropdownContainer {\n\tposition: absolute;\n\tz-index: 1000;\n\toverflow: auto;\n\tcursor: default;\n\twidth: 250px;\n\theight: 250px;\n\tbackground-color: white;\n\tborder: 1px solid black;\n}\n\n.ColorDropdownContainer {\n\tposition: absolute;\n\tz-index: 1000;\n\toverflow: auto;\n\tcursor: default;\n\twidth: 250px;\n\theight: 150px;\n\tbackground-color: white;\n\tborder: 1px solid black;\n}\n\n.EditorToolbarDomNode {\n\tbackground-image: url(buttons/bg-fade.png);\n\tbackground-repeat: repeat-x;\n\tbackground-position: 0px -50px;\n}\n\n.EditorToolbarSmallBg {\n\tbackground-image: url(images/toolbar-bg.gif);\n\tbackground-repeat: repeat-x;\n\tbackground-position: 0px 0px;\n}\n\n/*\nbody {\n\tbackground:url(images/blank.gif) fixed;\n}*/\n\n.IEFixedToolbar {\n\tposition:absolute;\n\t/* top:0; */\n\ttop: expression(eval((document.documentElement||document.body).scrollTop));\n}\n\ndiv.bigIcon {\n\twidth: 40px;\n\theight: 40px; \n\t/* background-color: white; */\n\t/* border: 1px solid #a6a7a3; */\n\tfont-family: Verdana, Trebuchet, Tahoma, Arial;\n}\n\n.iconContainer {\n\tfont-family: Verdana, Trebuchet, Tahoma, Arial;\n\tfont-size: 13px;\n\tfloat: left;\n\theight: 18px;\n\tdisplay: block;\n\t/* background-color: white; */\n\tcursor: pointer;\n\tpadding: 1px 4px 1px 1px; /* almost the same as a transparent border */\n\tborder: 0px;\n}\n\n.dojoE2TBIcon {\n\tdisplay: block;\n\ttext-align: center;\n\tmin-width: 18px;\n\twidth: 18px;\n\theight: 18px;\n\t/* background-color: #a6a7a3; */\n\tbackground-repeat: no-repeat;\n\tbackground-image: url(buttons/aggregate.gif);\n}\n\n\n.dojoE2TBIcon[class~=dojoE2TBIcon] {\n}\n\n.ToolbarButtonLatched {\n    border: #316ac5 1px solid; !important;\n    padding: 0px 3px 0px 0px; !important; /* make room for border */\n    background-color: #c1d2ee;\n}\n\n.ToolbarButtonHighlighted {\n    border: #316ac5 1px solid; !important;\n    padding: 0px 3px 0px 0px; !important; /* make room for border */\n    background-color: #dff1ff;\n}\n\n.ToolbarButtonDisabled{\n    filter: gray() alpha(opacity=30); /* IE */\n    opacity: 0.30; /* Safari, Opera and Mozilla */\n}\n\n.headingContainer {\n\twidth: 150px;\n\theight: 30px;\n\tmargin: 0px;\n\t/* padding-left: 5px; */\n\toverflow: hidden;\n\tline-height: 25px;\n\tborder-bottom: 1px solid black;\n\tborder-top: 1px solid white;\n}\n\n.EditorToolbarDomNode select {\n\tfont-size: 14px;\n}\n \n.dojoE2TBIcon_Sep { width: 5px; min-width: 5px; max-width: 5px; background-position: 0px 0px}\n.dojoE2TBIcon_Backcolor { background-position: -18px 0px}\n.dojoE2TBIcon_Bold { background-position: -36px 0px}\n.dojoE2TBIcon_Cancel { background-position: -54px 0px}\n.dojoE2TBIcon_Copy { background-position: -72px 0px}\n.dojoE2TBIcon_Link { background-position: -90px 0px}\n.dojoE2TBIcon_Cut { background-position: -108px 0px}\n.dojoE2TBIcon_Delete { background-position: -126px 0px}\n.dojoE2TBIcon_TextColor { background-position: -144px 0px}\n.dojoE2TBIcon_BackgroundColor { background-position: -162px 0px}\n.dojoE2TBIcon_Indent { background-position: -180px 0px}\n.dojoE2TBIcon_HorizontalLine { background-position: -198px 0px}\n.dojoE2TBIcon_Image { background-position: -216px 0px}\n.dojoE2TBIcon_NumberedList { background-position: -234px 0px}\n.dojoE2TBIcon_Table { background-position: -252px 0px}\n.dojoE2TBIcon_BulletedList { background-position: -270px 0px}\n.dojoE2TBIcon_Italic { background-position: -288px 0px}\n.dojoE2TBIcon_CenterJustify { background-position: -306px 0px}\n.dojoE2TBIcon_BlockJustify { background-position: -324px 0px}\n.dojoE2TBIcon_LeftJustify { background-position: -342px 0px}\n.dojoE2TBIcon_RightJustify { background-position: -360px 0px}\n.dojoE2TBIcon_left_to_right { background-position: -378px 0px}\n.dojoE2TBIcon_list_bullet_indent { background-position: -396px 0px}\n.dojoE2TBIcon_list_bullet_outdent { background-position: -414px 0px}\n.dojoE2TBIcon_list_num_indent { background-position: -432px 0px}\n.dojoE2TBIcon_list_num_outdent { background-position: -450px 0px}\n.dojoE2TBIcon_Outdent { background-position: -468px 0px}\n.dojoE2TBIcon_Paste { background-position: -486px 0px}\n.dojoE2TBIcon_Redo { background-position: -504px 0px}\ndojoE2TBIcon_RemoveFormat { background-position: -522px 0px}\n.dojoE2TBIcon_right_to_left { background-position: -540px 0px}\n.dojoE2TBIcon_Save { background-position: -558px 0px}\n.dojoE2TBIcon_Space { background-position: -576px 0px}\n.dojoE2TBIcon_StrikeThrough { background-position: -594px 0px}\n.dojoE2TBIcon_Subscript { background-position: -612px 0px}\n.dojoE2TBIcon_Superscript { background-position: -630px 0px}\n.dojoE2TBIcon_Underline { background-position: -648px 0px}\n.dojoE2TBIcon_Undo { background-position: -666px 0px}\n.dojoE2TBIcon_WikiWord { background-position: -684px 0px}\n\n",templateCssPath: dojo.uri.moduleUri("dojo.widget", "templates/EditorToolbar.css"),

//		itemNodeType: 'span', //all the items (with attribute dojoETItemName set) defined in the toolbar should be a of this type
		itemManager: dojo.widget.Editor2ToolbarItemManager,
		postCreate: function(){
			var i=0,node,nodes = dojo.html.getElementsByClass("dojoEditorToolbarItem", this.domNode/*, this.itemNodeType*/);

			this.items = {};
			while(node=nodes[i++]){
				var itemname = node.getAttribute("dojoETItemName");
				if(itemname){
					var item = this.itemManager.getToolbarItem(itemname);
					if(item){
						item.create(node, this);
						this.items[itemname.toLowerCase()] = item;
					}else{
						//hide unsupported toolbar items
						node.style.display = "none";
					}
				}
			}
		},

		update: function(){
			// summary: update all the toolbar items
			for(var cmd in this.items){
				this.items[cmd].refreshState();
			}
		},

		shareGroup: '',
		checkAvailability: function(){
			// summary: returns whether items in this toolbar can be executed
			// description: 
			//		For unshared toolbar, when clicking on a toolbar, the corresponding
			//		editor will be focused, and this function always return true. For shared
			//		toolbar, if the current focued editor is not one of the instances sharing
			//		this toolbar, this function return false, otherwise true.
			var curInst = dojo.widget.Editor2Manager.getCurrentInstance();
			if(!this.shareGroup||!curInst){
				this.parent.focus();
				return true;
			}
			return (this.shareGroup == curInst.toolbarGroup);
		},
		destroy: function(){
			for(var it in this.items){
				this.items[it].destroy();
				delete this.items[it];
			}
			dojo.widget.Editor2Toolbar.superclass.destroy.call(this);
		}
	}
);
