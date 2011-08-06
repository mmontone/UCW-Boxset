/*
	Copyright (c) 2004-2006, The Dojo Foundation
	All Rights Reserved.

	Licensed under the Academic Free License version 2.1 or above OR the
	modified BSD license. For more information on Dojo licensing, see:

		http://dojotoolkit.org/community/licensing.shtml
*/

dojo.provide("dojo.widget.Button");

dojo.require("dojo.lang.extras");
dojo.require("dojo.html.*");
dojo.require("dojo.html.selection");
dojo.require("dojo.widget.*");

/*
 * usage
 *	<button dojoType="button" onClick="...">Hello world</button>
 *
 *  var button1 = dojo.widget.createWidget("Button", {caption: "hello world", onClick: foo});
 *	document.body.appendChild(button1.domNode);
 */
dojo.widget.defineWidget(
	"dojo.widget.Button",
	dojo.widget.HtmlWidget,
	{
		// summary
		//	Basically the same thing as a normal HTML button, but with special styling.

		isContainer: true,
		// caption: String
		//	text to display in button
		caption: "",
		// name: String
		//      name attribute for HTML button element
		name: "",
		// id: String
		//      id attribute for HTML button element
		id: "",
		// alt: String
		//      alt attribute for HTML button element
		alt: "",
		// type: String
		//      type attribute for HTML button element
		type: "button",
		// disabled: Boolean
		//      disabled attribute for HTML button element
		disabled: false,
		templateString:"<button tabIndex=\"0\" type=\"${this.type}\" id=\"${this.id}\" name=\"${this.name}\" alt=\"${this.alt}\" dojoAttachPoint=\"containerNode\" class=\"dojoButtonContents dojoButton\" dojoAttachEvent=\"onClick:buttonClick; onFocus;\"></button>\n",
		templateCssString:".dojoButtonContents {\n\tpadding: 0px;\n\ttext-align: center;\t/* if icon and label are split across two lines, center icon */\n        vertical-align: middle; /* if icon and label are on same line, center them */\n\twhite-space: nowrap;\n\tcolor: inherit;\n}\n\n.dojoButtonContents * {\n        text-align: center;\n        vertical-align: middle; /* if icon and label are on same line, center them */\n}\n\n/* ---------- drop down button specific ---------- */\n\n/* border between label and arrow (for drop down buttons */\n.dojoButtonContentsDropDown {\n        padding-right: 3px;\n}\n\n.dojoButtonDownArrowBackground {\n        padding-left: 2px;\n        padding-right: 2px;\n}\n\n.dojoButtonDownArrowBorder {\n        border: 1px solid white;\n}\n\n.dojoButtonDownArrow {\n        width: 0px;\n        height: 0px;\n\tline-height: 0px !important;\n        border-style: solid dotted none dotted;\n        border-color: black transparent transparent transparent;\n\tborder-width: 6px 5px 0px 5px;\n}\n\n.dojoButton[popupActive] .dojoButtonDownArrow,\n.dojoButtonDownArrowBackground[popupActive] .dojoButtonDownArrow,\n.dojoButton:active .dojoButtonDownArrow {\n\tborder-top-color: white;\n}\n\n.dojoButton {\n\tfont-family: Myriad, Arial, Helvetica;\n\tfont-weight:normal;\n\tmargin:0 3px;\n\tpadding:3px;;\n\twhite-space: nowrap;\n\tcursor: pointer;\n\tcolor: invert;\n\tbackground-color:#e1e1e1;\n\tvertical-align: middle;\n\toverflow: visible;\n}\n\n.dojoButton:hover {\n\tcursor: pointer;\n\tcolor:#0e4978;\n\tbackground-color:#e9e9e9;\n}\n\n.dojoButton[popupActive],\n.dojoButtonDownArrowBackground[popupActive],\n.dojoButton:active {\n\tborder-style: inset !important;\n\tcolor:white;\n\tbackground-color:gray;\n}\n\n.dojoButton[disabled] {\n\tcursor: default;        /* makes opera happy */\n\tcursor: url(\"images/no.gif\"), default;\n\topacity: 0.3;\n\tfilter: alpha(opacity=30);\n}\n",templateCssPath: dojo.uri.moduleUri("dojo.widget", "templates/ButtonTemplate.css"),
		
		fillInTemplate: function(args, frag){
			dojo.widget.Button.superclass.fillInTemplate.apply(this, arguments);
			if(this.caption){
				this.setCaption(this.caption);
			}
			dojo.html.disableSelection(this.containerNode);
			var source = this.getFragNodeRef(frag);
			// Copy style info from input node to output node
			dojo.html.copyStyle(this.domNode, source);
			if (!this.focusNode){
				this.focusNode = this.domNode;
			}
		},

		postCreate: function(){
			this.setDisabled(this.disabled == true);
		},
	
		onFocus: function(/*Event*/ e){
			// summary: callback on focus to the button
			if (e.target == this.domNode && this.focusNode != this.domNode){
			        try { this.focusNode.focus(); return; } catch(e2) {}
			}
		},

		buttonClick: function(/*Event*/ e){
			// summary: internal function for handling button clicks via mouse or keybd
			if(!this.disabled){ 
				// focus may fail when tabIndex is not supported on div's
				// by the browser, or when the node is disabled
				try { this.focusNode.focus(); } catch(e2) {};
				if (this._shouldNotify(e)){
					this.onClick(e); 
				}
			}
		},

		_shouldNotify: function(/*Event*/ e) {
		        // summary: determine if onClick should be called
		        return true;
		},

		onClick: function(/*Event*/ e) {
			// summary: callback for when button is clicked; user can override this function
		},

		setCaption: function(/*String*/ content){
			// summary: reset the caption (text) of the button; takes an HTML string
			this.containerNode.innerHTML = this.caption = content;
		},
		
		setDisabled: function(/*Boolean*/ disabled){
			// summary: set disabled state of button
			this.domNode.disabled = this.disabled = disabled;
			dojo.widget.wai.setAttr(this.domNode, "waiState", "disabled", disabled);
		}
	});

/*
 * usage
 *	<button dojoType="DropDownButton" menuId="mymenu">Hello world</button>
 *
 *  var button1 = dojo.widget.createWidget("DropDownButton", {caption: "hello world", menuId: foo});
 *	document.body.appendChild(button1.domNode);
 */
dojo.widget.defineWidget(
	"dojo.widget.DropDownButton",
	dojo.widget.Button,
	{
		// summary
		//		push the button and a menu shows up

		isContainer: true,

		// menuId: String
		//	widget id of the menu that this button should activate
		menuId: "",

		templateString:"<button class=dojoButton tabIndex=\"-1\" type=\"${this.type}\" id=\"${this.id}\" name=\"${this.name}\" alt=\"${this.alt}\" dojoAttachEvent=\"onClick:buttonClick; onKey; onFocus;\"\n\t><table dojoAttachPoint=\"focusNode\" tabIndex=\"0\" cellpadding=0 cellspacing=0 cols=2>\n\t\t<tr>\n\t\t\t<td class=\"dojoButtonContents dojoButtonContentsDropDown\" dojoAttachPoint=\"containerNode\"></td>\n\t\t\t<td class=dojoButtonDownArrowBackground\n\t\t\t\t><div class=dojoButtonDownArrow></div\n\t\t\t></td>\n\t\t</tr>\n\t</table\n></button>\n",

		fillInTemplate: function(){
			dojo.widget.DropDownButton.superclass.fillInTemplate.apply(this, arguments);
			if (!this.popupStateNode){
				this.popupStateNode = this.domNode;
			}
			if (dojo.render.html.opera){
				this.focusNode = this.domNode; 
				this.domNode.tabIndex = "0";
			}
			dojo.widget.wai.setAttr(this.domNode, "waiState", "haspopup", this.menuId);
		},

		onKey: function(/*Event*/ e){
			// summary: callback when the user presses a key (on key-down)
			if (!e.key || this.disabled) { return; }
			if(e.key == e.KEY_DOWN_ARROW){
				if (!this._menu || !this._menu.isShowingNow){
					this.buttonClick(null);
					dojo.event.browser.stopEvent(e);
				}
			}
		},

		_shouldNotify: function(/*Event*/ e){
			// summary: callback when button is clicked; user shouldn't override this function or else the menu won't toggle
			var menu = dojo.widget.getWidgetById(this.menuId); 
			if ( !menu ) { return; }
			if ( menu.open && !menu.isShowingNow) {
				var pos = dojo.html.getAbsolutePosition(this.domNode, true, dojo.html.boxSizing.BORDER_BOX);
				menu.open(pos.x, pos.y+dojo.html.getBorderBox(this.domNode).height, this);
				if (menu.isShowingNow) {
					this._menu = menu;
					this.popupStateNode.setAttribute("popupActive", "true");
					this._oldMenuClose = menu.close;
					var _this = this;
					menu.close = function(){
						_this._menu = null;
						if (typeof _this._oldMenuClose == "function") {
							_this.popupStateNode.removeAttribute("popupActive");
							this.close = _this._oldMenuClose;
							_this._oldMenuClose = null;
							this.close();
						}
					}
				}
			} else if ( menu.close && menu.isShowingNow ){
				menu.close();
			}
			return false;
		}
	});

/*
 * usage
 *	<button dojoType="ComboButton" onClick="..." menuId="mymenu">Hello world</button>
 *
 *  var button1 = dojo.widget.createWidget("DropDownButton", {caption: "hello world", onClick: foo, menuId: "myMenu"});
 *	document.body.appendChild(button1.domNode);
 */
dojo.widget.defineWidget(
	"dojo.widget.ComboButton",
	dojo.widget.DropDownButton,
	{
		// summary
		//		left side is normal button, right side displays menu
	
		templateString:"<button class=dojoButton tabIndex=\"-1\" type=\"${this.type}\" id=\"${this.id}\" name=\"${this.name}\" alt=\"${this.alt}\" dojoAttachEvent=\"onClick:buttonClick; onKey; onFocus;\"\n\t><table cellpadding=0 cellspacing=0 cols=2 style=\"display:inline;\">\n\t\t<tr>\n\t\t\t<td class=\"dojoButtonContents dojoButtonContentsDropDown\" tabIndex=\"0\" dojoAttachPoint=\"containerNode\"></td>\n\t\t\t<td class=\"dojoButtonDownArrowBackground dojoButtonDownArrowBorder\" dojoAttachPoint=\"popupStateNode\"\n\t\t\t\t><div class=dojoButtonDownArrow></div\n\t\t\t></td>\n\t\t</tr>\n\t</table\n></button>\n",
	
		fillInTemplate: function(){
			this.focusNode = this.containerNode;
			dojo.widget.ComboButton.superclass.fillInTemplate.apply(this, arguments);
		},

		_shouldNotify: function(/*Event*/ e){
			if (e == null || (this._menu && this._menu.isShowingNow) || 
				dojo.html.getCursorPosition(e).x >= 
					// workaround for opera bug: getAbsolutePosition on a table cell returns the wrong thing
					(dojo.html.getAbsolutePosition(this.popupStateNode.parentNode).x+this.containerNode.offsetWidth)){
				return dojo.widget.ComboButton.superclass._shouldNotify.apply(this, arguments);
			}else{
				return dojo.widget.DropDownButton.superclass._shouldNotify.apply(this, arguments);
			}
		}
	});
