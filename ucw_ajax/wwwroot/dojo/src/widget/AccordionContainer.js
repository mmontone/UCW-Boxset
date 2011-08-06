/*
	Copyright (c) 2004-2006, The Dojo Foundation
	All Rights Reserved.

	Licensed under the Academic Free License version 2.1 or above OR the
	modified BSD license. For more information on Dojo licensing, see:

		http://dojotoolkit.org/community/licensing.shtml
*/

dojo.provide("dojo.widget.AccordionContainer");

dojo.require("dojo.widget.*");
dojo.require("dojo.html.*");
dojo.require("dojo.lfx.html");
dojo.require("dojo.html.selection");
dojo.require("dojo.widget.html.layout");
dojo.require("dojo.widget.PageContainer");
dojo.require("dojo.html.iframe");


/**
 * description
 *	Front view (3 panes, pane #2 open)
 *	------------------------
 *	|:::Pane#1 title:::    |
 * 	|:::Pane#2 title:::    |
 *	|                      |
 *	|    pane#2 contents   |
 *	|                      |
 *	|:::Pane#3 title:::    |
 *	------------------------
 *
 *	Side view (showing implementation):
 *
 *         viewport    pane#3     pane#2     pane#1
 *            =                                
 *            |                                =
 *            |                      =         |
 *	front     |                      |         |
 *            |                      |         =
 *            |                      =
 *            |          =
 *            =          |
 *                       |
 *                       =
 *
 *	Panes are stacked by z-index like a stack of cards, so they can be slid correctly.
 *	The panes on the bottom extend past the bottom of the viewport (but are hidden).
 *
 * usage
 *	<div dojoType="AccordionContainer">
 *		<div dojoType="ContentPane" label="pane 1">...</div>
 *		...
 *	</div>
 *
 * TODO:
 *	* this widget should extend PageContainer
 */
 dojo.widget.defineWidget(
	"dojo.widget.AccordionContainer",
	dojo.widget.HtmlWidget,
	{
		// summary: 
		//		Holds a set of panes where every pane's title is visible, but only one pane's content is visible at a time,
		//		and switching between panes is visualized by sliding the other panes up/down.

		isContainer: true,
		
		// labelNodeClass: String
		//		CSS class name for dom node w/the title
		labelNodeClass: "label",
		
		// containerNodeClass: String
		//		CSS class name for dom node holding the content
		containerNodeClass: "accBody",

		// duration: Integer
		//		Amount of time (in ms) it takes to slide panes
		duration: 250,

		fillInTemplate: function(){
			with(this.domNode.style){
				// position must be either relative or absolute
				if(position!="absolute"){
					position="relative";
				}
				overflow="hidden";
			}
		},

		addChild: function(/*Widget*/ widget){
			var child = this._addChild(widget);
			this._setSizes();
			return child;	// Widget
		},
		
		_addChild: function(/*Widget*/ widget){
			// summary
			//		Internal call to add child, used during postCreate() and by the real addChild() call
			if (widget.widgetType != "AccordionPane") {
				var wrapper=dojo.widget.createWidget("AccordionPane",{label: widget.label, selected: widget.selected, labelNodeClass: this.labelNodeClass, containerNodeClass: this.containerNodeClass, allowCollapse: this.allowCollapse });
				wrapper.addChild(widget);
				this.addWidgetAsDirectChild(wrapper);
				this.registerChild(wrapper, this.children.length);
				return wrapper;	// Widget
			} else {
				dojo.html.addClass(widget.containerNode, this.containerNodeClass);
				dojo.html.addClass(widget.labelNode, this.labelNodeClass);
				this.addWidgetAsDirectChild(widget);
				this.registerChild(widget, this.children.length);	
				return widget;	// Widget
			}
		},
	
		postCreate: function() {
			var tmpChildren = this.children;
			this.children=[];
			dojo.html.removeChildren(this.domNode);
			dojo.lang.forEach(tmpChildren, dojo.lang.hitch(this,"_addChild"));
			this._setSizes();
		},
	
		removeChild: function(/*Widget*/ widget) {
			dojo.widget.AccordionContainer.superclass.removeChild.call(this, widget);
			this._setSizes();
		},
		
		onResized: function(){
			this._setSizes();
		},

		_setSizes: function() {
			// summary
			//		Set panes' size/position based on my size, and the current open node.

			// get cumulative height of all the title bars, and figure out which pane is open
			var totalCollapsedHeight = 0;
			var openIdx = 0;
			dojo.lang.forEach(this.children, function(child, idx){
				totalCollapsedHeight += child.getLabelHeight();
				if(child.selected){ openIdx=idx; }
			});

			// size and position each pane
			var mySize=dojo.html.getContentBox(this.domNode);
			var y = 0;
			dojo.lang.forEach(this.children, function(child, idx){
				var childCollapsedHeight = child.getLabelHeight();
				child.resizeTo(mySize.width, mySize.height-totalCollapsedHeight+childCollapsedHeight);
				child.domNode.style.zIndex=idx+1;
				child.domNode.style.position="absolute";
				child.domNode.style.top = y+"px";
				y += (idx==openIdx) ? dojo.html.getBorderBox(child.domNode).height : childCollapsedHeight;
			});
		},

		selectChild: function(/*Widget*/ page){
			// summary
			//		close the current page and select a new one
			dojo.lang.forEach(this.children, function(child){child.setSelected(child==page);});

			// slide each pane that needs to be moved
			var y = 0;
			var anims = [];
			dojo.lang.forEach(this.children, function(child, idx){
				if(child.domNode.style.top != (y+"px")){
					anims.push(dojo.lfx.html.slideTo(child.domNode, {top: y, left: 0}, this.duration));
				}
				y += child.selected ? dojo.html.getBorderBox(child.domNode).height : child.getLabelHeight();
			}, this);
			dojo.lfx.combine(anims).play();
		},
		focusNextLabel: function(/*Widget*/ page){
			var bFound = false;
			dojo.lang.forEach(this.children, function(child, idx){
				if(child==page){
					bFound = true;
					child.setFocus(false);
				}else{
					if(bFound){
						child.setFocus(true);
						bFound = false;
					}
				}
			}, this);
		},

		focusPreviousLabel: function(/*Widget*/ page){
			var nPrevious = null;
			dojo.lang.forEach(this.children, function(child, idx){
				if(child==page){
					child.setFocus(false);
					if(nPrevious){
						nPrevious.setFocus(true);
					}
				}
				nPrevious = child;
			}, this);
 		}
 	}
 );
	

dojo.widget.defineWidget(
	"dojo.widget.AccordionPane",
	dojo.widget.HtmlWidget,
{
	// summary
	//		AccordionPane is a box with a title that contains another widget (often a ContentPane).
	//		It's a widget used internally by AccordionContainer.
	// label: String
	//		label to print on top of AccordionPane
	label: "",

	// class: String
	//	CSS class name for the AccordionPane's dom node
	"class": "dojoAccordionPane",

	// labelNodeClass: String
	//	CSS class name for the AccordionPane's label node
	labelNodeClass: "label",

	// containerNodeClass: String
	//	CSS class name for the AccordionPane's container node
	containerNodeClass: "accBody",
	
	// selected: Boolean
	//	if true, this is the open pane
	selected: false,

	templateString:"<div dojoAttachPoint=\"domNode\" waiRole=\"tabcontainer\">\n<div dojoAttachPoint=\"labelNode\" dojoAttachEvent=\"onclick: onLabelClick; onkey: onLabelKey; onFocus; onBlur;\" tabindex=\"0\" waiRole=\"tab\" class=\"${this.labelNodeClass}\">${this.label}</div>\n<div dojoAttachPoint=\"containerNode\" style=\"overflow: hidden;\" class=\"${this.containerNodeClass}\" waiRole=\"tabpanel\"></div>\n</div>\n",
	templateCssString:".dojoAccordionPane .label {\n\tcolor: #000;\n\tfont-weight: bold;\n\tbackground: url(\"images/soriaAccordionOff.gif\") repeat-x top left #85aeec;\n\tborder:1px solid #d9d9d9;\n\tfont-size:0.9em;\n}\n\n.dojoAccordionPane-selected .label {\n\tbackground: url(\"images/soriaAccordionSelected.gif\") repeat-x top left #85aeec;\n\tborder:1px solid #84a3d1;\n}\n\n\n.dojoAccordionPane-focused .label {\n\toutline:1px invert dotted !important;\n\tborder:0 !important;\n}\n\n.dojoAccordionPane .label:hover {\n\tcursor: pointer;\n}\n\n.dojoAccordionPane .accBody {\n\tbackground: #fff;\n\toverflow: auto;\n\tborder:1px solid #84a3d1;\n}\n\n",templateCssPath: dojo.uri.moduleUri("dojo.widget", "templates/AccordionPane.css"),

	isContainer: true,

	fillInTemplate: function() {
		dojo.html.addClass(this.domNode, this["class"]);
		dojo.widget.AccordionPane.superclass.fillInTemplate.call(this);
		//dojo.html.disableSelection(this.labelNode);
		this.setSelected(this.selected);

		// Prevent IE bleed-through problem
		this.bgIframe = new dojo.html.BackgroundIframe(this.domNode);
	},

	setLabel: function(/*String*/ label) {
		// summary: set the  title of the node
		this.labelNode.innerHTML=label;
	},
	
	resizeTo: function(width, height){
		dojo.html.setMarginBox(this.domNode, {width: width, height: height});
		var children = [
			{domNode: this.labelNode, layoutAlign: "top"},
			{domNode: this.containerNode, layoutAlign: "client"}
		];
		dojo.widget.html.layout(this.domNode, children);
		var childSize = dojo.html.getContentBox(this.containerNode);
		this.children[0].resizeTo(childSize.width, childSize.height);
	},

	getLabelHeight: function() {
		// summary: returns the height of the title dom node
		return dojo.html.getMarginBox(this.labelNode).height;	// Integer
	},

	onLabelClick: function() {
		// summary: callback when someone clicks my label
		this.parent.selectChild(this);
	},
        onLabelKey: function(/*Event*/ e) {
		// summary: callback when someone presses a key while focus is on my label
		var k = dojo.event.browser.keys;
		switch(e.key){
			case " ":
				this.onLabelClick();
				dojo.event.browser.stopEvent(e);
				return;
			case k.KEY_LEFT_ARROW:
				this.parent.focusPreviousLabel(this);
				dojo.event.browser.stopEvent(e);
				return;
			case k.KEY_RIGHT_ARROW:
				this.parent.focusNextLabel(this);
				dojo.event.browser.stopEvent(e);
				return;
	 	}
	},

	onFocus: function(/*Event*/ e){dojo.debug((this["class"]+"-focused"));
		dojo.html.addClass(this.domNode, this["class"]+"-focused");
		this.onLabelClick();
	},

	onBlur: function(/*Event*/ e){
		dojo.html.removeClass(this.domNode, this["class"]+"-focused");
		
	},

	setFocus: function(/*Boolean*/ isFocused){
		if(isFocused){
			this.labelNode.focus();
		}
	},

	
	setSelected: function(/*Boolean*/ isSelected){
		this.selected=isSelected;
		(isSelected ? dojo.html.addClass : dojo.html.removeClass)(this.domNode, this["class"]+"-selected");
		this.labelNode.tabIndex = isSelected ? 0 : -1;
		// make sure child is showing (lazy load), and also that onShow()/onHide() is called
		var child = this.children[0];
		if(child){
			if(isSelected){
				if(!child.isShowing()){
					child.show();
				}else{
					child.onShow();
				}
			}else{
				child.onHide();
			}
		}
	}
});
