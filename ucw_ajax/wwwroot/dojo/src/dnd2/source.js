/*
	Copyright (c) 2004-2006, The Dojo Foundation
	All Rights Reserved.

	Licensed under the Academic Free License version 2.1 or above OR the
	modified BSD license. For more information on Dojo licensing, see:

		http://dojotoolkit.org/community/licensing.shtml
*/

dojo.provide("dojo.dnd2.source");

dojo.require("dojo.lang.declare");
dojo.require("dojo.event.*");
dojo.require("dojo.html.style");
dojo.require("dojo.html.layout");

dojo.require("dojo.dnd2.selector");
dojo.require("dojo.dnd2.manager");

/*
	Container property:
		"Horizontal"- if this is the horizontal container
	Source states:
		""			- normal state
		"Moved"		- this source is being moved
		"Copied"	- this source is being copied
	Target states:
		""			- normal state
		"Disabled"	- the target cannot accept an avatar
	Target anchor state:
		""			- item is not selected
		"Before"	- insert point is before the anchor
		"After"		- insert point is after the anchor
*/

dojo.declare("dojo.dnd2.Source", dojo.dnd2.Selector,
	// summary: a Source object, which can be used as a DnD source, or a DnD target
function(node, params){
	// summary: a constructor of the Selector
	// node: Node: node or node's id to build the source on
	// params: Object: a dict of parameters, recognized parameters are:
	//	isSource: Boolean: can be used as a DnD source, if true; assumed to be "true" if omitted
	//	accept: Array: list of accepted types (text strings) for a target; assumed to be ["text"] if omitted
	//	horizontal: Boolean: a horizontal container, if true, vertical otherwise or when omitted
	//	the rest of parameters are passed to the selector
	if(!params){ params = {}; }
	this.isSource = typeof params.isSource == "undefined" ? true : params.isSource;
	var types = params.accept instanceof Array ? params.accept : ["text"];
	this.accept = null;
	if(types.length){
		this.accept = {};
		for(var i = 0; i < types.length; ++i){
			this.accept[types[i]] = 1;
		}
	}
	this.horizontal = params.horizontal;
	// class-specific variables
	this.isDragging = false;
	this.mouseDown = false;
	this.targetAnchor = null;
	this.targetBox = null;
	this.before = true;
	// states
	this.sourceState  = "";
	dojo.html.addClass(this.node, "dojoDndSource");
	this.targetState  = "";
	dojo.html.addClass(this.node, "dojoDndTarget");
	if(this.horizontal){ dojo.html.addClass(this.node, "dojoDndHorizontal"); }
	// set up events
	dojo.event.topic.subscribe("dndSourceOver", this, "onDndSourceOver");
	dojo.event.topic.subscribe("dndStart",  this, "onDndStart");
	dojo.event.topic.subscribe("dndDrop",   this, "onDndDrop");
	dojo.event.topic.subscribe("dndCancel", this, "onDndCancel");
},
{
	// mouse event processors
	onMouseMove: function(e){
		// summary: event processor for onmousemove
		// e: Event: mouse event
		if(this.isDragging && this.targetState == "Disabled"){ return; }
		dojo.dnd2.Source.superclass.onMouseMove.call(this, e);
		var m = dojo.dnd2.manager();
		if(this.isDragging){
			// calculate before/after
			var before = false;
			if(this.current){
				if(!this.targetBox || this.targetAnchor != this.current){
					this.targetBox = {
						xy: dojo.html.getAbsolutePosition(this.current, true),
						wh: dojo.html.getBorderBox(this.current)
					};
				}
				if(this.horizontal){
					before = (e.pageX - this.targetBox.xy.x) < (this.targetBox.wh.width / 2);
				}else{
					before = (e.pageY - this.targetBox.xy.y) < (this.targetBox.wh.height / 2);
				}
			}
			if(this.current != this.targetAnchor || before != this.before){
				this._markTargetAnchor(before);
				m.canDrop(!this.current || m.source != this || !(this.current.id in this.selection));
			}
		}else{
			if(this.mouseDown && this.isSource){
				m.startDrag(this, this.getSelectedNodes(), dojo.dnd2.multiSelectKey(e));
			}
		}
	},
	onMouseDown: function(e){
		// summary: event processor for onmousedown
		// e: Event: mouse event
		this.mouseDown = true;
		dojo.dnd2.Source.superclass.onMouseDown.call(this, e);
	},
	onMouseUp: function(e){
		// summary: event processor for onmouseup
		// e: Event: mouse event
		this.mouseDown = false;
		dojo.dnd2.Source.superclass.onMouseUp.call(this, e);
	},
	// topic event processors
	onDndSourceOver: function(source){
		// summary: topic event processor for ondndsourceover, called when detected a current source
		// source: Object: the source which has the mouse over it
		if(this != source){
			this.mouseDown = false;
			if(this.targetAnchor){
				this._unmarkTargetAnchor();
			}
		}else if(this.isDragging){
			var m = dojo.dnd2.manager();
			m.canDrop(this.targetState != "Disabled" && (!this.current || m.source != this || !(this.current.id in this.selection)));
		}
	},
	onDndStart: function(source, nodes, copy){
		// summary: topic event processor for ondndstart, called to initiate the DnD operation
		// source: Object: the source which provides items
		// nodes: Array: the list of transferred items
		// copy: Boolean: copy items, if true, move items otherwise
		if(this.isSource){
			this._changeState("Source", this == source ? (copy ? "Copied" : "Moved") : "");
		}
		var accepted = this.accept && this.checkAcceptance(source, nodes);
		this._changeState("Target", accepted ? "" : "Disabled");
		if(accepted){
			dojo.dnd2.manager().overSource(this);
		}
		this.isDragging = true;
	},
	onDndDrop: function(source, nodes, copy){
		// summary: topic event processor for ondnddrop, called to finish the DnD operation
		// source: Object: the source which provides items
		// nodes: Array: the list of transferred items
		// copy: Boolean: copy items, if true, move items otherwise
		do{ //break box
			if(this.containerState != "Over"){ break; }
			var oldCreator = this.nodeCreator;
			if(this != source || copy){
				this.selectNone();
				this.nodeCreator = function(n){
					return oldCreator(source.map[n.id].data);
				};
			}else{
				if(this.current.id in this.selection){ break; }
				this.nodeCreator = function(n){
					var t = source.map[n.id]; return {node: n, data: t.data, types: t.types};
				};
			}
			this.insertNodes(true, nodes, this.before, this.current);
			this.nodeCreator = oldCreator;
			if(this != source && !copy){
				source.deleteSelectedNodes();
			}
		}while(false);
		this.onDndCancel();
	},
	onDndCancel: function(){
		// summary: topic event processor for ondndcancel, called to cancel the DnD operation
		if(this.targetAnchor){
			this._unmarkTargetAnchor();
			this.targetAnchor = null;
		}
		this.before = true;
		this.isDragging = false;
		this._changeState("Source", "");
		this._changeState("Target", "");
	},
	// utilities
	onOverEvent: function(){
		// summary: this function is called once, when mouse is over our container
		dojo.dnd2.Source.superclass.onOverEvent.call(this);
		dojo.dnd2.manager().overSource(this);
	},
	onOutEvent: function(){
		// summary: this function is called once, when mouse is out of our container
		dojo.dnd2.Source.superclass.onOutEvent.call(this);
		dojo.dnd2.manager().outSource(this);
	},
	// methods
	checkAcceptance: function(source, nodes){
		// summary: checks, if the target can accept nodes from this source
		// source: Object: the source which provides items
		// nodes: Array: the list of transferred items
		if(this == source){ return true; }
		var accepted = true;
		for(var i = 0; i < nodes.length; ++i){
			var types = source.map[nodes[i].id].types;
			if(types instanceof Array){
				var flag = false;
				for(var j = 0; j < types.length; ++j){
					if(types[j] in this.accept){
						flag = true;
						break;
					}
				}
				if(!flag){
					accepted = false;
					break;
				}
			}else{
				accepted = false;
			}
			if(!accepted){ break; }
		}
		return accepted;	// Boolean
	},
	_markTargetAnchor: function(before){
		// summary: assigns a class to the current target anchor based on "before" status
		// before: Boolean: insert before, if true, after otherwise
		if(this.current == this.targetAnchor && this.before == before){ return; }
		if(this.targetAnchor){
			this._removeItemClass(this.targetAnchor, this.before ? "Before" : "After");
		}
		this.targetAnchor = this.current;
		this.targetBox = null;
		this.before = before;
		if(this.targetAnchor){
			this._addItemClass(this.targetAnchor, this.before ? "Before" : "After");
		}
	},
	_unmarkTargetAnchor: function(){
		// summary: removes a class of the current target anchor based on "before" status
		if(!this.targetAnchor){ return; }
		this._removeItemClass(this.targetAnchor, this.before ? "Before" : "After");
		this.targetAnchor = null;
		this.targetBox = null;
		this.before = true;
	},
	_markDndStatus: function(copy){
		// summary: changes source's state based on "copy" status
		this._changeState("Source", copy ? "Copied" : "Moved");
	}
});
