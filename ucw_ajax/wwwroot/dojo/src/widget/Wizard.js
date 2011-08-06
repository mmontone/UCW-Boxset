/*
	Copyright (c) 2004-2006, The Dojo Foundation
	All Rights Reserved.

	Licensed under the Academic Free License version 2.1 or above OR the
	modified BSD license. For more information on Dojo licensing, see:

		http://dojotoolkit.org/community/licensing.shtml
*/

dojo.provide("dojo.widget.Wizard");

dojo.require("dojo.widget.*");
dojo.require("dojo.widget.LayoutContainer");
dojo.require("dojo.widget.ContentPane");
dojo.require("dojo.event.*");
dojo.require("dojo.html.style");
dojo.require("dojo.i18n.common");
dojo.requireLocalization("dojo.widget", "common", null, "ROOT,it,de");
dojo.requireLocalization("dojo.widget", "Wizard", null, "ROOT,it,de");

// TODO: base this on PageContainer
dojo.widget.defineWidget(
	"dojo.widget.WizardContainer",
	dojo.widget.LayoutContainer,
{
	// summary
	//		A set of panels that display sequentially, typically notating a step-by-step
	//		procedure like an install
	
	templateString:"<div class=\"WizardContainer\" dojoAttachPoint=\"wizardNode\">\n    <div class=\"WizardText\" dojoAttachPoint=\"wizardPanelContainerNode\">\n    </div>\n    <div class=\"WizardButtonHolder\" dojoAttachPoint=\"wizardControlContainerNode\">\n        <input class=\"WizardButton\" type=\"button\" dojoAttachPoint=\"previousButton\" value=\"${this.messages.buttonPrevious}\">\n        <input class=\"WizardButton\" type=\"button\" dojoAttachPoint=\"nextButton\" value=\"${this.messages.buttonNext}\">\n        <input class=\"WizardButton\" type=\"button\" dojoAttachPoint=\"doneButton\" style=\"display:none\" value=\"${this.messages.buttonDone}\">\n        <input class=\"WizardButton\" type=\"button\" dojoAttachPoint=\"cancelButton\" value=\"${this.commonMessages.buttonCancel}\">\n    </div>\n</div>\n",
	templateCssString:".WizardContainer {\n\tbackground: #EEEEEE;\n\tborder: #798EC5 1px solid;\n\tpadding: 2px;\n}\n\n.WizardTitle {\n\tcolor: #003366;\n\tpadding: 8px 5px 15px 2px;\n\tfont-weight: bold;\n\tfont-size: x-small;\n\tfont-style: normal;\n\tfont-family: Verdana, Arial, Helvetica;\n\ttext-align: left;\n}\n\n.WizardText {\n\tcolor: #000033;\n\tfont-weight: normal;\n\tfont-size: xx-small;\n\tfont-family: Verdana, Arial, Helvetica;\n\tpadding: 2 50; text-align: justify;\n}\n\n.WizardLightText {\n\tcolor: #666666;\n\tfont-weight: normal;\n\tfont-size: xx-small;\n\tfont-family: verdana, arial, helvetica;\n\tpadding: 2px 50px;\n\ttext-align: justify;\n}\n\n.WizardButtonHolder {\n\ttext-align: right;\n\tpadding: 10px 5px;\n}\n\n.WizardButton {\n\tcolor: #ffffff;\n\tbackground: #798EC5;\n\tfont-size: xx-small;\n\tfont-family: verdana, arial, helvetica, sans-serif;\n\tborder-right: #000000 1px solid;\n\tborder-bottom: #000000 1px solid;\n\tborder-left: #666666 1px solid;\n\tborder-top: #666666 1px solid;\n\tpadding-right: 4px;\n\tpadding-left: 4px;\n\ttext-decoration: none; height: 18px;\n}\n\n.WizardButton:hover {\n\tcursor: pointer;\n}\n\n.WizardButtonDisabled {\n\tcolor: #eeeeee;\n\tbackground-color: #999999;\n\tfont-size: xx-small;\n\tFONT-FAMILY: verdana, arial, helvetica, sans-serif;\n\tborder-right: #000000 1px solid;\n\tborder-bottom: #000000 1px solid;\n\tborder-left: #798EC5 1px solid;\n\tborder-top: #798EC5 1px solid;\n\tpadding-right: 4px;\n\tpadding-left: 4px;\n\ttext-decoration: none;\n\theight: 18px;\n}\n\n\n",templateCssPath: dojo.uri.moduleUri("dojo.widget", "templates/Wizard.css"),

	// selected: DomNode
	//		Currently selected panel.  (Read-only)
	selected: null,

	// nextButtonLabel: String
	//		Label override for the "Next" button.
	nextButtonLabel: "",

	// previousButtonLabel: String
	//		Label override for the "Previous" button.
	previousButtonLabel: "",

	// cancelButtonLabel: String
	//		Label override for the "Cancel" button.
	cancelButtonLabel: "",

	// doneButtonLabel: String
	//		Label override for the "Done" button.
	doneButtonLabel: "",

	// cancelButtonLabel: FunctionName
	//		Name of function to call if user presses cancel button.
	//		Cancel button is not displayed if function is not specified.
	cancelFunction: "",

	// hideDisabledButtons: Boolean
	//		If true, disabled buttons are hidden; otherwise, they are assigned the
	//		"WizardButtonDisabled" CSS class
	hideDisabledButtons: false,

	postMixInProperties: function(){
		dojo.widget.LayoutContainer.superclass.postMixInProperties.apply(this, arguments);
		this.commonMessages = dojo.i18n.getLocalization("dojo.widget", "common", this.lang);
		this.messages = dojo.i18n.getLocalization("dojo.widget", "Wizard", this.lang);
	},

	fillInTemplate: function(args, frag){
		dojo.event.connect(this.nextButton, "onclick", this, "_onNextButtonClick");
		dojo.event.connect(this.previousButton, "onclick", this, "_onPreviousButtonClick");
		if (this.cancelFunction){
			dojo.event.connect(this.cancelButton, "onclick", this.cancelFunction);
		}else{
			this.cancelButton.style.display = "none";
		}
		dojo.event.connect(this.doneButton, "onclick", this, "done");

		// #607 if created from markup we should respect any style attributes
		// Copy style info from input node to output node
		var source = this.getFragNodeRef(frag);
		if(source != null){
			dojo.html.copyStyle(this.domNode, source);
		}
	},

	postCreate: function(){
		dojo.widget.LayoutContainer.superclass.postCreate.apply(this, arguments);
		if(this.nextButtonLabel){this.nextButton.value = this.nextButtonLabel;}
		if(this.previousButtonLabel){this.previousButton.value = this.previousButtonLabel;}
		if(this.cancelButtonLabel){this.cancelButton.value = this.cancelButtonLabel;}
		if(this.doneButtonLabel){this.doneButton.value = this.doneButtonLabel;}
	},

	_checkButtons: function(){
		var lastStep = !this.hasNextPanel();
		this.nextButton.disabled = lastStep;
		this._setButtonClass(this.nextButton);
		if(this.selected.doneFunction){
			this.doneButton.style.display = "";
			// hide the next button if this is the last one and we have a done function
			if(lastStep){
				this.nextButton.style.display = "none";
			}
		}else{
			this.doneButton.style.display = "none";
		}
		this.previousButton.disabled = ((!this.hasPreviousPanel()) || (!this.selected.canGoBack));
		this._setButtonClass(this.previousButton);
	},

	_setButtonClass: function(button){
		if(!this.hideDisabledButtons){
			button.style.display = "";
			dojo.html.setClass(button, button.disabled ? "WizardButtonDisabled" : "WizardButton");
		}else{
			button.style.display = button.disabled ? "none" : "";
		}
	},

	registerChild: function(panel, insertionIndex){
		dojo.widget.WizardContainer.superclass.registerChild.call(this, panel, insertionIndex);
		this.wizardPanelContainerNode.appendChild(panel.domNode);
		panel.hide();

		if(!this.selected){
			this.onSelected(panel);
		}
		this._checkButtons();
	},

	onSelected: function(/*WizardPanel*/ panel){
		// summary: Callback when new panel is selected..  Deselect old panel and select new one
		if(this.selected ){
			if (this.selected._checkPass()) {
				this.selected.hide();
			} else {
				return;
			}
		}
		panel.show();
		this.selected = panel;
	},

	getPanels: function() {
		// summary: returns array of WizardPane children
		return this.getChildrenOfType("WizardPane", false);		// WizardPane[]
	},

	selectedIndex: function() {
		// summary: Returns index (into this.children[]) for currently selected child.
		if (this.selected) {
			return dojo.lang.indexOf(this.getPanels(), this.selected);	// Integer
		}
		return -1;
	},

	_onNextButtonClick: function() {
		// summary: callback when next button is clicked
		var selectedIndex = this.selectedIndex();
		if ( selectedIndex > -1 ) {
			var childPanels = this.getPanels();
			if (childPanels[selectedIndex + 1]) {
				this.onSelected(childPanels[selectedIndex + 1]);
			}
		}
		this._checkButtons();
	},

	_onPreviousButtonClick: function() {
		// summary: callback when previous button is clicked
		var selectedIndex = this.selectedIndex();
		if ( selectedIndex > -1 ) {
			var childPanels = this.getPanels();
			if (childPanels[selectedIndex - 1]) {
				this.onSelected(childPanels[selectedIndex - 1]);
			}
		}
		this._checkButtons();
	},

	hasNextPanel: function() {
		// summary: Returns true if there's a another panel after the current panel
		var selectedIndex = this.selectedIndex();
		return (selectedIndex < (this.getPanels().length - 1));
	},

	hasPreviousPanel: function() {
		// summary: Returns true if there's a panel before the current panel
		var selectedIndex = this.selectedIndex();
		return (selectedIndex > 0);
	},

	done: function() {
		// summary: Finish the wizard's operation
		this.selected.done();
	}
});

dojo.widget.defineWidget(
	"dojo.widget.WizardPane",
	dojo.widget.ContentPane,
{
	// summary
	//		a panel in a WizardContainer

	// canGoBack: Boolean
	//		If true, then can move back to a previous panel (by clicking the "Previous" button)
	canGoBack: true,

	// passFunction: String
	//		Name of function that checks if it's OK to advance to the next panel.
	//		If it's not OK (for example, mandatory field hasn't been entered), then
	//		returns an error message (String) explaining the reason.
	passFunction: "",
	
	// doneFunction: String
	//		Name of function that is run if you press the "Done" button from this panel
	doneFunction: "",

	postMixInProperties: function(args, frag) {
		if (this.passFunction) {
			this.passFunction = dj_global[this.passFunction];
		}
		if (this.doneFunction) {
			this.doneFunction = dj_global[this.doneFunction];
		}
		dojo.widget.WizardPane.superclass.postMixInProperties.apply(this, arguments);
	},

	_checkPass: function() {
		// summary:
		//		Called when the user presses the "next" button.
		//		Calls passFunction to see if it's OK to advance to next panel, and
		//		if it isn't, then display error.
		//		Returns true to advance, false to not advance.
		if (this.passFunction && dojo.lang.isFunction(this.passFunction)) {
			var failMessage = this.passFunction();
			if (failMessage) {
				alert(failMessage);
				return false;
			}
		}
		return true;
	},

	done: function() {
		if (this.doneFunction && dojo.lang.isFunction(this.doneFunction)) {
			this.doneFunction();
		}
	}
});
