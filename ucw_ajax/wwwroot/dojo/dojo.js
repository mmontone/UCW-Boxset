/*
	Copyright (c) 2004-2006, The Dojo Foundation
	All Rights Reserved.

	Licensed under the Academic Free License version 2.1 or above OR the
	modified BSD license. For more information on Dojo licensing, see:

		http://dojotoolkit.org/community/licensing.shtml
*/

/*
	This is a compiled version of Dojo, built for deployment and not for
	development. To get an editable version, please visit:

		http://dojotoolkit.org

	for documentation and information on getting the source.
*/

if(typeof dojo=="undefined"){
var dj_global=this;
var dj_currentContext=this;
function dj_undef(_1,_2){
return (typeof (_2||dj_currentContext)[_1]=="undefined");
}
if(dj_undef("djConfig",this)){
var djConfig={};
}
if(dj_undef("dojo",this)){
var dojo={};
}
dojo.global=function(){
return dj_currentContext;
};
dojo.locale=djConfig.locale;
dojo.version={major:0,minor:0,patch:0,flag:"dev",revision:Number("$Rev: 8582 $".match(/[0-9]+/)[0]),toString:function(){
with(dojo.version){
return major+"."+minor+"."+patch+flag+" ("+revision+")";
}
}};
dojo.getObject=function(_3,_4,_5,_6){
var _7,_8;
if(typeof _3!="string"){
return undefined;
}
_7=_5;
if(!_7){
_7=dojo.global();
}
var _9=_3.split("."),i=0,_b,_c,_d;
do{
_b=_7;
_d=_9[i];
_c=_7[_9[i]];
if((_4)&&(!_c)){
_c=_7[_9[i]]={};
}
_7=_c;
i++;
}while(i<_9.length&&_7);
_8=_7;
_7=_b;
return (_6)?{obj:_7,prop:_d}:_8;
};
dojo.exists=function(_e,_f){
if(typeof _f=="string"){
dojo.deprecated("dojo.exists(obj, name)","use dojo.exists(name, obj, /*optional*/create)","0.6");
var tmp=_e;
_e=_f;
_f=tmp;
}
return (!!dojo.getObject(_e,false,_f));
};
dojo.evalProp=function(_11,_12,_13){
dojo.deprecated("dojo.evalProp","just use hash syntax. Sheesh.","0.6");
return _12[_11]||(_13?(_12[_11]={}):undefined);
};
dojo.parseObjPath=function(_14,_15,_16){
dojo.deprecated("dojo.parseObjPath","use dojo.getObject(path, create, context, true)","0.6");
return dojo.getObject(_14,_16,_15,true);
};
dojo.evalObjPath=function(_17,_18){
dojo.deprecated("dojo.evalObjPath","use dojo.getObject(path, create)","0.6");
return dojo.getObject(_17,_18);
};
dojo.errorToString=function(_19){
return (_19["message"]||_19["description"]||_19);
};
dojo.raise=function(_1a,_1b){
if(_1b){
_1a=_1a+": "+dojo.errorToString(_1b);
}else{
_1a=dojo.errorToString(_1a);
}
try{
if(djConfig.isDebug){
dojo.hostenv.println("FATAL exception raised: "+_1a);
}
}
catch(e){
}
throw _1b||Error(_1a);
};
dojo.debug=function(){
};
dojo.debugShallow=function(obj){
};
dojo.profile={start:function(){
},end:function(){
},stop:function(){
},dump:function(){
}};
function dj_eval(_1d){
return dj_global.eval?dj_global.eval(_1d):eval(_1d);
}
dojo.unimplemented=function(_1e,_1f){
var _20="'"+_1e+"' not implemented";
if(_1f!=null){
_20+=" "+_1f;
}
dojo.raise(_20);
};
dojo.deprecated=function(_21,_22,_23){
var _24="DEPRECATED: "+_21;
if(_22){
_24+=" "+_22;
}
if(_23){
_24+=" -- will be removed in version: "+_23;
}
dojo.debug(_24);
};
dojo.render=(function(){
function vscaffold(_25,_26){
var tmp={capable:false,support:{builtin:false,plugin:false},prefixes:_25};
for(var i=0;i<_26.length;i++){
tmp[_26[i]]=false;
}
return tmp;
}
return {name:"",ver:dojo.version,os:{win:false,linux:false,osx:false},html:vscaffold(["html"],["ie","opera","khtml","safari","moz"]),svg:vscaffold(["svg"],["corel","adobe","batik"]),vml:vscaffold(["vml"],["ie"]),swf:vscaffold(["Swf","Flash","Mm"],["mm"]),swt:vscaffold(["Swt"],["ibm"])};
})();
dojo.hostenv=(function(){
var _29={isDebug:false,allowQueryConfig:false,baseScriptUri:"",baseRelativePath:"",libraryScriptUri:"",iePreventClobber:false,ieClobberMinimal:true,preventBackButtonFix:true,delayMozLoadingFix:false,searchIds:[],parseWidgets:true};
if(typeof djConfig=="undefined"){
djConfig=_29;
}else{
for(var _2a in _29){
if(typeof djConfig[_2a]=="undefined"){
djConfig[_2a]=_29[_2a];
}
}
}
return {name_:"(unset)",version_:"(unset)",getName:function(){
return this.name_;
},getVersion:function(){
return this.version_;
},getText:function(uri){
dojo.unimplemented("getText","uri="+uri);
}};
})();
dojo.hostenv.getBaseScriptUri=function(){
if(djConfig.baseScriptUri.length){
return djConfig.baseScriptUri;
}
var uri=new String(djConfig.libraryScriptUri||djConfig.baseRelativePath);
if(!uri){
dojo.raise("Nothing returned by getLibraryScriptUri(): "+uri);
}
djConfig.baseScriptUri=djConfig.baseRelativePath;
return djConfig.baseScriptUri;
};
(function(){
var _2d={pkgFileName:"__package__",loading_modules_:{},loaded_modules_:{},addedToLoadingCount:[],removedFromLoadingCount:[],inFlightCount:0,modulePrefixes_:{dojo:{name:"dojo",value:"src"}},registerModulePath:function(_2e,_2f){
this.modulePrefixes_[_2e]={name:_2e,value:_2f};
},moduleHasPrefix:function(_30){
var mp=this.modulePrefixes_;
return Boolean(mp[_30]&&mp[_30].value);
},getModulePrefix:function(_32){
if(this.moduleHasPrefix(_32)){
return this.modulePrefixes_[_32].value;
}
return _32;
},getTextStack:[],loadUriStack:[],loadedUris:[],post_load_:false,modulesLoadedListeners:[],unloadListeners:[],loadNotifying:false,insideBindHandler:false};
for(var _33 in _2d){
dojo.hostenv[_33]=_2d[_33];
}
})();
dojo.hostenv.loadPath=function(_34,_35,cb){
var uri;
if(_34.charAt(0)=="/"||_34.match(/^\w+:/)){
uri=_34;
}else{
uri=this.getBaseScriptUri()+_34;
}
if(djConfig.cacheBust&&dojo.render.html.capable){
uri+="?"+String(djConfig.cacheBust).replace(/\W+/g,"");
}
try{
return !_35?this.loadUri(uri,cb):this.loadUriAndCheck(uri,_35,cb);
}
catch(e){
dojo.debug(e);
return false;
}
};
dojo.hostenv.loadUri=function(uri,cb){
if(this.loadedUris[uri]){
return true;
}
var _3a=this.getText(uri,null,true);
if(!_3a){
return false;
}
this.loadedUris[uri]=true;
if(cb){
_3a="("+_3a+")";
}
var _3b=dj_eval(_3a);
if(cb){
cb(_3b);
}
return true;
};
dojo.hostenv.loadUriAndCheck=function(uri,_3d,cb){
var ok=true;
try{
ok=this.loadUri(uri,cb);
}
catch(e){
dojo.debug("failed loading ",uri," with error: ",e);
}
return Boolean(ok&&this.findModule(_3d,false));
};
dojo.loaded=function(){
};
dojo.unloaded=function(){
};
dojo.hostenv.loaded=function(){
this.loadNotifying=true;
this.post_load_=true;
var mll=this.modulesLoadedListeners;
for(var x=0;x<mll.length;x++){
mll[x]();
}
this.modulesLoadedListeners=[];
this.loadNotifying=false;
dojo.loaded();
};
dojo.hostenv.unloaded=function(){
var mll=this.unloadListeners;
while(mll.length){
(mll.pop())();
}
dojo.unloaded();
};
dojo.addOnLoad=function(obj,_44){
var dh=dojo.hostenv;
if(arguments.length==1){
dh.modulesLoadedListeners.push(obj);
}else{
if(arguments.length>1){
dh.modulesLoadedListeners.push(function(){
obj[_44]();
});
}
}
if(dh.post_load_&&dh.inFlightCount==0&&!dh.loadNotifying&&!dh.insideBindHandler){
dh.callLoaded();
}
};
dojo.addOnUnload=function(obj,_47){
var dh=dojo.hostenv;
if(arguments.length==1){
dh.unloadListeners.push(obj);
}else{
if(arguments.length>1){
dh.unloadListeners.push(function(){
obj[_47]();
});
}
}
};
dojo.hostenv.modulesLoaded=function(){
if(this.post_load_){
return;
}
if(this.loadUriStack.length==0&&this.getTextStack.length==0){
if(this.inFlightCount>0){
dojo.debug("files still in flight!");
return;
}
dojo.hostenv.callLoaded();
}
};
dojo.hostenv.callLoaded=function(){
if(typeof setTimeout=="object"||(djConfig["useXDomain"]&&dojo.render.html.opera)){
setTimeout("dojo.hostenv.loaded();",0);
}else{
dojo.hostenv.loaded();
}
};
dojo.hostenv.getModuleSymbols=function(_49){
var _4a=_49.split(".");
for(var i=_4a.length;i>0;i--){
var _4c=_4a.slice(0,i).join(".");
if((i==1)&&!this.moduleHasPrefix(_4c)){
_4a[0]="../"+_4a[0];
}else{
var _4d=this.getModulePrefix(_4c);
if(_4d!=_4c){
_4a.splice(0,i,_4d);
break;
}
}
}
return _4a;
};
dojo.hostenv._global_omit_module_check=false;
dojo.hostenv.loadModule=function(_4e,_4f,_50){
if(!_4e){
return;
}
_50=this._global_omit_module_check||_50;
var _51=this.findModule(_4e,false);
if(_51){
return _51;
}
if(dj_undef(_4e,this.loading_modules_)){
this.addedToLoadingCount.push(_4e);
}
this.loading_modules_[_4e]=1;
var _52=_4e.replace(/\./g,"/")+".js";
var _53=_4e.split(".");
var _54=this.getModuleSymbols(_4e);
var _55=((_54[0].charAt(0)!="/")&&!_54[0].match(/^\w+:/));
var _56=_54[_54.length-1];
var ok;
if(_56=="*"){
_4e=_53.slice(0,-1).join(".");
while(_54.length){
_54.pop();
_54.push(this.pkgFileName);
_52=_54.join("/")+".js";
if(_55&&_52.charAt(0)=="/"){
_52=_52.slice(1);
}
ok=this.loadPath(_52,!_50?_4e:null);
if(ok){
break;
}
_54.pop();
}
}else{
_52=_54.join("/")+".js";
_4e=_53.join(".");
var _58=!_50?_4e:null;
ok=this.loadPath(_52,_58);
if(!ok&&!_4f){
_54.pop();
while(_54.length){
_52=_54.join("/")+".js";
ok=this.loadPath(_52,_58);
if(ok){
break;
}
_54.pop();
_52=_54.join("/")+"/"+this.pkgFileName+".js";
if(_55&&_52.charAt(0)=="/"){
_52=_52.slice(1);
}
ok=this.loadPath(_52,_58);
if(ok){
break;
}
}
}
if(!ok&&!_50){
dojo.raise("Could not load '"+_4e+"'; last tried '"+_52+"'");
}
}
if(!_50&&!this["isXDomain"]){
_51=this.findModule(_4e,false);
if(!_51){
dojo.raise("symbol '"+_4e+"' is not defined after loading '"+_52+"'");
}
}
return _51;
};
dojo.hostenv.startPackage=function(_59){
var _5a=String(_59);
var _5b=_5a;
var _5c=_59.split(/\./);
if(_5c[_5c.length-1]=="*"){
_5c.pop();
_5b=_5c.join(".");
}
var _5d=dojo.getObject(_5b,true);
this.loaded_modules_[_5a]=_5d;
this.loaded_modules_[_5b]=_5d;
return _5d;
};
dojo.hostenv.findModule=function(_5e,_5f){
var lmn=String(_5e);
if(this.loaded_modules_[lmn]){
return this.loaded_modules_[lmn];
}
if(_5f){
dojo.raise("no loaded module named '"+_5e+"'");
}
return null;
};
dojo.kwCompoundRequire=function(_61){
var _62=_61["common"]||[];
var _63=_62.concat(_61[dojo.hostenv.name_]||_61["default"]||[]);
for(var x=0;x<_63.length;x++){
var _65=_63[x];
if(_65.constructor==Array){
dojo.hostenv.loadModule.apply(dojo.hostenv,_65);
}else{
dojo.hostenv.loadModule(_65);
}
}
};
dojo.require=function(_66){
dojo.hostenv.loadModule.apply(dojo.hostenv,arguments);
};
dojo.requireIf=function(_67,_68){
var _69=arguments[0];
if((_69===true)||(_69=="common")||(_69&&dojo.render[_69].capable)){
var _6a=[];
for(var i=1;i<arguments.length;i++){
_6a.push(arguments[i]);
}
dojo.require.apply(dojo,_6a);
}
};
dojo.requireAfterIf=dojo.requireIf;
dojo.provide=function(_6c){
return dojo.hostenv.startPackage.apply(dojo.hostenv,arguments);
};
dojo.registerModulePath=function(_6d,_6e){
return dojo.hostenv.registerModulePath(_6d,_6e);
};
if(djConfig["modulePaths"]){
for(var param in djConfig["modulePaths"]){
dojo.registerModulePath(param,djConfig["modulePaths"][param]);
}
}
dojo.requireLocalization=function(_6f,_70,_71,_72){
dojo.require("dojo.i18n.loader");
dojo.i18n._requireLocalization.apply(dojo.hostenv,arguments);
};
}
if(typeof window!="undefined"){
(function(){
if(djConfig.allowQueryConfig){
var _73=document.location.toString();
var _74=_73.split("?",2);
if(_74.length>1){
var _75=_74[1];
var _76=_75.split("&");
for(var x in _76){
var sp=_76[x].split("=");
if((sp[0].length>9)&&(sp[0].substr(0,9)=="djConfig.")){
var opt=sp[0].substr(9);
try{
djConfig[opt]=eval(sp[1]);
}
catch(e){
djConfig[opt]=sp[1];
}
}
}
}
}
if(((djConfig["baseScriptUri"]=="")||(djConfig["baseRelativePath"]==""))&&(document&&document.getElementsByTagName)){
var _7a=document.getElementsByTagName("script");
var _7b=/(__package__|dojo|bootstrap1)\.js([\?\.]|$)/i;
for(var i=0;i<_7a.length;i++){
var src=_7a[i].getAttribute("src");
if(!src){
continue;
}
var m=src.match(_7b);
if(m){
var _7f=src.substring(0,m.index);
if(src.indexOf("bootstrap1")>-1){
_7f+="../";
}
if(!this["djConfig"]){
djConfig={};
}
if(djConfig["baseScriptUri"]==""){
djConfig["baseScriptUri"]=_7f;
}
if(djConfig["baseRelativePath"]==""){
djConfig["baseRelativePath"]=_7f;
}
break;
}
}
}
var dr=dojo.render;
var drh=dojo.render.html;
var drs=dojo.render.svg;
var dua=(drh.UA=navigator.userAgent);
var dav=(drh.AV=navigator.appVersion);
var t=true;
var f=false;
drh.capable=t;
drh.support.builtin=t;
dr.ver=parseFloat(drh.AV);
dr.os.mac=dav.indexOf("Macintosh")>=0;
dr.os.win=dav.indexOf("Windows")>=0;
dr.os.linux=dav.indexOf("X11")>=0;
drh.opera=dua.indexOf("Opera")>=0;
drh.khtml=(dav.indexOf("Konqueror")>=0)||(dav.indexOf("Safari")>=0);
drh.safari=dav.indexOf("Safari")>=0;
var _87=dua.indexOf("Gecko");
drh.mozilla=drh.moz=(_87>=0)&&(!drh.khtml);
if(drh.mozilla){
drh.geckoVersion=dua.substring(_87+6,_87+14);
}
drh.ie=(document.all)&&(!drh.opera);
drh.ie50=drh.ie&&dav.indexOf("MSIE 5.0")>=0;
drh.ie55=drh.ie&&dav.indexOf("MSIE 5.5")>=0;
drh.ie60=drh.ie&&dav.indexOf("MSIE 6.0")>=0;
drh.ie70=drh.ie&&dav.indexOf("MSIE 7.0")>=0;
var cm=document["compatMode"];
drh.quirks=(cm=="BackCompat")||(cm=="QuirksMode")||drh.ie55||drh.ie50;
dojo.locale=dojo.locale||(drh.ie?navigator.userLanguage:navigator.language).toLowerCase();
dr.vml.capable=drh.ie;
drs.capable=f;
drs.support.plugin=f;
drs.support.builtin=f;
var _89=window["document"];
var tdi=_89["implementation"];
if((tdi)&&(tdi["hasFeature"])&&(tdi.hasFeature("org.w3c.dom.svg","1.0"))){
drs.capable=t;
drs.support.builtin=t;
drs.support.plugin=f;
}
if(drh.safari){
var tmp=dua.split("AppleWebKit/")[1];
var ver=parseFloat(tmp.split(" ")[0]);
if(ver>=420){
drs.capable=t;
drs.support.builtin=t;
drs.support.plugin=f;
}
}else{
}
})();
dojo.hostenv.startPackage("dojo.hostenv");
dojo.render.name=dojo.hostenv.name_="browser";
dojo.hostenv.searchIds=[];
dojo.hostenv._XMLHTTP_PROGIDS=["Msxml2.XMLHTTP","Microsoft.XMLHTTP","Msxml2.XMLHTTP.4.0"];
dojo.hostenv.getXmlhttpObject=function(){
var _8d=null;
var _8e=null;
try{
_8d=new XMLHttpRequest();
}
catch(e){
}
if(!_8d){
for(var i=0;i<3;++i){
var _90=dojo.hostenv._XMLHTTP_PROGIDS[i];
try{
_8d=new ActiveXObject(_90);
}
catch(e){
_8e=e;
}
if(_8d){
dojo.hostenv._XMLHTTP_PROGIDS=[_90];
break;
}
}
}
if(!_8d){
return dojo.raise("XMLHTTP not available",_8e);
}
return _8d;
};
dojo.hostenv._blockAsync=false;
dojo.hostenv.getText=function(uri,_92,_93){
if(!_92){
this._blockAsync=true;
}
var _94=this.getXmlhttpObject();
function isDocumentOk(_95){
var _96=_95["status"];
return Boolean((!_96)||((200<=_96)&&(300>_96))||(_96==304));
}
if(_92){
var _97=this,_98=null,gbl=dojo.global();
var xhr=dojo.getObject("dojo.io.XMLHTTPTransport");
_94.onreadystatechange=function(){
if(_98){
gbl.clearTimeout(_98);
_98=null;
}
if(_97._blockAsync||(xhr&&xhr._blockAsync)){
_98=gbl.setTimeout(function(){
_94.onreadystatechange.apply(this);
},10);
}else{
if(4==_94.readyState){
if(isDocumentOk(_94)){
_92(_94.responseText);
}
}
}
};
}
_94.open("GET",uri,_92?true:false);
try{
_94.send(null);
if(_92){
return null;
}
if(!isDocumentOk(_94)){
var err=Error("Unable to load "+uri+" status:"+_94.status);
err.status=_94.status;
err.responseText=_94.responseText;
throw err;
}
}
catch(e){
this._blockAsync=false;
if((_93)&&(!_92)){
return null;
}else{
throw e;
}
}
this._blockAsync=false;
return _94.responseText;
};
dojo.hostenv.defaultDebugContainerId="dojoDebug";
dojo.hostenv._println_buffer=[];
dojo.hostenv._println_safe=false;
dojo.hostenv.println=function(_9c){
if(!dojo.hostenv._println_safe){
dojo.hostenv._println_buffer.push(_9c);
}else{
try{
var _9d=document.getElementById(djConfig.debugContainerId?djConfig.debugContainerId:dojo.hostenv.defaultDebugContainerId);
if(!_9d){
_9d=dojo.body();
}
var div=document.createElement("div");
div.appendChild(document.createTextNode(_9c));
_9d.appendChild(div);
}
catch(e){
try{
document.write("<div>"+_9c+"</div>");
}
catch(e2){
window.status=_9c;
}
}
}
};
dojo.addOnLoad(function(){
dojo.hostenv._println_safe=true;
while(dojo.hostenv._println_buffer.length>0){
dojo.hostenv.println(dojo.hostenv._println_buffer.shift());
}
});
function dj_addNodeEvtHdlr(_9f,_a0,fp){
var _a2=_9f["on"+_a0]||function(){
};
_9f["on"+_a0]=function(){
fp.apply(_9f,arguments);
_a2.apply(_9f,arguments);
};
return true;
}
dojo.hostenv._djInitFired=false;
function dj_load_init(e){
dojo.hostenv._djInitFired=true;
var _a4=(e&&e.type)?e.type.toLowerCase():"load";
if(arguments.callee.initialized||(_a4!="domcontentloaded"&&_a4!="load")){
return;
}
arguments.callee.initialized=true;
if(typeof (_timer)!="undefined"){
clearInterval(_timer);
delete _timer;
}
var _a5=function(){
if(dojo.render.html.ie){
dojo.hostenv.makeWidgets();
}
};
if(dojo.hostenv.inFlightCount==0){
_a5();
dojo.hostenv.modulesLoaded();
}else{
dojo.hostenv.modulesLoadedListeners.unshift(_a5);
}
}
if(document.addEventListener){
if(dojo.render.html.opera||(dojo.render.html.moz&&(djConfig["enableMozDomContentLoaded"]===true))){
document.addEventListener("DOMContentLoaded",dj_load_init,null);
}
window.addEventListener("load",dj_load_init,null);
}
if(dojo.render.html.ie&&dojo.render.os.win){
document.write("<scr"+"ipt defer src=\"//:\" "+"onreadystatechange=\"if(this.readyState=='complete'){dj_load_init();}\">"+"</scr"+"ipt>");
}
if(/(WebKit|khtml)/i.test(navigator.userAgent)){
var _timer=setInterval(function(){
if(/loaded|complete/.test(document.readyState)){
dj_load_init();
}
},10);
}
if(dojo.render.html.ie){
dj_addNodeEvtHdlr(window,"beforeunload",function(){
dojo.hostenv._unloading=true;
window.setTimeout(function(){
dojo.hostenv._unloading=false;
},0);
});
}
dj_addNodeEvtHdlr(window,"unload",function(){
if((!dojo.render.html.ie)||(dojo.render.html.ie&&dojo.hostenv._unloading)){
dojo.hostenv.unloaded();
}
});
dojo.hostenv.makeWidgets=function(){
var _a6=[];
if(djConfig.searchIds&&djConfig.searchIds.length>0){
_a6=_a6.concat(djConfig.searchIds);
}
if(dojo.hostenv.searchIds&&dojo.hostenv.searchIds.length>0){
_a6=_a6.concat(dojo.hostenv.searchIds);
}
if((djConfig.parseWidgets)||(_a6.length>0)){
if(dojo.getObject("dojo.widget.Parse")){
var _a7=new dojo.xml.Parse();
if(_a6.length>0){
for(var x=0;x<_a6.length;x++){
var _a9=document.getElementById(_a6[x]);
if(!_a9){
continue;
}
var _aa=_a7.parseElement(_a9,null,true);
dojo.widget.getParser().createComponents(_aa);
}
}else{
if(djConfig.parseWidgets){
var _aa=_a7.parseElement(dojo.body(),null,true);
dojo.widget.getParser().createComponents(_aa);
}
}
}
}
};
dojo.addOnLoad(function(){
if(!dojo.render.html.ie){
dojo.hostenv.makeWidgets();
}
});
try{
if(dojo.render.html.ie){
document.namespaces.add("v","urn:schemas-microsoft-com:vml");
document.createStyleSheet().addRule("v\\:*","behavior:url(#default#VML)");
}
}
catch(e){
}
dojo.hostenv.writeIncludes=function(){
};
if(!dj_undef("document",this)){
dj_currentDocument=this.document;
}
dojo.doc=function(){
return dj_currentDocument;
};
dojo.body=function(){
return dojo.doc().body||dojo.doc().getElementsByTagName("body")[0];
};
dojo.byId=function(id,doc){
if((id)&&((typeof id=="string")||(id instanceof String))){
if(!doc){
doc=dj_currentDocument;
}
var ele=doc.getElementById(id);
if(ele&&(ele.id!=id)&&doc.all){
ele=null;
eles=doc.all[id];
if(eles){
if(eles.length){
for(var i=0;i<eles.length;i++){
if(eles[i].id==id){
ele=eles[i];
break;
}
}
}else{
ele=eles;
}
}
}
return ele;
}
return id;
};
dojo.setContext=function(_af,_b0){
dj_currentContext=_af;
dj_currentDocument=_b0;
};
dojo._fireCallback=function(_b1,_b2,_b3){
if((_b2)&&((typeof _b1=="string")||(_b1 instanceof String))){
_b1=_b2[_b1];
}
return (_b2?_b1.apply(_b2,_b3||[]):_b1());
};
dojo.withGlobal=function(_b4,_b5,_b6,_b7){
var _b8;
var _b9=dj_currentContext;
var _ba=dj_currentDocument;
try{
dojo.setContext(_b4,_b4.document);
_b8=dojo._fireCallback(_b5,_b6,_b7);
}
finally{
dojo.setContext(_b9,_ba);
}
return _b8;
};
dojo.withDoc=function(_bb,_bc,_bd,_be){
var _bf;
var _c0=dj_currentDocument;
try{
dj_currentDocument=_bb;
_bf=dojo._fireCallback(_bc,_bd,_be);
}
finally{
dj_currentDocument=_c0;
}
return _bf;
};
}
dojo.requireIf((djConfig["isDebug"]||djConfig["debugAtAllCosts"]),"dojo.debug");
dojo.requireIf(djConfig["debugAtAllCosts"]&&!window.widget&&!djConfig["useXDomain"],"dojo.browser_debug");
dojo.requireIf(djConfig["debugAtAllCosts"]&&!window.widget&&djConfig["useXDomain"],"dojo.browser_debug_xd");
if(!this["dojo"]){
alert("\"dojo/__package__.js\" is now located at \"dojo/dojo.js\". Please update your includes accordingly");
}
dojo.provide("dojo.string.common");
dojo.string.trim=function(str,wh){
if(!str.replace){
return str;
}
if(!str.length){
return str;
}
var re=(wh>0)?(/^\s+/):(wh<0)?(/\s+$/):(/^\s+|\s+$/g);
return str.replace(re,"");
};
dojo.string.trimStart=function(str){
return dojo.string.trim(str,1);
};
dojo.string.trimEnd=function(str){
return dojo.string.trim(str,-1);
};
dojo.string.repeat=function(str,_c7,_c8){
var out="";
for(var i=0;i<_c7;i++){
out+=str;
if(_c8&&i<_c7-1){
out+=_c8;
}
}
return out;
};
dojo.string.pad=function(str,len,c,dir){
var out=String(str);
if(!c){
c="0";
}
if(!dir){
dir=1;
}
while(out.length<len){
if(dir>0){
out=c+out;
}else{
out+=c;
}
}
return out;
};
dojo.string.padLeft=function(str,len,c){
return dojo.string.pad(str,len,c,1);
};
dojo.string.padRight=function(str,len,c){
return dojo.string.pad(str,len,c,-1);
};
dojo.provide("dojo.date.serialize");
dojo.date.setIso8601=function(_d6,_d7){
var _d8=(_d7.indexOf("T")==-1)?_d7.split(" "):_d7.split("T");
_d6=dojo.date.setIso8601Date(_d6,_d8[0]);
if(_d8.length==2){
_d6=dojo.date.setIso8601Time(_d6,_d8[1]);
}
return _d6;
};
dojo.date.fromIso8601=function(_d9){
return dojo.date.setIso8601(new Date(0,0),_d9);
};
dojo.date.setIso8601Date=function(_da,_db){
var _dc="^([0-9]{4})((-?([0-9]{2})(-?([0-9]{2}))?)|"+"(-?([0-9]{3}))|(-?W([0-9]{2})(-?([1-7]))?))?$";
var d=_db.match(new RegExp(_dc));
if(!d){
dojo.debug("invalid date string: "+_db);
return null;
}
var _de=d[1];
var _df=d[4];
var _e0=d[6];
var _e1=d[8];
var _e2=d[10];
var _e3=d[12]||1;
_da.setFullYear(_de);
if(_e1){
_da.setMonth(0);
_da.setDate(Number(_e1));
}else{
if(_e2){
_da.setMonth(0);
_da.setDate(1);
var day=_da.getDay()||7;
var _e5=Number(_e3)+(7*Number(_e2));
if(day<=4){
_da.setDate(_e5+1-day);
}else{
_da.setDate(_e5+8-day);
}
}else{
if(_df){
_da.setDate(1);
_da.setMonth(_df-1);
}
if(_e0){
_da.setDate(_e0);
}
}
}
return _da;
};
dojo.date.fromIso8601Date=function(_e6){
return dojo.date.setIso8601Date(new Date(0,0),_e6);
};
dojo.date.setIso8601Time=function(_e7,_e8){
var _e9="Z|(([-+])([0-9]{2})(:?([0-9]{2}))?)$";
var d=_e8.match(new RegExp(_e9));
var _eb=0;
if(d){
if(d[0]!="Z"){
_eb=(Number(d[3])*60)+Number(d[5]||0);
if(d[2]!="-"){
_eb*=-1;
}
}
_eb-=_e7.getTimezoneOffset();
_e8=_e8.substr(0,_e8.length-d[0].length);
}
var _ec="^([0-9]{2})(:?([0-9]{2})(:?([0-9]{2})(.([0-9]+))?)?)?$";
d=_e8.match(new RegExp(_ec));
if(!d){
dojo.debug("invalid time string: "+_e8);
return null;
}
var _ed=d[1];
var _ee=Number(d[3]||0);
var _ef=d[5]||0;
var ms=d[7]?(Number("0."+d[7])*1000):0;
_e7.setHours(_ed);
_e7.setMinutes(_ee);
_e7.setSeconds(_ef);
_e7.setMilliseconds(ms);
if(_eb!==0){
_e7.setTime(_e7.getTime()+_eb*60000);
}
return _e7;
};
dojo.date.fromIso8601Time=function(_f1){
return dojo.date.setIso8601Time(new Date(0,0),_f1);
};
dojo.date.toRfc3339=function(_f2,_f3){
if(!_f2){
_f2=new Date();
}
var _=dojo.string.pad;
var _f5=[];
if(_f3!="timeOnly"){
var _f6=[_(_f2.getFullYear(),4),_(_f2.getMonth()+1,2),_(_f2.getDate(),2)].join("-");
_f5.push(_f6);
}
if(_f3!="dateOnly"){
var _f7=[_(_f2.getHours(),2),_(_f2.getMinutes(),2),_(_f2.getSeconds(),2)].join(":");
var _f8=_f2.getTimezoneOffset();
_f7+=(_f8>0?"-":"+")+_(Math.floor(Math.abs(_f8)/60),2)+":"+_(Math.abs(_f8)%60,2);
_f5.push(_f7);
}
return _f5.join("T");
};
dojo.date.fromRfc3339=function(_f9){
if(_f9.indexOf("Tany")!=-1){
_f9=_f9.replace("Tany","");
}
var _fa=new Date();
return dojo.date.setIso8601(_fa,_f9);
};
dojo.provide("dojo.lang.common");
dojo.lang.inherits=function(_fb,_fc){
if(!dojo.lang.isFunction(_fc)){
dojo.raise("dojo.inherits: superclass argument ["+_fc+"] must be a function (subclass: ["+_fb+"']");
}
_fb.prototype=new _fc();
_fb.prototype.constructor=_fb;
_fb.superclass=_fc.prototype;
_fb["super"]=_fc.prototype;
};
dojo.lang._mixin=function(obj,_fe){
var _ff={};
for(var x in _fe){
if((typeof _ff[x]=="undefined")||(_ff[x]!=_fe[x])){
obj[x]=_fe[x];
}
}
if(dojo.render.html.ie&&(typeof (_fe["toString"])=="function")&&(_fe["toString"]!=obj["toString"])&&(_fe["toString"]!=_ff["toString"])){
obj.toString=_fe.toString;
}
return obj;
};
dojo.lang.mixin=function(obj,_102){
for(var i=1,l=arguments.length;i<l;i++){
dojo.lang._mixin(obj,arguments[i]);
}
return obj;
};
dojo.lang.extend=function(_105,_106){
for(var i=1,l=arguments.length;i<l;i++){
dojo.lang._mixin(_105.prototype,arguments[i]);
}
return _105;
};
dojo.lang._delegate=function(obj,_10a){
function TMP(){
}
TMP.prototype=obj;
var tmp=new TMP();
if(_10a){
dojo.lang.mixin(tmp,_10a);
}
return tmp;
};
dojo.inherits=dojo.lang.inherits;
dojo.mixin=dojo.lang.mixin;
dojo.extend=dojo.lang.extend;
dojo.lang.find=function(_10c,_10d,_10e,_10f){
var _110=dojo.lang.isString(_10c);
if(_110){
_10c=_10c.split("");
}
if(_10f){
var step=-1;
var i=_10c.length-1;
var end=-1;
}else{
var step=1;
var i=0;
var end=_10c.length;
}
if(_10e){
while(i!=end){
if(_10c[i]===_10d){
return i;
}
i+=step;
}
}else{
while(i!=end){
if(_10c[i]==_10d){
return i;
}
i+=step;
}
}
return -1;
};
dojo.lang.indexOf=dojo.lang.find;
dojo.lang.findLast=function(_114,_115,_116){
return dojo.lang.find(_114,_115,_116,true);
};
dojo.lang.lastIndexOf=dojo.lang.findLast;
dojo.lang.inArray=function(_117,_118){
return dojo.lang.find(_117,_118)>-1;
};
dojo.lang.isObject=function(it){
if(typeof it=="undefined"){
return false;
}
return (typeof it=="object"||it===null||dojo.lang.isArray(it)||dojo.lang.isFunction(it));
};
dojo.lang.isArray=function(it){
return (it&&it instanceof Array||typeof it=="array");
};
dojo.lang.isArrayLike=function(it){
if((!it)||(dojo.lang.isUndefined(it))){
return false;
}
if(dojo.lang.isString(it)){
return false;
}
if(dojo.lang.isFunction(it)){
return false;
}
if(dojo.lang.isArray(it)){
return true;
}
if((it.tagName)&&(it.tagName.toLowerCase()=="form")){
return false;
}
if(dojo.lang.isNumber(it.length)&&isFinite(it.length)){
return true;
}
return false;
};
dojo.lang.isFunction=function(it){
return (it instanceof Function||typeof it=="function");
};
(function(){
if((dojo.render.html.capable)&&(dojo.render.html["safari"])){
dojo.lang.isFunction=function(it){
if((typeof (it)=="function")&&(it=="[object NodeList]")){
return false;
}
return (it instanceof Function||typeof it=="function");
};
}
})();
dojo.lang.isString=function(it){
return (typeof it=="string"||it instanceof String);
};
dojo.lang.isAlien=function(it){
if(!it){
return false;
}
return !dojo.lang.isFunction(it)&&/\{\s*\[native code\]\s*\}/.test(String(it));
};
dojo.lang.isBoolean=function(it){
return (it instanceof Boolean||typeof it=="boolean");
};
dojo.lang.isNumber=function(it){
return (it instanceof Number||typeof it=="number");
};
dojo.lang.isUndefined=function(it){
return ((typeof (it)=="undefined")&&(it==undefined));
};
dojo.provide("dojo.dom");
dojo.dom.ELEMENT_NODE=1;
dojo.dom.ATTRIBUTE_NODE=2;
dojo.dom.TEXT_NODE=3;
dojo.dom.CDATA_SECTION_NODE=4;
dojo.dom.ENTITY_REFERENCE_NODE=5;
dojo.dom.ENTITY_NODE=6;
dojo.dom.PROCESSING_INSTRUCTION_NODE=7;
dojo.dom.COMMENT_NODE=8;
dojo.dom.DOCUMENT_NODE=9;
dojo.dom.DOCUMENT_TYPE_NODE=10;
dojo.dom.DOCUMENT_FRAGMENT_NODE=11;
dojo.dom.NOTATION_NODE=12;
dojo.dom.dojoml="http://www.dojotoolkit.org/2004/dojoml";
dojo.dom.xmlns={svg:"http://www.w3.org/2000/svg",smil:"http://www.w3.org/2001/SMIL20/",mml:"http://www.w3.org/1998/Math/MathML",cml:"http://www.xml-cml.org",xlink:"http://www.w3.org/1999/xlink",xhtml:"http://www.w3.org/1999/xhtml",xul:"http://www.mozilla.org/keymaster/gatekeeper/there.is.only.xul",xbl:"http://www.mozilla.org/xbl",fo:"http://www.w3.org/1999/XSL/Format",xsl:"http://www.w3.org/1999/XSL/Transform",xslt:"http://www.w3.org/1999/XSL/Transform",xi:"http://www.w3.org/2001/XInclude",xforms:"http://www.w3.org/2002/01/xforms",saxon:"http://icl.com/saxon",xalan:"http://xml.apache.org/xslt",xsd:"http://www.w3.org/2001/XMLSchema",dt:"http://www.w3.org/2001/XMLSchema-datatypes",xsi:"http://www.w3.org/2001/XMLSchema-instance",rdf:"http://www.w3.org/1999/02/22-rdf-syntax-ns#",rdfs:"http://www.w3.org/2000/01/rdf-schema#",dc:"http://purl.org/dc/elements/1.1/",dcq:"http://purl.org/dc/qualifiers/1.0","soap-env":"http://schemas.xmlsoap.org/soap/envelope/",wsdl:"http://schemas.xmlsoap.org/wsdl/",AdobeExtensions:"http://ns.adobe.com/AdobeSVGViewerExtensions/3.0/"};
dojo.dom.isNode=function(wh){
if(typeof Element=="function"){
try{
return wh instanceof Element;
}
catch(e){
}
}else{
return wh&&!isNaN(wh.nodeType);
}
};
dojo.dom.getUniqueId=function(){
var _124=dojo.doc();
do{
var id="dj_unique_"+(++arguments.callee._idIncrement);
}while(_124.getElementById(id));
return id;
};
dojo.dom.getUniqueId._idIncrement=0;
dojo.dom.firstElement=dojo.dom.getFirstChildElement=function(_126,_127){
var node=_126.firstChild;
while(node&&node.nodeType!=dojo.dom.ELEMENT_NODE){
node=node.nextSibling;
}
if(_127&&node&&node.tagName&&node.tagName.toLowerCase()!=_127.toLowerCase()){
node=dojo.dom.nextElement(node,_127);
}
return node;
};
dojo.dom.lastElement=dojo.dom.getLastChildElement=function(_129,_12a){
var node=_129.lastChild;
while(node&&node.nodeType!=dojo.dom.ELEMENT_NODE){
node=node.previousSibling;
}
if(_12a&&node&&node.tagName&&node.tagName.toLowerCase()!=_12a.toLowerCase()){
node=dojo.dom.prevElement(node,_12a);
}
return node;
};
dojo.dom.nextElement=dojo.dom.getNextSiblingElement=function(node,_12d){
if(!node){
return null;
}
do{
node=node.nextSibling;
}while(node&&node.nodeType!=dojo.dom.ELEMENT_NODE);
if(node&&_12d&&_12d.toLowerCase()!=node.tagName.toLowerCase()){
return dojo.dom.nextElement(node,_12d);
}
return node;
};
dojo.dom.prevElement=dojo.dom.getPreviousSiblingElement=function(node,_12f){
if(!node){
return null;
}
if(_12f){
_12f=_12f.toLowerCase();
}
do{
node=node.previousSibling;
}while(node&&node.nodeType!=dojo.dom.ELEMENT_NODE);
if(node&&_12f&&_12f.toLowerCase()!=node.tagName.toLowerCase()){
return dojo.dom.prevElement(node,_12f);
}
return node;
};
dojo.dom.moveChildren=function(_130,_131,trim){
var _133=0;
if(trim){
while(_130.hasChildNodes()&&_130.firstChild.nodeType==dojo.dom.TEXT_NODE){
_130.removeChild(_130.firstChild);
}
while(_130.hasChildNodes()&&_130.lastChild.nodeType==dojo.dom.TEXT_NODE){
_130.removeChild(_130.lastChild);
}
}
while(_130.hasChildNodes()){
_131.appendChild(_130.firstChild);
_133++;
}
return _133;
};
dojo.dom.copyChildren=function(_134,_135,trim){
var _137=_134.cloneNode(true);
return this.moveChildren(_137,_135,trim);
};
dojo.dom.replaceChildren=function(node,_139){
var _13a=[];
if(dojo.render.html.ie){
for(var i=0;i<node.childNodes.length;i++){
_13a.push(node.childNodes[i]);
}
}
dojo.dom.removeChildren(node);
node.appendChild(_139);
for(var i=0;i<_13a.length;i++){
dojo.dom.destroyNode(_13a[i]);
}
};
dojo.dom.removeChildren=function(node){
var _13d=node.childNodes.length;
while(node.hasChildNodes()){
dojo.dom.removeNode(node.firstChild);
}
return _13d;
};
dojo.dom.replaceNode=function(node,_13f){
return node.parentNode.replaceChild(_13f,node);
};
dojo.dom.destroyNode=function(node){
if(node.parentNode){
node=dojo.dom.removeNode(node);
}
if(node.nodeType!=3){
if(dojo.exists("dojo.event.browser.clean")){
dojo.event.browser.clean(node);
}
if(dojo.render.html.ie){
node.outerHTML="";
}
}
};
dojo.dom.removeNode=function(node){
if(node&&node.parentNode){
return node.parentNode.removeChild(node);
}
};
dojo.dom.getAncestors=function(node,_143,_144){
var _145=[];
var _146=(_143&&(_143 instanceof Function||typeof _143=="function"));
while(node){
if(!_146||_143(node)){
_145.push(node);
}
if(_144&&_145.length>0){
return _145[0];
}
node=node.parentNode;
}
if(_144){
return null;
}
return _145;
};
dojo.dom.getAncestorsByTag=function(node,tag,_149){
tag=tag.toLowerCase();
return dojo.dom.getAncestors(node,function(el){
return ((el.tagName)&&(el.tagName.toLowerCase()==tag));
},_149);
};
dojo.dom.getFirstAncestorByTag=function(node,tag){
return dojo.dom.getAncestorsByTag(node,tag,true);
};
dojo.dom.isDescendantOf=function(node,_14e,_14f){
if(_14f&&node){
node=node.parentNode;
}
while(node){
if(node==_14e){
return true;
}
node=node.parentNode;
}
return false;
};
dojo.dom.innerXML=function(node){
if(node.innerXML){
return node.innerXML;
}else{
if(node.xml){
return node.xml;
}else{
if(typeof XMLSerializer!="undefined"){
return (new XMLSerializer()).serializeToString(node);
}
}
}
};
dojo.dom.createDocument=function(){
var doc=null;
var _152=dojo.doc();
if(!dj_undef("ActiveXObject")){
var _153=["MSXML2","Microsoft","MSXML","MSXML3"];
for(var i=0;i<_153.length;i++){
try{
doc=new ActiveXObject(_153[i]+".XMLDOM");
}
catch(e){
}
if(doc){
break;
}
}
}else{
if((_152.implementation)&&(_152.implementation.createDocument)){
doc=_152.implementation.createDocument("","",null);
}
}
return doc;
};
dojo.dom.createDocumentFromText=function(str,_156){
if(!_156){
_156="text/xml";
}
if(!dj_undef("DOMParser")){
var _157=new DOMParser();
return _157.parseFromString(str,_156);
}else{
if(!dj_undef("ActiveXObject")){
var _158=dojo.dom.createDocument();
if(_158){
_158.async=false;
_158.loadXML(str);
return _158;
}else{
dojo.debug("toXml didn't work?");
}
}else{
var _159=dojo.doc();
if(_159.createElement){
var tmp=_159.createElement("xml");
tmp.innerHTML=str;
if(_159.implementation&&_159.implementation.createDocument){
var _15b=_159.implementation.createDocument("foo","",null);
for(var i=0;i<tmp.childNodes.length;i++){
_15b.importNode(tmp.childNodes.item(i),true);
}
return _15b;
}
return ((tmp.document)&&(tmp.document.firstChild?tmp.document.firstChild:tmp));
}
}
}
return null;
};
dojo.dom.prependChild=function(node,_15e){
if(_15e.firstChild){
_15e.insertBefore(node,_15e.firstChild);
}else{
_15e.appendChild(node);
}
return true;
};
dojo.dom.insertBefore=function(node,ref,_161){
if((_161!=true)&&(node===ref||node.nextSibling===ref)){
return false;
}
var _162=ref.parentNode;
_162.insertBefore(node,ref);
return true;
};
dojo.dom.insertAfter=function(node,ref,_165){
var pn=ref.parentNode;
if(ref==pn.lastChild){
if((_165!=true)&&(node===ref)){
return false;
}
pn.appendChild(node);
}else{
return this.insertBefore(node,ref.nextSibling,_165);
}
return true;
};
dojo.dom.insertAtPosition=function(node,ref,_169){
if((!node)||(!ref)||(!_169)){
return false;
}
switch(_169.toLowerCase()){
case "before":
return dojo.dom.insertBefore(node,ref);
case "after":
return dojo.dom.insertAfter(node,ref);
case "first":
if(ref.firstChild){
return dojo.dom.insertBefore(node,ref.firstChild);
}else{
ref.appendChild(node);
return true;
}
break;
default:
ref.appendChild(node);
return true;
}
};
dojo.dom.insertAtIndex=function(node,_16b,_16c){
var _16d=_16b.childNodes;
if(!_16d.length||_16d.length==_16c){
_16b.appendChild(node);
return true;
}
if(_16c==0){
return dojo.dom.prependChild(node,_16b);
}
return dojo.dom.insertAfter(node,_16d[_16c-1]);
};
dojo.dom.textContent=function(node,text){
if(arguments.length>1){
var _170=dojo.doc();
dojo.dom.replaceChildren(node,_170.createTextNode(text));
return text;
}else{
if(node["textContent"]!=undefined){
return node.textContent;
}
var _171="";
if(node==null){
return _171;
}
var i=0,n;
while(n=node.childNodes[i++]){
switch(n.nodeType){
case 1:
case 5:
_171+=dojo.dom.textContent(n);
break;
case 3:
case 2:
case 4:
_171+=n.nodeValue;
break;
default:
break;
}
}
return _171;
}
};
dojo.dom.hasParent=function(node){
return Boolean(node&&node.parentNode&&dojo.dom.isNode(node.parentNode));
};
dojo.dom.isTag=function(node){
if(node&&node.tagName){
for(var i=1;i<arguments.length;i++){
if(node.tagName==String(arguments[i])){
return String(arguments[i]);
}
}
}
return "";
};
dojo.dom.setAttributeNS=function(elem,_178,_179,_17a){
if(elem==null||((elem==undefined)&&(typeof elem=="undefined"))){
dojo.raise("No element given to dojo.dom.setAttributeNS");
}
if(!((elem.setAttributeNS==undefined)&&(typeof elem.setAttributeNS=="undefined"))){
elem.setAttributeNS(_178,_179,_17a);
}else{
var _17b=elem.ownerDocument;
var _17c=_17b.createNode(2,_179,_178);
_17c.nodeValue=_17a;
elem.setAttributeNode(_17c);
}
};
dojo.provide("dojo.html.common");
dojo.lang.mixin(dojo.html,dojo.dom);
dojo.html.getEventTarget=function(evt){
if(!evt){
evt=dojo.global().event||{};
}
var t=(evt.srcElement?evt.srcElement:(evt.target?evt.target:null));
while((t)&&(t.nodeType!=1)){
t=t.parentNode;
}
return t;
};
dojo.html.getViewport=function(){
var _17f=dojo.global();
var _180=dojo.doc();
var w=0;
var h=0;
if(dojo.render.html.mozilla){
w=_180.documentElement.clientWidth;
h=_17f.innerHeight;
}else{
if(!dojo.render.html.opera&&_17f.innerWidth){
w=_17f.innerWidth;
h=_17f.innerHeight;
}else{
if(!dojo.render.html.opera&&dojo.exists("documentElement.clientWidth",_180)){
var w2=_180.documentElement.clientWidth;
if(!w||w2&&w2<w){
w=w2;
}
h=_180.documentElement.clientHeight;
}else{
if(dojo.body().clientWidth){
w=dojo.body().clientWidth;
h=dojo.body().clientHeight;
}
}
}
}
return {width:w,height:h};
};
dojo.html.getScroll=function(){
var _184=dojo.global();
var _185=dojo.doc();
var top=_184.pageYOffset||_185.documentElement.scrollTop||dojo.body().scrollTop||0;
var left=_184.pageXOffset||_185.documentElement.scrollLeft||dojo.body().scrollLeft||0;
return {top:top,left:left,offset:{x:left,y:top}};
};
dojo.html.getParentByType=function(node,type){
var _18a=dojo.doc();
var _18b=dojo.byId(node);
type=type.toLowerCase();
while((_18b)&&(_18b.nodeName.toLowerCase()!=type)){
if(_18b==(_18a["body"]||_18a["documentElement"])){
return null;
}
_18b=_18b.parentNode;
}
return _18b;
};
dojo.html.getAttribute=function(node,attr){
node=dojo.byId(node);
if((!node)||(!node.getAttribute)){
return null;
}
var ta=typeof attr=="string"?attr:new String(attr);
var v=node.getAttribute(ta.toUpperCase());
if((v)&&(typeof v=="string")&&(v!="")){
return v;
}
if(v&&v.value){
return v.value;
}
if((node.getAttributeNode)&&(node.getAttributeNode(ta))){
return (node.getAttributeNode(ta)).value;
}else{
if(node.getAttribute(ta)){
return node.getAttribute(ta);
}else{
if(node.getAttribute(ta.toLowerCase())){
return node.getAttribute(ta.toLowerCase());
}
}
}
return null;
};
dojo.html.hasAttribute=function(node,attr){
return dojo.html.getAttribute(dojo.byId(node),attr)?true:false;
};
dojo.html.getCursorPosition=function(e){
e=e||dojo.global().event;
var _193={x:0,y:0};
if(e.pageX||e.pageY){
_193.x=e.pageX;
_193.y=e.pageY;
}else{
var de=dojo.doc().documentElement;
var db=dojo.body();
_193.x=e.clientX+((de||db)["scrollLeft"])-((de||db)["clientLeft"]);
_193.y=e.clientY+((de||db)["scrollTop"])-((de||db)["clientTop"]);
}
return _193;
};
dojo.html.isTag=function(node){
node=dojo.byId(node);
if(node&&node.tagName){
for(var i=1;i<arguments.length;i++){
if(node.tagName.toLowerCase()==String(arguments[i]).toLowerCase()){
return String(arguments[i]).toLowerCase();
}
}
}
return "";
};
if(dojo.render.html.ie&&!dojo.render.html.ie70){
if(window.location.href.substr(0,6).toLowerCase()!="https:"){
(function(){
var _198=dojo.doc().createElement("script");
_198.src="javascript:'dojo.html.createExternalElement=function(doc, tag){ return doc.createElement(tag); }'";
dojo.doc().getElementsByTagName("head")[0].appendChild(_198);
})();
}
}else{
dojo.html.createExternalElement=function(doc,tag){
return doc.createElement(tag);
};
}
dojo.provide("dojo.uri.Uri");
dojo.uri=new function(){
this.dojoUri=function(uri){
return new dojo.uri.Uri(dojo.hostenv.getBaseScriptUri(),uri);
};
this.moduleUri=function(_19c,uri){
var loc=dojo.hostenv.getModuleSymbols(_19c).join("/");
if(!loc){
return null;
}
if(loc.lastIndexOf("/")!=loc.length-1){
loc+="/";
}
var _19f=loc.indexOf(":");
var _1a0=loc.indexOf("/");
if(loc.charAt(0)!="/"&&(_19f==-1||_19f>_1a0)){
loc=dojo.hostenv.getBaseScriptUri()+loc;
}
return new dojo.uri.Uri(loc,uri);
};
this.Uri=function(){
var uri=arguments[0];
for(var i=1;i<arguments.length;i++){
if(!arguments[i]){
continue;
}
var _1a3=new dojo.uri.Uri(arguments[i].toString());
var _1a4=new dojo.uri.Uri(uri.toString());
if((_1a3.path=="")&&(_1a3.scheme==null)&&(_1a3.authority==null)&&(_1a3.query==null)){
if(_1a3.fragment!=null){
_1a4.fragment=_1a3.fragment;
}
_1a3=_1a4;
}else{
if(_1a3.scheme==null){
_1a3.scheme=_1a4.scheme;
if(_1a3.authority==null){
_1a3.authority=_1a4.authority;
if(_1a3.path.charAt(0)!="/"){
var path=_1a4.path.substring(0,_1a4.path.lastIndexOf("/")+1)+_1a3.path;
var segs=path.split("/");
for(var j=0;j<segs.length;j++){
if(segs[j]=="."){
if(j==segs.length-1){
segs[j]="";
}else{
segs.splice(j,1);
j--;
}
}else{
if(j>0&&!(j==1&&segs[0]=="")&&segs[j]==".."&&segs[j-1]!=".."){
if(j==segs.length-1){
segs.splice(j,1);
segs[j-1]="";
}else{
segs.splice(j-1,2);
j-=2;
}
}
}
}
_1a3.path=segs.join("/");
}
}
}
}
uri="";
if(_1a3.scheme!=null){
uri+=_1a3.scheme+":";
}
if(_1a3.authority!=null){
uri+="//"+_1a3.authority;
}
uri+=_1a3.path;
if(_1a3.query!=null){
uri+="?"+_1a3.query;
}
if(_1a3.fragment!=null){
uri+="#"+_1a3.fragment;
}
}
this.uri=uri.toString();
var _1a8="^(([^:/?#]+):)?(//([^/?#]*))?([^?#]*)(\\?([^#]*))?(#(.*))?$";
var r=this.uri.match(new RegExp(_1a8));
this.scheme=r[2]||(r[1]?"":null);
this.authority=r[4]||(r[3]?"":null);
this.path=r[5];
this.query=r[7]||(r[6]?"":null);
this.fragment=r[9]||(r[8]?"":null);
if(this.authority!=null){
_1a8="^((([^:]+:)?([^@]+))@)?([^:]*)(:([0-9]+))?$";
r=this.authority.match(new RegExp(_1a8));
this.user=r[3]||null;
this.password=r[4]||null;
this.host=r[5];
this.port=r[7]||null;
}
this.toString=function(){
return this.uri;
};
};
};
dojo.provide("dojo.html.style");
dojo.html.getClass=function(node){
node=dojo.byId(node);
if(!node){
return "";
}
var cs="";
if(node.className){
cs=node.className;
}else{
if(dojo.html.hasAttribute(node,"class")){
cs=dojo.html.getAttribute(node,"class");
}
}
return cs.replace(/^\s+|\s+$/g,"");
};
dojo.html.getClasses=function(node){
var c=dojo.html.getClass(node);
return (c=="")?[]:c.split(/\s+/g);
};
dojo.html.hasClass=function(node,_1af){
return (new RegExp("(^|\\s+)"+_1af+"(\\s+|$)")).test(dojo.html.getClass(node));
};
dojo.html.prependClass=function(node,_1b1){
_1b1+=" "+dojo.html.getClass(node);
return dojo.html.setClass(node,_1b1);
};
dojo.html.addClass=function(node,_1b3){
if(dojo.html.hasClass(node,_1b3)){
return false;
}
_1b3=(dojo.html.getClass(node)+" "+_1b3).replace(/^\s+|\s+$/g,"");
return dojo.html.setClass(node,_1b3);
};
dojo.html.setClass=function(node,_1b5){
node=dojo.byId(node);
var cs=new String(_1b5);
try{
if(typeof node.className=="string"){
node.className=cs;
}else{
if(node.setAttribute){
node.setAttribute("class",_1b5);
node.className=cs;
}else{
return false;
}
}
}
catch(e){
dojo.debug("dojo.html.setClass() failed",e);
}
return true;
};
dojo.html.removeClass=function(node,_1b8,_1b9){
try{
if(!_1b9){
var _1ba=dojo.html.getClass(node).replace(new RegExp("(^|\\s+)"+_1b8+"(\\s+|$)"),"$1$2");
}else{
var _1ba=dojo.html.getClass(node).replace(_1b8,"");
}
dojo.html.setClass(node,_1ba);
}
catch(e){
dojo.debug("dojo.html.removeClass() failed",e);
}
return true;
};
dojo.html.replaceClass=function(node,_1bc,_1bd){
dojo.html.removeClass(node,_1bd);
dojo.html.addClass(node,_1bc);
};
dojo.html.classMatchType={ContainsAll:0,ContainsAny:1,IsOnly:2};
dojo.html.getElementsByClass=function(_1be,_1bf,_1c0,_1c1,_1c2){
_1c2=false;
var _1c3=dojo.doc();
_1bf=dojo.byId(_1bf)||_1c3;
var _1c4=_1be.split(/\s+/g);
var _1c5=[];
if(_1c1!=1&&_1c1!=2){
_1c1=0;
}
var _1c6=new RegExp("(\\s|^)(("+_1c4.join(")|(")+"))(\\s|$)");
var _1c7=_1c4.join(" ").length;
var _1c8=[];
if(!_1c2&&_1c3.evaluate){
var _1c9=".//"+(_1c0||"*")+"[contains(";
if(_1c1!=dojo.html.classMatchType.ContainsAny){
_1c9+="concat(' ',@class,' '), ' "+_1c4.join(" ') and contains(concat(' ',@class,' '), ' ")+" ')";
if(_1c1==2){
_1c9+=" and string-length(@class)="+_1c7+"]";
}else{
_1c9+="]";
}
}else{
_1c9+="concat(' ',@class,' '), ' "+_1c4.join(" ') or contains(concat(' ',@class,' '), ' ")+" ')]";
}
var _1ca=_1c3.evaluate(_1c9,_1bf,null,XPathResult.ANY_TYPE,null);
var _1cb=_1ca.iterateNext();
while(_1cb){
try{
_1c8.push(_1cb);
_1cb=_1ca.iterateNext();
}
catch(e){
break;
}
}
return _1c8;
}else{
if(!_1c0){
_1c0="*";
}
_1c8=_1bf.getElementsByTagName(_1c0);
var node,i=0;
outer:
while(node=_1c8[i++]){
var _1ce=dojo.html.getClasses(node);
if(_1ce.length==0){
continue outer;
}
var _1cf=0;
for(var j=0;j<_1ce.length;j++){
if(_1c6.test(_1ce[j])){
if(_1c1==dojo.html.classMatchType.ContainsAny){
_1c5.push(node);
continue outer;
}else{
_1cf++;
}
}else{
if(_1c1==dojo.html.classMatchType.IsOnly){
continue outer;
}
}
}
if(_1cf==_1c4.length){
if((_1c1==dojo.html.classMatchType.IsOnly)&&(_1cf==_1ce.length)){
_1c5.push(node);
}else{
if(_1c1==dojo.html.classMatchType.ContainsAll){
_1c5.push(node);
}
}
}
}
return _1c5;
}
};
dojo.html.getElementsByClassName=dojo.html.getElementsByClass;
dojo.html.toCamelCase=function(_1d1){
var arr=_1d1.split("-"),cc=arr[0];
for(var i=1;i<arr.length;i++){
cc+=arr[i].charAt(0).toUpperCase()+arr[i].substring(1);
}
return cc;
};
dojo.html.toSelectorCase=function(_1d5){
return _1d5.replace(/([A-Z])/g,"-$1").toLowerCase();
};
if(dojo.render.html.ie){
dojo.html.getComputedStyle=function(node,_1d7,_1d8){
node=dojo.byId(node);
if(!node||!node.currentStyle){
return _1d8;
}
return node.currentStyle[dojo.html.toCamelCase(_1d7)];
};
dojo.html.getComputedStyles=function(node){
return node.currentStyle;
};
}else{
dojo.html.getComputedStyle=function(node,_1db,_1dc){
node=dojo.byId(node);
if(!node||!node.style){
return _1dc;
}
var s=node.ownerDocument.defaultView.getComputedStyle(node,null);
return (s&&s[dojo.html.toCamelCase(_1db)])||"";
};
dojo.html.getComputedStyles=function(node){
return node.ownerDocument.defaultView.getComputedStyle(node,null);
};
}
dojo.html.getStyleProperty=function(node,_1e0){
node=dojo.byId(node);
return (node&&node.style?node.style[dojo.html.toCamelCase(_1e0)]:undefined);
};
dojo.html.getStyle=function(node,_1e2){
var _1e3=dojo.html.getStyleProperty(node,_1e2);
return (_1e3?_1e3:dojo.html.getComputedStyle(node,_1e2));
};
dojo.html.setStyle=function(node,_1e5,_1e6){
node=dojo.byId(node);
if(node&&node.style){
var _1e7=dojo.html.toCamelCase(_1e5);
node.style[_1e7]=_1e6;
}
};
dojo.html.setStyleText=function(_1e8,text){
try{
_1e8.style.cssText=text;
}
catch(e){
_1e8.setAttribute("style",text);
}
};
dojo.html.copyStyle=function(_1ea,_1eb){
if(!_1eb.style.cssText){
_1ea.setAttribute("style",_1eb.getAttribute("style"));
}else{
_1ea.style.cssText=_1eb.style.cssText;
}
dojo.html.addClass(_1ea,dojo.html.getClass(_1eb));
};
dojo.html.getUnitValue=function(node,_1ed,_1ee){
var s=dojo.html.getComputedStyle(node,_1ed);
if((!s)||((s=="auto")&&(_1ee))){
return {value:0,units:"px"};
}
var _1f0=s.match(/(\-?[\d.]+)([a-z%]*)/i);
if(!_1f0){
return dojo.html.getUnitValue.bad;
}
return {value:Number(_1f0[1]),units:_1f0[2].toLowerCase()};
};
dojo.html.getUnitValue.bad={value:NaN,units:""};
if(dojo.render.html.ie){
dojo.html.toPixelValue=function(_1f1,_1f2){
if(!_1f2){
return 0;
}
if(_1f2.slice(-2)=="px"){
return parseFloat(_1f2);
}
var _1f3=0;
with(_1f1){
var _1f4=style.left;
var _1f5=runtimeStyle.left;
runtimeStyle.left=currentStyle.left;
try{
style.left=_1f2||0;
_1f3=style.pixelLeft;
style.left=_1f4;
runtimeStyle.left=_1f5;
}
catch(e){
}
}
return _1f3;
};
}else{
dojo.html.toPixelValue=function(_1f6,_1f7){
return (_1f7&&(_1f7.slice(-2)=="px")?parseFloat(_1f7):0);
};
}
dojo.html.getPixelValue=function(node,_1f9,_1fa){
return dojo.html.toPixelValue(node,dojo.html.getComputedStyle(node,_1f9));
};
dojo.html.setPositivePixelValue=function(node,_1fc,_1fd){
if(isNaN(_1fd)){
return false;
}
node.style[_1fc]=Math.max(0,_1fd)+"px";
return true;
};
dojo.html.styleSheet=null;
dojo.html.insertCssRule=function(_1fe,_1ff,_200){
if(!dojo.html.styleSheet){
if(document.createStyleSheet){
dojo.html.styleSheet=document.createStyleSheet();
}else{
if(document.styleSheets[0]){
dojo.html.styleSheet=document.styleSheets[0];
}else{
return null;
}
}
}
var ss=dojo.html.styleSheet;
if(arguments.length<3){
if(ss.cssRules){
_200=ss.cssRules.length;
}else{
if(ss.rules){
_200=ss.rules.length;
}else{
return null;
}
}
}
if(ss.insertRule){
var rule=_1fe+" { "+_1ff+" }";
return ss.insertRule(rule,_200);
}else{
if(ss.addRule){
return ss.addRule(_1fe,_1ff,_200);
}else{
return null;
}
}
};
dojo.html.removeCssRule=function(_203){
var ss=dojo.html.styleSheet;
if(!ss){
dojo.debug("no stylesheet defined for removing rules");
return false;
}
if(dojo.render.html.ie){
if(!_203){
_203=ss.rules.length;
ss.removeRule(_203);
}
}else{
if(document.styleSheets[0]){
if(!_203){
_203=ss.cssRules.length;
}
ss.deleteRule(_203);
}
}
return true;
};
dojo.html._insertedCssFiles=[];
dojo.html.insertCssFile=function(URI,doc,_207,_208){
if(!URI){
return;
}
if(!doc){
doc=document;
}
var _209=dojo.hostenv.getText(URI,false,_208);
if(_209===null){
return;
}
_209=dojo.html.fixPathsInCssText(_209,URI);
if(_207){
var idx=-1,node,ent=dojo.html._insertedCssFiles;
for(var i=0;i<ent.length;i++){
if((ent[i].doc==doc)&&(ent[i].cssText==_209)){
idx=i;
node=ent[i].nodeRef;
break;
}
}
if(node){
var _20e=doc.getElementsByTagName("style");
for(var i=0;i<_20e.length;i++){
if(_20e[i]==node){
return;
}
}
dojo.html._insertedCssFiles.shift(idx,1);
}
}
var _20f=dojo.html.insertCssText(_209,doc);
dojo.html._insertedCssFiles.push({"doc":doc,"cssText":_209,"nodeRef":_20f});
if(_20f&&djConfig.isDebug){
_20f.setAttribute("dbgHref",URI);
}
return _20f;
};
dojo.html.insertCssText=function(_210,doc,URI){
if(!_210){
return;
}
if(!doc){
doc=document;
}
if(URI){
_210=dojo.html.fixPathsInCssText(_210,URI);
}
var _213=doc.createElement("style");
_213.setAttribute("type","text/css");
var head=doc.getElementsByTagName("head")[0];
if(!head){
dojo.debug("No head tag in document, aborting styles");
return;
}else{
head.appendChild(_213);
}
if(_213.styleSheet){
var _215=function(){
try{
_213.styleSheet.cssText=_210;
}
catch(e){
dojo.debug(e);
}
};
if(_213.styleSheet.disabled){
setTimeout(_215,10);
}else{
_215();
}
}else{
var _216=doc.createTextNode(_210);
_213.appendChild(_216);
}
return _213;
};
dojo.html.fixPathsInCssText=function(_217,URI){
if(!_217||!URI){
return;
}
var _219,str="",url="",_21c="[\\t\\s\\w\\(\\)\\/\\.\\\\'\"-:#=&?~]+";
var _21d=new RegExp("url\\(\\s*("+_21c+")\\s*\\)");
var _21e=/(file|https?|ftps?):\/\//;
regexTrim=new RegExp("^[\\s]*(['\"]?)("+_21c+")\\1[\\s]*?$");
if(dojo.render.html.ie55||dojo.render.html.ie60){
var _21f=new RegExp("AlphaImageLoader\\((.*)src=['\"]("+_21c+")['\"]");
while(_219=_21f.exec(_217)){
url=_219[2].replace(regexTrim,"$2");
if(!_21e.exec(url)){
url=(new dojo.uri.Uri(URI,url).toString());
}
str+=_217.substring(0,_219.index)+"AlphaImageLoader("+_219[1]+"src='"+url+"'";
_217=_217.substr(_219.index+_219[0].length);
}
_217=str+_217;
str="";
}
while(_219=_21d.exec(_217)){
url=_219[1].replace(regexTrim,"$2");
if(!_21e.exec(url)){
url=(new dojo.uri.Uri(URI,url).toString());
}
str+=_217.substring(0,_219.index)+"url("+url+")";
_217=_217.substr(_219.index+_219[0].length);
}
return str+_217;
};
dojo.html.setActiveStyleSheet=function(_220){
var i=0,a,els=dojo.doc().getElementsByTagName("link");
while(a=els[i++]){
if(a.getAttribute("rel").indexOf("style")!=-1&&a.getAttribute("title")){
a.disabled=true;
if(a.getAttribute("title")==_220){
a.disabled=false;
}
}
}
};
dojo.html.getActiveStyleSheet=function(){
var i=0,a,els=dojo.doc().getElementsByTagName("link");
while(a=els[i++]){
if((a.getAttribute("rel").indexOf("style")!=-1)&&(a.getAttribute("title"))&&(!a.disabled)){
return a.getAttribute("title");
}
}
return null;
};
dojo.html.getPreferredStyleSheet=function(){
var i=0,a,els=dojo.doc().getElementsByTagName("link");
while(a=els[i++]){
if((a.getAttribute("rel").indexOf("style")!=-1)&&(a.getAttribute("rel").indexOf("alt")==-1)&&(a.getAttribute("title"))){
return a.getAttribute("title");
}
}
return null;
};
dojo.html.applyBrowserClass=function(node){
var drh=dojo.render.html;
var _22c={dj_ie:drh.ie,dj_ie55:drh.ie55,dj_ie6:drh.ie60,dj_ie7:drh.ie70,dj_iequirks:drh.ie&&drh.quirks,dj_opera:drh.opera,dj_opera8:drh.opera&&(Math.floor(dojo.render.version)==8),dj_opera9:drh.opera&&(Math.floor(dojo.render.version)==9),dj_khtml:drh.khtml,dj_safari:drh.safari,dj_gecko:drh.mozilla};
for(var p in _22c){
if(_22c[p]){
dojo.html.addClass(node,p);
}
}
};
dojo.kwCompoundRequire({common:["dojo.html.common","dojo.html.style"]});
dojo.provide("dojo.html.*");
dojo.provide("dojo.html.selection");
dojo.html.selectionType={NONE:0,TEXT:1,CONTROL:2};
dojo.html.clearSelection=function(){
var _22e=dojo.global();
var _22f=dojo.doc();
try{
if(_22e["getSelection"]){
if(dojo.render.html.safari){
_22e.getSelection().collapse();
}else{
_22e.getSelection().removeAllRanges();
}
}else{
if(_22f.selection){
if(_22f.selection.empty){
_22f.selection.empty();
}else{
if(_22f.selection.clear){
_22f.selection.clear();
}
}
}
}
return true;
}
catch(e){
dojo.debug(e);
return false;
}
};
dojo.html.disableSelection=function(_230){
_230=dojo.byId(_230)||dojo.body();
var h=dojo.render.html;
if(h.mozilla){
_230.style.MozUserSelect="none";
}else{
if(h.safari){
_230.style.KhtmlUserSelect="none";
}else{
if(h.ie){
_230.unselectable="on";
}else{
return false;
}
}
}
return true;
};
dojo.html.enableSelection=function(_232){
_232=dojo.byId(_232)||dojo.body();
var h=dojo.render.html;
if(h.mozilla){
_232.style.MozUserSelect="";
}else{
if(h.safari){
_232.style.KhtmlUserSelect="";
}else{
if(h.ie){
_232.unselectable="off";
}else{
return false;
}
}
}
return true;
};
dojo.html.selectInputText=function(_234){
var _235=dojo.global();
var _236=dojo.doc();
_234=dojo.byId(_234);
if(_236["selection"]&&dojo.body()["createTextRange"]){
var _237=_234.createTextRange();
_237.moveStart("character",0);
_237.moveEnd("character",_234.value.length);
_237.select();
}else{
if(_235["getSelection"]){
var _238=_235.getSelection();
_234.setSelectionRange(0,_234.value.length);
}
}
_234.focus();
};
dojo.lang.mixin(dojo.html.selection,{getType:function(){
if(dojo.doc()["selection"]){
return dojo.html.selectionType[dojo.doc().selection.type.toUpperCase()];
}else{
var _239=dojo.html.selectionType.TEXT;
var oSel;
try{
oSel=dojo.global().getSelection();
}
catch(e){
}
if(oSel&&oSel.rangeCount==1){
var _23b=oSel.getRangeAt(0);
if(_23b.startContainer==_23b.endContainer&&(_23b.endOffset-_23b.startOffset)==1&&_23b.startContainer.nodeType!=dojo.dom.TEXT_NODE){
_239=dojo.html.selectionType.CONTROL;
}
}
return _239;
}
},isCollapsed:function(){
var _23c=dojo.global();
var _23d=dojo.doc();
if(_23d["selection"]){
return _23d.selection.createRange().text=="";
}else{
if(_23c["getSelection"]){
var _23e=_23c.getSelection();
if(dojo.lang.isString(_23e)){
return _23e=="";
}else{
return _23e.isCollapsed||_23e.toString()=="";
}
}
}
},getSelectedElement:function(){
if(dojo.html.selection.getType()==dojo.html.selectionType.CONTROL){
if(dojo.doc()["selection"]){
var _23f=dojo.doc().selection.createRange();
if(_23f&&_23f.item){
return dojo.doc().selection.createRange().item(0);
}
}else{
var _240=dojo.global().getSelection();
return _240.anchorNode.childNodes[_240.anchorOffset];
}
}
},getParentElement:function(){
if(dojo.html.selection.getType()==dojo.html.selectionType.CONTROL){
var p=dojo.html.selection.getSelectedElement();
if(p){
return p.parentNode;
}
}else{
if(dojo.doc()["selection"]){
return dojo.doc().selection.createRange().parentElement();
}else{
var _242=dojo.global().getSelection();
if(_242){
var node=_242.anchorNode;
while(node&&node.nodeType!=dojo.dom.ELEMENT_NODE){
node=node.parentNode;
}
return node;
}
}
}
},getSelectedText:function(){
if(dojo.doc()["selection"]){
if(dojo.html.selection.getType()==dojo.html.selectionType.CONTROL){
return null;
}
return dojo.doc().selection.createRange().text;
}else{
var _244=dojo.global().getSelection();
if(_244){
return _244.toString();
}
}
},getSelectedHtml:function(){
if(dojo.doc()["selection"]){
if(dojo.html.selection.getType()==dojo.html.selectionType.CONTROL){
return null;
}
return dojo.doc().selection.createRange().htmlText;
}else{
var _245=dojo.global().getSelection();
if(_245&&_245.rangeCount){
var frag=_245.getRangeAt(0).cloneContents();
var div=document.createElement("div");
div.appendChild(frag);
return div.innerHTML;
}
return null;
}
},hasAncestorElement:function(_248){
return (dojo.html.selection.getAncestorElement.apply(this,arguments)!=null);
},getAncestorElement:function(_249){
var node=dojo.html.selection.getSelectedElement()||dojo.html.selection.getParentElement();
return dojo.html.selection.getParentOfType(node,arguments);
},getParentOfType:function(node,tags){
while(node){
if(dojo.html.selection.isTag(node,tags).length>0){
return node;
}
node=node.parentNode;
}
return null;
},isTag:function(node,tags){
if(node&&node.tagName){
for(var i=0;i<tags.length;i++){
if(node.tagName.toLowerCase()==String(tags[i]).toLowerCase()){
return String(tags[i]).toLowerCase();
}
}
}
return "";
},selectElement:function(_250){
var _251=dojo.global();
var _252=dojo.doc();
_250=dojo.byId(_250);
if(_252.selection&&dojo.body().createTextRange){
try{
var _253=dojo.body().createControlRange();
_253.addElement(_250);
_253.select();
}
catch(e){
dojo.html.selection.selectElementChildren(_250);
}
}else{
if(_251["getSelection"]){
var _254=_251.getSelection();
if(_254["removeAllRanges"]){
var _253=_252.createRange();
_253.selectNode(_250);
_254.removeAllRanges();
_254.addRange(_253);
}
}
}
},selectElementChildren:function(_255){
var _256=dojo.global();
var _257=dojo.doc();
_255=dojo.byId(_255);
if(_257.selection&&dojo.body().createTextRange){
var _258=_255.ownerDocument.body.createTextRange();
_258.moveToElementText(_255);
_258.select();
}else{
if(_256["getSelection"]){
var _259=_256.getSelection();
if(_259["setBaseAndExtent"]){
_259.setBaseAndExtent(_255,0,_255,_255.innerText.length-1);
}else{
if(_259["selectAllChildren"]){
_259.selectAllChildren(_255);
}
}
}
}
},getBookmark:function(){
var _25a;
var _25b=dojo.doc();
if(_25b["selection"]){
var _25c=_25b.selection.createRange();
if(_25b.selection.type.toUpperCase()=="CONTROL"){
if(_25c.length){
_25a=[];
var i=0;
while(i<_25c.length){
_25a.push(_25c.item(i++));
}
}else{
_25a=null;
}
}else{
_25a=_25c.getBookmark();
}
}else{
var _25e;
try{
_25e=dojo.global().getSelection();
}
catch(e){
}
if(_25e){
var _25c=_25e.getRangeAt(0);
_25a=_25c.cloneRange();
}else{
dojo.debug("No idea how to store the current selection for this browser!");
}
}
return _25a;
},moveToBookmark:function(_25f){
var _260=dojo.doc();
if(_260["selection"]){
if(dojo.lang.isArray(_25f)){
var _261=_260.body.createControlRange();
var i=0;
while(i<_25f.length){
_261.addElement(_25f[i++]);
}
_261.select();
}else{
var _261=_260.selection.createRange();
_261.moveToBookmark(_25f);
_261.select();
}
}else{
var _263;
try{
_263=dojo.global().getSelection();
}
catch(e){
}
if(_263&&_263["removeAllRanges"]){
_263.removeAllRanges();
_263.addRange(_25f);
}else{
dojo.debug("No idea how to restore selection for this browser!");
}
}
},collapse:function(_264){
if(dojo.global()["getSelection"]){
var _265=dojo.global().getSelection();
if(_265.removeAllRanges){
if(_264){
_265.collapseToStart();
}else{
_265.collapseToEnd();
}
}else{
dojo.global().getSelection().collapse(_264);
}
}else{
if(dojo.doc().selection){
var _266=dojo.doc().selection.createRange();
_266.collapse(_264);
_266.select();
}
}
},remove:function(){
if(dojo.doc().selection){
var _267=dojo.doc().selection;
if(_267.type.toUpperCase()!="NONE"){
_267.clear();
}
return _267;
}else{
var _267=dojo.global().getSelection();
_267.deleteFromDocument();
return _267;
}
}});
dojo.provide("dojo.string");
dojo.provide("dojo.lang.extras");
dojo.lang.setTimeout=function(func,_269){
var _26a=window,_26b=2;
if(!dojo.lang.isFunction(func)){
_26a=func;
func=_269;
_269=arguments[2];
_26b++;
}
if(dojo.lang.isString(func)){
func=_26a[func];
}
var args=[];
for(var i=_26b;i<arguments.length;i++){
args.push(arguments[i]);
}
return dojo.global().setTimeout(function(){
func.apply(_26a,args);
},_269);
};
dojo.lang.clearTimeout=function(_26e){
dojo.global().clearTimeout(_26e);
};
dojo.lang.getNameInObj=function(ns,item){
if(!ns){
ns=dj_global;
}
for(var x in ns){
if(ns[x]===item){
return new String(x);
}
}
return null;
};
dojo.lang.shallowCopy=function(obj,deep){
var i,ret;
if(obj===null){
return null;
}
if(dojo.lang.isObject(obj)){
ret=new obj.constructor();
for(i in obj){
if(dojo.lang.isUndefined(ret[i])){
ret[i]=deep?dojo.lang.shallowCopy(obj[i],deep):obj[i];
}
}
}else{
if(dojo.lang.isArray(obj)){
ret=[];
for(i=0;i<obj.length;i++){
ret[i]=deep?dojo.lang.shallowCopy(obj[i],deep):obj[i];
}
}else{
ret=obj;
}
}
return ret;
};
dojo.lang.firstValued=function(){
for(var i=0;i<arguments.length;i++){
if(typeof arguments[i]!="undefined"){
return arguments[i];
}
}
return undefined;
};
dojo.lang.getObjPathValue=function(_277,_278,_279){
dojo.deprecated("dojo.lang.getObjPathValue","use dojo.getObject","0.6");
with(dojo.parseObjPath(_277,_278,_279)){
return dojo.evalProp(prop,obj,_279);
}
};
dojo.lang.setObjPathValue=function(_27a,_27b,_27c,_27d){
dojo.deprecated("dojo.lang.setObjPathValue","use dojo.parseObjPath and the '=' operator","0.6");
if(arguments.length<4){
_27d=true;
}
with(dojo.parseObjPath(_27a,_27c,_27d)){
if(obj&&(_27d||(prop in obj))){
obj[prop]=_27b;
}
}
};
dojo.provide("dojo.io.common");
dojo.io.transports=[];
dojo.io.hdlrFuncNames=["load","error","timeout"];
dojo.io.Request=function(url,_27f,_280,_281){
if((arguments.length==1)&&(arguments[0].constructor==Object)){
this.fromKwArgs(arguments[0]);
}else{
this.url=url;
if(_27f){
this.mimetype=_27f;
}
if(_280){
this.transport=_280;
}
if(arguments.length>=4){
this.changeUrl=_281;
}
}
};
dojo.lang.extend(dojo.io.Request,{url:"",mimetype:"text/plain",method:"GET",content:undefined,transport:undefined,changeUrl:undefined,formNode:undefined,sync:false,bindSuccess:false,useCache:false,preventCache:false,jsonFilter:function(_282){
if((this.mimetype=="text/json-comment-filtered")||(this.mimetype=="application/json-comment-filtered")){
var _283=_282.indexOf("/*");
var _284=_282.lastIndexOf("*/");
if((_283==-1)||(_284==-1)){
dojo.debug("your JSON wasn't comment filtered!");
return "";
}
return _282.substring(_283+2,_284);
}
dojo.debug("please consider using a mimetype of text/json-comment-filtered to avoid potential security issues with JSON endpoints");
return _282;
},load:function(type,data,_287,_288){
},error:function(type,_28a,_28b,_28c){
},timeout:function(type,_28e,_28f,_290){
},handle:function(type,data,_293,_294){
},timeoutSeconds:0,abort:function(){
},fromKwArgs:function(_295){
if(_295["url"]){
_295.url=_295.url.toString();
}
if(_295["formNode"]){
_295.formNode=dojo.byId(_295.formNode);
}
if(!_295["method"]&&_295["formNode"]&&_295["formNode"].method){
_295.method=_295["formNode"].method;
}
if(!_295["handle"]&&_295["handler"]){
_295.handle=_295.handler;
}
if(!_295["load"]&&_295["loaded"]){
_295.load=_295.loaded;
}
if(!_295["changeUrl"]&&_295["changeURL"]){
_295.changeUrl=_295.changeURL;
}
_295.encoding=dojo.lang.firstValued(_295["encoding"],djConfig["bindEncoding"],"");
_295.sendTransport=dojo.lang.firstValued(_295["sendTransport"],djConfig["ioSendTransport"],false);
var _296=dojo.lang.isFunction;
for(var x=0;x<dojo.io.hdlrFuncNames.length;x++){
var fn=dojo.io.hdlrFuncNames[x];
if(_295[fn]&&_296(_295[fn])){
continue;
}
if(_295["handle"]&&_296(_295["handle"])){
_295[fn]=_295.handle;
}
}
dojo.lang.mixin(this,_295);
}});
dojo.io.Error=function(msg,type,num){
this.message=msg;
this.type=type||"unknown";
this.number=num||0;
};
dojo.io.transports.addTransport=function(name){
this.push(name);
this[name]=dojo.io[name];
};
dojo.io.bind=function(_29d){
if(!(_29d instanceof dojo.io.Request)){
try{
_29d=new dojo.io.Request(_29d);
}
catch(e){
dojo.debug(e);
}
}
var _29e="";
if(_29d["transport"]){
_29e=_29d["transport"];
if(!this[_29e]){
dojo.io.sendBindError(_29d,"No dojo.io.bind() transport with name '"+_29d["transport"]+"'.");
return _29d;
}
if(!this[_29e].canHandle(_29d)){
dojo.io.sendBindError(_29d,"dojo.io.bind() transport with name '"+_29d["transport"]+"' cannot handle this type of request.");
return _29d;
}
}else{
for(var x=0;x<dojo.io.transports.length;x++){
var tmp=dojo.io.transports[x];
if((this[tmp])&&(this[tmp].canHandle(_29d))){
_29e=tmp;
break;
}
}
if(_29e==""){
dojo.io.sendBindError(_29d,"None of the loaded transports for dojo.io.bind()"+" can handle the request.");
return _29d;
}
}
this[_29e].bind(_29d);
_29d.bindSuccess=true;
return _29d;
};
dojo.io.sendBindError=function(_2a1,_2a2){
if((typeof _2a1.error=="function"||typeof _2a1.handle=="function")&&(typeof setTimeout=="function"||typeof setTimeout=="object")){
var _2a3=new dojo.io.Error(_2a2);
setTimeout(function(){
_2a1[(typeof _2a1.error=="function")?"error":"handle"]("error",_2a3,null,_2a1);
},50);
}else{
dojo.raise(_2a2);
}
};
dojo.io.queueBind=function(_2a4){
if(!(_2a4 instanceof dojo.io.Request)){
try{
_2a4=new dojo.io.Request(_2a4);
}
catch(e){
dojo.debug(e);
}
}
var _2a5=_2a4.load;
_2a4.load=function(){
dojo.io._queueBindInFlight=false;
var ret=_2a5.apply(this,arguments);
dojo.io._dispatchNextQueueBind();
return ret;
};
var _2a7=_2a4.error;
_2a4.error=function(){
dojo.io._queueBindInFlight=false;
var ret=_2a7.apply(this,arguments);
dojo.io._dispatchNextQueueBind();
return ret;
};
dojo.io._bindQueue.push(_2a4);
dojo.io._dispatchNextQueueBind();
return _2a4;
};
dojo.io._dispatchNextQueueBind=function(){
if(!dojo.io._queueBindInFlight){
dojo.io._queueBindInFlight=true;
if(dojo.io._bindQueue.length>0){
dojo.io.bind(dojo.io._bindQueue.shift());
}else{
dojo.io._queueBindInFlight=false;
}
}
};
dojo.io._bindQueue=[];
dojo.io._queueBindInFlight=false;
dojo.io.argsFromMap=function(map,_2aa){
var _2ab=/utf/i.test(_2aa||"")?encodeURIComponent:dojo.string.encodeAscii;
var _2ac=[];
var _2ad=new Object();
for(var name in map){
var _2af=function(elt){
_2ac.push(_2ab(name)+"="+_2ab(elt));
};
if(!_2ad[name]){
var _2b1=map[name];
if(dojo.lang.isArray(_2b1)){
dojo.lang.forEach(_2b1,_2af);
}else{
_2af(_2b1);
}
}
}
return _2ac.join("&");
};
dojo.io.setIFrameSrc=function(_2b2,src,_2b4){
try{
var r=dojo.render.html;
if(!_2b4){
if(r.safari){
_2b2.location=src;
}else{
frames[_2b2.name].location=src;
}
}else{
var idoc;
if(r.ie){
idoc=_2b2.contentWindow.document;
}else{
if(r.safari){
idoc=_2b2.document;
}else{
idoc=_2b2.contentWindow;
}
}
if(!idoc){
_2b2.location=src;
return;
}else{
idoc.location.replace(src);
}
}
}
catch(e){
dojo.debug(e);
dojo.debug("setIFrameSrc: "+e);
}
};
dojo.provide("dojo.lang.array");
dojo.lang.mixin(dojo.lang,{has:function(obj,name){
try{
return typeof obj[name]!="undefined";
}
catch(e){
return false;
}
},isEmpty:function(obj){
if(dojo.lang.isArrayLike(obj)||dojo.lang.isString(obj)){
return obj.length===0;
}else{
if(dojo.lang.isObject(obj)){
var tmp={};
for(var x in obj){
if(obj[x]&&(!tmp[x])){
return false;
}
}
return true;
}
}
},map:function(arr,obj,_2be){
var _2bf=dojo.lang.isString(arr);
if(_2bf){
arr=arr.split("");
}
if(dojo.lang.isFunction(obj)&&(!_2be)){
_2be=obj;
obj=dj_global;
}else{
if(dojo.lang.isFunction(obj)&&_2be){
var _2c0=obj;
obj=_2be;
_2be=_2c0;
}
}
if(Array.map){
var _2c1=Array.map(arr,_2be,obj);
}else{
var _2c1=[];
for(var i=0;i<arr.length;++i){
_2c1.push(_2be.call(obj,arr[i]));
}
}
if(_2bf){
return _2c1.join("");
}else{
return _2c1;
}
},reduce:function(arr,_2c4,_2c5,_2c6){
var _2c7=_2c5;
if(arguments.length==2){
_2c7=arr[0];
arr=arr.slice(1);
}
var ob=_2c6||dj_global;
dojo.lang.map(arr,function(val){
_2c7=_2c4.call(ob,_2c7,val);
});
return _2c7;
},forEach:function(_2ca,_2cb,_2cc){
if(dojo.lang.isString(_2ca)){
_2ca=_2ca.split("");
}
if(Array.forEach){
Array.forEach(_2ca,_2cb,_2cc);
}else{
if(!_2cc){
_2cc=dj_global;
}
for(var i=0,l=_2ca.length;i<l;i++){
_2cb.call(_2cc,_2ca[i],i,_2ca);
}
}
},_everyOrSome:function(_2cf,arr,_2d1,_2d2){
if(dojo.lang.isString(arr)){
arr=arr.split("");
}
if(Array.every){
return Array[_2cf?"every":"some"](arr,_2d1,_2d2);
}else{
if(!_2d2){
_2d2=dj_global;
}
for(var i=0,l=arr.length;i<l;i++){
var _2d5=_2d1.call(_2d2,arr[i],i,arr);
if(_2cf&&!_2d5){
return false;
}else{
if((!_2cf)&&(_2d5)){
return true;
}
}
}
return Boolean(_2cf);
}
},every:function(arr,_2d7,_2d8){
return this._everyOrSome(true,arr,_2d7,_2d8);
},some:function(arr,_2da,_2db){
return this._everyOrSome(false,arr,_2da,_2db);
},filter:function(arr,_2dd,_2de){
var _2df=dojo.lang.isString(arr);
if(_2df){
arr=arr.split("");
}
var _2e0;
if(Array.filter){
_2e0=Array.filter(arr,_2dd,_2de);
}else{
if(!_2de){
if(arguments.length>=3){
dojo.raise("thisObject doesn't exist!");
}
_2de=dj_global;
}
_2e0=[];
for(var i=0;i<arr.length;i++){
if(_2dd.call(_2de,arr[i],i,arr)){
_2e0.push(arr[i]);
}
}
}
if(_2df){
return _2e0.join("");
}else{
return _2e0;
}
},unnest:function(){
var out=[];
for(var i=0;i<arguments.length;i++){
if(dojo.lang.isArrayLike(arguments[i])){
var add=dojo.lang.unnest.apply(this,arguments[i]);
out=out.concat(add);
}else{
out.push(arguments[i]);
}
}
return out;
},toArray:function(_2e5,_2e6){
var _2e7=[];
for(var i=_2e6||0;i<_2e5.length;i++){
_2e7.push(_2e5[i]);
}
return _2e7;
}});
dojo.provide("dojo.lang.func");
dojo.lang.hitch=function(_2e9,_2ea){
var args=[];
for(var x=2;x<arguments.length;x++){
args.push(arguments[x]);
}
var fcn=(dojo.lang.isString(_2ea)?_2e9[_2ea]:_2ea)||function(){
};
return function(){
var ta=args.concat([]);
for(var x=0;x<arguments.length;x++){
ta.push(arguments[x]);
}
return fcn.apply(_2e9,ta);
};
};
dojo.lang.anonCtr=0;
dojo.lang.anon={};
dojo.lang.nameAnonFunc=function(_2f0,_2f1,_2f2){
var isIE=(dojo.render.html.capable&&dojo.render.html["ie"]);
var jpn="$joinpoint";
var nso=(_2f1||dojo.lang.anon);
if(isIE){
var cn=_2f0["__dojoNameCache"];
if(cn&&nso[cn]===_2f0){
return _2f0["__dojoNameCache"];
}else{
if(cn){
var _2f7=cn.indexOf(jpn);
if(_2f7!=-1){
return cn.substring(0,_2f7);
}
}
}
}
if((_2f2)||((dj_global["djConfig"])&&(djConfig["slowAnonFuncLookups"]==true))){
for(var x in nso){
try{
if(nso[x]===_2f0){
if(isIE){
_2f0["__dojoNameCache"]=x;
var _2f7=x.indexOf(jpn);
if(_2f7!=-1){
x=x.substring(0,_2f7);
}
}
return x;
}
}
catch(e){
}
}
}
var ret="__"+dojo.lang.anonCtr++;
while(typeof nso[ret]!="undefined"){
ret="__"+dojo.lang.anonCtr++;
}
nso[ret]=_2f0;
return ret;
};
dojo.lang.forward=function(_2fa){
return function(){
return this[_2fa].apply(this,arguments);
};
};
dojo.lang.curry=function(_2fb,func){
var _2fd=[];
_2fb=_2fb||dj_global;
if(dojo.lang.isString(func)){
func=_2fb[func];
}
for(var x=2;x<arguments.length;x++){
_2fd.push(arguments[x]);
}
var _2ff=(func["__preJoinArity"]||func.length)-_2fd.length;
function gather(_300,_301,_302){
var _303=_302;
var _304=_301.slice(0);
for(var x=0;x<_300.length;x++){
_304.push(_300[x]);
}
_302=_302-_300.length;
if(_302<=0){
var res=func.apply(_2fb,_304);
_302=_303;
return res;
}else{
return function(){
return gather(arguments,_304,_302);
};
}
}
return gather([],_2fd,_2ff);
};
dojo.lang.curryArguments=function(_307,func,args,_30a){
var _30b=[];
var x=_30a||0;
for(x=_30a;x<args.length;x++){
_30b.push(args[x]);
}
return dojo.lang.curry.apply(dojo.lang,[_307,func].concat(_30b));
};
dojo.lang.tryThese=function(){
for(var x=0;x<arguments.length;x++){
try{
if(typeof arguments[x]=="function"){
var ret=(arguments[x]());
if(ret){
return ret;
}
}
}
catch(e){
dojo.debug(e);
}
}
};
dojo.lang.delayThese=function(farr,cb,_311,_312){
if(!farr.length){
if(typeof _312=="function"){
_312();
}
return;
}
if((typeof _311=="undefined")&&(typeof cb=="number")){
_311=cb;
cb=function(){
};
}else{
if(!cb){
cb=function(){
};
if(!_311){
_311=0;
}
}
}
setTimeout(function(){
(farr.shift())();
cb();
dojo.lang.delayThese(farr,cb,_311,_312);
},_311);
};
dojo.provide("dojo.string.extras");
dojo.string.substitute=function(_313,map,_315,_316){
return _313.replace(/\$\{([^\s\:\}]+)(?:\:(\S+))?\}/g,function(_317,key,_319){
var _31a=dojo.getObject(key,false,map).toString();
if(_319){
_31a=dojo.getObject(_319,false,_316)(_31a);
}
if(_315){
_31a=_315(_31a);
}
return _31a;
});
};
dojo.string.capitalize=function(str){
if(!dojo.lang.isString(str)){
return "";
}
return str.replace(/[^\s]+/g,function(word){
return word.substring(0,1).toUpperCase()+word.substring(1);
});
};
dojo.string.isBlank=function(str){
if(!dojo.lang.isString(str)){
return true;
}
return (dojo.string.trim(str).length==0);
};
dojo.string.encodeAscii=function(str){
if(!dojo.lang.isString(str)){
return str;
}
var ret="";
var _320=escape(str);
var _321,re=/%u([0-9A-F]{4})/i;
while((_321=_320.match(re))){
var num=Number("0x"+_321[1]);
var _324=escape("&#"+num+";");
ret+=_320.substring(0,_321.index)+_324;
_320=_320.substring(_321.index+_321[0].length);
}
ret+=_320.replace(/\+/g,"%2B");
return ret;
};
dojo.string.escape=function(type,str){
var args=dojo.lang.toArray(arguments,1);
switch(type.toLowerCase()){
case "xml":
case "html":
case "xhtml":
return dojo.string.escapeXml.apply(this,args);
case "sql":
return dojo.string.escapeSql.apply(this,args);
case "regexp":
case "regex":
return dojo.string.escapeRegExp.apply(this,args);
case "javascript":
case "jscript":
case "js":
return dojo.string.escapeJavaScript.apply(this,args);
case "ascii":
return dojo.string.encodeAscii.apply(this,args);
default:
return str;
}
};
dojo.string.escapeXml=function(str,_329){
str=str.replace(/&/gm,"&amp;").replace(/</gm,"&lt;").replace(/>/gm,"&gt;").replace(/"/gm,"&quot;");
if(!_329){
str=str.replace(/'/gm,"&#39;");
}
return str;
};
dojo.string.escapeSql=function(str){
return str.replace(/'/gm,"''");
};
dojo.string.escapeRegExp=function(str,_32c){
return str.replace(/([\.$?*!=:|{}\(\)\[\]\\\/^])/g,function(ch){
if(_32c&&_32c.indexOf(ch)!=-1){
return ch;
}
return "\\"+ch;
});
};
dojo.string.escapeJavaScript=function(str){
return str.replace(/(["'\f\b\n\t\r])/gm,"\\$1");
};
dojo.string.escapeString=function(str){
return ("\""+str.replace(/(["\\])/g,"\\$1")+"\"").replace(/[\f]/g,"\\f").replace(/[\b]/g,"\\b").replace(/[\n]/g,"\\n").replace(/[\t]/g,"\\t").replace(/[\r]/g,"\\r");
};
dojo.string.summary=function(str,len){
if(!len||str.length<=len){
return str;
}
return str.substring(0,len).replace(/\.+$/,"")+"...";
};
dojo.string.endsWith=function(str,end,_334){
if(_334){
str=str.toLowerCase();
end=end.toLowerCase();
}
if((str.length-end.length)<0){
return false;
}
return str.lastIndexOf(end)==str.length-end.length;
};
dojo.string.endsWithAny=function(str){
for(var i=1;i<arguments.length;i++){
if(dojo.string.endsWith(str,arguments[i])){
return true;
}
}
return false;
};
dojo.string.startsWith=function(str,_338,_339){
if(_339){
str=str.toLowerCase();
_338=_338.toLowerCase();
}
return str.indexOf(_338)==0;
};
dojo.string.startsWithAny=function(str){
for(var i=1;i<arguments.length;i++){
if(dojo.string.startsWith(str,arguments[i])){
return true;
}
}
return false;
};
dojo.string.has=function(str){
for(var i=1;i<arguments.length;i++){
if(str.indexOf(arguments[i])>-1){
return true;
}
}
return false;
};
dojo.string.normalizeNewlines=function(text,_33f){
if(_33f=="\n"){
text=text.replace(/\r\n/g,"\n");
text=text.replace(/\r/g,"\n");
}else{
if(_33f=="\r"){
text=text.replace(/\r\n/g,"\r");
text=text.replace(/\n/g,"\r");
}else{
text=text.replace(/([^\r])\n/g,"$1\r\n").replace(/\r([^\n])/g,"\r\n$1");
}
}
return text;
};
dojo.string.splitEscaped=function(str,_341){
var _342=[];
for(var i=0,_344=0;i<str.length;i++){
if(str.charAt(i)=="\\"){
i++;
continue;
}
if(str.charAt(i)==_341){
_342.push(str.substring(_344,i));
_344=i+1;
}
}
_342.push(str.substr(_344));
return _342;
};
dojo.provide("dojo.undo.browser");
try{
if((!djConfig["preventBackButtonFix"])&&(!dojo.hostenv.post_load_)){
document.write("<iframe style='border: 0px; width: 1px; height: 1px; position: absolute; bottom: 0px; right: 0px; visibility: visible;' name='djhistory' id='djhistory' src='"+(djConfig["dojoIframeHistoryUrl"]||dojo.hostenv.getBaseScriptUri()+"iframe_history.html")+"'></iframe>");
}
}
catch(e){
}
if(dojo.render.html.opera){
dojo.debug("Opera is not supported with dojo.undo.browser, so back/forward detection will not work.");
}
dojo.undo.browser={initialHref:(!dj_undef("window"))?window.location.href:"",initialHash:(!dj_undef("window"))?window.location.hash:"",moveForward:false,historyStack:[],forwardStack:[],historyIframe:null,bookmarkAnchor:null,locationTimer:null,setInitialState:function(args){
this.initialState=this._createState(this.initialHref,args,this.initialHash);
},addToHistory:function(args){
this.forwardStack=[];
var hash=null;
var url=null;
if(!this.historyIframe){
if(djConfig["useXDomain"]&&!djConfig["dojoIframeHistoryUrl"]){
dojo.debug("dojo.undo.browser: When using cross-domain Dojo builds,"+" please save iframe_history.html to your domain and set djConfig.dojoIframeHistoryUrl"+" to the path on your domain to iframe_history.html");
}
this.historyIframe=window.frames["djhistory"];
}
if(!this.bookmarkAnchor){
this.bookmarkAnchor=document.createElement("a");
dojo.body().appendChild(this.bookmarkAnchor);
this.bookmarkAnchor.style.display="none";
}
if(args["changeUrl"]){
hash="#"+((args["changeUrl"]!==true)?args["changeUrl"]:(new Date()).getTime());
if(this.historyStack.length==0&&this.initialState.urlHash==hash){
this.initialState=this._createState(url,args,hash);
return;
}else{
if(this.historyStack.length>0&&this.historyStack[this.historyStack.length-1].urlHash==hash){
this.historyStack[this.historyStack.length-1]=this._createState(url,args,hash);
return;
}
}
this.changingUrl=true;
setTimeout("window.location.href = '"+hash+"'; dojo.undo.browser.changingUrl = false;",1);
this.bookmarkAnchor.href=hash;
if(dojo.render.html.ie){
url=this._loadIframeHistory();
var _349=args["back"]||args["backButton"]||args["handle"];
var tcb=function(_34b){
if(window.location.hash!=""){
setTimeout("window.location.href = '"+hash+"';",1);
}
_349.apply(this,[_34b]);
};
if(args["back"]){
args.back=tcb;
}else{
if(args["backButton"]){
args.backButton=tcb;
}else{
if(args["handle"]){
args.handle=tcb;
}
}
}
var _34c=args["forward"]||args["forwardButton"]||args["handle"];
var tfw=function(_34e){
if(window.location.hash!=""){
window.location.href=hash;
}
if(_34c){
_34c.apply(this,[_34e]);
}
};
if(args["forward"]){
args.forward=tfw;
}else{
if(args["forwardButton"]){
args.forwardButton=tfw;
}else{
if(args["handle"]){
args.handle=tfw;
}
}
}
}else{
if(dojo.render.html.moz){
if(!this.locationTimer){
this.locationTimer=setInterval("dojo.undo.browser.checkLocation();",200);
}
}
}
}else{
url=this._loadIframeHistory();
}
this.historyStack.push(this._createState(url,args,hash));
},checkLocation:function(){
if(!this.changingUrl){
var hsl=this.historyStack.length;
if((window.location.hash==this.initialHash||window.location.href==this.initialHref)&&(hsl==1)){
this.handleBackButton();
return;
}
if(this.forwardStack.length>0){
if(this.forwardStack[this.forwardStack.length-1].urlHash==window.location.hash){
this.handleForwardButton();
return;
}
}
if((hsl>=2)&&(this.historyStack[hsl-2])){
if(this.historyStack[hsl-2].urlHash==window.location.hash){
this.handleBackButton();
return;
}
}
}
},iframeLoaded:function(evt,_351){
if(!dojo.render.html.opera){
var _352=this._getUrlQuery(_351.href);
if(_352==null){
if(this.historyStack.length==1){
this.handleBackButton();
}
return;
}
if(this.moveForward){
this.moveForward=false;
return;
}
if(this.historyStack.length>=2&&_352==this._getUrlQuery(this.historyStack[this.historyStack.length-2].url)){
this.handleBackButton();
}else{
if(this.forwardStack.length>0&&_352==this._getUrlQuery(this.forwardStack[this.forwardStack.length-1].url)){
this.handleForwardButton();
}
}
}
},handleBackButton:function(){
var _353=this.historyStack.pop();
if(!_353){
return;
}
var last=this.historyStack[this.historyStack.length-1];
if(!last&&this.historyStack.length==0){
last=this.initialState;
}
if(last){
if(last.kwArgs["back"]){
last.kwArgs["back"]();
}else{
if(last.kwArgs["backButton"]){
last.kwArgs["backButton"]();
}else{
if(last.kwArgs["handle"]){
last.kwArgs.handle("back");
}
}
}
}
this.forwardStack.push(_353);
},handleForwardButton:function(){
var last=this.forwardStack.pop();
if(!last){
return;
}
if(last.kwArgs["forward"]){
last.kwArgs.forward();
}else{
if(last.kwArgs["forwardButton"]){
last.kwArgs.forwardButton();
}else{
if(last.kwArgs["handle"]){
last.kwArgs.handle("forward");
}
}
}
this.historyStack.push(last);
},_createState:function(url,args,hash){
return {"url":url,"kwArgs":args,"urlHash":hash};
},_getUrlQuery:function(url){
var _35a=url.split("?");
if(_35a.length<2){
return null;
}else{
return _35a[1];
}
},_loadIframeHistory:function(){
var url=(djConfig["dojoIframeHistoryUrl"]||dojo.hostenv.getBaseScriptUri()+"iframe_history.html")+"?"+(new Date()).getTime();
this.moveForward=true;
dojo.io.setIFrameSrc(this.historyIframe,url,false);
return url;
}};
dojo.provide("dojo.io.BrowserIO");
if(!dj_undef("window")){
dojo.io.checkChildrenForFile=function(node){
var _35d=false;
var _35e=node.getElementsByTagName("input");
dojo.lang.forEach(_35e,function(_35f){
if(_35d){
return;
}
if(_35f.getAttribute("type")=="file"){
_35d=true;
}
});
return _35d;
};
dojo.io.formHasFile=function(_360){
return dojo.io.checkChildrenForFile(_360);
};
dojo.io.updateNode=function(node,_362){
node=dojo.byId(node);
var args=_362;
if(dojo.lang.isString(_362)){
args={url:_362};
}
args.mimetype="text/html";
args.load=function(t,d,e){
while(node.firstChild){
dojo.dom.destroyNode(node.firstChild);
}
node.innerHTML=d;
};
dojo.io.bind(args);
};
dojo.io.formFilter=function(node){
var type=(node.type||"").toLowerCase();
return !node.disabled&&node.name&&!dojo.lang.inArray(["file","submit","image","reset","button"],type);
};
dojo.io.encodeForm=function(_369,_36a,_36b){
if((!_369)||(!_369.tagName)||(!_369.tagName.toLowerCase()=="form")){
dojo.raise("Attempted to encode a non-form element.");
}
if(!_36b){
_36b=dojo.io.formFilter;
}
var enc=/utf/i.test(_36a||"")?encodeURIComponent:dojo.string.encodeAscii;
var _36d=[];
for(var i=0;i<_369.elements.length;i++){
var elm=_369.elements[i];
if(!elm||elm.tagName.toLowerCase()=="fieldset"||!_36b(elm)){
continue;
}
var name=enc(elm.name);
var type=elm.type.toLowerCase();
if(type=="select-multiple"){
for(var j=0;j<elm.options.length;j++){
if(elm.options[j].selected){
_36d.push(name+"="+enc(elm.options[j].value));
}
}
}else{
if(dojo.lang.inArray(["radio","checkbox"],type)){
if(elm.checked){
_36d.push(name+"="+enc(elm.value));
}
}else{
_36d.push(name+"="+enc(elm.value));
}
}
}
var _373=_369.getElementsByTagName("input");
for(var i=0;i<_373.length;i++){
var _374=_373[i];
if(_374.type.toLowerCase()=="image"&&_374.form==_369&&_36b(_374)){
var name=enc(_374.name);
_36d.push(name+"="+enc(_374.value));
_36d.push(name+".x=0");
_36d.push(name+".y=0");
}
}
return _36d.join("&")+"&";
};
dojo.io.FormBind=function(args){
this.bindArgs={};
if(args&&args.formNode){
this.init(args);
}else{
if(args){
this.init({formNode:args});
}
}
};
dojo.lang.extend(dojo.io.FormBind,{form:null,bindArgs:null,clickedButton:null,init:function(args){
var form=dojo.byId(args.formNode);
if(!form||!form.tagName||form.tagName.toLowerCase()!="form"){
throw new Error("FormBind: Couldn't apply, invalid form");
}else{
if(this.form==form){
return;
}else{
if(this.form){
throw new Error("FormBind: Already applied to a form");
}
}
}
dojo.lang.mixin(this.bindArgs,args);
this.form=form;
this.connect(form,"onsubmit","submit");
for(var i=0;i<form.elements.length;i++){
var node=form.elements[i];
if(node&&node.type&&dojo.lang.inArray(["submit","button"],node.type.toLowerCase())){
this.connect(node,"onclick","click");
}
}
var _37a=form.getElementsByTagName("input");
for(var i=0;i<_37a.length;i++){
var _37b=_37a[i];
if(_37b.type.toLowerCase()=="image"&&_37b.form==form){
this.connect(_37b,"onclick","click");
}
}
},onSubmit:function(form){
return true;
},submit:function(e){
e.preventDefault();
if(this.onSubmit(this.form)){
dojo.io.bind(dojo.lang.mixin(this.bindArgs,{formFilter:dojo.lang.hitch(this,"formFilter")}));
}
},click:function(e){
var node=e.currentTarget;
if(node.disabled){
return;
}
this.clickedButton=node;
},formFilter:function(node){
var type=(node.type||"").toLowerCase();
var _382=false;
if(node.disabled||!node.name){
_382=false;
}else{
if(dojo.lang.inArray(["submit","button","image"],type)){
if(!this.clickedButton){
this.clickedButton=node;
}
_382=node==this.clickedButton;
}else{
_382=!dojo.lang.inArray(["file","submit","reset","button"],type);
}
}
return _382;
},connect:function(_383,_384,_385){
if(dojo.getObject("dojo.event.connect")){
dojo.event.connect(_383,_384,this,_385);
}else{
var fcn=dojo.lang.hitch(this,_385);
_383[_384]=function(e){
if(!e){
e=window.event;
}
if(!e.currentTarget){
e.currentTarget=e.srcElement;
}
if(!e.preventDefault){
e.preventDefault=function(){
window.event.returnValue=false;
};
}
fcn(e);
};
}
}});
dojo.io.XMLHTTPTransport=new function(){
var _388=this;
var _389={};
this.useCache=false;
this.preventCache=false;
function getCacheKey(url,_38b,_38c){
return url+"|"+_38b+"|"+_38c.toLowerCase();
}
function addToCache(url,_38e,_38f,http){
_389[getCacheKey(url,_38e,_38f)]=http;
}
function getFromCache(url,_392,_393){
return _389[getCacheKey(url,_392,_393)];
}
this.clearCache=function(){
_389={};
};
function doLoad(_394,http,url,_397,_398){
if(((http.status>=200)&&(http.status<300))||(http.status==304)||(http.status==1223)||(location.protocol=="file:"&&(http.status==0||http.status==undefined))||(location.protocol=="chrome:"&&(http.status==0||http.status==undefined))){
var ret;
if(_394.method.toLowerCase()=="head"){
var _39a=http.getAllResponseHeaders();
ret={};
ret.toString=function(){
return _39a;
};
var _39b=_39a.split(/[\r\n]+/g);
for(var i=0;i<_39b.length;i++){
var pair=_39b[i].match(/^([^:]+)\s*:\s*(.+)$/i);
if(pair){
ret[pair[1]]=pair[2];
}
}
}else{
if(_394.mimetype=="text/javascript"){
try{
ret=dj_eval(http.responseText);
}
catch(e){
dojo.debug(e);
dojo.debug(http.responseText);
ret=null;
}
}else{
if(_394.mimetype.substr(0,9)=="text/json"||_394.mimetype.substr(0,16)=="application/json"){
try{
ret=dj_eval("("+_394.jsonFilter(http.responseText)+")");
}
catch(e){
dojo.debug(e);
dojo.debug(http.responseText);
ret=false;
}
}else{
if((_394.mimetype=="application/xml")||(_394.mimetype=="text/xml")){
ret=http.responseXML;
if(!ret||typeof ret=="string"||!http.getResponseHeader("Content-Type")){
ret=dojo.dom.createDocumentFromText(http.responseText);
}
}else{
ret=http.responseText;
}
}
}
}
if(_398){
addToCache(url,_397,_394.method,http);
}
try{
dojo.hostenv.insideBindHandler=true;
_394[(typeof _394.load=="function")?"load":"handle"]("load",ret,http,_394);
dojo.hostenv.insideBindHandler=false;
if(dojo.hostenv.post_load_){
dojo.hostenv.callLoaded();
}
}
finally{
dojo.hostenv.insideBindHandler=false;
}
}else{
var _39e=new dojo.io.Error("XMLHttpTransport Error: "+http.status+" "+http.statusText);
_394[(typeof _394.error=="function")?"error":"handle"]("error",_39e,http,_394);
}
}
function setHeaders(http,_3a0){
if(_3a0["headers"]){
for(var _3a1 in _3a0["headers"]){
if(_3a1.toLowerCase()=="content-type"&&!_3a0["contentType"]){
_3a0["contentType"]=_3a0["headers"][_3a1];
}else{
http.setRequestHeader(_3a1,_3a0["headers"][_3a1]);
}
}
}
}
this.inFlight=[];
this.inFlightTimer=null;
this.startWatchingInFlight=function(){
if(!this.inFlightTimer){
this.inFlightTimer=setTimeout("dojo.io.XMLHTTPTransport.watchInFlight();",10);
}
};
this.watchInFlight=function(){
var now=null;
if(!dojo.hostenv._blockAsync&&!_388._blockAsync){
for(var x=this.inFlight.length-1;x>=0;x--){
try{
var tif=this.inFlight[x];
if(!tif||tif.http._aborted||!tif.http.readyState){
this.inFlight.splice(x,1);
continue;
}
if(4==tif.http.readyState){
this.inFlight.splice(x,1);
doLoad(tif.req,tif.http,tif.url,tif.query,tif.useCache);
}else{
if(tif.startTime){
if(!now){
now=(new Date()).getTime();
}
if(tif.startTime+(tif.req.timeoutSeconds*1000)<now){
if(typeof tif.http.abort=="function"){
tif.http.abort();
}
this.inFlight.splice(x,1);
tif.req[(typeof tif.req.timeout=="function")?"timeout":"handle"]("timeout",null,tif.http,tif.req);
}
}
}
}
catch(e){
try{
var _3a5=new dojo.io.Error("XMLHttpTransport.watchInFlight Error: "+e);
tif.req[(typeof tif.req.error=="function")?"error":"handle"]("error",_3a5,tif.http,tif.req);
}
catch(e2){
dojo.debug("XMLHttpTransport error callback failed: "+e2);
}
}
}
}
clearTimeout(this.inFlightTimer);
if(this.inFlight.length==0){
this.inFlightTimer=null;
return;
}
this.inFlightTimer=setTimeout("dojo.io.XMLHTTPTransport.watchInFlight();",10);
};
var _3a6=dojo.hostenv.getXmlhttpObject()?true:false;
this.canHandle=function(_3a7){
var mlc=_3a7["mimetype"]||"";
mlc=mlc.toLowerCase();
return _3a6&&((dojo.lang.inArray(["text/plain","text/html","application/xml","text/xml","text/javascript"],mlc))||(mlc.substr(0,9)=="text/json"||mlc.substr(0,16)=="application/json"))&&!(_3a7["formNode"]&&dojo.io.formHasFile(_3a7["formNode"]));
};
this.multipartBoundary="45309FFF-BD65-4d50-99C9-36986896A96F";
this.bind=function(_3a9){
var url=_3a9.url;
var _3ab="";
if(_3a9["formNode"]){
var ta=_3a9.formNode.getAttribute("action");
if(typeof (ta)!="string"){
ta=_3a9.formNode.attributes.action.value;
}
if((ta)&&(!_3a9["url"])){
url=ta;
}
var tp=_3a9.formNode.getAttribute("method");
if((tp)&&(!_3a9["method"])){
_3a9.method=tp;
}
_3ab+=dojo.io.encodeForm(_3a9.formNode,_3a9.encoding,_3a9["formFilter"]);
}
if(url.indexOf("#")>-1){
dojo.debug("Warning: dojo.io.bind: stripping hash values from url:",url);
url=url.split("#")[0];
}
if(_3a9["file"]){
_3a9.method="post";
}
if(!_3a9["method"]){
_3a9.method="get";
}
if(_3a9.method.toLowerCase()=="get"){
_3a9.multipart=false;
}else{
if(_3a9["file"]){
_3a9.multipart=true;
}else{
if(!_3a9["multipart"]){
_3a9.multipart=false;
}
}
}
if(_3a9["backButton"]||_3a9["back"]||_3a9["changeUrl"]){
dojo.undo.browser.addToHistory(_3a9);
}
var _3ae=_3a9["content"]||{};
if(_3a9.sendTransport){
_3ae["dojo.transport"]="xmlhttp";
}
do{
if(_3a9.postContent){
_3ab=_3a9.postContent;
break;
}
if(_3ae){
_3ab+=dojo.io.argsFromMap(_3ae,_3a9.encoding);
}
if(_3a9.method.toLowerCase()=="get"||!_3a9.multipart){
break;
}
var t=[];
if(_3ab.length){
var q=_3ab.split("&");
for(var i=0;i<q.length;++i){
if(q[i].length){
var p=q[i].split("=");
t.push("--"+this.multipartBoundary,"Content-Disposition: form-data; name=\""+p[0]+"\"","",p[1]);
}
}
}
if(_3a9.file){
if(dojo.lang.isArray(_3a9.file)){
for(var i=0;i<_3a9.file.length;++i){
var o=_3a9.file[i];
t.push("--"+this.multipartBoundary,"Content-Disposition: form-data; name=\""+o.name+"\"; filename=\""+("fileName" in o?o.fileName:o.name)+"\"","Content-Type: "+("contentType" in o?o.contentType:"application/octet-stream"),"",o.content);
}
}else{
var o=_3a9.file;
t.push("--"+this.multipartBoundary,"Content-Disposition: form-data; name=\""+o.name+"\"; filename=\""+("fileName" in o?o.fileName:o.name)+"\"","Content-Type: "+("contentType" in o?o.contentType:"application/octet-stream"),"",o.content);
}
}
if(t.length){
t.push("--"+this.multipartBoundary+"--","");
_3ab=t.join("\r\n");
}
}while(false);
var _3b4=_3a9["sync"]?false:true;
var _3b5=_3a9["preventCache"]||(this.preventCache==true&&_3a9["preventCache"]!=false);
var _3b6=_3a9["useCache"]==true||(this.useCache==true&&_3a9["useCache"]!=false);
if(!_3b5&&_3b6){
var _3b7=getFromCache(url,_3ab,_3a9.method);
if(_3b7){
doLoad(_3a9,_3b7,url,_3ab,false);
return;
}
}
var http=dojo.hostenv.getXmlhttpObject(_3a9);
var _3b9=false;
if(_3b4){
var _3ba=this.inFlight.push({"req":_3a9,"http":http,"url":url,"query":_3ab,"useCache":_3b6,"startTime":_3a9.timeoutSeconds?(new Date()).getTime():0});
this.startWatchingInFlight();
}else{
_388._blockAsync=true;
}
if(_3a9.method.toLowerCase()=="post"){
if(!_3a9.user){
http.open("POST",url,_3b4);
}else{
http.open("POST",url,_3b4,_3a9.user,_3a9.password);
}
setHeaders(http,_3a9);
http.setRequestHeader("Content-Type",_3a9.multipart?("multipart/form-data; boundary="+this.multipartBoundary):(_3a9.contentType||"application/x-www-form-urlencoded"));
try{
http.send(_3ab);
}
catch(e){
if(typeof http.abort=="function"){
http.abort();
}
doLoad(_3a9,{status:404},url,_3ab,_3b6);
}
}else{
var _3bb=url;
if(_3ab!=""){
_3bb+=(_3bb.indexOf("?")>-1?"&":"?")+_3ab;
}
if(_3b5){
_3bb+=(dojo.string.endsWithAny(_3bb,"?","&")?"":(_3bb.indexOf("?")>-1?"&":"?"))+"dojo.preventCache="+new Date().valueOf();
}
if(!_3a9.user){
http.open(_3a9.method.toUpperCase(),_3bb,_3b4);
}else{
http.open(_3a9.method.toUpperCase(),_3bb,_3b4,_3a9.user,_3a9.password);
}
setHeaders(http,_3a9);
try{
http.send(null);
}
catch(e){
if(typeof http.abort=="function"){
http.abort();
}
doLoad(_3a9,{status:404},url,_3ab,_3b6);
}
}
if(!_3b4){
doLoad(_3a9,http,url,_3ab,_3b6);
_388._blockAsync=false;
}
_3a9.abort=function(){
try{
http._aborted=true;
}
catch(e){
}
return http.abort();
};
return;
};
dojo.io.transports.addTransport("XMLHTTPTransport");
};
}
dojo.provide("dojo.io.cookie");
dojo.io.cookie.setCookie=function(name,_3bd,days,path,_3c0,_3c1){
var _3c2=-1;
if((typeof days=="number")&&(days>=0)){
var d=new Date();
d.setTime(d.getTime()+(days*24*60*60*1000));
_3c2=d.toGMTString();
}
_3bd=escape(_3bd);
document.cookie=name+"="+_3bd+";"+(_3c2!=-1?" expires="+_3c2+";":"")+(path?"path="+path:"")+(_3c0?"; domain="+_3c0:"")+(_3c1?"; secure":"");
};
dojo.io.cookie.set=dojo.io.cookie.setCookie;
dojo.io.cookie.getCookie=function(name){
var idx=document.cookie.lastIndexOf(name+"=");
if(idx==-1){
return null;
}
var _3c6=document.cookie.substring(idx+name.length+1);
var end=_3c6.indexOf(";");
if(end==-1){
end=_3c6.length;
}
_3c6=_3c6.substring(0,end);
_3c6=unescape(_3c6);
return _3c6;
};
dojo.io.cookie.get=dojo.io.cookie.getCookie;
dojo.io.cookie.deleteCookie=function(name){
dojo.io.cookie.setCookie(name,"-",0);
};
dojo.io.cookie.setObjectCookie=function(name,obj,days,path,_3cd,_3ce,_3cf){
if(arguments.length==5){
_3cf=_3cd;
_3cd=null;
_3ce=null;
}
var _3d0=[],_3d1,_3d2="";
if(!_3cf){
_3d1=dojo.io.cookie.getObjectCookie(name);
}
if(days>=0){
if(!_3d1){
_3d1={};
}
for(var prop in obj){
if(obj[prop]==null){
delete _3d1[prop];
}else{
if((typeof obj[prop]=="string")||(typeof obj[prop]=="number")){
_3d1[prop]=obj[prop];
}
}
}
prop=null;
for(var prop in _3d1){
_3d0.push(escape(prop)+"="+escape(_3d1[prop]));
}
_3d2=_3d0.join("&");
}
dojo.io.cookie.setCookie(name,_3d2,days,path,_3cd,_3ce);
};
dojo.io.cookie.getObjectCookie=function(name){
var _3d5=null,_3d6=dojo.io.cookie.getCookie(name);
if(_3d6){
_3d5={};
var _3d7=_3d6.split("&");
for(var i=0;i<_3d7.length;i++){
var pair=_3d7[i].split("=");
var _3da=pair[1];
if(isNaN(_3da)){
_3da=unescape(pair[1]);
}
_3d5[unescape(pair[0])]=_3da;
}
}
return _3d5;
};
dojo.io.cookie.isSupported=function(){
if(typeof navigator.cookieEnabled!="boolean"){
dojo.io.cookie.setCookie("__TestingYourBrowserForCookieSupport__","CookiesAllowed",90,null);
var _3db=dojo.io.cookie.getCookie("__TestingYourBrowserForCookieSupport__");
navigator.cookieEnabled=(_3db=="CookiesAllowed");
if(navigator.cookieEnabled){
this.deleteCookie("__TestingYourBrowserForCookieSupport__");
}
}
return navigator.cookieEnabled;
};
if(!dojo.io.cookies){
dojo.io.cookies=dojo.io.cookie;
}
dojo.kwCompoundRequire({common:["dojo.io.common"],rhino:["dojo.io.RhinoIO"],browser:["dojo.io.BrowserIO","dojo.io.cookie"],dashboard:["dojo.io.BrowserIO","dojo.io.cookie"]});
dojo.provide("dojo.io.*");
dojo.kwCompoundRequire({common:[["dojo.uri.Uri",false,false]]});
dojo.provide("dojo.uri.*");
dojo.provide("dojo.io.IframeIO");
dojo.io.createIFrame=function(_3dc,_3dd,uri){
if(window[_3dc]){
return window[_3dc];
}
if(window.frames[_3dc]){
return window.frames[_3dc];
}
var r=dojo.render.html;
var _3e0=null;
var turi=uri;
if(!turi){
if(djConfig["useXDomain"]&&!djConfig["dojoIframeHistoryUrl"]){
dojo.debug("dojo.io.createIFrame: When using cross-domain Dojo builds,"+" please save iframe_history.html to your domain and set djConfig.dojoIframeHistoryUrl"+" to the path on your domain to iframe_history.html");
}
turi=(djConfig["dojoIframeHistoryUrl"]||dojo.uri.moduleUri("dojo","../iframe_history.html"))+"#noInit=true";
}
var _3e2=((r.ie)&&(dojo.render.os.win))?"<iframe name=\""+_3dc+"\" src=\""+turi+"\" onload=\""+_3dd+"\">":"iframe";
_3e0=document.createElement(_3e2);
with(_3e0){
name=_3dc;
setAttribute("name",_3dc);
id=_3dc;
}
dojo.body().appendChild(_3e0);
window[_3dc]=_3e0;
with(_3e0.style){
if(!r.safari){
position="absolute";
}
left=top="0px";
height=width="1px";
visibility="hidden";
}
if(!r.ie){
dojo.io.setIFrameSrc(_3e0,turi,true);
_3e0.onload=new Function(_3dd);
}
return _3e0;
};
dojo.io.IframeTransport=new function(){
var _3e3=this;
this.currentRequest=null;
this.requestQueue=[];
this.iframeName="dojoIoIframe";
this.fireNextRequest=function(){
try{
if((this.currentRequest)||(this.requestQueue.length==0)){
return;
}
var cr=this.currentRequest=this.requestQueue.shift();
cr._contentToClean=[];
var fn=cr["formNode"];
var _3e6=cr["content"]||{};
if(cr.sendTransport){
_3e6["dojo.transport"]="iframe";
}
if(fn){
if(_3e6){
for(var x in _3e6){
if(!fn[x]){
var tn;
if(dojo.render.html.ie){
tn=document.createElement("<input type='hidden' name='"+x+"' value='"+_3e6[x]+"'>");
fn.appendChild(tn);
}else{
tn=document.createElement("input");
fn.appendChild(tn);
tn.type="hidden";
tn.name=x;
tn.value=_3e6[x];
}
cr._contentToClean.push(x);
}else{
fn[x].value=_3e6[x];
}
}
}
if(cr["url"]){
cr._originalAction=fn.getAttribute("action");
fn.setAttribute("action",cr.url);
}
if(!fn.getAttribute("method")){
fn.setAttribute("method",(cr["method"])?cr["method"]:"post");
}
cr._originalTarget=fn.getAttribute("target");
fn.setAttribute("target",this.iframeName);
fn.target=this.iframeName;
fn.submit();
}else{
var _3e9=dojo.io.argsFromMap(this.currentRequest.content);
var _3ea=cr.url+(cr.url.indexOf("?")>-1?"&":"?")+_3e9;
dojo.io.setIFrameSrc(this.iframe,_3ea,true);
}
}
catch(e){
this.iframeOnload(e);
}
};
this.canHandle=function(_3eb){
return ((dojo.lang.inArray(["text/xml","text/plain","text/html","text/javascript","text/json","application/json"],_3eb["mimetype"]))&&(dojo.lang.inArray(["post","get"],_3eb["method"].toLowerCase()))&&(!((_3eb["sync"])&&(_3eb["sync"]==true))));
};
this.bind=function(_3ec){
if(!this["iframe"]){
this.setUpIframe();
}
this.requestQueue.push(_3ec);
this.fireNextRequest();
return;
};
this.setUpIframe=function(){
this.iframe=dojo.io.createIFrame(this.iframeName,"dojo.io.IframeTransport.iframeOnload();");
};
this.iframeOnload=function(_3ed){
if(!_3e3.currentRequest){
_3e3.fireNextRequest();
return;
}
var req=_3e3.currentRequest;
if(req.formNode){
var _3ef=req._contentToClean;
for(var i=0;i<_3ef.length;i++){
var key=_3ef[i];
if(dojo.render.html.safari){
var _3f2=req.formNode;
for(var j=0;j<_3f2.childNodes.length;j++){
var _3f4=_3f2.childNodes[j];
if(_3f4.name==key){
var _3f5=_3f4.parentNode;
_3f5.removeChild(_3f4);
break;
}
}
}else{
var _3f6=req.formNode[key];
req.formNode.removeChild(_3f6);
req.formNode[key]=null;
}
}
if(req["_originalAction"]){
req.formNode.setAttribute("action",req._originalAction);
}
if(req["_originalTarget"]){
req.formNode.setAttribute("target",req._originalTarget);
req.formNode.target=req._originalTarget;
}
}
var _3f7=function(_3f8){
var doc=_3f8.contentDocument||((_3f8.contentWindow)&&(_3f8.contentWindow.document))||((_3f8.name)&&(document.frames[_3f8.name])&&(document.frames[_3f8.name].document))||null;
return doc;
};
var _3fa;
var _3fb=false;
if(_3ed){
this._callError(req,"IframeTransport Request Error: "+_3ed);
}else{
var ifd=_3f7(_3e3.iframe);
try{
var cmt=req.mimetype;
if((cmt=="text/javascript")||(cmt=="text/json")||(cmt=="application/json")){
var js=ifd.getElementsByTagName("textarea")[0].value;
if(cmt=="text/json"||cmt=="application/json"){
js="("+js+")";
}
_3fa=dj_eval(js);
}else{
if(cmt=="text/html"){
_3fa=ifd;
}else{
_3fa=ifd.getElementsByTagName("textarea")[0].value;
}
}
_3fb=true;
}
catch(e){
this._callError(req,"IframeTransport Error: "+e);
}
}
try{
if(_3fb&&dojo.lang.isFunction(req["load"])){
req.load("load",_3fa,req);
}
}
catch(e){
throw e;
}
finally{
_3e3.currentRequest=null;
_3e3.fireNextRequest();
}
};
this._callError=function(req,_400){
var _401=new dojo.io.Error(_400);
if(dojo.lang.isFunction(req["error"])){
req.error("error",_401,req);
}
};
dojo.io.transports.addTransport("IframeTransport");
};
dojo.provide("dojo.collections.Collections");
dojo.collections.DictionaryEntry=function(k,v){
this.key=k;
this.value=v;
this.valueOf=function(){
return this.value;
};
this.toString=function(){
return String(this.value);
};
};
dojo.collections.Iterator=function(arr){
var a=arr;
var _406=0;
this.element=a[_406]||null;
this.atEnd=function(){
return (_406>=a.length);
};
this.get=function(){
if(this.atEnd()){
return null;
}
this.element=a[_406++];
return this.element;
};
this.map=function(fn,_408){
var s=_408||dj_global;
if(Array.map){
return Array.map(a,fn,s);
}else{
var arr=[];
for(var i=0;i<a.length;i++){
arr.push(fn.call(s,a[i]));
}
return arr;
}
};
this.reset=function(){
_406=0;
this.element=a[_406];
};
};
dojo.collections.DictionaryIterator=function(obj){
var a=[];
var _40e={};
for(var p in obj){
if(!_40e[p]){
a.push(obj[p]);
}
}
var _410=0;
this.element=a[_410]||null;
this.atEnd=function(){
return (_410>=a.length);
};
this.get=function(){
if(this.atEnd()){
return null;
}
this.element=a[_410++];
return this.element;
};
this.map=function(fn,_412){
var s=_412||dj_global;
if(Array.map){
return Array.map(a,fn,s);
}else{
var arr=[];
for(var i=0;i<a.length;i++){
arr.push(fn.call(s,a[i]));
}
return arr;
}
};
this.reset=function(){
_410=0;
this.element=a[_410];
};
};
dojo.provide("dojo.collections.Dictionary");
dojo.collections.Dictionary=function(_416){
var _417={};
this.count=0;
var _418={};
this.add=function(k,v){
var b=(k in _417);
_417[k]=new dojo.collections.DictionaryEntry(k,v);
if(!b){
this.count++;
}
};
this.clear=function(){
_417={};
this.count=0;
};
this.clone=function(){
return new dojo.collections.Dictionary(this);
};
this.contains=this.containsKey=function(k){
if(_418[k]){
return false;
}
return (_417[k]!=null);
};
this.containsValue=function(v){
var e=this.getIterator();
while(e.get()){
if(e.element.value==v){
return true;
}
}
return false;
};
this.entry=function(k){
return _417[k];
};
this.forEach=function(fn,_421){
var a=[];
for(var p in _417){
if(!_418[p]){
a.push(_417[p]);
}
}
var s=_421||dj_global;
if(Array.forEach){
Array.forEach(a,fn,s);
}else{
for(var i=0;i<a.length;i++){
fn.call(s,a[i],i,a);
}
}
};
this.getKeyList=function(){
return (this.getIterator()).map(function(_426){
return _426.key;
});
};
this.getValueList=function(){
return (this.getIterator()).map(function(_427){
return _427.value;
});
};
this.item=function(k){
if(k in _417){
return _417[k].valueOf();
}
return undefined;
};
this.getIterator=function(){
return new dojo.collections.DictionaryIterator(_417);
};
this.remove=function(k){
if(k in _417&&!_418[k]){
delete _417[k];
this.count--;
return true;
}
return false;
};
if(_416){
var e=_416.getIterator();
while(e.get()){
this.add(e.element.key,e.element.value);
}
}
};
dojo.provide("dojo.xml.Parse");
dojo.xml.Parse=function(){
var isIE=((dojo.render.html.capable)&&(dojo.render.html.ie));
function getTagName(node){
try{
return node.tagName.toLowerCase();
}
catch(e){
return "";
}
}
function getDojoTagName(node){
var _42e=getTagName(node);
if(!_42e){
return "";
}
if((dojo.widget)&&(dojo.widget.tags[_42e])){
return _42e;
}
var p=_42e.indexOf(":");
if(p>=0){
return _42e;
}
if(_42e.substr(0,5)=="dojo:"){
return _42e;
}
if(dojo.render.html.capable&&dojo.render.html.ie&&node.scopeName&&node.scopeName!="HTML"){
return node.scopeName.toLowerCase()+":"+_42e;
}
if(_42e.substr(0,4)=="dojo"){
return "dojo:"+_42e.substring(4);
}
var djt=node.getAttribute("dojoType")||node.getAttribute("dojotype");
if(djt){
if(djt.indexOf(":")<0){
djt="dojo:"+djt;
}
return djt.toLowerCase();
}
djt=node.getAttributeNS&&node.getAttributeNS(dojo.dom.dojoml,"type");
if(djt){
return "dojo:"+djt.toLowerCase();
}
try{
djt=node.getAttribute("dojo:type");
}
catch(e){
}
if(djt){
return "dojo:"+djt.toLowerCase();
}
if((dj_global["djConfig"])&&(!djConfig["ignoreClassNames"])){
var _431=node.className||node.getAttribute("class");
if((_431)&&(_431.indexOf)&&(_431.indexOf("dojo-")!=-1)){
var _432=_431.split(" ");
for(var x=0,c=_432.length;x<c;x++){
if(_432[x].slice(0,5)=="dojo-"){
return "dojo:"+_432[x].substr(5).toLowerCase();
}
}
}
}
return "";
}
this.parseElement=function(node,_436,_437,_438){
var _439=getTagName(node);
if(isIE&&_439.indexOf("/")==0){
return null;
}
try{
var attr=node.getAttribute("parseWidgets");
if(attr&&attr.toLowerCase()=="false"){
return {};
}
}
catch(e){
}
var _43b=true;
if(_437){
var _43c=getDojoTagName(node);
_439=_43c||_439;
_43b=Boolean(_43c);
}
var _43d={};
_43d[_439]=[];
var pos=_439.indexOf(":");
if(pos>0){
var ns=_439.substring(0,pos);
_43d["ns"]=ns;
if((dojo.ns)&&(!dojo.ns.allow(ns))){
_43b=false;
}
}
if(_43b){
var _440=this.parseAttributes(node);
for(var attr in _440){
if((!_43d[_439][attr])||(typeof _43d[_439][attr]!="array")){
_43d[_439][attr]=[];
}
_43d[_439][attr].push(_440[attr]);
}
_43d[_439].nodeRef=node;
_43d.tagName=_439;
_43d.index=_438||0;
}
var _441=0;
for(var i=0;i<node.childNodes.length;i++){
var tcn=node.childNodes.item(i);
switch(tcn.nodeType){
case dojo.dom.ELEMENT_NODE:
var ctn=getDojoTagName(tcn)||getTagName(tcn);
if(!_43d[ctn]){
_43d[ctn]=[];
}
_43d[ctn].push(this.parseElement(tcn,true,_437,_441));
if((tcn.childNodes.length==1)&&(tcn.childNodes.item(0).nodeType==dojo.dom.TEXT_NODE)){
_43d[ctn][_43d[ctn].length-1].value=tcn.childNodes.item(0).nodeValue;
}
_441++;
break;
case dojo.dom.TEXT_NODE:
if(node.childNodes.length==1){
_43d[_439].push({value:node.childNodes.item(0).nodeValue});
}
break;
default:
break;
}
}
return _43d;
};
this.parseAttributes=function(node){
var _446={};
var atts=node.attributes;
var _448,i=0;
while((_448=atts[i++])){
if(isIE){
if(!_448){
continue;
}
if((typeof _448=="object")&&(typeof _448.nodeValue=="undefined")||(_448.nodeValue==null)||(_448.nodeValue=="")){
continue;
}
}
var nn=_448.nodeName.split(":");
nn=(nn.length==2)?nn[1]:_448.nodeName;
_446[nn]={value:_448.nodeValue};
}
return _446;
};
};
dojo.provide("dojo.lang.declare");
dojo.lang.declare=function(_44b,_44c,init,_44e){
if((dojo.lang.isFunction(_44e))||((!_44e)&&(!dojo.lang.isFunction(init)))){
if(dojo.lang.isFunction(_44e)){
dojo.deprecated("dojo.lang.declare("+_44b+"...):","use class, superclass, initializer, properties argument order","0.6");
}
var temp=_44e;
_44e=init;
init=temp;
}
if(_44e&&_44e.initializer){
dojo.deprecated("dojo.lang.declare("+_44b+"...):","specify initializer as third argument, not as an element in properties","0.6");
}
var _450=[];
if(dojo.lang.isArray(_44c)){
_450=_44c;
_44c=_450.shift();
}
if(!init){
init=dojo.getObject(_44b,false);
if((init)&&(!dojo.lang.isFunction(init))){
init=null;
}
}
var ctor=dojo.lang.declare._makeConstructor();
var scp=(_44c?_44c.prototype:null);
if(scp){
scp.prototyping=true;
ctor.prototype=new _44c();
scp.prototyping=false;
}
ctor.superclass=scp;
ctor.mixins=_450;
for(var i=0,l=_450.length;i<l;i++){
dojo.lang.extend(ctor,_450[i].prototype);
}
ctor.prototype.initializer=null;
ctor.prototype.declaredClass=_44b;
if(dojo.lang.isArray(_44e)){
dojo.lang.extend.apply(dojo.lang,[ctor].concat(_44e));
}else{
dojo.lang.extend(ctor,(_44e)||{});
}
dojo.lang.extend(ctor,dojo.lang.declare._common);
ctor.prototype.constructor=ctor;
ctor.prototype.initializer=(ctor.prototype.initializer)||(init)||(function(){
});
var _455=dojo.getObject(_44b,true,null,true);
_455.obj[_455.prop]=ctor;
return ctor;
};
dojo.lang.declare._makeConstructor=function(){
return function(){
var self=this._getPropContext();
var s=self.constructor.superclass;
if((s)&&(s.constructor)){
if(s.constructor==arguments.callee){
this._inherited("constructor",arguments);
}else{
this._contextMethod(s,"constructor",arguments);
}
}
var ms=(self.constructor.mixins)||([]);
for(var i=0,m;(m=ms[i]);i++){
(((m.prototype)&&(m.prototype.initializer))||(m)).apply(this,arguments);
}
if((!this.prototyping)&&(self.initializer)){
self.initializer.apply(this,arguments);
}
};
};
dojo.lang.declare._common={_getPropContext:function(){
return (this.___proto||this);
},_contextMethod:function(_45b,_45c,args){
var _45e,_45f=this.___proto;
this.___proto=_45b;
try{
_45e=_45b[_45c].apply(this,(args||[]));
}
catch(e){
throw e;
}
finally{
this.___proto=_45f;
}
return _45e;
},_inherited:function(prop,args){
var p=this._getPropContext();
do{
if((!p.constructor)||(!p.constructor.superclass)){
return;
}
p=p.constructor.superclass;
}while(!(prop in p));
return (dojo.lang.isFunction(p[prop])?this._contextMethod(p,prop,args):p[prop]);
}};
dojo.declare=dojo.lang.declare;
dojo.provide("dojo.ns");
dojo.ns={namespaces:{},failed:{},loading:{},loaded:{},register:function(name,_464,_465,_466){
if(!_466||!this.namespaces[name]){
this.namespaces[name]=new dojo.ns.Ns(name,_464,_465);
}
},allow:function(name){
if(this.failed[name]){
return false;
}
if((djConfig.excludeNamespace)&&(dojo.lang.inArray(djConfig.excludeNamespace,name))){
return false;
}
return ((name==this.dojo)||(!djConfig.includeNamespace)||(dojo.lang.inArray(djConfig.includeNamespace,name)));
},get:function(name){
return this.namespaces[name];
},require:function(name){
var ns=this.namespaces[name];
if((ns)&&(this.loaded[name])){
return ns;
}
if(!this.allow(name)){
return false;
}
if(this.loading[name]){
dojo.debug("dojo.namespace.require: re-entrant request to load namespace \""+name+"\" must fail.");
return false;
}
var req=dojo.require;
this.loading[name]=true;
try{
if(name=="dojo"){
req("dojo.namespaces.dojo");
}else{
if(!dojo.hostenv.moduleHasPrefix(name)){
dojo.registerModulePath(name,"../"+name);
}
req([name,"manifest"].join("."),false,true);
}
if(!this.namespaces[name]){
this.failed[name]=true;
}
}
finally{
this.loading[name]=false;
}
return this.namespaces[name];
}};
dojo.ns.Ns=function(name,_46d,_46e){
this.name=name;
this.module=_46d;
this.resolver=_46e;
this._loaded=[];
this._failed=[];
};
dojo.ns.Ns.prototype.resolve=function(name,_470,_471){
if(!this.resolver||djConfig["skipAutoRequire"]){
return false;
}
var _472=this.resolver(name,_470);
if((_472)&&(!this._loaded[_472])&&(!this._failed[_472])){
var req=dojo.require;
req(_472,false,true);
if(dojo.hostenv.findModule(_472,false)){
this._loaded[_472]=true;
}else{
if(!_471){
dojo.raise("dojo.ns.Ns.resolve: module '"+_472+"' not found after loading via namespace '"+this.name+"'");
}
this._failed[_472]=true;
}
}
return Boolean(this._loaded[_472]);
};
dojo.registerNamespace=function(name,_475,_476){
dojo.ns.register.apply(dojo.ns,arguments);
};
dojo.registerNamespaceResolver=function(name,_478){
var n=dojo.ns.namespaces[name];
if(n){
n.resolver=_478;
}
};
dojo.registerNamespaceManifest=function(_47a,path,name,_47d,_47e){
dojo.registerModulePath(_47a,path);
dojo.registerNamespace(name,_47d,_47e);
};
dojo.registerNamespace("dojo","dojo.widget");
dojo.provide("dojo.event.common");
dojo.event=new function(){
this._canTimeout=dojo.lang.isFunction(dj_global["setTimeout"])||dojo.lang.isAlien(dj_global["setTimeout"]);
function interpolateArgs(args,_480){
var dl=dojo.lang;
var ao={srcObj:dj_global,srcFunc:null,adviceObj:dj_global,adviceFunc:null,aroundObj:null,aroundFunc:null,adviceType:(args.length>2)?args[0]:"after",precedence:"last",once:false,delay:null,rate:0,adviceMsg:false,maxCalls:-1};
switch(args.length){
case 0:
return;
case 1:
return;
case 2:
ao.srcFunc=args[0];
ao.adviceFunc=args[1];
break;
case 3:
if((dl.isObject(args[0]))&&(dl.isString(args[1]))&&(dl.isString(args[2]))){
ao.adviceType="after";
ao.srcObj=args[0];
ao.srcFunc=args[1];
ao.adviceFunc=args[2];
}else{
if((dl.isString(args[1]))&&(dl.isString(args[2]))){
ao.srcFunc=args[1];
ao.adviceFunc=args[2];
}else{
if((dl.isObject(args[0]))&&(dl.isString(args[1]))&&(dl.isFunction(args[2]))){
ao.adviceType="after";
ao.srcObj=args[0];
ao.srcFunc=args[1];
var _483=dl.nameAnonFunc(args[2],ao.adviceObj,_480);
ao.adviceFunc=_483;
}else{
if((dl.isFunction(args[0]))&&(dl.isObject(args[1]))&&(dl.isString(args[2]))){
ao.adviceType="after";
ao.srcObj=dj_global;
var _483=dl.nameAnonFunc(args[0],ao.srcObj,_480);
ao.srcFunc=_483;
ao.adviceObj=args[1];
ao.adviceFunc=args[2];
}
}
}
}
break;
case 4:
if((dl.isObject(args[0]))&&(dl.isObject(args[2]))){
ao.adviceType="after";
ao.srcObj=args[0];
ao.srcFunc=args[1];
ao.adviceObj=args[2];
ao.adviceFunc=args[3];
}else{
if((dl.isString(args[0]))&&(dl.isString(args[1]))&&(dl.isObject(args[2]))){
ao.adviceType=args[0];
ao.srcObj=dj_global;
ao.srcFunc=args[1];
ao.adviceObj=args[2];
ao.adviceFunc=args[3];
}else{
if((dl.isString(args[0]))&&(dl.isFunction(args[1]))&&(dl.isObject(args[2]))){
ao.adviceType=args[0];
ao.srcObj=dj_global;
var _483=dl.nameAnonFunc(args[1],dj_global,_480);
ao.srcFunc=_483;
ao.adviceObj=args[2];
ao.adviceFunc=args[3];
}else{
if((dl.isString(args[0]))&&(dl.isObject(args[1]))&&(dl.isString(args[2]))&&(dl.isFunction(args[3]))){
ao.srcObj=args[1];
ao.srcFunc=args[2];
var _483=dl.nameAnonFunc(args[3],dj_global,_480);
ao.adviceObj=dj_global;
ao.adviceFunc=_483;
}else{
if(dl.isObject(args[1])){
ao.srcObj=args[1];
ao.srcFunc=args[2];
ao.adviceObj=dj_global;
ao.adviceFunc=args[3];
}else{
if(dl.isObject(args[2])){
ao.srcObj=dj_global;
ao.srcFunc=args[1];
ao.adviceObj=args[2];
ao.adviceFunc=args[3];
}else{
ao.srcObj=ao.adviceObj=ao.aroundObj=dj_global;
ao.srcFunc=args[1];
ao.adviceFunc=args[2];
ao.aroundFunc=args[3];
}
}
}
}
}
}
break;
case 6:
ao.srcObj=args[1];
ao.srcFunc=args[2];
ao.adviceObj=args[3];
ao.adviceFunc=args[4];
ao.aroundFunc=args[5];
ao.aroundObj=dj_global;
break;
default:
ao.srcObj=args[1];
ao.srcFunc=args[2];
ao.adviceObj=args[3];
ao.adviceFunc=args[4];
ao.aroundObj=args[5];
ao.aroundFunc=args[6];
ao.once=args[7];
ao.delay=args[8];
ao.rate=args[9];
ao.adviceMsg=args[10];
ao.maxCalls=(!isNaN(parseInt(args[11])))?args[11]:-1;
break;
}
if(dl.isFunction(ao.aroundFunc)){
var _483=dl.nameAnonFunc(ao.aroundFunc,ao.aroundObj,_480);
ao.aroundFunc=_483;
}
if(dl.isFunction(ao.srcFunc)){
ao.srcFunc=dl.getNameInObj(ao.srcObj,ao.srcFunc);
}
if(dl.isFunction(ao.adviceFunc)){
ao.adviceFunc=dl.getNameInObj(ao.adviceObj,ao.adviceFunc);
}
if((ao.aroundObj)&&(dl.isFunction(ao.aroundFunc))){
ao.aroundFunc=dl.getNameInObj(ao.aroundObj,ao.aroundFunc);
}
if(!ao.srcObj){
dojo.raise("bad srcObj for srcFunc: "+ao.srcFunc);
}
if(!ao.adviceObj){
dojo.raise("bad adviceObj for adviceFunc: "+ao.adviceFunc);
}
if(!ao.adviceFunc){
dojo.debug("bad adviceFunc for srcFunc: "+ao.srcFunc);
dojo.debugShallow(ao);
}
return ao;
}
this.connect=function(){
if(arguments.length==1){
var ao=arguments[0];
}else{
var ao=interpolateArgs(arguments,true);
}
if(dojo.lang.isString(ao.srcFunc)&&(ao.srcFunc.toLowerCase()=="onkey")){
if(dojo.render.html.ie){
ao.srcFunc="onkeydown";
this.connect(ao);
}
ao.srcFunc="onkeypress";
}
if(dojo.lang.isArray(ao.srcObj)&&ao.srcObj!=""){
var _485={};
for(var x in ao){
_485[x]=ao[x];
}
var mjps=[];
dojo.lang.forEach(ao.srcObj,function(src){
if((dojo.render.html.capable)&&(dojo.lang.isString(src))){
src=dojo.byId(src);
}
_485.srcObj=src;
mjps.push(dojo.event.connect.call(dojo.event,_485));
});
return mjps;
}
var mjp=dojo.event.MethodJoinPoint.getForMethod(ao.srcObj,ao.srcFunc);
if(ao.adviceFunc){
var mjp2=dojo.event.MethodJoinPoint.getForMethod(ao.adviceObj,ao.adviceFunc);
}
mjp.kwAddAdvice(ao);
return mjp;
};
this.log=function(a1,a2){
var _48d;
if((arguments.length==1)&&(typeof a1=="object")){
_48d=a1;
}else{
_48d={srcObj:a1,srcFunc:a2};
}
_48d.adviceFunc=function(){
var _48e=[];
for(var x=0;x<arguments.length;x++){
_48e.push(arguments[x]);
}
dojo.debug("("+_48d.srcObj+")."+_48d.srcFunc,":",_48e.join(", "));
};
this.kwConnect(_48d);
};
this.connectBefore=function(){
var args=["before"];
for(var i=0;i<arguments.length;i++){
args.push(arguments[i]);
}
return this.connect.apply(this,args);
};
this.connectAround=function(){
var args=["around"];
for(var i=0;i<arguments.length;i++){
args.push(arguments[i]);
}
return this.connect.apply(this,args);
};
this.connectOnce=function(){
var ao=interpolateArgs(arguments,true);
ao.once=true;
return this.connect(ao);
};
this.connectRunOnce=function(){
var ao=interpolateArgs(arguments,true);
ao.maxCalls=1;
return this.connect(ao);
};
this._kwConnectImpl=function(_496,_497){
var fn=(_497)?"disconnect":"connect";
if(typeof _496["srcFunc"]=="function"){
_496.srcObj=_496["srcObj"]||dj_global;
var _499=dojo.lang.nameAnonFunc(_496.srcFunc,_496.srcObj,true);
_496.srcFunc=_499;
}
if(typeof _496["adviceFunc"]=="function"){
_496.adviceObj=_496["adviceObj"]||dj_global;
var _499=dojo.lang.nameAnonFunc(_496.adviceFunc,_496.adviceObj,true);
_496.adviceFunc=_499;
}
_496.srcObj=_496["srcObj"]||dj_global;
_496.adviceObj=_496["adviceObj"]||_496["targetObj"]||dj_global;
_496.adviceFunc=_496["adviceFunc"]||_496["targetFunc"];
return dojo.event[fn](_496);
};
this.kwConnect=function(_49a){
return this._kwConnectImpl(_49a,false);
};
this.disconnect=function(){
if(arguments.length==1){
var ao=arguments[0];
}else{
var ao=interpolateArgs(arguments,true);
}
if(!ao.adviceFunc){
return;
}
if(dojo.lang.isString(ao.srcFunc)&&(ao.srcFunc.toLowerCase()=="onkey")){
if(dojo.render.html.ie){
ao.srcFunc="onkeydown";
this.disconnect(ao);
}
ao.srcFunc="onkeypress";
}
if(!ao.srcObj[ao.srcFunc]){
return null;
}
var mjp=dojo.event.MethodJoinPoint.getForMethod(ao.srcObj,ao.srcFunc,true);
mjp.removeAdvice(ao.adviceObj,ao.adviceFunc,ao.adviceType,ao.once);
return mjp;
};
this.kwDisconnect=function(_49d){
return this._kwConnectImpl(_49d,true);
};
};
dojo.event.MethodInvocation=function(_49e,obj,args){
this.jp_=_49e;
this.object=obj;
this.args=[];
for(var x=0;x<args.length;x++){
this.args[x]=args[x];
}
this.around_index=-1;
};
dojo.event.MethodInvocation.prototype.proceed=function(){
this.around_index++;
if(this.around_index>=this.jp_.around.length){
return this.jp_.object[this.jp_.methodname].apply(this.jp_.object,this.args);
}else{
var ti=this.jp_.around[this.around_index];
var mobj=ti[0]||dj_global;
var meth=ti[1];
return mobj[meth].call(mobj,this);
}
};
dojo.event.MethodJoinPoint=function(obj,_4a6){
this.object=obj||dj_global;
this.methodname=_4a6;
this.methodfunc=this.object[_4a6];
};
dojo.event.MethodJoinPoint.getForMethod=function(obj,_4a8){
if(!obj){
obj=dj_global;
}
var ofn=obj[_4a8];
if(!ofn){
ofn=obj[_4a8]=function(){
};
if(!obj[_4a8]){
dojo.raise("Cannot set do-nothing method on that object "+_4a8);
}
}else{
if((typeof ofn!="function")&&(!dojo.lang.isFunction(ofn))&&(!dojo.lang.isAlien(ofn))){
return null;
}
}
var _4aa=_4a8+"$joinpoint";
var _4ab=_4a8+"$joinpoint$method";
var _4ac=obj[_4aa];
if(!_4ac){
var _4ad=false;
if(dojo.event["browser"]){
if((obj["attachEvent"])||(obj["nodeType"])||(obj["addEventListener"])){
_4ad=true;
dojo.event.browser.addClobberNodeAttrs(obj,[_4aa,_4ab,_4a8]);
}
}
var _4ae=ofn.length;
obj[_4ab]=ofn;
_4ac=obj[_4aa]=new dojo.event.MethodJoinPoint(obj,_4ab);
if(!_4ad){
obj[_4a8]=function(){
return _4ac.run.apply(_4ac,arguments);
};
}else{
obj[_4a8]=function(){
var args=[];
if(!arguments.length){
var evt=null;
try{
if(obj.ownerDocument){
evt=obj.ownerDocument.parentWindow.event;
}else{
if(obj.documentElement){
evt=obj.documentElement.ownerDocument.parentWindow.event;
}else{
if(obj.event){
evt=obj.event;
}else{
evt=window.event;
}
}
}
}
catch(e){
evt=window.event;
}
if(evt){
args.push(dojo.event.browser.fixEvent(evt,this));
}
}else{
for(var x=0;x<arguments.length;x++){
if((x==0)&&(dojo.event.browser.isEvent(arguments[x]))){
args.push(dojo.event.browser.fixEvent(arguments[x],this));
}else{
args.push(arguments[x]);
}
}
}
return _4ac.run.apply(_4ac,args);
};
}
obj[_4a8].__preJoinArity=_4ae;
}
return _4ac;
};
dojo.lang.extend(dojo.event.MethodJoinPoint,{squelch:false,unintercept:function(){
this.object[this.methodname]=this.methodfunc;
this.before=[];
this.after=[];
this.around=[];
},disconnect:dojo.lang.forward("unintercept"),run:function(){
var obj=this.object||dj_global;
var args=arguments;
var _4b4=[];
for(var x=0;x<args.length;x++){
_4b4[x]=args[x];
}
var _4b6=function(marr){
if(!marr){
dojo.debug("Null argument to unrollAdvice()");
return;
}
var _4b8=marr[0]||dj_global;
var _4b9=marr[1];
if(!_4b8[_4b9]){
dojo.raise("function \""+_4b9+"\" does not exist on \""+_4b8+"\"");
}
var _4ba=marr[2]||dj_global;
var _4bb=marr[3];
var msg=marr[6];
var _4bd=marr[7];
if(_4bd>-1){
if(_4bd==0){
return;
}
marr[7]--;
}
var _4be;
var to={args:[],jp_:this,object:obj,proceed:function(){
return _4b8[_4b9].apply(_4b8,to.args);
}};
to.args=_4b4;
var _4c0=parseInt(marr[4]);
var _4c1=((!isNaN(_4c0))&&(marr[4]!==null)&&(typeof marr[4]!="undefined"));
if(marr[5]){
var rate=parseInt(marr[5]);
var cur=new Date();
var _4c4=false;
if((marr["last"])&&((cur-marr.last)<=rate)){
if(dojo.event._canTimeout){
if(marr["delayTimer"]){
clearTimeout(marr.delayTimer);
}
var tod=parseInt(rate*2);
var mcpy=dojo.lang.shallowCopy(marr);
marr.delayTimer=setTimeout(function(){
mcpy[5]=0;
_4b6(mcpy);
},tod);
}
return;
}else{
marr.last=cur;
}
}
if(_4bb){
_4ba[_4bb].call(_4ba,to);
}else{
if((_4c1)&&((dojo.render.html)||(dojo.render.svg))){
dj_global["setTimeout"](function(){
if(msg){
_4b8[_4b9].call(_4b8,to);
}else{
_4b8[_4b9].apply(_4b8,args);
}
},_4c0);
}else{
if(msg){
_4b8[_4b9].call(_4b8,to);
}else{
_4b8[_4b9].apply(_4b8,args);
}
}
}
};
var _4c7=function(){
if(this.squelch){
try{
return _4b6.apply(this,arguments);
}
catch(e){
dojo.debug(e);
}
}else{
return _4b6.apply(this,arguments);
}
};
if((this["before"])&&(this.before.length>0)){
dojo.lang.forEach(this.before.concat(new Array()),_4c7);
}
var _4c8;
try{
if((this["around"])&&(this.around.length>0)){
var mi=new dojo.event.MethodInvocation(this,obj,args);
_4c8=mi.proceed();
}else{
if(this.methodfunc){
_4c8=this.object[this.methodname].apply(this.object,args);
}
}
}
catch(e){
if(!this.squelch){
dojo.debug(e,"when calling",this.methodname,"on",this.object,"with arguments",args);
dojo.raise(e);
}
}
if((this["after"])&&(this.after.length>0)){
dojo.lang.forEach(this.after.concat(new Array()),_4c7);
}
return (this.methodfunc)?_4c8:null;
},getArr:function(kind){
var type="after";
if((typeof kind=="string")&&(kind.indexOf("before")!=-1)){
type="before";
}else{
if(kind=="around"){
type="around";
}
}
if(!this[type]){
this[type]=[];
}
return this[type];
},kwAddAdvice:function(args){
this.addAdvice(args["adviceObj"],args["adviceFunc"],args["aroundObj"],args["aroundFunc"],args["adviceType"],args["precedence"],args["once"],args["delay"],args["rate"],args["adviceMsg"],args["maxCalls"]);
},addAdvice:function(_4cd,_4ce,_4cf,_4d0,_4d1,_4d2,once,_4d4,rate,_4d6,_4d7){
var arr=this.getArr(_4d1);
if(!arr){
dojo.raise("bad this: "+this);
}
var ao=[_4cd,_4ce,_4cf,_4d0,_4d4,rate,_4d6,_4d7];
if(once){
if(this.hasAdvice(_4cd,_4ce,_4d1,arr)>=0){
return;
}
}
if(_4d2=="first"){
arr.unshift(ao);
}else{
arr.push(ao);
}
},hasAdvice:function(_4da,_4db,_4dc,arr){
if(!arr){
arr=this.getArr(_4dc);
}
var ind=-1;
for(var x=0;x<arr.length;x++){
var aao=(typeof _4db=="object")?(new String(_4db)).toString():_4db;
var a1o=(typeof arr[x][1]=="object")?(new String(arr[x][1])).toString():arr[x][1];
if((arr[x][0]==_4da)&&(a1o==aao)){
ind=x;
}
}
return ind;
},removeAdvice:function(_4e2,_4e3,_4e4,once){
var arr=this.getArr(_4e4);
var ind=this.hasAdvice(_4e2,_4e3,_4e4,arr);
if(ind==-1){
return false;
}
while(ind!=-1){
arr.splice(ind,1);
if(once){
break;
}
ind=this.hasAdvice(_4e2,_4e3,_4e4,arr);
}
return true;
}});
dojo.provide("dojo.event.topic");
dojo.event.topic=new function(){
this.topics={};
this.getTopic=function(_4e8){
if(!this.topics[_4e8]){
this.topics[_4e8]=new this.TopicImpl(_4e8);
}
return this.topics[_4e8];
};
this.registerPublisher=function(_4e9,obj,_4eb){
var _4e9=this.getTopic(_4e9);
_4e9.registerPublisher(obj,_4eb);
};
this.subscribe=function(_4ec,obj,_4ee){
var _4ec=this.getTopic(_4ec);
_4ec.subscribe(obj,_4ee);
};
this.unsubscribe=function(_4ef,obj,_4f1){
var _4ef=this.getTopic(_4ef);
_4ef.unsubscribe(obj,_4f1);
};
this.destroy=function(_4f2){
this.getTopic(_4f2).destroy();
delete this.topics[_4f2];
};
this.publishApply=function(_4f3,args){
var _4f3=this.getTopic(_4f3);
_4f3.sendMessage.apply(_4f3,args);
};
this.publish=function(_4f5,_4f6){
var _4f5=this.getTopic(_4f5);
var args=[];
for(var x=1;x<arguments.length;x++){
args.push(arguments[x]);
}
_4f5.sendMessage.apply(_4f5,args);
};
};
dojo.event.topic.TopicImpl=function(_4f9){
this.topicName=_4f9;
this.subscribe=function(_4fa,_4fb){
var tf=_4fb||_4fa;
var to=(!_4fb)?dj_global:_4fa;
return dojo.event.kwConnect({srcObj:this,srcFunc:"sendMessage",adviceObj:to,adviceFunc:tf});
};
this.unsubscribe=function(_4fe,_4ff){
var tf=(!_4ff)?_4fe:_4ff;
var to=(!_4ff)?null:_4fe;
return dojo.event.kwDisconnect({srcObj:this,srcFunc:"sendMessage",adviceObj:to,adviceFunc:tf});
};
this._getJoinPoint=function(){
return dojo.event.MethodJoinPoint.getForMethod(this,"sendMessage");
};
this.setSquelch=function(_502){
this._getJoinPoint().squelch=_502;
};
this.destroy=function(){
this._getJoinPoint().disconnect();
};
this.registerPublisher=function(_503,_504){
dojo.event.connect(_503,_504,this,"sendMessage");
};
this.sendMessage=function(_505){
};
};
dojo.provide("dojo.event.browser");
dojo._ie_clobber=new function(){
this.clobberNodes=[];
function nukeProp(node,prop){
try{
node[prop]=null;
}
catch(e){
}
try{
delete node[prop];
}
catch(e){
}
try{
node.removeAttribute(prop);
}
catch(e){
}
}
this.clobber=function(_508){
var na;
var tna;
if(_508){
tna=_508.all||_508.getElementsByTagName("*");
na=[_508];
for(var x=0;x<tna.length;x++){
if(tna[x]["__doClobber__"]){
na.push(tna[x]);
}
}
}else{
try{
window.onload=null;
}
catch(e){
}
na=(this.clobberNodes.length)?this.clobberNodes:document.all;
}
tna=null;
var _50c={};
for(var i=na.length-1;i>=0;i=i-1){
var el=na[i];
try{
if(el&&el["__clobberAttrs__"]){
for(var j=0;j<el.__clobberAttrs__.length;j++){
nukeProp(el,el.__clobberAttrs__[j]);
}
nukeProp(el,"__clobberAttrs__");
nukeProp(el,"__doClobber__");
}
}
catch(e){
}
}
na=null;
};
};
if(dojo.render.html.ie){
dojo.addOnUnload(function(){
dojo._ie_clobber.clobber();
try{
if((dojo["widget"])&&(dojo.widget["manager"])){
dojo.widget.manager.destroyAll();
}
}
catch(e){
}
if(dojo.widget){
for(var name in dojo.widget._templateCache){
if(dojo.widget._templateCache[name].node){
dojo.dom.destroyNode(dojo.widget._templateCache[name].node);
dojo.widget._templateCache[name].node=null;
delete dojo.widget._templateCache[name].node;
}
}
}
try{
window.onload=null;
}
catch(e){
}
try{
window.onunload=null;
}
catch(e){
}
dojo._ie_clobber.clobberNodes=[];
});
}
dojo.event.browser=new function(){
var _511=0;
this.normalizedEventName=function(_512){
switch(_512){
case "CheckboxStateChange":
case "DOMAttrModified":
case "DOMMenuItemActive":
case "DOMMenuItemInactive":
case "DOMMouseScroll":
case "DOMNodeInserted":
case "DOMNodeRemoved":
case "RadioStateChange":
return _512;
break;
default:
var lcn=_512.toLowerCase();
return (lcn.indexOf("on")==0)?lcn.substr(2):lcn;
break;
}
};
this.clean=function(node){
if(dojo.render.html.ie){
dojo._ie_clobber.clobber(node);
}
};
this.addClobberNode=function(node){
if(!dojo.render.html.ie){
return;
}
if(!node["__doClobber__"]){
node.__doClobber__=true;
dojo._ie_clobber.clobberNodes.push(node);
node.__clobberAttrs__=[];
}
};
this.addClobberNodeAttrs=function(node,_517){
if(!dojo.render.html.ie){
return;
}
this.addClobberNode(node);
for(var x=0;x<_517.length;x++){
node.__clobberAttrs__.push(_517[x]);
}
};
this.removeListener=function(node,_51a,fp,_51c){
if(!_51c){
var _51c=false;
}
_51a=dojo.event.browser.normalizedEventName(_51a);
if(_51a=="key"){
if(dojo.render.html.ie){
this.removeListener(node,"onkeydown",fp,_51c);
}
_51a="keypress";
}
if(node.removeEventListener){
node.removeEventListener(_51a,fp,_51c);
}
};
this.addListener=function(node,_51e,fp,_520,_521){
if(!node){
return;
}
if(!_520){
var _520=false;
}
_51e=dojo.event.browser.normalizedEventName(_51e);
if(_51e=="key"){
if(dojo.render.html.ie){
this.addListener(node,"onkeydown",fp,_520,_521);
}
_51e="keypress";
}
if(!_521){
var _522=function(evt){
if(!evt){
evt=window.event;
}
var ret=fp(dojo.event.browser.fixEvent(evt,this));
if(_520){
dojo.event.browser.stopEvent(evt);
}
return ret;
};
}else{
_522=fp;
}
if(node.addEventListener){
node.addEventListener(_51e,_522,_520);
return _522;
}else{
_51e="on"+_51e;
if(typeof node[_51e]=="function"){
var _525=node[_51e];
node[_51e]=function(e){
_525(e);
return _522(e);
};
}else{
node[_51e]=_522;
}
if(dojo.render.html.ie){
this.addClobberNodeAttrs(node,[_51e]);
}
return _522;
}
};
this.isEvent=function(obj){
return (typeof obj!="undefined")&&(obj)&&(typeof Event!="undefined")&&(obj.eventPhase);
};
this.currentEvent=null;
this.callListener=function(_528,_529){
if(typeof _528!="function"){
dojo.raise("listener not a function: "+_528);
}
dojo.event.browser.currentEvent.currentTarget=_529;
return _528.call(_529,dojo.event.browser.currentEvent);
};
this._stopPropagation=function(){
dojo.event.browser.currentEvent.cancelBubble=true;
};
this._preventDefault=function(){
dojo.event.browser.currentEvent.returnValue=false;
};
this.keys={KEY_BACKSPACE:8,KEY_TAB:9,KEY_CLEAR:12,KEY_ENTER:13,KEY_SHIFT:16,KEY_CTRL:17,KEY_ALT:18,KEY_PAUSE:19,KEY_CAPS_LOCK:20,KEY_ESCAPE:27,KEY_SPACE:32,KEY_PAGE_UP:33,KEY_PAGE_DOWN:34,KEY_END:35,KEY_HOME:36,KEY_LEFT_ARROW:37,KEY_UP_ARROW:38,KEY_RIGHT_ARROW:39,KEY_DOWN_ARROW:40,KEY_INSERT:45,KEY_DELETE:46,KEY_HELP:47,KEY_LEFT_WINDOW:91,KEY_RIGHT_WINDOW:92,KEY_SELECT:93,KEY_NUMPAD_0:96,KEY_NUMPAD_1:97,KEY_NUMPAD_2:98,KEY_NUMPAD_3:99,KEY_NUMPAD_4:100,KEY_NUMPAD_5:101,KEY_NUMPAD_6:102,KEY_NUMPAD_7:103,KEY_NUMPAD_8:104,KEY_NUMPAD_9:105,KEY_NUMPAD_MULTIPLY:106,KEY_NUMPAD_PLUS:107,KEY_NUMPAD_ENTER:108,KEY_NUMPAD_MINUS:109,KEY_NUMPAD_PERIOD:110,KEY_NUMPAD_DIVIDE:111,KEY_F1:112,KEY_F2:113,KEY_F3:114,KEY_F4:115,KEY_F5:116,KEY_F6:117,KEY_F7:118,KEY_F8:119,KEY_F9:120,KEY_F10:121,KEY_F11:122,KEY_F12:123,KEY_F13:124,KEY_F14:125,KEY_F15:126,KEY_NUM_LOCK:144,KEY_SCROLL_LOCK:145};
this.revKeys=[];
for(var key in this.keys){
this.revKeys[this.keys[key]]=key;
}
this.fixEvent=function(evt,_52c){
if(!evt){
if(window["event"]){
evt=window.event;
}
}
if((evt["type"])&&(evt["type"].indexOf("key")==0)){
evt.keys=this.revKeys;
for(var key in this.keys){
evt[key]=this.keys[key];
}
if(evt["type"]=="keydown"&&dojo.render.html.ie){
switch(evt.keyCode){
case evt.KEY_SHIFT:
case evt.KEY_CTRL:
case evt.KEY_ALT:
case evt.KEY_CAPS_LOCK:
case evt.KEY_LEFT_WINDOW:
case evt.KEY_RIGHT_WINDOW:
case evt.KEY_SELECT:
case evt.KEY_NUM_LOCK:
case evt.KEY_SCROLL_LOCK:
case evt.KEY_NUMPAD_0:
case evt.KEY_NUMPAD_1:
case evt.KEY_NUMPAD_2:
case evt.KEY_NUMPAD_3:
case evt.KEY_NUMPAD_4:
case evt.KEY_NUMPAD_5:
case evt.KEY_NUMPAD_6:
case evt.KEY_NUMPAD_7:
case evt.KEY_NUMPAD_8:
case evt.KEY_NUMPAD_9:
case evt.KEY_NUMPAD_PERIOD:
break;
case evt.KEY_NUMPAD_MULTIPLY:
case evt.KEY_NUMPAD_PLUS:
case evt.KEY_NUMPAD_ENTER:
case evt.KEY_NUMPAD_MINUS:
case evt.KEY_NUMPAD_DIVIDE:
break;
case evt.KEY_PAUSE:
case evt.KEY_TAB:
case evt.KEY_BACKSPACE:
case evt.KEY_ENTER:
case evt.KEY_ESCAPE:
case evt.KEY_PAGE_UP:
case evt.KEY_PAGE_DOWN:
case evt.KEY_END:
case evt.KEY_HOME:
case evt.KEY_LEFT_ARROW:
case evt.KEY_UP_ARROW:
case evt.KEY_RIGHT_ARROW:
case evt.KEY_DOWN_ARROW:
case evt.KEY_INSERT:
case evt.KEY_DELETE:
case evt.KEY_F1:
case evt.KEY_F2:
case evt.KEY_F3:
case evt.KEY_F4:
case evt.KEY_F5:
case evt.KEY_F6:
case evt.KEY_F7:
case evt.KEY_F8:
case evt.KEY_F9:
case evt.KEY_F10:
case evt.KEY_F11:
case evt.KEY_F12:
case evt.KEY_F12:
case evt.KEY_F13:
case evt.KEY_F14:
case evt.KEY_F15:
case evt.KEY_CLEAR:
case evt.KEY_HELP:
evt.key=evt.keyCode;
break;
default:
if(evt.ctrlKey||evt.altKey){
var _52e=evt.keyCode;
if(_52e>=65&&_52e<=90&&evt.shiftKey==false){
_52e+=32;
}
if(_52e>=1&&_52e<=26&&evt.ctrlKey){
_52e+=96;
}
evt.key=String.fromCharCode(_52e);
}
}
}else{
if(evt["type"]=="keypress"){
if(dojo.render.html.opera){
if(evt.which==0){
evt.key=evt.keyCode;
}else{
if(evt.which>0){
switch(evt.which){
case evt.KEY_SHIFT:
case evt.KEY_CTRL:
case evt.KEY_ALT:
case evt.KEY_CAPS_LOCK:
case evt.KEY_NUM_LOCK:
case evt.KEY_SCROLL_LOCK:
break;
case evt.KEY_PAUSE:
case evt.KEY_TAB:
case evt.KEY_BACKSPACE:
case evt.KEY_ENTER:
case evt.KEY_ESCAPE:
evt.key=evt.which;
break;
default:
var _52e=evt.which;
if((evt.ctrlKey||evt.altKey||evt.metaKey)&&(evt.which>=65&&evt.which<=90&&evt.shiftKey==false)){
_52e+=32;
}
evt.key=String.fromCharCode(_52e);
}
}
}
}else{
if(dojo.render.html.ie){
if(!evt.ctrlKey&&!evt.altKey&&evt.keyCode>=evt.KEY_SPACE){
evt.key=String.fromCharCode(evt.keyCode);
}
}else{
if(dojo.render.html.safari){
switch(evt.keyCode){
case 25:
evt.key=evt.KEY_TAB;
evt.shift=true;
break;
case 63232:
evt.key=evt.KEY_UP_ARROW;
break;
case 63233:
evt.key=evt.KEY_DOWN_ARROW;
break;
case 63234:
evt.key=evt.KEY_LEFT_ARROW;
break;
case 63235:
evt.key=evt.KEY_RIGHT_ARROW;
break;
case 63236:
evt.key=evt.KEY_F1;
break;
case 63237:
evt.key=evt.KEY_F2;
break;
case 63238:
evt.key=evt.KEY_F3;
break;
case 63239:
evt.key=evt.KEY_F4;
break;
case 63240:
evt.key=evt.KEY_F5;
break;
case 63241:
evt.key=evt.KEY_F6;
break;
case 63242:
evt.key=evt.KEY_F7;
break;
case 63243:
evt.key=evt.KEY_F8;
break;
case 63244:
evt.key=evt.KEY_F9;
break;
case 63245:
evt.key=evt.KEY_F10;
break;
case 63246:
evt.key=evt.KEY_F11;
break;
case 63247:
evt.key=evt.KEY_F12;
break;
case 63250:
evt.key=evt.KEY_PAUSE;
break;
case 63272:
evt.key=evt.KEY_DELETE;
break;
case 63273:
evt.key=evt.KEY_HOME;
break;
case 63275:
evt.key=evt.KEY_END;
break;
case 63276:
evt.key=evt.KEY_PAGE_UP;
break;
case 63277:
evt.key=evt.KEY_PAGE_DOWN;
break;
case 63302:
evt.key=evt.KEY_INSERT;
break;
case 63248:
case 63249:
case 63289:
break;
default:
evt.key=evt.charCode>=evt.KEY_SPACE?String.fromCharCode(evt.charCode):evt.keyCode;
}
}else{
evt.key=evt.charCode>0?String.fromCharCode(evt.charCode):evt.keyCode;
}
}
}
}
}
}
if(dojo.render.html.ie){
if(!evt.target){
evt.target=evt.srcElement;
}
if(!evt.currentTarget){
evt.currentTarget=(_52c?_52c:evt.srcElement);
}
if(!evt.layerX){
evt.layerX=evt.offsetX;
}
if(!evt.layerY){
evt.layerY=evt.offsetY;
}
var doc=(evt.srcElement&&evt.srcElement.ownerDocument)?evt.srcElement.ownerDocument:document;
var _530=((dojo.render.html.ie55)||(doc["compatMode"]=="BackCompat"))?doc.body:doc.documentElement;
if(!evt.pageX){
evt.pageX=evt.clientX+(_530.scrollLeft||0);
}
if(!evt.pageY){
evt.pageY=evt.clientY+(_530.scrollTop||0);
}
if(evt.type=="mouseover"){
evt.relatedTarget=evt.fromElement;
}
if(evt.type=="mouseout"){
evt.relatedTarget=evt.toElement;
}
this.currentEvent=evt;
evt.callListener=this.callListener;
evt.stopPropagation=this._stopPropagation;
evt.preventDefault=this._preventDefault;
}
return evt;
};
this.stopEvent=function(evt){
if(window.event){
evt.cancelBubble=true;
evt.returnValue=false;
}else{
evt.preventDefault();
evt.stopPropagation();
}
};
};
dojo.kwCompoundRequire({common:["dojo.event.common","dojo.event.topic"],browser:["dojo.event.browser"],dashboard:["dojo.event.browser"]});
dojo.provide("dojo.event.*");
dojo.provide("dojo.widget.Manager");
dojo.widget.manager=new function(){
this.widgets=[];
this.widgetIds=[];
this.topWidgets={};
var _532={};
var _533=[];
this.getUniqueId=function(_534){
var _535;
do{
_535=_534+"_"+(_532[_534]!=undefined?++_532[_534]:_532[_534]=0);
}while(this.getWidgetById(_535));
return _535;
};
this.add=function(_536){
this.widgets.push(_536);
if(!_536.extraArgs["id"]){
_536.extraArgs["id"]=_536.extraArgs["ID"];
}
if(_536.widgetId==""){
if(_536["id"]){
_536.widgetId=_536["id"];
}else{
if(_536.extraArgs["id"]){
_536.widgetId=_536.extraArgs["id"];
}else{
_536.widgetId=this.getUniqueId(_536.ns+"_"+_536.widgetType);
}
}
}
if(this.widgetIds[_536.widgetId]){
dojo.debug("widget ID collision on ID: "+_536.widgetId);
}
this.widgetIds[_536.widgetId]=_536;
};
this.destroyAll=function(){
for(var x=this.widgets.length-1;x>=0;x--){
try{
this.widgets[x].destroy(true);
delete this.widgets[x];
}
catch(e){
}
}
};
this.remove=function(_538){
if(dojo.lang.isNumber(_538)){
var tw=this.widgets[_538].widgetId;
delete this.topWidgets[tw];
delete this.widgetIds[tw];
this.widgets.splice(_538,1);
}else{
this.removeById(_538);
}
};
this.removeById=function(id){
if(!dojo.lang.isString(id)){
id=id["widgetId"];
if(!id){
dojo.debug("invalid widget or id passed to removeById");
return;
}
}
for(var i=0;i<this.widgets.length;i++){
if(this.widgets[i].widgetId==id){
this.remove(i);
break;
}
}
};
this.getWidgetById=function(id){
if(dojo.lang.isString(id)){
return this.widgetIds[id];
}
return id;
};
this.getWidgetsByType=function(type){
var lt=type.toLowerCase();
var _53f=(type.indexOf(":")<0?function(x){
return x.widgetType.toLowerCase();
}:function(x){
return x.getNamespacedType();
});
var ret=[];
dojo.lang.forEach(this.widgets,function(x){
if(_53f(x)==lt){
ret.push(x);
}
});
return ret;
};
this.getWidgetsByFilter=function(_544,_545){
var ret=[];
dojo.lang.every(this.widgets,function(x){
if(_544(x)){
ret.push(x);
if(_545){
return false;
}
}
return true;
});
return (_545?ret[0]:ret);
};
this.getAllWidgets=function(){
return this.widgets.concat();
};
this.getWidgetByNode=function(node){
var w=this.getAllWidgets();
node=dojo.byId(node);
for(var i=0;i<w.length;i++){
if(w[i].domNode==node){
return w[i];
}
}
return null;
};
this.byId=this.getWidgetById;
this.byType=this.getWidgetsByType;
this.byFilter=this.getWidgetsByFilter;
this.byNode=this.getWidgetByNode;
var _54b={};
var _54c=["dojo.widget"];
for(var i=0;i<_54c.length;i++){
_54c[_54c[i]]=true;
}
this.registerWidgetPackage=function(_54e){
if(!_54c[_54e]){
_54c[_54e]=true;
_54c.push(_54e);
}
};
this.getWidgetPackageList=function(){
return dojo.lang.map(_54c,function(elt){
return (elt!==true?elt:undefined);
});
};
this.getImplementation=function(_550,_551,_552,ns){
var impl=this.getImplementationName(_550,ns);
if(impl){
var ret=_551?new impl(_551):new impl();
return ret;
}
};
function buildPrefixCache(){
for(var _556 in dojo.render){
if(dojo.render[_556]["capable"]===true){
var _557=dojo.render[_556].prefixes;
for(var i=0;i<_557.length;i++){
_533.push(_557[i].toLowerCase());
}
}
}
}
var _559=function(_55a,_55b){
if(!_55b){
return null;
}
for(var i=0,l=_533.length,_55e;i<=l;i++){
_55e=(i<l?_55b[_533[i]]:_55b);
if(!_55e){
continue;
}
for(var name in _55e){
if(name.toLowerCase()==_55a){
return _55e[name];
}
}
}
return null;
};
var _560=function(_561,_562){
var _563=dojo.getObject(_562,false);
return (_563?_559(_561,_563):null);
};
this.getImplementationName=function(_564,ns){
var _566=_564.toLowerCase();
ns=ns||"dojo";
var imps=_54b[ns]||(_54b[ns]={});
var impl=imps[_566];
if(impl){
return impl;
}
if(!_533.length){
buildPrefixCache();
}
var _569=dojo.ns.get(ns);
if(!_569){
dojo.ns.register(ns,ns+".widget");
_569=dojo.ns.get(ns);
}
if(_569){
_569.resolve(_564);
}
impl=_560(_566,_569.module);
if(impl){
return (imps[_566]=impl);
}
_569=dojo.ns.require(ns);
if((_569)&&(_569.resolver)){
_569.resolve(_564);
impl=_560(_566,_569.module);
if(impl){
return (imps[_566]=impl);
}
}
throw new Error("Could not locate widget implementation for \""+_564+"\" in \""+_569.module+"\" registered to namespace \""+_569.name+"\"");
};
this.resizing=false;
this.onWindowResized=function(){
if(this.resizing){
return;
}
try{
this.resizing=true;
for(var id in this.topWidgets){
var _56b=this.topWidgets[id];
if(_56b.checkSize){
_56b.checkSize();
}
}
}
catch(e){
}
finally{
this.resizing=false;
}
};
if(typeof window!="undefined"){
dojo.addOnLoad(this,"onWindowResized");
dojo.event.connect(window,"onresize",this,"onWindowResized");
}
};
(function(){
var dw=dojo.widget;
var dwm=dw.manager;
var h=dojo.lang.curry(dojo.lang,"hitch",dwm);
var g=function(_570,_571){
dw[(_571||_570)]=h(_570);
};
g("add","addWidget");
g("destroyAll","destroyAllWidgets");
g("remove","removeWidget");
g("removeById","removeWidgetById");
g("getWidgetById");
g("getWidgetById","byId");
g("getWidgetsByType");
g("getWidgetsByFilter");
g("getWidgetsByType","byType");
g("getWidgetsByFilter","byFilter");
g("getWidgetByNode","byNode");
dw.all=function(n){
var _573=dwm.getAllWidgets.apply(dwm,arguments);
if(arguments.length>0){
return _573[n];
}
return _573;
};
g("registerWidgetPackage");
g("getImplementation","getWidgetImplementation");
g("getImplementationName","getWidgetImplementationName");
dw.widgets=dwm.widgets;
dw.widgetIds=dwm.widgetIds;
dw.root=dwm.root;
})();
dojo.provide("dojo.a11y");
dojo.a11y={imgPath:dojo.uri.moduleUri("dojo.widget","templates/images"),doAccessibleCheck:true,accessible:null,checkAccessible:function(){
if(this.accessible===null){
this.accessible=false;
if(this.doAccessibleCheck==true){
this.accessible=this.testAccessible();
}
}
return this.accessible;
},testAccessible:function(){
this.accessible=false;
if(dojo.render.html.ie||dojo.render.html.mozilla){
var div=document.createElement("div");
div.style.backgroundImage="url(\""+this.imgPath+"/tab_close.gif\")";
dojo.body().appendChild(div);
var _575=null;
if(window.getComputedStyle){
var _576=getComputedStyle(div,"");
_575=_576.getPropertyValue("background-image");
}else{
_575=div.currentStyle.backgroundImage;
}
var _577=false;
if(_575!=null&&(_575=="none"||_575=="url(invalid-url:)")){
this.accessible=true;
}
dojo.body().removeChild(div);
}
return this.accessible;
},setAccessible:function(_578){
this.accessible=_578;
},setCheckAccessible:function(_579){
this.doAccessibleCheck=_579;
},setAccessibleMode:function(){
if(this.accessible===null){
if(this.checkAccessible()){
dojo.render.html.prefixes.unshift("a11y");
}
}
return this.accessible;
}};
dojo.provide("dojo.widget.Widget");
dojo.declare("dojo.widget.Widget",null,function(){
this.children=[];
this.extraArgs={};
},{parent:null,isTopLevel:false,disabled:false,isContainer:false,widgetId:"",widgetType:"Widget",ns:"dojo",getNamespacedType:function(){
return (this.ns?this.ns+":"+this.widgetType:this.widgetType).toLowerCase();
},toString:function(){
return "[Widget "+this.getNamespacedType()+", "+(this.widgetId||"NO ID")+"]";
},repr:function(){
return this.toString();
},enable:function(){
this.disabled=false;
},disable:function(){
this.disabled=true;
},onResized:function(){
this.notifyChildrenOfResize();
},notifyChildrenOfResize:function(){
for(var i=0;i<this.children.length;i++){
var _57b=this.children[i];
if(_57b.onResized){
_57b.onResized();
}
}
},create:function(args,_57d,_57e,ns){
if(ns){
this.ns=ns;
}
this.satisfyPropertySets(args,_57d,_57e);
this.mixInProperties(args,_57d,_57e);
this.postMixInProperties(args,_57d,_57e);
dojo.widget.manager.add(this);
this.buildRendering(args,_57d,_57e);
this.initialize(args,_57d,_57e);
this.postInitialize(args,_57d,_57e);
this.postCreate(args,_57d,_57e);
return this;
},destroy:function(_580){
if(this.parent){
this.parent.removeChild(this);
}
this.destroyChildren();
this.uninitialize();
this.destroyRendering(_580);
dojo.widget.manager.removeById(this.widgetId);
},destroyChildren:function(){
var _581;
var i=0;
while(this.children.length>i){
_581=this.children[i];
if(_581 instanceof dojo.widget.Widget){
this.removeChild(_581);
_581.destroy();
continue;
}
i++;
}
},getChildrenOfType:function(type,_584){
var ret=[];
var _586=dojo.lang.isFunction(type);
if(!_586){
type=type.toLowerCase();
}
for(var x=0;x<this.children.length;x++){
if(_586){
if(this.children[x] instanceof type){
ret.push(this.children[x]);
}
}else{
if(this.children[x].widgetType.toLowerCase()==type){
ret.push(this.children[x]);
}
}
if(_584){
ret=ret.concat(this.children[x].getChildrenOfType(type,_584));
}
}
return ret;
},getDescendants:function(){
var _588=[];
var _589=[this];
var elem;
while((elem=_589.pop())){
_588.push(elem);
if(elem.children){
dojo.lang.forEach(elem.children,function(elem){
_589.push(elem);
});
}
}
return _588;
},isFirstChild:function(){
return this===this.parent.children[0];
},isLastChild:function(){
return this===this.parent.children[this.parent.children.length-1];
},satisfyPropertySets:function(args){
return args;
},mixInProperties:function(args,frag){
if((args["fastMixIn"])||(frag["fastMixIn"])){
for(var x in args){
this[x]=args[x];
}
return;
}
var _590;
var _591=dojo.widget.lcArgsCache[this.widgetType];
if(_591==null){
_591={};
for(var y in this){
_591[((new String(y)).toLowerCase())]=y;
}
dojo.widget.lcArgsCache[this.widgetType]=_591;
}
var _593={};
for(var x in args){
if(!this[x]){
var y=_591[(new String(x)).toLowerCase()];
if(y){
args[y]=args[x];
x=y;
}
}
if(_593[x]){
continue;
}
_593[x]=true;
if((typeof this[x])!=(typeof _590)){
if(typeof args[x]!="string"){
this[x]=args[x];
}else{
if(dojo.lang.isString(this[x])){
this[x]=args[x];
}else{
if(dojo.lang.isNumber(this[x])){
this[x]=new Number(args[x]);
}else{
if(dojo.lang.isBoolean(this[x])){
this[x]=(args[x].toLowerCase()=="false")?false:true;
}else{
if(dojo.lang.isFunction(this[x])){
if(args[x].search(/[^\w\.]+/i)==-1){
this[x]=dojo.getObject(args[x],false);
}else{
var tn=dojo.lang.nameAnonFunc(new Function(args[x]),this);
dojo.event.kwConnect({srcObj:this,srcFunc:x,adviceObj:this,adviceFunc:tn});
}
}else{
if(dojo.lang.isArray(this[x])){
this[x]=args[x].split(";");
}else{
if(this[x] instanceof Date){
this[x]=new Date(Number(args[x]));
}else{
if(typeof this[x]=="object"){
if(this[x] instanceof dojo.uri.Uri){
this[x]=dojo.uri.dojoUri(args[x]);
}else{
var _595=args[x].split(";");
for(var y=0;y<_595.length;y++){
var si=_595[y].indexOf(":");
if((si!=-1)&&(_595[y].length>si)){
this[x][_595[y].substr(0,si).replace(/^\s+|\s+$/g,"")]=_595[y].substr(si+1);
}
}
}
}else{
this[x]=args[x];
}
}
}
}
}
}
}
}
}else{
this.extraArgs[x.toLowerCase()]=args[x];
}
}
},postMixInProperties:function(args,frag,_599){
},initialize:function(args,frag,_59c){
return false;
},postInitialize:function(args,frag,_59f){
return false;
},postCreate:function(args,frag,_5a2){
return false;
},uninitialize:function(){
return false;
},buildRendering:function(args,frag,_5a5){
dojo.unimplemented("dojo.widget.Widget.buildRendering, on "+this.toString()+", ");
return false;
},destroyRendering:function(){
dojo.unimplemented("dojo.widget.Widget.destroyRendering");
return false;
},addedTo:function(_5a6){
},addChild:function(_5a7){
dojo.unimplemented("dojo.widget.Widget.addChild");
return false;
},removeChild:function(_5a8){
for(var x=0;x<this.children.length;x++){
if(this.children[x]===_5a8){
this.children.splice(x,1);
_5a8.parent=null;
break;
}
}
return _5a8;
},getPreviousSibling:function(){
var idx=this.getParentIndex();
if(idx<=0){
return null;
}
return this.parent.children[idx-1];
},getSiblings:function(){
return this.parent.children;
},getParentIndex:function(){
return dojo.lang.indexOf(this.parent.children,this,true);
},getNextSibling:function(){
var idx=this.getParentIndex();
if(idx==this.parent.children.length-1){
return null;
}
if(idx<0){
return null;
}
return this.parent.children[idx+1];
}});
dojo.widget.lcArgsCache={};
dojo.widget.tags={};
dojo.widget.tags["dojo:propertyset"]=function(_5ac,_5ad,_5ae){
var _5af=_5ad.parseProperties(_5ac["dojo:propertyset"]);
};
dojo.widget.tags["dojo:connect"]=function(_5b0,_5b1,_5b2){
var _5b3=_5b1.parseProperties(_5b0["dojo:connect"]);
};
dojo.widget.buildWidgetFromParseTree=function(type,frag,_5b6,_5b7,_5b8,_5b9){
dojo.a11y.setAccessibleMode();
var _5ba=type.split(":");
_5ba=(_5ba.length==2)?_5ba[1]:type;
var _5bb=_5b9||_5b6.parseProperties(frag[frag["ns"]+":"+_5ba]);
var _5bc=dojo.widget.manager.getImplementation(_5ba,null,null,frag["ns"]);
if(!_5bc){
throw new Error("cannot find \""+type+"\" widget");
}else{
if(!_5bc.create){
throw new Error("\""+type+"\" widget object has no \"create\" method and does not appear to implement *Widget");
}
}
_5bb["dojoinsertionindex"]=_5b8;
var ret=_5bc.create(_5bb,frag,_5b7,frag["ns"]);
return ret;
};
dojo.widget.defineWidget=function(_5be,_5bf,_5c0,init,_5c2){
if(dojo.lang.isString(arguments[3])){
dojo.widget._defineWidget(arguments[0],arguments[3],arguments[1],arguments[4],arguments[2]);
}else{
var args=[arguments[0]],p=3;
if(dojo.lang.isString(arguments[1])){
args.push(arguments[1],arguments[2]);
}else{
args.push("",arguments[1]);
p=2;
}
if(dojo.lang.isFunction(arguments[p])){
args.push(arguments[p],arguments[p+1]);
}else{
args.push(null,arguments[p]);
}
dojo.widget._defineWidget.apply(this,args);
}
};
dojo.widget.defineWidget.renderers="html|svg|vml";
dojo.widget._defineWidget=function(_5c5,_5c6,_5c7,init,_5c9){
var _5ca=_5c5.split(".");
var type=_5ca.pop();
var regx="\\.("+(_5c6?_5c6+"|":"")+dojo.widget.defineWidget.renderers+")\\.";
var r=_5c5.search(new RegExp(regx));
_5ca=(r<0?_5ca.join("."):_5c5.substr(0,r));
dojo.widget.manager.registerWidgetPackage(_5ca);
var pos=_5ca.indexOf(".");
var _5cf=(pos>-1)?_5ca.substring(0,pos):_5ca;
_5c9=(_5c9)||{};
_5c9.widgetType=type;
if((!init)&&(_5c9["classConstructor"])){
init=_5c9.classConstructor;
delete _5c9.classConstructor;
}
dojo.declare(_5c5,_5c7,init,_5c9);
};
dojo.provide("dojo.widget.Parse");
dojo.widget.Parse=function(_5d0){
this.propertySetsList=[];
this.fragment=_5d0;
this.createComponents=function(frag,_5d2){
var _5d3=[];
var _5d4=false;
try{
if(frag&&frag.tagName&&(frag!=frag.nodeRef)){
var _5d5=dojo.widget.tags;
var tna=String(frag.tagName).split(";");
for(var x=0;x<tna.length;x++){
var ltn=tna[x].replace(/^\s+|\s+$/g,"").toLowerCase();
frag.tagName=ltn;
var ret;
if(_5d5[ltn]){
_5d4=true;
ret=_5d5[ltn](frag,this,_5d2,frag.index);
_5d3.push(ret);
}else{
if(ltn.indexOf(":")==-1){
ltn="dojo:"+ltn;
}
ret=dojo.widget.buildWidgetFromParseTree(ltn,frag,this,_5d2,frag.index);
if(ret){
_5d4=true;
_5d3.push(ret);
}
}
}
}
}
catch(e){
dojo.debug("dojo.widget.Parse: error:",e);
}
if(!_5d4){
_5d3=_5d3.concat(this.createSubComponents(frag,_5d2));
}
return _5d3;
};
this.createSubComponents=function(_5da,_5db){
var frag,_5dd=[];
for(var item in _5da){
frag=_5da[item];
if(frag&&typeof frag=="object"&&(frag!=_5da.nodeRef)&&(frag!=_5da.tagName)&&(item.indexOf("$")==-1)){
_5dd=_5dd.concat(this.createComponents(frag,_5db));
}
}
return _5dd;
};
this.parsePropertySets=function(_5df){
return [];
};
this.parseProperties=function(_5e0){
var _5e1={};
for(var item in _5e0){
if((_5e0[item]==_5e0.tagName)||(_5e0[item]==_5e0.nodeRef)){
}else{
var frag=_5e0[item];
if(frag.tagName&&dojo.widget.tags[frag.tagName.toLowerCase()]){
}else{
if(frag[0]&&frag[0].value!=""&&frag[0].value!=null){
try{
if(item.toLowerCase()=="dataprovider"){
var _5e4=this;
this.getDataProvider(_5e4,frag[0].value);
_5e1.dataProvider=this.dataProvider;
}
_5e1[item]=frag[0].value;
var _5e5=this.parseProperties(frag);
for(var _5e6 in _5e5){
_5e1[_5e6]=_5e5[_5e6];
}
}
catch(e){
dojo.debug(e);
}
}
}
switch(item.toLowerCase()){
case "checked":
case "disabled":
if(typeof _5e1[item]!="boolean"){
_5e1[item]=true;
}
break;
}
}
}
return _5e1;
};
this.getDataProvider=function(_5e7,_5e8){
dojo.io.bind({url:_5e8,load:function(type,_5ea){
if(type=="load"){
_5e7.dataProvider=_5ea;
}
},mimetype:"text/javascript",sync:true});
};
this.getPropertySetById=function(_5eb){
for(var x=0;x<this.propertySetsList.length;x++){
if(_5eb==this.propertySetsList[x]["id"][0].value){
return this.propertySetsList[x];
}
}
return "";
};
this.getPropertySetsByType=function(_5ed){
var _5ee=[];
for(var x=0;x<this.propertySetsList.length;x++){
var cpl=this.propertySetsList[x];
var cpcc=cpl.componentClass||cpl.componentType||null;
var _5f2=this.propertySetsList[x]["id"][0].value;
if(cpcc&&(_5f2==cpcc[0].value)){
_5ee.push(cpl);
}
}
return _5ee;
};
this.getPropertySets=function(_5f3){
var ppl="dojo:propertyproviderlist";
var _5f5=[];
var _5f6=_5f3.tagName;
if(_5f3[ppl]){
var _5f7=_5f3[ppl].value.split(" ");
for(var _5f8 in _5f7){
if((_5f8.indexOf("..")==-1)&&(_5f8.indexOf("://")==-1)){
var _5f9=this.getPropertySetById(_5f8);
if(_5f9!=""){
_5f5.push(_5f9);
}
}else{
}
}
}
return this.getPropertySetsByType(_5f6).concat(_5f5);
};
this.createComponentFromScript=function(_5fa,_5fb,_5fc,ns){
_5fc.fastMixIn=true;
var ltn=(ns||"dojo")+":"+_5fb.toLowerCase();
if(dojo.widget.tags[ltn]){
return [dojo.widget.tags[ltn](_5fc,this,null,null,_5fc)];
}
return [dojo.widget.buildWidgetFromParseTree(ltn,_5fc,this,null,null,_5fc)];
};
};
dojo.widget._parser_collection={"dojo":new dojo.widget.Parse()};
dojo.widget.getParser=function(name){
if(!name){
name="dojo";
}
if(!this._parser_collection[name]){
this._parser_collection[name]=new dojo.widget.Parse();
}
return this._parser_collection[name];
};
dojo.widget.createWidget=function(name,_601,_602,_603){
var _604=false;
var _605=(typeof name=="string");
if(_605){
var pos=name.indexOf(":");
var ns=(pos>-1)?name.substring(0,pos):"dojo";
if(pos>-1){
name=name.substring(pos+1);
}
var _608=name.toLowerCase();
var _609=ns+":"+_608;
_604=(dojo.byId(name)&&!dojo.widget.tags[_609]);
}
if((arguments.length==1)&&(_604||!_605)){
var xp=new dojo.xml.Parse();
var tn=_604?dojo.byId(name):name;
return dojo.widget.getParser().createComponents(xp.parseElement(tn,null,true))[0];
}
function fromScript(_60c,name,_60e,ns){
_60e[_609]={dojotype:[{value:_608}],nodeRef:_60c,fastMixIn:true};
_60e.ns=ns;
return dojo.widget.getParser().createComponentFromScript(_60c,name,_60e,ns);
}
_601=_601||{};
var _610=false;
var tn=null;
var h=dojo.render.html.capable;
if(h){
tn=document.createElement("span");
}
if(!_602){
_610=true;
_602=tn;
if(h){
dojo.body().appendChild(_602);
}
}else{
if(_603){
dojo.dom.insertAtPosition(tn,_602,_603);
}else{
tn=_602;
}
}
var _612=fromScript(tn,name.toLowerCase(),_601,ns);
if((!_612)||(!_612[0])||(typeof _612[0].widgetType=="undefined")){
throw new Error("createWidget: Creation of \""+name+"\" widget failed.");
}
try{
if(_610&&_612[0].domNode.parentNode){
_612[0].domNode.parentNode.removeChild(_612[0].domNode);
}
}
catch(e){
dojo.debug(e);
}
return _612[0];
};
dojo.provide("dojo.widget.DomWidget");
dojo.widget._cssFiles={};
dojo.widget._cssStrings={};
dojo.widget._templateCache={};
dojo.widget.defaultStrings={dojoRoot:dojo.hostenv.getBaseScriptUri(),dojoWidgetModuleUri:dojo.uri.moduleUri("dojo.widget"),baseScriptUri:dojo.hostenv.getBaseScriptUri()};
dojo.widget.fillFromTemplateCache=function(obj,_614,_615,_616){
var _617=_614||obj.templatePath;
var _618=dojo.widget._templateCache;
if(!_617&&!obj["widgetType"]){
do{
var _619="__dummyTemplate__"+dojo.widget._templateCache.dummyCount++;
}while(_618[_619]);
obj.widgetType=_619;
}
var wt=_617?_617.toString():obj.widgetType;
var ts=_618[wt];
if(!ts){
_618[wt]={"string":null,"node":null};
if(_616){
ts={};
}else{
ts=_618[wt];
}
}
if((!obj.templateString)&&(!_616)){
obj.templateString=_615||ts["string"];
}
if(obj.templateString){
obj.templateString=this._sanitizeTemplateString(obj.templateString);
}
if((!obj.templateNode)&&(!_616)){
obj.templateNode=ts["node"];
}
if((!obj.templateNode)&&(!obj.templateString)&&(_617)){
var _61c=this._sanitizeTemplateString(dojo.hostenv.getText(_617));
obj.templateString=_61c;
if(!_616){
_618[wt]["string"]=_61c;
}
}
if((!ts["string"])&&(!_616)){
ts.string=obj.templateString;
}
};
dojo.widget._sanitizeTemplateString=function(_61d){
if(_61d){
_61d=_61d.replace(/^\s*<\?xml(\s)+version=[\'\"](\d)*.(\d)*[\'\"](\s)*\?>/im,"");
var _61e=_61d.match(/<body[^>]*>\s*([\s\S]+)\s*<\/body>/im);
if(_61e){
_61d=_61e[1];
}
}else{
_61d="";
}
return _61d;
};
dojo.widget._templateCache.dummyCount=0;
dojo.widget.attachProperties=["dojoAttachPoint","id"];
dojo.widget.eventAttachProperty="dojoAttachEvent";
dojo.widget.onBuildProperty="dojoOnBuild";
dojo.widget.waiNames=["waiRole","waiState"];
dojo.widget.wai={waiRole:{name:"waiRole","namespace":"http://www.w3.org/TR/xhtml2",alias:"x2",prefix:"wairole:"},waiState:{name:"waiState","namespace":"http://www.w3.org/2005/07/aaa",alias:"aaa",prefix:""},setAttr:function(node,ns,attr,_622){
if(dojo.render.html.ie){
node.setAttribute(this[ns].alias+":"+attr,this[ns].prefix+_622);
}else{
node.setAttributeNS(this[ns]["namespace"],attr,this[ns].prefix+_622);
}
},getAttr:function(node,ns,attr){
if(dojo.render.html.ie){
return node.getAttribute(this[ns].alias+":"+attr);
}else{
return node.getAttributeNS(this[ns]["namespace"],attr);
}
},removeAttr:function(node,ns,attr){
var _629=true;
if(dojo.render.html.ie){
_629=node.removeAttribute(this[ns].alias+":"+attr);
}else{
node.removeAttributeNS(this[ns]["namespace"],attr);
}
return _629;
}};
dojo.widget.attachTemplateNodes=function(_62a,_62b,_62c){
var _62d=dojo.dom.ELEMENT_NODE;
function trim(str){
return str.replace(/^\s+|\s+$/g,"");
}
if(!_62a){
_62a=_62b.domNode;
}
if(_62a.nodeType!=_62d){
return;
}
var _62f=_62a.all||_62a.getElementsByTagName("*");
var _630=_62b;
for(var x=-1;x<_62f.length;x++){
var _632=(x==-1)?_62a:_62f[x];
var _633=[];
if(!_62b.widgetsInTemplate||!_632.getAttribute("dojoType")){
for(var y=0;y<this.attachProperties.length;y++){
var _635=_632.getAttribute(this.attachProperties[y]);
if(_635){
_633=_635.split(";");
for(var z=0;z<_633.length;z++){
if(dojo.lang.isArray(_62b[_633[z]])){
_62b[_633[z]].push(_632);
}else{
_62b[_633[z]]=_632;
}
}
break;
}
}
var _637=_632.getAttribute(this.eventAttachProperty);
if(_637){
var evts=_637.split(";");
for(var y=0;y<evts.length;y++){
if((!evts[y])||(!evts[y].length)){
continue;
}
var _639=null;
var tevt=trim(evts[y]);
if(evts[y].indexOf(":")>=0){
var _63b=tevt.split(":");
tevt=trim(_63b[0]);
_639=trim(_63b[1]);
}
if(!_639){
_639=tevt;
}
var tf=function(){
var ntf=new String(_639);
return function(evt){
if(_630[ntf]){
_630[ntf](dojo.event.browser.fixEvent(evt,this));
}
};
}();
dojo.event.browser.addListener(_632,tevt,tf,false,true);
}
}
for(var y=0;y<_62c.length;y++){
var _63f=_632.getAttribute(_62c[y]);
if((_63f)&&(_63f.length)){
var _639=null;
var _640=_62c[y].substr(4);
_639=trim(_63f);
var _641=[_639];
if(_639.indexOf(";")>=0){
_641=dojo.lang.map(_639.split(";"),trim);
}
for(var z=0;z<_641.length;z++){
if(!_641[z].length){
continue;
}
var tf=function(){
var ntf=new String(_641[z]);
return function(evt){
if(_630[ntf]){
_630[ntf](dojo.event.browser.fixEvent(evt,this));
}
};
}();
dojo.event.browser.addListener(_632,_640,tf,false,true);
}
}
}
}
var _644=_632.getAttribute(this.templateProperty);
if(_644){
_62b[_644]=_632;
}
dojo.lang.forEach(dojo.widget.waiNames,function(name){
var wai=dojo.widget.wai[name];
var val=_632.getAttribute(wai.name);
if(val){
if(val.indexOf("-")==-1){
dojo.widget.wai.setAttr(_632,wai.name,"role",val);
}else{
var _648=val.split("-");
dojo.widget.wai.setAttr(_632,wai.name,_648[0],_648[1]);
}
}
},this);
var _649=_632.getAttribute(this.onBuildProperty);
if(_649){
eval("var node = baseNode; var widget = targetObj; "+_649);
}
}
};
dojo.widget.getDojoEventsFromStr=function(str){
var re=/(dojoOn([a-z]+)(\s?))=/gi;
var evts=str?str.match(re)||[]:[];
var ret=[];
var lem={};
for(var x=0;x<evts.length;x++){
if(evts[x].length<1){
continue;
}
var cm=evts[x].replace(/\s/,"");
cm=(cm.slice(0,cm.length-1));
if(!lem[cm]){
lem[cm]=true;
ret.push(cm);
}
}
return ret;
};
dojo.declare("dojo.widget.DomWidget",dojo.widget.Widget,function(){
if((arguments.length>0)&&(typeof arguments[0]=="object")){
this.create(arguments[0]);
}
},{templateNode:null,templateString:null,templateCssString:null,preventClobber:false,domNode:null,containerNode:null,widgetsInTemplate:false,addChild:function(_651,_652,pos,ref,_655){
if(typeof _655=="undefined"){
_655=this.children.length;
}
this.addWidgetAsDirectChild(_651,_652,pos,ref,_655);
this.registerChild(_651,_655);
return _651;
},addWidgetAsDirectChild:function(_656,_657,pos,ref,_65a){
if((!this.containerNode)&&(!_657)){
this.containerNode=this.domNode;
}
var cn=(_657)?_657:this.containerNode;
if(!pos){
pos="after";
}
if(!ref){
if(!cn){
cn=dojo.body();
}
ref=cn.lastChild;
}
if(!_65a){
_65a=0;
}
_656.domNode.setAttribute("dojoinsertionindex",_65a);
if(!ref){
cn.appendChild(_656.domNode);
}else{
if(pos=="insertAtIndex"){
dojo.dom.insertAtIndex(_656.domNode,ref.parentNode,_65a);
}else{
if((pos=="after")&&(ref===cn.lastChild)){
cn.appendChild(_656.domNode);
}else{
dojo.dom.insertAtPosition(_656.domNode,ref,pos);
}
}
}
},registerChild:function(_65c,_65d){
_65c.dojoInsertionIndex=_65d;
var idx=-1;
for(var i=0;i<this.children.length;i++){
if(this.children[i].dojoInsertionIndex<=_65d){
idx=i;
}
}
this.children.splice(idx+1,0,_65c);
_65c.parent=this;
_65c.addedTo(this,idx+1);
delete dojo.widget.manager.topWidgets[_65c.widgetId];
},removeChild:function(_660){
dojo.dom.removeNode(_660.domNode);
return dojo.widget.DomWidget.superclass.removeChild.call(this,_660);
},getFragNodeRef:function(frag){
if(!frag){
return null;
}
if(!frag[this.getNamespacedType()]){
dojo.raise("Error: no frag for widget type "+this.getNamespacedType()+", id "+this.widgetId+" (maybe a widget has set it's type incorrectly)");
}
return frag[this.getNamespacedType()]["nodeRef"];
},postInitialize:function(args,frag,_664){
var _665=this.getFragNodeRef(frag);
if(_664&&(_664.snarfChildDomOutput||!_665)){
_664.addWidgetAsDirectChild(this,"","insertAtIndex","",args["dojoinsertionindex"],_665);
}else{
if(_665){
if(this.domNode&&(this.domNode!==_665)){
this._sourceNodeRef=dojo.dom.replaceNode(_665,this.domNode);
}
}
}
if(_664){
_664.registerChild(this,args.dojoinsertionindex);
}else{
dojo.widget.manager.topWidgets[this.widgetId]=this;
}
if(this.widgetsInTemplate){
var _666=new dojo.xml.Parse();
var _667;
var _668=this.domNode.getElementsByTagName("*");
for(var i=0;i<_668.length;i++){
if(_668[i].getAttribute("dojoAttachPoint")=="subContainerWidget"){
_667=_668[i];
}
if(_668[i].getAttribute("dojoType")){
_668[i].setAttribute("isSubWidget",true);
}
}
if(this.isContainer&&!this.containerNode){
if(_667){
var src=this.getFragNodeRef(frag);
if(src){
dojo.dom.moveChildren(src,_667);
frag["dojoDontFollow"]=true;
}
}else{
dojo.debug("No subContainerWidget node can be found in template file for widget "+this);
}
}
var _66b=_666.parseElement(this.domNode,null,true);
dojo.widget.getParser().createSubComponents(_66b,this);
var _66c=[];
var _66d=[this];
var w;
while((w=_66d.pop())){
for(var i=0;i<w.children.length;i++){
var _66f=w.children[i];
if(_66f._processedSubWidgets||!_66f.extraArgs["issubwidget"]){
continue;
}
_66c.push(_66f);
if(_66f.isContainer){
_66d.push(_66f);
}
}
}
for(var i=0;i<_66c.length;i++){
var _670=_66c[i];
if(_670._processedSubWidgets){
dojo.debug("This should not happen: widget._processedSubWidgets is already true!");
return;
}
_670._processedSubWidgets=true;
if(_670.extraArgs["dojoattachevent"]){
var evts=_670.extraArgs["dojoattachevent"].split(";");
for(var j=0;j<evts.length;j++){
var _673=null;
var tevt=dojo.string.trim(evts[j]);
if(tevt.indexOf(":")>=0){
var _675=tevt.split(":");
tevt=dojo.string.trim(_675[0]);
_673=dojo.string.trim(_675[1]);
}
if(!_673){
_673=tevt;
}
if(dojo.lang.isFunction(_670[tevt])){
dojo.event.kwConnect({srcObj:_670,srcFunc:tevt,targetObj:this,targetFunc:_673});
}else{
alert(tevt+" is not a function in widget "+_670);
}
}
}
if(_670.extraArgs["dojoattachpoint"]){
this[_670.extraArgs["dojoattachpoint"]]=_670;
}
}
}
if(this.isContainer&&!frag["dojoDontFollow"]){
dojo.widget.getParser().createSubComponents(frag,this);
}
},buildRendering:function(args,frag){
var ts=dojo.widget._templateCache[this.widgetType];
if(args["templatecsspath"]){
args["templateCssPath"]=args["templatecsspath"];
}
var _679=args["templateCssPath"]||this.templateCssPath;
if(_679&&!dojo.widget._cssFiles[_679.toString()]){
if((!this.templateCssString)&&(_679)){
this.templateCssString=dojo.hostenv.getText(_679);
this.templateCssPath=null;
}
dojo.widget._cssFiles[_679.toString()]=true;
}
if((this["templateCssString"])&&(!dojo.widget._cssStrings[this.templateCssString])){
dojo.html.insertCssText(this.templateCssString,null,_679);
dojo.widget._cssStrings[this.templateCssString]=true;
}
if((!this.preventClobber)&&((this.templatePath)||(this.templateNode)||((this["templateString"])&&(this.templateString.length))||((typeof ts!="undefined")&&((ts["string"])||(ts["node"]))))){
this.buildFromTemplate(args,frag);
}else{
this.domNode=this.getFragNodeRef(frag);
}
this.fillInTemplate(args,frag);
},buildFromTemplate:function(args,frag){
var _67c=false;
if(args["templatepath"]){
args["templatePath"]=args["templatepath"];
}
dojo.widget.fillFromTemplateCache(this,args["templatePath"],null,_67c);
var ts=dojo.widget._templateCache[this.templatePath?this.templatePath.toString():this.widgetType];
if((ts)&&(!_67c)){
if(!this.templateString.length){
this.templateString=ts["string"];
}
if(!this.templateNode){
this.templateNode=ts["node"];
}
}
var _67e=false;
var node=null;
var tstr=this.templateString;
if((!this.templateNode)&&(this.templateString)){
_67e=this.templateString.match(/\$\{([^\}]+)\}/g);
if(_67e){
var hash=this.strings||{};
for(var key in dojo.widget.defaultStrings){
if(dojo.lang.isUndefined(hash[key])){
hash[key]=dojo.widget.defaultStrings[key];
}
}
for(var i=0;i<_67e.length;i++){
var key=_67e[i];
key=key.substring(2,key.length-1);
var kval=(key.substring(0,5)=="this.")?dojo.getObject(key.substring(5),false,this):hash[key];
var _685;
if((kval)||(dojo.lang.isString(kval))){
_685=new String((dojo.lang.isFunction(kval))?kval.call(this,key,this.templateString):kval);
while(_685.indexOf("\"")>-1){
_685=_685.replace("\"","&quot;");
}
tstr=tstr.replace(_67e[i],_685);
}
}
}else{
this.templateNode=this.createNodesFromText(this.templateString,true)[0];
if(!_67c){
ts.node=this.templateNode;
}
}
}
if((!this.templateNode)&&(!_67e)){
dojo.debug("DomWidget.buildFromTemplate: could not create template");
return false;
}else{
if(!_67e){
node=this.templateNode.cloneNode(true);
if(!node){
return false;
}
}else{
node=this.createNodesFromText(tstr,true)[0];
}
}
this.domNode=node;
this.attachTemplateNodes();
if(this.isContainer&&this.containerNode){
var src=this.getFragNodeRef(frag);
if(src){
dojo.dom.moveChildren(src,this.containerNode);
}
}
},attachTemplateNodes:function(_687,_688){
if(!_687){
_687=this.domNode;
}
if(!_688){
_688=this;
}
return dojo.widget.attachTemplateNodes(_687,_688,dojo.widget.getDojoEventsFromStr(this.templateString));
},fillInTemplate:function(){
},destroyRendering:function(){
try{
dojo.dom.destroyNode(this.domNode);
delete this.domNode;
}
catch(e){
}
if(this._sourceNodeRef){
try{
dojo.dom.destroyNode(this._sourceNodeRef);
}
catch(e){
}
}
},createNodesFromText:function(){
dojo.unimplemented("dojo.widget.DomWidget.createNodesFromText");
}});
dojo.provide("dojo.html.display");
dojo.html._toggle=function(node,_68a,_68b){
node=dojo.byId(node);
_68b(node,!_68a(node));
return _68a(node);
};
dojo.html.show=function(node){
node=dojo.byId(node);
if(dojo.html.getStyleProperty(node,"display")=="none"){
var _68d=dojo.html.getAttribute("djDisplayCache");
dojo.html.setStyle(node,"display",(_68d||""));
node.removeAttribute("djDisplayCache");
}
};
dojo.html.hide=function(node){
node=dojo.byId(node);
var _68f=dojo.html.getAttribute("djDisplayCache");
if(_68f==null){
var d=dojo.html.getStyleProperty(node,"display");
if(d!="none"){
node.setAttribute("djDisplayCache",d);
}
}
dojo.html.setStyle(node,"display","none");
};
dojo.html.setShowing=function(node,_692){
dojo.html[(_692?"show":"hide")](node);
};
dojo.html.isShowing=function(node){
return (dojo.html.getStyleProperty(node,"display")!="none");
};
dojo.html.toggleShowing=function(node){
return dojo.html._toggle(node,dojo.html.isShowing,dojo.html.setShowing);
};
dojo.html.displayMap={tr:"",td:"",th:"",img:"inline",span:"inline",input:"inline",button:"inline"};
dojo.html.suggestDisplayByTagName=function(node){
node=dojo.byId(node);
if(node&&node.tagName){
var tag=node.tagName.toLowerCase();
return (tag in dojo.html.displayMap?dojo.html.displayMap[tag]:"block");
}
};
dojo.html.setDisplay=function(node,_698){
dojo.html.setStyle(node,"display",((_698 instanceof String||typeof _698=="string")?_698:(_698?dojo.html.suggestDisplayByTagName(node):"none")));
};
dojo.html.isDisplayed=function(node){
return (dojo.html.getComputedStyle(node,"display")!="none");
};
dojo.html.toggleDisplay=function(node){
return dojo.html._toggle(node,dojo.html.isDisplayed,dojo.html.setDisplay);
};
dojo.html.setVisibility=function(node,_69c){
dojo.html.setStyle(node,"visibility",((_69c instanceof String||typeof _69c=="string")?_69c:(_69c?"visible":"hidden")));
};
dojo.html.isVisible=function(node){
return (dojo.html.getComputedStyle(node,"visibility")!="hidden");
};
dojo.html.toggleVisibility=function(node){
return dojo.html._toggle(node,dojo.html.isVisible,dojo.html.setVisibility);
};
dojo.html.setOpacity=function(node,_6a0,_6a1){
node=dojo.byId(node);
var h=dojo.render.html;
if(!_6a1){
if(_6a0>=1){
if(h.ie){
dojo.html.clearOpacity(node);
return;
}else{
_6a0=0.999999;
}
}else{
if(_6a0<0){
_6a0=0;
}
}
}
if(h.ie){
if(node.nodeName.toLowerCase()=="tr"){
var tds=node.getElementsByTagName("td");
for(var x=0;x<tds.length;x++){
tds[x].style.filter="Alpha(Opacity="+_6a0*100+")";
}
}
node.style.filter="Alpha(Opacity="+_6a0*100+")";
}else{
if(h.moz){
node.style.opacity=_6a0;
node.style.MozOpacity=_6a0;
}else{
if(h.safari){
node.style.opacity=_6a0;
node.style.KhtmlOpacity=_6a0;
}else{
node.style.opacity=_6a0;
}
}
}
};
dojo.html.clearOpacity=function(node){
node=dojo.byId(node);
var ns=node.style;
var h=dojo.render.html;
if(h.ie){
try{
if(node.filters&&node.filters.alpha){
ns.filter="";
}
}
catch(e){
}
}else{
if(h.moz){
ns.opacity=1;
ns.MozOpacity=1;
}else{
if(h.safari){
ns.opacity=1;
ns.KhtmlOpacity=1;
}else{
ns.opacity=1;
}
}
}
};
dojo.html.getOpacity=function(node){
node=dojo.byId(node);
var h=dojo.render.html;
if(h.ie){
var opac=(node.filters&&node.filters.alpha&&typeof node.filters.alpha.opacity=="number"?node.filters.alpha.opacity:100)/100;
}else{
var opac=node.style.opacity||node.style.MozOpacity||node.style.KhtmlOpacity||1;
}
return opac>=0.999999?1:Number(opac);
};
dojo.provide("dojo.html.layout");
dojo.html.sumAncestorProperties=function(node,prop){
node=dojo.byId(node);
if(!node){
return 0;
}
var _6ad=0;
while(node){
if(dojo.html.getComputedStyle(node,"position")=="fixed"){
return 0;
}
var val=node[prop];
if(val){
_6ad+=val-0;
if(node==dojo.body()){
break;
}
}
node=node.parentNode;
}
return _6ad;
};
dojo.html.setStyleAttributes=function(node,_6b0){
node=dojo.byId(node);
var _6b1=_6b0.replace(/(;)?\s*$/,"").split(";");
for(var i=0;i<_6b1.length;i++){
var _6b3=_6b1[i].split(":");
var name=_6b3[0].replace(/\s*$/,"").replace(/^\s*/,"").toLowerCase();
var _6b5=_6b3[1].replace(/\s*$/,"").replace(/^\s*/,"");
switch(name){
case "opacity":
dojo.html.setOpacity(node,_6b5);
break;
case "content-height":
dojo.html.setContentBox(node,{height:_6b5});
break;
case "content-width":
dojo.html.setContentBox(node,{width:_6b5});
break;
case "outer-height":
dojo.html.setMarginBox(node,{height:_6b5});
break;
case "outer-width":
dojo.html.setMarginBox(node,{width:_6b5});
break;
default:
node.style[dojo.html.toCamelCase(name)]=_6b5;
}
}
};
dojo.html.boxSizing={MARGIN_BOX:"margin-box",BORDER_BOX:"border-box",PADDING_BOX:"padding-box",CONTENT_BOX:"content-box"};
dojo.html.getAbsolutePosition=dojo.html.abs=function(node,_6b7,_6b8){
node=dojo.byId(node);
var _6b9=dojo.doc();
var ret={x:0,y:0};
var bs=dojo.html.boxSizing;
if(!_6b8){
_6b8=bs.CONTENT_BOX;
}
var _6bc=2;
var _6bd;
switch(_6b8){
case bs.MARGIN_BOX:
_6bd=3;
break;
case bs.BORDER_BOX:
_6bd=2;
break;
case bs.PADDING_BOX:
default:
_6bd=1;
break;
case bs.CONTENT_BOX:
_6bd=0;
break;
}
var h=dojo.render.html;
var db=_6b9["body"]||_6b9["documentElement"];
if(h.ie){
with(node.getBoundingClientRect()){
ret.x=left-2;
ret.y=top-2;
}
}else{
if(_6b9["getBoxObjectFor"]){
_6bc=1;
try{
var bo=_6b9.getBoxObjectFor(node);
ret.x=bo.x-dojo.html.sumAncestorProperties(node,"scrollLeft");
ret.y=bo.y-dojo.html.sumAncestorProperties(node,"scrollTop");
}
catch(e){
}
}else{
if(node["offsetParent"]){
var _6c1;
if((h.safari)&&(node.style.getPropertyValue("position")=="absolute")&&(node.parentNode==db)){
_6c1=db;
}else{
_6c1=db.parentNode;
}
if(node.parentNode!=db){
var nd=node;
if(dojo.render.html.opera){
nd=db;
}
ret.x-=dojo.html.sumAncestorProperties(nd,"scrollLeft");
ret.y-=dojo.html.sumAncestorProperties(nd,"scrollTop");
}
var _6c3=node;
do{
var n=_6c3["offsetLeft"];
if(!h.opera||n>0){
ret.x+=isNaN(n)?0:n;
}
var m=_6c3["offsetTop"];
ret.y+=isNaN(m)?0:m;
_6c3=_6c3.offsetParent;
}while((_6c3!=_6c1)&&(_6c3!=null));
}else{
if(node["x"]&&node["y"]){
ret.x+=isNaN(node.x)?0:node.x;
ret.y+=isNaN(node.y)?0:node.y;
}
}
}
}
if(_6b7){
var _6c6=dojo.html.getScroll();
ret.y+=_6c6.top;
ret.x+=_6c6.left;
}
var _6c7=[dojo.html.getPaddingExtent,dojo.html.getBorderExtent,dojo.html.getMarginExtent];
if(_6bc>_6bd){
for(var i=_6bd;i<_6bc;++i){
ret.y+=_6c7[i](node,"top");
ret.x+=_6c7[i](node,"left");
}
}else{
if(_6bc<_6bd){
for(var i=_6bd;i>_6bc;--i){
ret.y-=_6c7[i-1](node,"top");
ret.x-=_6c7[i-1](node,"left");
}
}
}
ret.top=ret.y;
ret.left=ret.x;
return ret;
};
dojo.html.isPositionAbsolute=function(node){
return (dojo.html.getComputedStyle(node,"position")=="absolute");
};
dojo.html._getComponentPixelValues=function(node,_6cb,_6cc,_6cd){
var _6ce=["top","bottom","left","right"];
var obj={};
for(var i in _6ce){
side=_6ce[i];
obj[side]=_6cc(node,_6cb+side,_6cd);
}
obj.width=obj.left+obj.right;
obj.height=obj.top+obj.bottom;
return obj;
};
dojo.html.getMargin=function(node){
return dojo.html._getComponentPixelValues(node,"margin-",dojo.html.getPixelValue,dojo.html.isPositionAbsolute(node));
};
dojo.html.getBorder=function(node){
return dojo.html._getComponentPixelValues(node,"",dojo.html.getBorderExtent);
};
dojo.html.getBorderExtent=function(node,side){
return (dojo.html.getStyle(node,"border-"+side+"-style")=="none"?0:dojo.html.getPixelValue(node,"border-"+side+"-width"));
};
dojo.html.getMarginExtent=function(node,side){
return dojo.html.getPixelValue(node,"margin-"+side,dojo.html.isPositionAbsolute(node));
};
dojo.html.getPaddingExtent=function(node,side){
return dojo.html.getPixelValue(node,"padding-"+side,true);
};
dojo.html.getPadding=function(node){
return dojo.html._getComponentPixelValues(node,"padding-",dojo.html.getPixelValue,true);
};
dojo.html.getPadBorder=function(node){
var pad=dojo.html.getPadding(node);
var _6dc=dojo.html.getBorder(node);
return {width:pad.width+_6dc.width,height:pad.height+_6dc.height};
};
dojo.html.getBoxSizing=function(node){
var h=dojo.render.html;
var bs=dojo.html.boxSizing;
if(((h.ie)||(h.opera))&&node.nodeName.toLowerCase()!="img"){
var cm=document["compatMode"];
if((cm=="BackCompat")||(cm=="QuirksMode")){
return bs.BORDER_BOX;
}else{
return bs.CONTENT_BOX;
}
}else{
if(arguments.length==0){
node=document.documentElement;
}
var _6e1;
if(!h.ie){
_6e1=dojo.html.getStyle(node,"-moz-box-sizing");
if(!_6e1){
_6e1=dojo.html.getStyle(node,"box-sizing");
}
}
return (_6e1?_6e1:bs.CONTENT_BOX);
}
};
dojo.html.isBorderBox=function(node){
return (dojo.html.getBoxSizing(node)==dojo.html.boxSizing.BORDER_BOX);
};
dojo.html.getBorderBox=function(node){
node=dojo.byId(node);
return {width:node.offsetWidth,height:node.offsetHeight};
};
dojo.html.getPaddingBox=function(node){
var box=dojo.html.getBorderBox(node);
var _6e6=dojo.html.getBorder(node);
return {width:box.width-_6e6.width,height:box.height-_6e6.height};
};
dojo.html.getContentBox=function(node){
node=dojo.byId(node);
var _6e8=dojo.html.getPadBorder(node);
return {width:node.offsetWidth-_6e8.width,height:node.offsetHeight-_6e8.height};
};
dojo.html.setContentBox=function(node,args){
node=dojo.byId(node);
var _6eb=0;
var _6ec=0;
var isbb=dojo.html.isBorderBox(node);
var _6ee=(isbb?dojo.html.getPadBorder(node):{width:0,height:0});
var ret={};
if(typeof args.width!="undefined"){
_6eb=args.width+_6ee.width;
ret.width=dojo.html.setPositivePixelValue(node,"width",_6eb);
}
if(typeof args.height!="undefined"){
_6ec=args.height+_6ee.height;
ret.height=dojo.html.setPositivePixelValue(node,"height",_6ec);
}
return ret;
};
dojo.html.getMarginBox=function(node){
var _6f1=dojo.html.getBorderBox(node);
var _6f2=dojo.html.getMargin(node);
return {width:_6f1.width+_6f2.width,height:_6f1.height+_6f2.height};
};
dojo.html.setMarginBox=function(node,args){
node=dojo.byId(node);
var _6f5=0;
var _6f6=0;
var isbb=dojo.html.isBorderBox(node);
var _6f8=(!isbb?dojo.html.getPadBorder(node):{width:0,height:0});
var _6f9=dojo.html.getMargin(node);
var ret={};
if(typeof args.width!="undefined"){
_6f5=args.width-_6f8.width;
_6f5-=_6f9.width;
ret.width=dojo.html.setPositivePixelValue(node,"width",_6f5);
}
if(typeof args.height!="undefined"){
_6f6=args.height-_6f8.height;
_6f6-=_6f9.height;
ret.height=dojo.html.setPositivePixelValue(node,"height",_6f6);
}
return ret;
};
dojo.html.getElementBox=function(node,type){
var bs=dojo.html.boxSizing;
switch(type){
case bs.MARGIN_BOX:
return dojo.html.getMarginBox(node);
case bs.BORDER_BOX:
return dojo.html.getBorderBox(node);
case bs.PADDING_BOX:
return dojo.html.getPaddingBox(node);
case bs.CONTENT_BOX:
default:
return dojo.html.getContentBox(node);
}
};
dojo.html.toCoordinateObject=dojo.html.toCoordinateArray=function(_6fe,_6ff,_700){
if(!_6fe.nodeType&&!(_6fe instanceof String||typeof _6fe=="string")&&("width" in _6fe||"height" in _6fe||"left" in _6fe||"x" in _6fe||"top" in _6fe||"y" in _6fe)){
var ret={left:_6fe.left||_6fe.x||0,top:_6fe.top||_6fe.y||0,width:_6fe.width||0,height:_6fe.height||0};
}else{
var node=dojo.byId(_6fe);
var pos=dojo.html.abs(node,_6ff,_700);
var _704=dojo.html.getMarginBox(node);
var ret={left:pos.left,top:pos.top,width:_704.width,height:_704.height};
}
ret.x=ret.left;
ret.y=ret.top;
return ret;
};
dojo.html.setMarginBoxWidth=dojo.html.setOuterWidth=function(node,_706){
return dojo.html._callDeprecated("setMarginBoxWidth","setMarginBox",arguments,"width");
};
dojo.html.setMarginBoxHeight=dojo.html.setOuterHeight=function(){
return dojo.html._callDeprecated("setMarginBoxHeight","setMarginBox",arguments,"height");
};
dojo.html.getMarginBoxWidth=dojo.html.getOuterWidth=function(){
return dojo.html._callDeprecated("getMarginBoxWidth","getMarginBox",arguments,null,"width");
};
dojo.html.getMarginBoxHeight=dojo.html.getOuterHeight=function(){
return dojo.html._callDeprecated("getMarginBoxHeight","getMarginBox",arguments,null,"height");
};
dojo.html.getTotalOffset=function(node,type,_709){
return dojo.html._callDeprecated("getTotalOffset","getAbsolutePosition",arguments,null,type);
};
dojo.html.getAbsoluteX=function(node,_70b){
return dojo.html._callDeprecated("getAbsoluteX","getAbsolutePosition",arguments,null,"x");
};
dojo.html.getAbsoluteY=function(node,_70d){
return dojo.html._callDeprecated("getAbsoluteY","getAbsolutePosition",arguments,null,"y");
};
dojo.html.totalOffsetLeft=function(node,_70f){
return dojo.html._callDeprecated("totalOffsetLeft","getAbsolutePosition",arguments,null,"left");
};
dojo.html.totalOffsetTop=function(node,_711){
return dojo.html._callDeprecated("totalOffsetTop","getAbsolutePosition",arguments,null,"top");
};
dojo.html.getMarginWidth=function(node){
return dojo.html._callDeprecated("getMarginWidth","getMargin",arguments,null,"width");
};
dojo.html.getMarginHeight=function(node){
return dojo.html._callDeprecated("getMarginHeight","getMargin",arguments,null,"height");
};
dojo.html.getBorderWidth=function(node){
return dojo.html._callDeprecated("getBorderWidth","getBorder",arguments,null,"width");
};
dojo.html.getBorderHeight=function(node){
return dojo.html._callDeprecated("getBorderHeight","getBorder",arguments,null,"height");
};
dojo.html.getPaddingWidth=function(node){
return dojo.html._callDeprecated("getPaddingWidth","getPadding",arguments,null,"width");
};
dojo.html.getPaddingHeight=function(node){
return dojo.html._callDeprecated("getPaddingHeight","getPadding",arguments,null,"height");
};
dojo.html.getPadBorderWidth=function(node){
return dojo.html._callDeprecated("getPadBorderWidth","getPadBorder",arguments,null,"width");
};
dojo.html.getPadBorderHeight=function(node){
return dojo.html._callDeprecated("getPadBorderHeight","getPadBorder",arguments,null,"height");
};
dojo.html.getBorderBoxWidth=dojo.html.getInnerWidth=function(){
return dojo.html._callDeprecated("getBorderBoxWidth","getBorderBox",arguments,null,"width");
};
dojo.html.getBorderBoxHeight=dojo.html.getInnerHeight=function(){
return dojo.html._callDeprecated("getBorderBoxHeight","getBorderBox",arguments,null,"height");
};
dojo.html.getContentBoxWidth=dojo.html.getContentWidth=function(){
return dojo.html._callDeprecated("getContentBoxWidth","getContentBox",arguments,null,"width");
};
dojo.html.getContentBoxHeight=dojo.html.getContentHeight=function(){
return dojo.html._callDeprecated("getContentBoxHeight","getContentBox",arguments,null,"height");
};
dojo.html.setContentBoxWidth=dojo.html.setContentWidth=function(node,_71b){
return dojo.html._callDeprecated("setContentBoxWidth","setContentBox",arguments,"width");
};
dojo.html.setContentBoxHeight=dojo.html.setContentHeight=function(node,_71d){
return dojo.html._callDeprecated("setContentBoxHeight","setContentBox",arguments,"height");
};
dojo.provide("dojo.html.util");
dojo.html.getElementWindow=function(_71e){
return dojo.html.getDocumentWindow(_71e.ownerDocument);
};
dojo.html.getDocumentWindow=function(doc){
if(dojo.render.html.safari&&!doc._parentWindow){
var fix=function(win){
win.document._parentWindow=win;
for(var i=0;i<win.frames.length;i++){
fix(win.frames[i]);
}
};
fix(window.top);
}
if(dojo.render.html.ie&&window!==document.parentWindow&&!doc._parentWindow){
doc.parentWindow.execScript("document._parentWindow = window;","Javascript");
var win=doc._parentWindow;
doc._parentWindow=null;
return win;
}
return doc._parentWindow||doc.parentWindow||doc.defaultView;
};
dojo.html.getAbsolutePositionExt=function(node,_725,_726,_727){
var _728=dojo.html.getElementWindow(node);
var ret=dojo.withGlobal(_728,"getAbsolutePosition",dojo.html,arguments);
var win=dojo.html.getElementWindow(node);
if(_727!=win&&win.frameElement){
var ext=dojo.html.getAbsolutePositionExt(win.frameElement,_725,_726,_727);
ret.x+=ext.x;
ret.y+=ext.y;
}
ret.top=ret.y;
ret.left=ret.x;
return ret;
};
dojo.html.gravity=function(node,e){
node=dojo.byId(node);
var _72e=dojo.html.getCursorPosition(e);
with(dojo.html){
var _72f=getAbsolutePosition(node,true);
var bb=getBorderBox(node);
var _731=_72f.x+(bb.width/2);
var _732=_72f.y+(bb.height/2);
}
with(dojo.html.gravity){
return ((_72e.x<_731?WEST:EAST)|(_72e.y<_732?NORTH:SOUTH));
}
};
dojo.html.gravity.NORTH=1;
dojo.html.gravity.SOUTH=1<<1;
dojo.html.gravity.EAST=1<<2;
dojo.html.gravity.WEST=1<<3;
dojo.html.overElement=function(_733,e){
_733=dojo.byId(_733);
var _735=dojo.html.getCursorPosition(e);
var bb=dojo.html.getBorderBox(_733);
var _737=dojo.html.getAbsolutePosition(_733,true,dojo.html.boxSizing.BORDER_BOX);
var top=_737.y;
var _739=top+bb.height;
var left=_737.x;
var _73b=left+bb.width;
return (_735.x>=left&&_735.x<=_73b&&_735.y>=top&&_735.y<=_739);
};
dojo.html.renderedTextContent=function(node){
node=dojo.byId(node);
var _73d="";
if(node==null){
return _73d;
}
for(var i=0;i<node.childNodes.length;i++){
switch(node.childNodes[i].nodeType){
case 1:
case 5:
var _73f="unknown";
try{
_73f=dojo.html.getStyle(node.childNodes[i],"display");
}
catch(E){
}
switch(_73f){
case "block":
case "list-item":
case "run-in":
case "table":
case "table-row-group":
case "table-header-group":
case "table-footer-group":
case "table-row":
case "table-column-group":
case "table-column":
case "table-cell":
case "table-caption":
_73d+="\n";
_73d+=dojo.html.renderedTextContent(node.childNodes[i]);
_73d+="\n";
break;
case "none":
break;
default:
if(node.childNodes[i].tagName&&node.childNodes[i].tagName.toLowerCase()=="br"){
_73d+="\n";
}else{
_73d+=dojo.html.renderedTextContent(node.childNodes[i]);
}
break;
}
break;
case 3:
case 2:
case 4:
var text=node.childNodes[i].nodeValue;
var _741="unknown";
try{
_741=dojo.html.getStyle(node,"text-transform");
}
catch(E){
}
switch(_741){
case "capitalize":
var _742=text.split(" ");
for(var i=0;i<_742.length;i++){
_742[i]=_742[i].charAt(0).toUpperCase()+_742[i].substring(1);
}
text=_742.join(" ");
break;
case "uppercase":
text=text.toUpperCase();
break;
case "lowercase":
text=text.toLowerCase();
break;
default:
break;
}
switch(_741){
case "nowrap":
break;
case "pre-wrap":
break;
case "pre-line":
break;
case "pre":
break;
default:
text=text.replace(/\s+/," ");
if(/\s$/.test(_73d)){
text.replace(/^\s/,"");
}
break;
}
_73d+=text;
break;
default:
break;
}
}
return _73d;
};
dojo.html.createNodesFromText=function(txt,trim){
if(trim){
txt=txt.replace(/^\s+|\s+$/g,"");
}
var tn=dojo.doc().createElement("div");
tn.style.visibility="hidden";
dojo.body().appendChild(tn);
var _746="none";
if((/^<t[dh][\s\r\n>]/i).test(txt.replace(/^\s+/))){
txt="<table><tbody><tr>"+txt+"</tr></tbody></table>";
_746="cell";
}else{
if((/^<tr[\s\r\n>]/i).test(txt.replace(/^\s+/))){
txt="<table><tbody>"+txt+"</tbody></table>";
_746="row";
}else{
if((/^<(thead|tbody|tfoot)[\s\r\n>]/i).test(txt.replace(/^\s+/))){
txt="<table>"+txt+"</table>";
_746="section";
}
}
}
tn.innerHTML=txt;
if(tn["normalize"]){
tn.normalize();
}
var _747=null;
switch(_746){
case "cell":
_747=tn.getElementsByTagName("tr")[0];
break;
case "row":
_747=tn.getElementsByTagName("tbody")[0];
break;
case "section":
_747=tn.getElementsByTagName("table")[0];
break;
default:
_747=tn;
break;
}
var _748=[];
for(var x=0;x<_747.childNodes.length;x++){
_748.push(_747.childNodes[x].cloneNode(true));
}
tn.style.display="none";
dojo.html.destroyNode(tn);
return _748;
};
dojo.html.placeOnScreen=function(node,_74b,_74c,_74d,_74e,_74f,_750){
if(_74b instanceof Array||typeof _74b=="array"){
_750=_74f;
_74f=_74e;
_74e=_74d;
_74d=_74c;
_74c=_74b[1];
_74b=_74b[0];
}
if(_74f instanceof String||typeof _74f=="string"){
_74f=_74f.split(",");
}
if(!isNaN(_74d)){
_74d=[Number(_74d),Number(_74d)];
}else{
if(!(_74d instanceof Array||typeof _74d=="array")){
_74d=[0,0];
}
}
var _751=dojo.html.getScroll().offset;
var view=dojo.html.getViewport();
node=dojo.byId(node);
var _753=node.style.display;
node.style.display="";
var bb=dojo.html.getBorderBox(node);
var w=bb.width;
var h=bb.height;
node.style.display=_753;
if(!(_74f instanceof Array||typeof _74f=="array")){
_74f=["TL"];
}
var _757,_758,_759=Infinity,_75a;
for(var _75b=0;_75b<_74f.length;++_75b){
var _75c=_74f[_75b];
var _75d=true;
var tryX=_74b-(_75c.charAt(1)=="L"?0:w)+_74d[0]*(_75c.charAt(1)=="L"?1:-1);
var tryY=_74c-(_75c.charAt(0)=="T"?0:h)+_74d[1]*(_75c.charAt(0)=="T"?1:-1);
if(_74e){
tryX-=_751.x;
tryY-=_751.y;
}
if(tryX<0){
tryX=0;
_75d=false;
}
if(tryY<0){
tryY=0;
_75d=false;
}
var x=tryX+w;
if(x>view.width){
x=view.width-w;
_75d=false;
}else{
x=tryX;
}
x=Math.max(_74d[0],x)+_751.x;
var y=tryY+h;
if(y>view.height){
y=view.height-h;
_75d=false;
}else{
y=tryY;
}
y=Math.max(_74d[1],y)+_751.y;
if(_75d){
_757=x;
_758=y;
_759=0;
_75a=_75c;
break;
}else{
var dist=Math.pow(x-tryX-_751.x,2)+Math.pow(y-tryY-_751.y,2);
if(_759>dist){
_759=dist;
_757=x;
_758=y;
_75a=_75c;
}
}
}
if(!_750){
node.style.left=_757+"px";
node.style.top=_758+"px";
}
return {left:_757,top:_758,x:_757,y:_758,dist:_759,corner:_75a};
};
dojo.html.placeOnScreenAroundElement=function(node,_764,_765,_766,_767,_768){
var best,_76a=Infinity;
_764=dojo.byId(_764);
var _76b=_764.style.display;
_764.style.display="";
var mb=dojo.html.getElementBox(_764,_766);
var _76d=mb.width;
var _76e=mb.height;
var _76f=dojo.html.getAbsolutePosition(_764,true,_766);
_764.style.display=_76b;
for(var _770 in _767){
var pos,_772,_773;
var _774=_767[_770];
_772=_76f.x+(_770.charAt(1)=="L"?0:_76d);
_773=_76f.y+(_770.charAt(0)=="T"?0:_76e);
pos=dojo.html.placeOnScreen(node,_772,_773,_765,true,_774,true);
if(pos.dist==0){
best=pos;
break;
}else{
if(_76a>pos.dist){
_76a=pos.dist;
best=pos;
}
}
}
if(!_768){
node.style.left=best.left+"px";
node.style.top=best.top+"px";
}
return best;
};
dojo.html.scrollIntoView=function(node){
if(!node){
return;
}
if(dojo.render.html.ie){
if(dojo.html.getBorderBox(node.parentNode).height<=node.parentNode.scrollHeight){
node.scrollIntoView(false);
}
}else{
if(dojo.render.html.mozilla){
node.scrollIntoView(false);
}else{
var _776=node.parentNode;
var _777=_776.scrollTop+dojo.html.getBorderBox(_776).height;
var _778=node.offsetTop+dojo.html.getMarginBox(node).height;
if(_777<_778){
_776.scrollTop+=(_778-_777);
}else{
if(_776.scrollTop>node.offsetTop){
_776.scrollTop-=(_776.scrollTop-node.offsetTop);
}
}
}
}
};
dojo.html.isLeftToRight=function(node){
for(;node;node=node.parentNode){
if(node.dir){
return node.dir=="ltr";
}
}
return true;
};
dojo.provide("dojo.gfx.color");
dojo.gfx.color.Color=function(r,g,b,a){
if(dojo.lang.isArray(r)){
this.r=r[0];
this.g=r[1];
this.b=r[2];
this.a=r[3]||1;
}else{
if(dojo.lang.isString(r)){
var rgb=dojo.gfx.color.extractRGB(r);
this.r=rgb[0];
this.g=rgb[1];
this.b=rgb[2];
this.a=g||1;
}else{
if(r instanceof dojo.gfx.color.Color){
this.r=r.r;
this.b=r.b;
this.g=r.g;
this.a=r.a;
}else{
this.r=r;
this.g=g;
this.b=b;
this.a=a;
}
}
}
};
dojo.gfx.color.Color.fromArray=function(arr){
return new dojo.gfx.color.Color(arr[0],arr[1],arr[2],arr[3]);
};
dojo.extend(dojo.gfx.color.Color,{toRgb:function(_780){
if(_780){
return this.toRgba();
}else{
return [this.r,this.g,this.b];
}
},toRgba:function(){
return [this.r,this.g,this.b,this.a];
},toHex:function(){
return dojo.gfx.color.rgb2hex(this.toRgb());
},toCss:function(){
return "rgb("+this.toRgb().join()+")";
},toString:function(){
return this.toHex();
},blend:function(_781,_782){
var rgb=null;
if(dojo.lang.isArray(_781)){
rgb=_781;
}else{
if(_781 instanceof dojo.gfx.color.Color){
rgb=_781.toRgb();
}else{
rgb=new dojo.gfx.color.Color(_781).toRgb();
}
}
return dojo.gfx.color.blend(this.toRgb(),rgb,_782);
}});
dojo.gfx.color.named={white:[255,255,255],black:[0,0,0],red:[255,0,0],green:[0,255,0],lime:[0,255,0],blue:[0,0,255],navy:[0,0,128],gray:[128,128,128],silver:[192,192,192]};
dojo.gfx.color.blend=function(a,b,_786){
if(typeof a=="string"){
return dojo.gfx.color.blendHex(a,b,_786);
}
if(!_786){
_786=0;
}
_786=Math.min(Math.max(-1,_786),1);
_786=((_786+1)/2);
var c=[];
for(var x=0;x<3;x++){
c[x]=parseInt(b[x]+((a[x]-b[x])*_786));
}
return c;
};
dojo.gfx.color.blendHex=function(a,b,_78b){
return dojo.gfx.color.rgb2hex(dojo.gfx.color.blend(dojo.gfx.color.hex2rgb(a),dojo.gfx.color.hex2rgb(b),_78b));
};
dojo.gfx.color.extractRGB=function(_78c){
_78c=_78c.toLowerCase();
if(_78c.indexOf("rgb")==0){
var _78d=_78c.match(/rgba*\((\d+), *(\d+), *(\d+)/i);
var ret=_78d.splice(1,3);
return ret;
}else{
var _78f=dojo.gfx.color.hex2rgb(_78c);
if(_78f){
return _78f;
}else{
return dojo.gfx.color.named[_78c]||[255,255,255];
}
}
};
dojo.gfx.color.hex2rgb=function(hex){
var _791="0123456789ABCDEF";
var rgb=new Array(3);
if(hex.indexOf("#")==0){
hex=hex.substring(1);
}
hex=hex.toUpperCase();
if(hex.replace(new RegExp("["+_791+"]","g"),"")!=""){
return null;
}
if(hex.length==3){
rgb[0]=hex.charAt(0)+hex.charAt(0);
rgb[1]=hex.charAt(1)+hex.charAt(1);
rgb[2]=hex.charAt(2)+hex.charAt(2);
}else{
rgb[0]=hex.substring(0,2);
rgb[1]=hex.substring(2,4);
rgb[2]=hex.substring(4);
}
for(var i=0;i<rgb.length;i++){
rgb[i]=_791.indexOf(rgb[i].charAt(0))*16+_791.indexOf(rgb[i].charAt(1));
}
return rgb;
};
dojo.gfx.color.rgb2hex=function(r,g,b){
if(dojo.lang.isArray(r)){
g=r[1]||0;
b=r[2]||0;
r=r[0]||0;
}
var ret=dojo.lang.map([r,g,b],function(x){
x=new Number(x);
var s=x.toString(16);
while(s.length<2){
s="0"+s;
}
return s;
});
ret.unshift("#");
return ret.join("");
};
dojo.provide("dojo.lfx.Animation");
dojo.lfx.Line=function(_79a,end){
this.start=_79a;
this.end=end;
if(dojo.lang.isArray(_79a)){
var diff=[];
dojo.lang.forEach(this.start,function(s,i){
diff[i]=this.end[i]-s;
},this);
this.getValue=function(n){
var res=[];
dojo.lang.forEach(this.start,function(s,i){
res[i]=(diff[i]*n)+s;
},this);
return res;
};
}else{
var diff=end-_79a;
this.getValue=function(n){
return (diff*n)+this.start;
};
}
};
if((dojo.render.html.khtml)&&(!dojo.render.html.safari)){
dojo.lfx.easeDefault=function(n){
return (parseFloat("0.5")+((Math.sin((n+parseFloat("1.5"))*Math.PI))/2));
};
}else{
dojo.lfx.easeDefault=function(n){
return (0.5+((Math.sin((n+1.5)*Math.PI))/2));
};
}
dojo.lfx.easeIn=function(n){
return Math.pow(n,3);
};
dojo.lfx.easeOut=function(n){
return (1-Math.pow(1-n,3));
};
dojo.lfx.easeInOut=function(n){
return ((3*Math.pow(n,2))-(2*Math.pow(n,3)));
};
dojo.lfx.IAnimation=function(){
};
dojo.lang.extend(dojo.lfx.IAnimation,{curve:null,duration:1000,easing:null,repeatCount:0,rate:10,handler:null,beforeBegin:null,onBegin:null,onAnimate:null,onEnd:null,onPlay:null,onPause:null,onStop:null,play:null,pause:null,stop:null,connect:function(evt,_7aa,_7ab){
if(!_7ab){
_7ab=_7aa;
_7aa=this;
}
_7ab=dojo.lang.hitch(_7aa,_7ab);
var _7ac=this[evt]||function(){
};
this[evt]=function(){
var ret=_7ac.apply(this,arguments);
_7ab.apply(this,arguments);
return ret;
};
return this;
},fire:function(evt,args){
if(this[evt]){
this[evt].apply(this,(args||[]));
}
return this;
},repeat:function(_7b0){
this.repeatCount=_7b0;
return this;
},_active:false,_paused:false});
dojo.lfx.Animation=function(_7b1,_7b2,_7b3,_7b4,_7b5,rate){
dojo.lfx.IAnimation.call(this);
if(dojo.lang.isNumber(_7b1)||(!_7b1&&_7b2.getValue)){
rate=_7b5;
_7b5=_7b4;
_7b4=_7b3;
_7b3=_7b2;
_7b2=_7b1;
_7b1=null;
}else{
if(_7b1.getValue||dojo.lang.isArray(_7b1)){
rate=_7b4;
_7b5=_7b3;
_7b4=_7b2;
_7b3=_7b1;
_7b2=null;
_7b1=null;
}
}
if(dojo.lang.isArray(_7b3)){
this.curve=new dojo.lfx.Line(_7b3[0],_7b3[1]);
}else{
this.curve=_7b3;
}
if(_7b2!=null&&_7b2>0){
this.duration=_7b2;
}
if(_7b5){
this.repeatCount=_7b5;
}
if(rate){
this.rate=rate;
}
if(_7b1){
dojo.lang.forEach(["handler","beforeBegin","onBegin","onEnd","onPlay","onStop","onAnimate"],function(item){
if(_7b1[item]){
this.connect(item,_7b1[item]);
}
},this);
}
if(_7b4&&dojo.lang.isFunction(_7b4)){
this.easing=_7b4;
}
};
dojo.inherits(dojo.lfx.Animation,dojo.lfx.IAnimation);
dojo.lang.extend(dojo.lfx.Animation,{_startTime:null,_endTime:null,_timer:null,_percent:0,_startRepeatCount:0,play:function(_7b8,_7b9){
if(_7b9){
clearTimeout(this._timer);
this._active=false;
this._paused=false;
this._percent=0;
}else{
if(this._active&&!this._paused){
return this;
}
}
this.fire("handler",["beforeBegin"]);
this.fire("beforeBegin");
if(_7b8>0){
setTimeout(dojo.lang.hitch(this,function(){
this.play(null,_7b9);
}),_7b8);
return this;
}
this._startTime=new Date().valueOf();
if(this._paused){
this._startTime-=(this.duration*this._percent/100);
}
this._endTime=this._startTime+this.duration;
this._active=true;
this._paused=false;
var step=this._percent/100;
var _7bb=this.curve.getValue(step);
if(this._percent==0){
if(!this._startRepeatCount){
this._startRepeatCount=this.repeatCount;
}
this.fire("handler",["begin",_7bb]);
this.fire("onBegin",[_7bb]);
}
this.fire("handler",["play",_7bb]);
this.fire("onPlay",[_7bb]);
this._cycle();
return this;
},pause:function(){
clearTimeout(this._timer);
if(!this._active){
return this;
}
this._paused=true;
var _7bc=this.curve.getValue(this._percent/100);
this.fire("handler",["pause",_7bc]);
this.fire("onPause",[_7bc]);
return this;
},gotoPercent:function(pct,_7be){
clearTimeout(this._timer);
this._active=true;
this._paused=true;
this._percent=pct;
if(_7be){
this.play();
}
return this;
},stop:function(_7bf){
clearTimeout(this._timer);
var step=this._percent/100;
if(_7bf){
step=1;
}
var _7c1=this.curve.getValue(step);
this.fire("handler",["stop",_7c1]);
this.fire("onStop",[_7c1]);
this._active=false;
this._paused=false;
return this;
},status:function(){
if(this._active){
return this._paused?"paused":"playing";
}else{
return "stopped";
}
return this;
},_cycle:function(){
clearTimeout(this._timer);
if(this._active){
var curr=new Date().valueOf();
var step=(curr-this._startTime)/(this._endTime-this._startTime);
if(step>=1){
step=1;
this._percent=100;
}else{
this._percent=step*100;
}
if((this.easing)&&(dojo.lang.isFunction(this.easing))){
step=this.easing(step);
}
var _7c4=this.curve.getValue(step);
this.fire("handler",["animate",_7c4]);
this.fire("onAnimate",[_7c4]);
if(step<1){
this._timer=setTimeout(dojo.lang.hitch(this,"_cycle"),this.rate);
}else{
this._active=false;
this.fire("handler",["end"]);
this.fire("onEnd");
if(this.repeatCount>0){
this.repeatCount--;
this.play(null,true);
}else{
if(this.repeatCount==-1){
this.play(null,true);
}else{
if(this._startRepeatCount){
this.repeatCount=this._startRepeatCount;
this._startRepeatCount=0;
}
}
}
}
}
return this;
}});
dojo.lfx.Combine=function(_7c5){
dojo.lfx.IAnimation.call(this);
this._anims=[];
this._animsEnded=0;
var _7c6=arguments;
if(_7c6.length==1&&(dojo.lang.isArray(_7c6[0])||dojo.lang.isArrayLike(_7c6[0]))){
_7c6=_7c6[0];
}
dojo.lang.forEach(_7c6,function(anim){
this._anims.push(anim);
anim.connect("onEnd",dojo.lang.hitch(this,"_onAnimsEnded"));
},this);
};
dojo.inherits(dojo.lfx.Combine,dojo.lfx.IAnimation);
dojo.lang.extend(dojo.lfx.Combine,{_animsEnded:0,play:function(_7c8,_7c9){
if(!this._anims.length){
return this;
}
this.fire("beforeBegin");
if(_7c8>0){
setTimeout(dojo.lang.hitch(this,function(){
this.play(null,_7c9);
}),_7c8);
return this;
}
if(_7c9||this._anims[0].percent==0){
this.fire("onBegin");
}
this.fire("onPlay");
this._animsCall("play",null,_7c9);
return this;
},pause:function(){
this.fire("onPause");
this._animsCall("pause");
return this;
},stop:function(_7ca){
this.fire("onStop");
this._animsCall("stop",_7ca);
return this;
},_onAnimsEnded:function(){
this._animsEnded++;
if(this._animsEnded>=this._anims.length){
this.fire("onEnd");
}
return this;
},_animsCall:function(_7cb){
var args=[];
if(arguments.length>1){
for(var i=1;i<arguments.length;i++){
args.push(arguments[i]);
}
}
var _7ce=this;
dojo.lang.forEach(this._anims,function(anim){
anim[_7cb](args);
},_7ce);
return this;
}});
dojo.lfx.Chain=function(_7d0){
dojo.lfx.IAnimation.call(this);
this._anims=[];
this._currAnim=-1;
var _7d1=arguments;
if(_7d1.length==1&&(dojo.lang.isArray(_7d1[0])||dojo.lang.isArrayLike(_7d1[0]))){
_7d1=_7d1[0];
}
var _7d2=this;
dojo.lang.forEach(_7d1,function(anim,i,_7d5){
this._anims.push(anim);
if(i<_7d5.length-1){
anim.connect("onEnd",dojo.lang.hitch(this,"_playNext"));
}else{
anim.connect("onEnd",dojo.lang.hitch(this,function(){
this.fire("onEnd");
}));
}
},this);
};
dojo.inherits(dojo.lfx.Chain,dojo.lfx.IAnimation);
dojo.lang.extend(dojo.lfx.Chain,{_currAnim:-1,play:function(_7d6,_7d7){
if(!this._anims.length){
return this;
}
if(_7d7||!this._anims[this._currAnim]){
this._currAnim=0;
}
var _7d8=this._anims[this._currAnim];
this.fire("beforeBegin");
if(_7d6>0){
setTimeout(dojo.lang.hitch(this,function(){
this.play(null,_7d7);
}),_7d6);
return this;
}
if(_7d8){
if(this._currAnim==0){
this.fire("handler",["begin",this._currAnim]);
this.fire("onBegin",[this._currAnim]);
}
this.fire("onPlay",[this._currAnim]);
_7d8.play(null,_7d7);
}
return this;
},pause:function(){
if(this._anims[this._currAnim]){
this._anims[this._currAnim].pause();
this.fire("onPause",[this._currAnim]);
}
return this;
},playPause:function(){
if(this._anims.length==0){
return this;
}
if(this._currAnim==-1){
this._currAnim=0;
}
var _7d9=this._anims[this._currAnim];
if(_7d9){
if(!_7d9._active||_7d9._paused){
this.play();
}else{
this.pause();
}
}
return this;
},stop:function(){
var _7da=this._anims[this._currAnim];
if(_7da){
_7da.stop();
this.fire("onStop",[this._currAnim]);
}
return _7da;
},_playNext:function(){
if(this._currAnim==-1||this._anims.length==0){
return this;
}
this._currAnim++;
if(this._anims[this._currAnim]){
this._anims[this._currAnim].play(null,true);
}
return this;
}});
dojo.lfx.combine=function(_7db){
var _7dc=arguments;
if(dojo.lang.isArray(arguments[0])){
_7dc=arguments[0];
}
if(_7dc.length==1){
return _7dc[0];
}
return new dojo.lfx.Combine(_7dc);
};
dojo.lfx.chain=function(_7dd){
var _7de=arguments;
if(dojo.lang.isArray(arguments[0])){
_7de=arguments[0];
}
if(_7de.length==1){
return _7de[0];
}
return new dojo.lfx.Chain(_7de);
};
dojo.provide("dojo.html.color");
dojo.html.getBackgroundColor=function(node){
node=dojo.byId(node);
var _7e0;
do{
_7e0=dojo.html.getStyle(node,"background-color");
if(_7e0.toLowerCase()=="rgba(0, 0, 0, 0)"){
_7e0="transparent";
}
if(node==document.getElementsByTagName("body")[0]){
node=null;
break;
}
node=node.parentNode;
}while(node&&dojo.lang.inArray(["transparent",""],_7e0));
if(_7e0=="transparent"){
_7e0=[255,255,255,0];
}else{
_7e0=dojo.gfx.color.extractRGB(_7e0);
}
return _7e0;
};
dojo.provide("dojo.lfx.html");
dojo.lfx.html._byId=function(_7e1){
if(!_7e1){
return [];
}
if(dojo.lang.isArrayLike(_7e1)){
if(!_7e1.alreadyChecked){
var n=[];
dojo.lang.forEach(_7e1,function(node){
n.push(dojo.byId(node));
});
n.alreadyChecked=true;
return n;
}else{
return _7e1;
}
}else{
var n=[];
n.push(dojo.byId(_7e1));
n.alreadyChecked=true;
return n;
}
};
dojo.lfx.html.propertyAnimation=function(_7e4,_7e5,_7e6,_7e7,_7e8){
_7e4=dojo.lfx.html._byId(_7e4);
var _7e9={"propertyMap":_7e5,"nodes":_7e4,"duration":_7e6,"easing":_7e7||dojo.lfx.easeDefault};
var _7ea=function(args){
if(args.nodes.length==1){
var pm=args.propertyMap;
if(!dojo.lang.isArray(args.propertyMap)){
var parr=[];
for(var _7ee in pm){
pm[_7ee].property=_7ee;
parr.push(pm[_7ee]);
}
pm=args.propertyMap=parr;
}
dojo.lang.forEach(pm,function(prop){
if(dj_undef("start",prop)){
if(prop.property!="opacity"){
prop.start=parseInt(dojo.html.getComputedStyle(args.nodes[0],prop.property));
}else{
prop.start=dojo.html.getOpacity(args.nodes[0]);
}
}
});
}
};
var _7f0=function(_7f1){
var _7f2=[];
dojo.lang.forEach(_7f1,function(c){
_7f2.push(Math.round(c));
});
return _7f2;
};
var _7f4=function(n,_7f6){
n=dojo.byId(n);
if(!n||!n.style){
return;
}
for(var s in _7f6){
try{
if(s=="opacity"){
dojo.html.setOpacity(n,_7f6[s]);
}else{
n.style[s]=_7f6[s];
}
}
catch(e){
dojo.debug(e);
}
}
};
var _7f8=function(_7f9){
this._properties=_7f9;
this.diffs=new Array(_7f9.length);
dojo.lang.forEach(_7f9,function(prop,i){
if(dojo.lang.isFunction(prop.start)){
prop.start=prop.start(prop,i);
}
if(dojo.lang.isFunction(prop.end)){
prop.end=prop.end(prop,i);
}
if(dojo.lang.isArray(prop.start)){
this.diffs[i]=null;
}else{
if(prop.start instanceof dojo.gfx.color.Color){
prop.startRgb=prop.start.toRgb();
prop.endRgb=prop.end.toRgb();
}else{
this.diffs[i]=prop.end-prop.start;
}
}
},this);
this.getValue=function(n){
var ret={};
dojo.lang.forEach(this._properties,function(prop,i){
var _800=null;
if(dojo.lang.isArray(prop.start)){
}else{
if(prop.start instanceof dojo.gfx.color.Color){
_800=(prop.units||"rgb")+"(";
for(var j=0;j<prop.startRgb.length;j++){
_800+=Math.round(((prop.endRgb[j]-prop.startRgb[j])*n)+prop.startRgb[j])+(j<prop.startRgb.length-1?",":"");
}
_800+=")";
}else{
_800=((this.diffs[i])*n)+prop.start+(prop.property!="opacity"?prop.units||"px":"");
}
}
ret[dojo.html.toCamelCase(prop.property)]=_800;
},this);
return ret;
};
};
var anim=new dojo.lfx.Animation({beforeBegin:function(){
_7ea(_7e9);
anim.curve=new _7f8(_7e9.propertyMap);
},onAnimate:function(_803){
dojo.lang.forEach(_7e9.nodes,function(node){
_7f4(node,_803);
});
}},_7e9.duration,null,_7e9.easing);
if(_7e8){
for(var x in _7e8){
if(dojo.lang.isFunction(_7e8[x])){
anim.connect(x,anim,_7e8[x]);
}
}
}
return anim;
};
dojo.lfx.html._makeFadeable=function(_806){
var _807=function(node){
if(dojo.render.html.ie){
if((node.style.zoom.length==0)&&(dojo.html.getStyle(node,"zoom")=="normal")){
node.style.zoom="1";
}
if((node.style.width.length==0)&&(dojo.html.getStyle(node,"width")=="auto")){
node.style.width="auto";
}
}
};
if(dojo.lang.isArrayLike(_806)){
dojo.lang.forEach(_806,_807);
}else{
_807(_806);
}
};
dojo.lfx.html.fade=function(_809,_80a,_80b,_80c,_80d){
_809=dojo.lfx.html._byId(_809);
var _80e={property:"opacity"};
if(!dj_undef("start",_80a)){
_80e.start=_80a.start;
}else{
_80e.start=function(){
return dojo.html.getOpacity(_809[0]);
};
}
if(!dj_undef("end",_80a)){
_80e.end=_80a.end;
}else{
dojo.raise("dojo.lfx.html.fade needs an end value");
}
var anim=dojo.lfx.propertyAnimation(_809,[_80e],_80b,_80c);
anim.connect("beforeBegin",function(){
dojo.lfx.html._makeFadeable(_809);
});
if(_80d){
anim.connect("onEnd",function(){
_80d(_809,anim);
});
}
return anim;
};
dojo.lfx.html.fadeIn=function(_810,_811,_812,_813){
return dojo.lfx.html.fade(_810,{end:1},_811,_812,_813);
};
dojo.lfx.html.fadeOut=function(_814,_815,_816,_817){
return dojo.lfx.html.fade(_814,{end:0},_815,_816,_817);
};
dojo.lfx.html.fadeShow=function(_818,_819,_81a,_81b){
_818=dojo.lfx.html._byId(_818);
dojo.lang.forEach(_818,function(node){
dojo.html.setOpacity(node,0);
});
var anim=dojo.lfx.html.fadeIn(_818,_819,_81a,_81b);
anim.connect("beforeBegin",function(){
if(dojo.lang.isArrayLike(_818)){
dojo.lang.forEach(_818,dojo.html.show);
}else{
dojo.html.show(_818);
}
});
return anim;
};
dojo.lfx.html.fadeHide=function(_81e,_81f,_820,_821){
var anim=dojo.lfx.html.fadeOut(_81e,_81f,_820,function(){
if(dojo.lang.isArrayLike(_81e)){
dojo.lang.forEach(_81e,dojo.html.hide);
}else{
dojo.html.hide(_81e);
}
if(_821){
_821(_81e,anim);
}
});
return anim;
};
dojo.lfx.html.wipeIn=function(_823,_824,_825,_826){
_823=dojo.lfx.html._byId(_823);
var _827=[];
dojo.lang.forEach(_823,function(node){
var _829={};
with(node.style){
visibility="hidden";
display="";
}
var _82a=dojo.html.getBorderBox(node).height;
with(node.style){
visibility="";
display="none";
}
var anim=dojo.lfx.propertyAnimation(node,{"height":{start:1,end:function(){
return _82a;
}}},_824,_825);
anim.connect("beforeBegin",function(){
_829.overflow=node.style.overflow;
_829.height=node.style.height;
with(node.style){
overflow="hidden";
height="1px";
}
dojo.html.show(node);
});
anim.connect("onEnd",function(){
with(node.style){
overflow=_829.overflow;
height=_829.height;
}
if(_826){
_826(node,anim);
}
});
_827.push(anim);
});
return dojo.lfx.combine(_827);
};
dojo.lfx.html.wipeOut=function(_82c,_82d,_82e,_82f){
_82c=dojo.lfx.html._byId(_82c);
var _830=[];
dojo.lang.forEach(_82c,function(node){
var _832={};
var anim=dojo.lfx.propertyAnimation(node,{"height":{start:function(){
return dojo.html.getContentBox(node).height;
},end:1}},_82d,_82e,{"beforeBegin":function(){
_832.overflow=node.style.overflow;
_832.height=node.style.height;
with(node.style){
overflow="hidden";
}
dojo.html.show(node);
},"onEnd":function(){
dojo.html.hide(node);
with(node.style){
overflow=_832.overflow;
height=_832.height;
}
if(_82f){
_82f(node,anim);
}
}});
_830.push(anim);
});
return dojo.lfx.combine(_830);
};
dojo.lfx.html.slideTo=function(_834,_835,_836,_837,_838){
_834=dojo.lfx.html._byId(_834);
var _839=[];
var _83a=dojo.html.getComputedStyle;
dojo.lang.forEach(_834,function(node){
var top=null;
var left=null;
var init=(function(){
var _83f=node;
return function(){
var pos=_83a(_83f,"position");
top=(pos=="absolute"?node.offsetTop:parseInt(_83a(node,"top"))||0);
left=(pos=="absolute"?node.offsetLeft:parseInt(_83a(node,"left"))||0);
if(!dojo.lang.inArray(["absolute","relative"],pos)){
var ret=dojo.html.abs(_83f,true);
dojo.html.setStyleAttributes(_83f,"position:absolute;top:"+ret.y+"px;left:"+ret.x+"px;");
top=ret.y;
left=ret.x;
}
};
})();
init();
var anim=dojo.lfx.propertyAnimation(node,{"top":{start:top,end:(_835.top||0)},"left":{start:left,end:(_835.left||0)}},_836,_837,{"beforeBegin":init});
if(_838){
anim.connect("onEnd",function(){
_838(_834,anim);
});
}
_839.push(anim);
});
return dojo.lfx.combine(_839);
};
dojo.lfx.html.slideBy=function(_843,_844,_845,_846,_847){
_843=dojo.lfx.html._byId(_843);
var _848=[];
var _849=dojo.html.getComputedStyle;
dojo.lang.forEach(_843,function(node){
var top=null;
var left=null;
var init=(function(){
var _84e=node;
return function(){
var pos=_849(_84e,"position");
top=(pos=="absolute"?node.offsetTop:parseInt(_849(node,"top"))||0);
left=(pos=="absolute"?node.offsetLeft:parseInt(_849(node,"left"))||0);
if(!dojo.lang.inArray(["absolute","relative"],pos)){
var ret=dojo.html.abs(_84e,true);
dojo.html.setStyleAttributes(_84e,"position:absolute;top:"+ret.y+"px;left:"+ret.x+"px;");
top=ret.y;
left=ret.x;
}
};
})();
init();
var anim=dojo.lfx.propertyAnimation(node,{"top":{start:top,end:top+(_844.top||0)},"left":{start:left,end:left+(_844.left||0)}},_845,_846).connect("beforeBegin",init);
if(_847){
anim.connect("onEnd",function(){
_847(_843,anim);
});
}
_848.push(anim);
});
return dojo.lfx.combine(_848);
};
dojo.lfx.html.explode=function(_852,_853,_854,_855,_856){
var h=dojo.html;
_852=dojo.byId(_852);
_853=dojo.byId(_853);
var _858=h.toCoordinateObject(_852,true);
var _859=document.createElement("div");
h.copyStyle(_859,_853);
if(_853.explodeClassName){
_859.className=_853.explodeClassName;
}
with(_859.style){
position="absolute";
display="none";
var _85a=h.getStyle(_852,"background-color");
backgroundColor=_85a?_85a.toLowerCase():"transparent";
backgroundColor=(backgroundColor=="transparent")?"rgb(221, 221, 221)":backgroundColor;
}
dojo.body().appendChild(_859);
with(_853.style){
visibility="hidden";
display="block";
}
var _85b=h.toCoordinateObject(_853,true);
with(_853.style){
display="none";
visibility="visible";
}
var _85c={opacity:{start:0.5,end:1}};
dojo.lang.forEach(["height","width","top","left"],function(type){
_85c[type]={start:_858[type],end:_85b[type]};
});
var anim=new dojo.lfx.propertyAnimation(_859,_85c,_854,_855,{"beforeBegin":function(){
h.setDisplay(_859,"block");
},"onEnd":function(){
h.setDisplay(_853,"block");
_859.parentNode.removeChild(_859);
}});
if(_856){
anim.connect("onEnd",function(){
_856(_853,anim);
});
}
return anim;
};
dojo.lfx.html.implode=function(_85f,end,_861,_862,_863){
var h=dojo.html;
_85f=dojo.byId(_85f);
end=dojo.byId(end);
var _865=dojo.html.toCoordinateObject(_85f,true);
var _866=dojo.html.toCoordinateObject(end,true);
var _867=document.createElement("div");
dojo.html.copyStyle(_867,_85f);
if(_85f.explodeClassName){
_867.className=_85f.explodeClassName;
}
dojo.html.setOpacity(_867,0.3);
with(_867.style){
position="absolute";
display="none";
backgroundColor=h.getStyle(_85f,"background-color").toLowerCase();
}
dojo.body().appendChild(_867);
var _868={opacity:{start:1,end:0.5}};
dojo.lang.forEach(["height","width","top","left"],function(type){
_868[type]={start:_865[type],end:_866[type]};
});
var anim=new dojo.lfx.propertyAnimation(_867,_868,_861,_862,{"beforeBegin":function(){
dojo.html.hide(_85f);
dojo.html.show(_867);
},"onEnd":function(){
_867.parentNode.removeChild(_867);
}});
if(_863){
anim.connect("onEnd",function(){
_863(_85f,anim);
});
}
return anim;
};
dojo.lfx.html.highlight=function(_86b,_86c,_86d,_86e,_86f){
_86b=dojo.lfx.html._byId(_86b);
var _870=[];
dojo.lang.forEach(_86b,function(node){
var _872=dojo.html.getBackgroundColor(node);
var bg=dojo.html.getStyle(node,"background-color").toLowerCase();
var _874=dojo.html.getStyle(node,"background-image");
var _875=(bg=="transparent"||bg=="rgba(0, 0, 0, 0)");
while(_872.length>3){
_872.pop();
}
var rgb=new dojo.gfx.color.Color(_86c);
var _877=new dojo.gfx.color.Color(_872);
var anim=dojo.lfx.propertyAnimation(node,{"background-color":{start:rgb,end:_877}},_86d,_86e,{"beforeBegin":function(){
if(_874){
node.style.backgroundImage="none";
}
node.style.backgroundColor="rgb("+rgb.toRgb().join(",")+")";
},"onEnd":function(){
if(_874){
node.style.backgroundImage=_874;
}
if(_875){
node.style.backgroundColor="transparent";
}
if(_86f){
_86f(node,anim);
}
}});
_870.push(anim);
});
return dojo.lfx.combine(_870);
};
dojo.lfx.html.unhighlight=function(_879,_87a,_87b,_87c,_87d){
_879=dojo.lfx.html._byId(_879);
var _87e=[];
dojo.lang.forEach(_879,function(node){
var _880=new dojo.gfx.color.Color(dojo.html.getBackgroundColor(node));
var rgb=new dojo.gfx.color.Color(_87a);
var _882=dojo.html.getStyle(node,"background-image");
var anim=dojo.lfx.propertyAnimation(node,{"background-color":{start:_880,end:rgb}},_87b,_87c,{"beforeBegin":function(){
if(_882){
node.style.backgroundImage="none";
}
node.style.backgroundColor="rgb("+_880.toRgb().join(",")+")";
},"onEnd":function(){
if(_87d){
_87d(node,anim);
}
}});
_87e.push(anim);
});
return dojo.lfx.combine(_87e);
};
dojo.lang.mixin(dojo.lfx,dojo.lfx.html);
dojo.kwCompoundRequire({browser:["dojo.lfx.html"],dashboard:["dojo.lfx.html"]});
dojo.provide("dojo.lfx.*");
dojo.provide("dojo.lfx.toggler");
dojo.lfx.toggler.plain=function(){
this.stop=function(){
};
this.show=function(node,_885,_886,_887){
dojo.html.show(node);
if(dojo.lang.isFunction(_887)){
_887();
}
};
this.hide=function(node,_889,_88a,_88b){
dojo.html.hide(node);
if(dojo.lang.isFunction(_88b)){
_88b();
}
};
};
dojo.lfx.toggler.common={stop:function(){
if(this.anim&&this.anim.status()!="stopped"){
this.anim.stop();
}
},_act:function(_88c,node,_88e,_88f,_890,_891){
this.stop();
this.anim=dojo.lfx[_88c](node,_88e,_88f,_890).play();
},show:function(node,_893,_894,_895,_896){
this._act(this.show_action,node,_893,_894,_895,_896);
},hide:function(node,_898,_899,_89a,_89b){
this._act(this.hide_action,node,_898,_899,_89a,_89b);
}};
dojo.lfx.toggler.fade=function(){
this.anim=null;
this.show_action="fadeShow";
this.hide_action="fadeHide";
};
dojo.extend(dojo.lfx.toggler.fade,dojo.lfx.toggler.common);
dojo.lfx.toggler.wipe=function(){
this.anim=null;
this.show_action="wipeIn";
this.hide_action="wipeOut";
};
dojo.extend(dojo.lfx.toggler.wipe,dojo.lfx.toggler.common);
dojo.lfx.toggler.explode=function(){
this.anim=null;
this.show_action="explode";
this.hide_action="implode";
this.show=function(node,_89d,_89e,_89f,_8a0){
this.stop();
this.anim=dojo.lfx.explode(_8a0||{x:0,y:0,width:0,height:0},node,_89d,_89e,_89f).play();
};
this.hide=function(node,_8a2,_8a3,_8a4,_8a5){
this.stop();
this.anim=dojo.lfx.implode(node,_8a5||{x:0,y:0,width:0,height:0},_8a2,_8a3,_8a4).play();
};
};
dojo.extend(dojo.lfx.toggler.explode,dojo.lfx.toggler.common);
dojo.provide("dojo.widget.HtmlWidget");
dojo.declare("dojo.widget.HtmlWidget",dojo.widget.DomWidget,{templateCssPath:null,templatePath:null,lang:"",toggle:"plain",toggleDuration:150,initialize:function(args,frag){
},postMixInProperties:function(args,frag){
if(this.lang===""){
this.lang=null;
}
this.toggleObj=new (dojo.lfx.toggler[this.toggle.toLowerCase()]||dojo.lfx.toggler.plain);
},createNodesFromText:function(txt,wrap){
return dojo.html.createNodesFromText(txt,wrap);
},destroyRendering:function(_8ac){
try{
if(this.bgIframe){
this.bgIframe.remove();
delete this.bgIframe;
}
if(!_8ac&&this.domNode){
dojo.event.browser.clean(this.domNode);
}
dojo.widget.HtmlWidget.superclass.destroyRendering.call(this);
}
catch(e){
}
},isShowing:function(){
return dojo.html.isShowing(this.domNode);
},toggleShowing:function(){
if(this.isShowing()){
this.hide();
}else{
this.show();
}
},show:function(){
if(this.isShowing()){
return;
}
this.animationInProgress=true;
this.toggleObj.show(this.domNode,this.toggleDuration,null,dojo.lang.hitch(this,this.onShow),this.explodeSrc);
},onShow:function(){
this.animationInProgress=false;
this.checkSize();
},hide:function(){
if(!this.isShowing()){
return;
}
this.animationInProgress=true;
this.toggleObj.hide(this.domNode,this.toggleDuration,null,dojo.lang.hitch(this,this.onHide),this.explodeSrc);
},onHide:function(){
this.animationInProgress=false;
},_isResized:function(w,h){
if(!this.isShowing()){
return false;
}
var wh=dojo.html.getMarginBox(this.domNode);
var _8b0=w||wh.width;
var _8b1=h||wh.height;
if(this.width==_8b0&&this.height==_8b1){
return false;
}
this.width=_8b0;
this.height=_8b1;
return true;
},checkSize:function(){
if(!this._isResized()){
return;
}
this.onResized();
},resizeTo:function(w,h){
dojo.html.setMarginBox(this.domNode,{width:w,height:h});
if(this.isShowing()){
this.onResized();
}
},resizeSoon:function(){
if(this.isShowing()){
dojo.lang.setTimeout(this,this.onResized,0);
}
},onResized:function(){
dojo.lang.forEach(this.children,function(_8b4){
if(_8b4.checkSize){
_8b4.checkSize();
}
});
}});
dojo.kwCompoundRequire({common:["dojo.xml.Parse","dojo.widget.Widget","dojo.widget.Parse","dojo.widget.Manager"],browser:["dojo.widget.DomWidget","dojo.widget.HtmlWidget"],dashboard:["dojo.widget.DomWidget","dojo.widget.HtmlWidget"],svg:["dojo.widget.SvgWidget"],rhino:["dojo.widget.SwtWidget"]});
dojo.provide("dojo.widget.*");
dojo.provide("dojo.html.iframe");
dojo.html.iframeContentWindow=function(_8b5){
var win=dojo.html.getDocumentWindow(dojo.html.iframeContentDocument(_8b5))||dojo.html.iframeContentDocument(_8b5)["__parent__"]||(_8b5.name&&document.frames[_8b5.name])||null;
return win;
};
dojo.html.iframeContentDocument=function(_8b7){
var doc=_8b7.contentDocument||((_8b7.contentWindow)&&(_8b7.contentWindow.document))||((_8b7.name)&&(document.frames[_8b7.name])&&(document.frames[_8b7.name].document))||null;
return doc;
};
dojo.html.BackgroundIframe=function(node){
if(dojo.render.html.ie55||dojo.render.html.ie60){
var html="<iframe src='javascript:false'"+" style='position: absolute; left: 0px; top: 0px; width: 100%; height: 100%;"+"z-index: -1; filter:Alpha(Opacity=\"0\");' "+">";
this.iframe=dojo.doc().createElement(html);
this.iframe.tabIndex=-1;
if(node){
node.appendChild(this.iframe);
this.domNode=node;
}else{
dojo.body().appendChild(this.iframe);
this.iframe.style.display="none";
}
}
};
dojo.lang.extend(dojo.html.BackgroundIframe,{iframe:null,onResized:function(){
if(this.iframe&&this.domNode&&this.domNode.parentNode){
var _8bb=dojo.html.getMarginBox(this.domNode);
if(_8bb.width==0||_8bb.height==0){
dojo.lang.setTimeout(this,this.onResized,100);
return;
}
this.iframe.style.width=_8bb.width+"px";
this.iframe.style.height=_8bb.height+"px";
}
},size:function(node){
if(!this.iframe){
return;
}
var _8bd=dojo.html.toCoordinateObject(node,true,dojo.html.boxSizing.BORDER_BOX);
with(this.iframe.style){
width=_8bd.width+"px";
height=_8bd.height+"px";
left=_8bd.left+"px";
top=_8bd.top+"px";
}
},setZIndex:function(node){
if(!this.iframe){
return;
}
if(dojo.dom.isNode(node)){
this.iframe.style.zIndex=dojo.html.getStyle(node,"z-index")-1;
}else{
if(!isNaN(node)){
this.iframe.style.zIndex=node;
}
}
},show:function(){
if(this.iframe){
this.iframe.style.display="block";
}
},hide:function(){
if(this.iframe){
this.iframe.style.display="none";
}
},remove:function(){
if(this.iframe){
dojo.html.removeNode(this.iframe,true);
delete this.iframe;
this.iframe=null;
}
}});
dojo.provide("dojo.lfx.scroll");
dojo.lfx.smoothScroll=function(node,win,_8c1,_8c2,_8c3,_8c4){
var _8c5={"window":win,"offset":_8c1||{x:0,y:0},"target":dojo.html.getAbsolutePositionExt(node,true,dojo.html.boxSizing.BORDER_BOX,win),"duration":_8c2,"easing":_8c3||dojo.lfx.easeOut};
var anim=new dojo.lfx.Animation({beforeBegin:function(){
var _8c7=dojo.withGlobal(_8c5.window,dojo.html.getScroll).offset;
delete this.curve;
anim.curve=new dojo.lfx.Line([_8c7.x,_8c7.y],[_8c5.target.x+_8c5.offset.x,_8c5.target.y+_8c5.offset.y]);
},onAnimate:function(_8c8){
_8c5.window.scrollTo(_8c8[0],_8c8[1]);
}},_8c2,null,_8c3||dojo.lfx.easeOut);
if(_8c4){
for(var x in _8c4){
if(dojo.lang.isFunction(_8c4[x])){
anim.connect(x,anim,_8c4[x]);
}
}
}
return anim;
};
dojo.provide("dojo.logging.Logger");
dojo.provide("dojo.logging.LogFilter");
dojo.provide("dojo.logging.Record");
dojo.provide("dojo.log");
dojo.logging.Record=function(_8ca,_8cb){
this.level=_8ca;
this.message="";
this.msgArgs=[];
this.time=new Date();
if(dojo.lang.isArray(_8cb)){
if(_8cb.length>0&&dojo.lang.isString(_8cb[0])){
this.message=_8cb.shift();
}
this.msgArgs=_8cb;
}else{
this.message=_8cb;
}
};
dojo.logging.LogFilter=function(_8cc){
this.passChain=_8cc||"";
this.filter=function(_8cd){
return true;
};
};
dojo.logging.Logger=function(){
this.cutOffLevel=0;
this.propagate=true;
this.parent=null;
this.data=[];
this.filters=[];
this.handlers=[];
};
dojo.extend(dojo.logging.Logger,{_argsToArr:function(args){
var ret=[];
for(var x=0;x<args.length;x++){
ret.push(args[x]);
}
return ret;
},setLevel:function(lvl){
this.cutOffLevel=parseInt(lvl);
},isEnabledFor:function(lvl){
return parseInt(lvl)>=this.cutOffLevel;
},getEffectiveLevel:function(){
if((this.cutOffLevel==0)&&(this.parent)){
return this.parent.getEffectiveLevel();
}
return this.cutOffLevel;
},addFilter:function(flt){
this.filters.push(flt);
return this.filters.length-1;
},removeFilterByIndex:function(_8d4){
if(this.filters[_8d4]){
delete this.filters[_8d4];
return true;
}
return false;
},removeFilter:function(_8d5){
for(var x=0;x<this.filters.length;x++){
if(this.filters[x]===_8d5){
delete this.filters[x];
return true;
}
}
return false;
},removeAllFilters:function(){
this.filters=[];
},filter:function(rec){
for(var x=0;x<this.filters.length;x++){
if((this.filters[x]["filter"])&&(!this.filters[x].filter(rec))||(rec.level<this.cutOffLevel)){
return false;
}
}
return true;
},addHandler:function(hdlr){
this.handlers.push(hdlr);
return this.handlers.length-1;
},handle:function(rec){
if((!this.filter(rec))||(rec.level<this.cutOffLevel)){
return false;
}
for(var x=0;x<this.handlers.length;x++){
if(this.handlers[x]["handle"]){
this.handlers[x].handle(rec);
}
}
return true;
},log:function(lvl,msg){
if((this.propagate)&&(this.parent)&&(this.parent.rec.level>=this.cutOffLevel)){
this.parent.log(lvl,msg);
return false;
}
this.handle(new dojo.logging.Record(lvl,msg));
return true;
},debug:function(msg){
return this.logType("DEBUG",this._argsToArr(arguments));
},info:function(msg){
return this.logType("INFO",this._argsToArr(arguments));
},warning:function(msg){
return this.logType("WARNING",this._argsToArr(arguments));
},error:function(msg){
return this.logType("ERROR",this._argsToArr(arguments));
},critical:function(msg){
return this.logType("CRITICAL",this._argsToArr(arguments));
},exception:function(msg,e,_8e5){
if(e){
var _8e6=[e.name,(e.description||e.message)];
if(e.fileName){
_8e6.push(e.fileName);
_8e6.push("line "+e.lineNumber);
}
msg+=" "+_8e6.join(" : ");
}
this.logType("ERROR",msg);
if(!_8e5){
throw e;
}
},logType:function(type,args){
return this.log.apply(this,[dojo.logging.log.getLevel(type),args]);
},warn:function(){
this.warning.apply(this,arguments);
},err:function(){
this.error.apply(this,arguments);
},crit:function(){
this.critical.apply(this,arguments);
}});
dojo.logging.LogHandler=function(_8e9){
this.cutOffLevel=(_8e9)?_8e9:0;
this.formatter=null;
this.data=[];
this.filters=[];
};
dojo.lang.extend(dojo.logging.LogHandler,{setFormatter:function(_8ea){
dojo.unimplemented("setFormatter");
},flush:function(){
},close:function(){
},handleError:function(){
dojo.deprecated("dojo.logging.LogHandler.handleError","use handle()","0.6");
},handle:function(_8eb){
if((this.filter(_8eb))&&(_8eb.level>=this.cutOffLevel)){
this.emit(_8eb);
}
},emit:function(_8ec){
dojo.unimplemented("emit");
}});
void (function(){
var _8ed=["setLevel","addFilter","removeFilterByIndex","removeFilter","removeAllFilters","filter"];
var tgt=dojo.logging.LogHandler.prototype;
var src=dojo.logging.Logger.prototype;
for(var x=0;x<_8ed.length;x++){
tgt[_8ed[x]]=src[_8ed[x]];
}
})();
dojo.logging.log=new dojo.logging.Logger();
dojo.logging.log.levels=[{"name":"DEBUG","level":1},{"name":"INFO","level":2},{"name":"WARNING","level":3},{"name":"ERROR","level":4},{"name":"CRITICAL","level":5}];
dojo.logging.log.loggers={};
dojo.logging.log.getLogger=function(name){
if(!this.loggers[name]){
this.loggers[name]=new dojo.logging.Logger();
this.loggers[name].parent=this;
}
return this.loggers[name];
};
dojo.logging.log.getLevelName=function(lvl){
for(var x=0;x<this.levels.length;x++){
if(this.levels[x].level==lvl){
return this.levels[x].name;
}
}
return null;
};
dojo.logging.log.getLevel=function(name){
for(var x=0;x<this.levels.length;x++){
if(this.levels[x].name.toUpperCase()==name.toUpperCase()){
return this.levels[x].level;
}
}
return null;
};
dojo.declare("dojo.logging.MemoryLogHandler",dojo.logging.LogHandler,function(_8f6,_8f7,_8f8,_8f9){
dojo.logging.LogHandler.call(this,_8f6);
this.numRecords=(typeof djConfig["loggingNumRecords"]!="undefined")?djConfig["loggingNumRecords"]:((_8f7)?_8f7:-1);
this.postType=(typeof djConfig["loggingPostType"]!="undefined")?djConfig["loggingPostType"]:(_8f8||-1);
this.postInterval=(typeof djConfig["loggingPostInterval"]!="undefined")?djConfig["loggingPostInterval"]:(_8f8||-1);
},{emit:function(_8fa){
if(!djConfig.isDebug){
return;
}
var _8fb=String(dojo.log.getLevelName(_8fa.level)+": "+_8fa.time.toLocaleTimeString())+": "+_8fa.message;
if(!dj_undef("println",dojo.hostenv)){
dojo.hostenv.println(_8fb,_8fa.msgArgs);
}
this.data.push(_8fa);
if(this.numRecords!=-1){
while(this.data.length>this.numRecords){
this.data.shift();
}
}
}});
dojo.logging.logQueueHandler=new dojo.logging.MemoryLogHandler(0,50,0,10000);
dojo.logging.log.addHandler(dojo.logging.logQueueHandler);
dojo.log=dojo.logging.log;
dojo.kwCompoundRequire({common:[["dojo.logging.Logger",false,false]],rhino:["dojo.logging.RhinoLogger"]});
dojo.provide("dojo.logging.*");

