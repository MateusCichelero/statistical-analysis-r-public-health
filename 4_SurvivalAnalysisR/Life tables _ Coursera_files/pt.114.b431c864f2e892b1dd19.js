(window.webpackJsonp=window.webpackJsonp||[]).push([[114],{"2RA0":function(module,exports,t){var e=t("y4s1"),n;"string"==typeof e&&(e=[[module.i,e,""]]);var a={transform:void 0},s=t("aET+")(e,a);e.locals&&(module.exports=e.locals)},"3g8Y":function(module,exports,t){var e,n,a,s;a=window,s=function(t,e,n){var a=function template(n){var a=[],s,r=n||{};return function(e,n,r){switch(e){case"countdown":a.push('<div class="vjs-control c-countdown"><span class="c-countdown-text"><em class="cif-spin cif-refresh"></em></span></div>');break;case"subtitlesMenuItem":a.push('<em class="cif-lg cif-fw c-subtitles-menu-item-selected-icon"></em><span data-js="c-subtitles-menu-item-label" class="c-subtitles-menu-item-label caption-text">'+t.escape(null==(s=n(r))?"":s)+"</span>");break;case"subtitlesOffLabel":a.push(""+t.escape(null==(s=n("Subtitles Off"))?"":s));break;case"subtitlesMenuTitle":a.push('<h3 class="vjs-menu-title headline-1-text menu-section-title">'+t.escape(null==(s=n("Subtitles"))?"":s)+"</h3>");break;case"menuDivider":a.push('<div class="c-menu-divider"></div>')}}.call(this,"controlName"in r?r.controlName:"undefined"!=typeof controlName?controlName:void 0,"_t"in r?r._t:void 0!==e?e:void 0,"label"in r?r.label:"undefined"!=typeof label?label:void 0),a.join("")};return function(t){return t&&"_t"in t&&(e=t._t.merge(e)),a(t)}},e=[t("xgPa"),t("Kq1R")],void 0===(n=function(t,e){var n;return s(t,e,n)}.apply(exports,e))||(module.exports=n)},Goki:function(module,t,e){"use strict";var n=e("VbXa"),a=e.n(n),s=e("PTN7"),r=e.n(s),l=e("wd/R"),i=e.n(l),o=function(t){function CountdownDisplay(e,n){var a;return(a=t.call(this,e,n)||this).on(e,"timeupdate",a.updateContent),a.on(e,"durationchange",a.updateContent),a}a()(CountdownDisplay,t);var e=CountdownDisplay.prototype;return e.createEl=function createEl(){var e=t.prototype.createEl.call(this,"div",{className:"vjs-countdown-time vjs-time-controls vjs-control"});return this.contentEl_=t.prototype.createEl.call(this,"div",{className:"vjs-countdown-time-display",innerHTML:'<span class="vjs-control-text">Current Time </span><em class="cif-spin cif-refresh"></em>'},{"aria-live":"off"}),e.appendChild(this.contentEl_),e},e.updateContent=function updateContent(){if(Number.isNaN(this.player().duration()))return;var t=this.player().duration()-this.player().currentTime();this.contentEl_.innerHTML='<span class="vjs-control-text">'+this.localize("Time Left")+"</span> "+i.a.utc(1e3*t).format("m:ss")},CountdownDisplay}(r.a.getComponent("Component"));r.a.registerComponent("CountdownDisplay",o)},Kq1R:function(module,exports,t){var e=t("wfto"),n=e.default?e.default:{},a,s=(0,t("HdzH").default)(n);s.getLocale=function(){return"pt"},module.exports=s},PRI2:function(module,t,e){"use strict";var n=e("VbXa"),a=e.n(n),s=e("PTN7"),r=e.n(s),l=e("oYk5"),i=e.n(l),o=e("u5HK"),c=e.n(o),u=function(t){function CPlayToggle(e,n){var a;a=t.call(this,e,n)||this;var s=c()("Play"),r;return a.el_.innerHTML='<span class="cif-2x cif-fw cif-play"></span>',a.el_.innerHTML+='<span class="vjs-control-text">'.concat(s,"</span>"),a.el_.setAttribute("aria-label",s),n.isAudio&&(a.el_.innerHTML='<span class="cif-fw cif-play"></span>'),a}a()(CPlayToggle,t);var e=CPlayToggle.prototype;return e.createEl=function createEl(){var e=t.prototype.createEl.call(this),n=i()(e);return n.attr("data-js","c-play-control"),n.addClass("c-video-control c-play-control"),e},e.handlePlay=function handlePlay(){t.prototype.handlePlay.call(this);var e=c()("Pause");this.el_.setAttribute("aria-label",e);var n=i()(this.el_).find("span.cif-fw"),a;n.removeClass("cif-play"),n.addClass("cif-pause"),i()(this.el_).find("span.vjs-control-text").html(e)},e.handlePause=function handlePause(){t.prototype.handlePause.call(this);var e=c()("Play");this.el_.setAttribute("aria-label",e);var n=i()(this.el_).find("span.cif-fw"),a;n.removeClass("cif-pause"),n.addClass("cif-play"),i()(this.el_).find("span.vjs-control-text").html(e)},CPlayToggle}(r.a.getComponent("PlayToggle"));r.a.registerComponent("CPlayToggle",u)},YH9Q:function(module,exports){exports.default={"Full Screen":"Tela cheia","Mute":"Sem som","Pause":"Pausar","Play":"Reproduzir","Play Video":"Reproduzir vídeo","Settings":"Configurações","Subtitles":"Legendas","Subtitles Off":"Legendas desativadas","Subtitles: closed":"Legendas: fechado","Subtitles: open":"Legendas: aberto","Unmute":"Com som","Volume":"Volume"}},u5HK:function(module,exports,t){var e=t("YH9Q"),n=e.default?e.default:{},a,s=(0,t("HdzH").default)(n);s.getLocale=function(){return"pt"},module.exports=s},wfto:function(module,exports){exports.default={"Subtitles":"Legendas","Subtitles Off":"Legendas desativadas"}},xgPa:function(module,exports,t){var e;!function(n){var exports={};
/*!
   * Jade - runtime
   * Copyright(c) 2010 TJ Holowaychuk <tj@vision-media.ca>
   * MIT Licensed
   */function nulls(t){return null!=t&&""!==t}function joinClasses(t){return Array.isArray(t)?t.map(joinClasses).filter(nulls).join(" "):t}Array.isArray||(Array.isArray=function(t){return"[object Array]"==Object.prototype.toString.call(t)}),Object.keys||(Object.keys=function(t){var e=[];for(var n in t)t.hasOwnProperty(n)&&e.push(n);return e}),exports.merge=function merge(t,e){if(1===arguments.length){for(var n=t[0],a=1;a<t.length;a++)n=merge(n,t[a]);return n}var s=t.class,r=e.class;for(var l in(s||r)&&(s=s||[],r=r||[],Array.isArray(s)||(s=[s]),Array.isArray(r)||(r=[r]),t.class=s.concat(r).filter(nulls)),e)"class"!=l&&(t[l]=e[l]);return t},exports.joinClasses=joinClasses,exports.cls=function cls(t,e){for(var n=[],a=0;a<t.length;a++)e&&e[a]?n.push(exports.escape(joinClasses([t[a]]))):n.push(joinClasses(t[a]));var s=joinClasses(n);return s.length?' class="'+s+'"':""},exports.attr=function attr(t,e,n,a){return"boolean"==typeof e||null==e?e?" "+(a?t:t+'="'+t+'"'):"":0===t.indexOf("data")&&"string"!=typeof e?" "+t+"='"+JSON.stringify(e).replace(/'/g,"&apos;")+"'":n?" "+t+'="'+exports.escape(e)+'"':" "+t+'="'+e+'"'},exports.attrs=function attrs(t,e){var n=[],a=Object.keys(t);if(a.length)for(var s=0;s<a.length;++s){var r=a[s],l=t[r];"class"==r?(l=joinClasses(l))&&n.push(" "+r+'="'+l+'"'):n.push(exports.attr(r,l,!1,e))}return n.join("")},exports.escape=function escape(t){var e=String(t).replace(/&/g,"&amp;").replace(/</g,"&lt;").replace(/>/g,"&gt;").replace(/"/g,"&quot;");return e===""+t?t:e},exports.rethrow=function rethrow(e,n,a,s){if(!(e instanceof Error))throw e;if(!("undefined"==typeof window&&n||s))throw e.message+=" on line "+a,e;try{s=s||t("mw/K").readFileSync(n,"utf8")}catch(t){rethrow(e,null,a)}var r=3,l=s.split("\n"),i=Math.max(a-r,0),o=Math.min(l.length,a+r);throw r=l.slice(i,o).map(function(t,e){var n=e+i+1;return(n==a?"  > ":"    ")+n+"| "+t}).join("\n"),e.path=n,e.message=(n||"Jade")+":"+a+"\n"+r+"\n\n"+e.message,e},void 0===(e=function(){return exports}.call(exports,t,exports,module))||(module.exports=e)}(this)},y4s1:function(module,exports,t){},zhcE:function(module,t,e){"use strict";e.r(t);var n=e("PTN7"),a=e.n(n),s=e("3g8Y"),r=e.n(s),l=e("PRI2"),i=e("Goki"),o=e("2RA0"),c=e.n(o);t.default=a.a}}]);