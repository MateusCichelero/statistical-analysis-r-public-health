(window.webpackJsonp=window.webpackJsonp||[]).push([[133],{"1r4n":function(module,e,n){"use strict";n.d(e,"a",function(){return p});var t=n("S+eF"),r=n.n(t),o=n("F/us"),a=n.n(o),c=n("TSOT"),s=n("fw5G"),i=n.n(s),d=n("mxbf"),u=Object(c.a)("/api/onDemandReferences.v1",{type:"rest"}),l=function hydrateResponse(e){return a()(e.elements).each(function(n){var t=n.content["org.coursera.ondemand.reference.AssetReferenceContent"].assetId,r=a()(e.linked["openCourseAssets.v1"]).where({id:t});r.length&&(n.content=r[0])})},p=function getReference(e,n){var t=n.referenceShortId,o=n.courseId,a=e.getStore(d.a).getCachedReference(t);if(!a){var c=(new i.a).addQueryParam("courseId",o).addQueryParam("q","shortId").addQueryParam("shortId",t).addQueryParam("fields","name,shortId,slug,content").addQueryParam("includes","assets"),s=r()(u.get(c.toString())).then(function(n){var t=l(n);e.dispatch("LOAD_REFERENCE",t[0])});return s.done(),s}e.dispatch("LOAD_REFERENCE",a)}},FhzP:function(module,e,n){"use strict";n.r(e);var t=n("VkAN"),r=n.n(t),o=n("VbXa"),a=n.n(o),c=n("17x9"),s=n.n(c),i=n("q1tI"),d=n.n(i),u=n("8cuT"),l=n("AeFk"),p=n("sQ/U"),f=n("7GkZ"),m=n("1r4n"),h=n("+eFp"),b=n.n(h),v=n("763+"),R=n("8Hdl"),g=n("epGu"),C=n("KmGn"),j=n.n(C);function _templateObject(){var e=r()(["\n                  margin: ",";\n                "]);return _templateObject=function _templateObject(){return e},e}function ThemeRenderProp(e){var n,t;return(0,e.children)({theme:Object(v.a)()})}var O=function(e){function CourseReferencesPage(n,t){var r;return(r=e.call(this,n,t)||this).componentWillReceiveProps=function(){r.getData()},r.getData=function(){var e=r.props.courseId,n=r.context.router.params;n.reference_id&&r.waitingForReferenceId!==n.reference_id&&(r.waitingForReferenceId=n.reference_id,r.context.executeAction(m.a,{courseId:e,referenceShortId:n.reference_id}))},r.getData(),r}a()(CourseReferencesPage,e);var n=CourseReferencesPage.prototype;return n.componentDidUpdate=function componentDidUpdate(e){var n,t,r;(null===(n=this.props)||void 0===n?void 0:null===(t=n.currentReference)||void 0===t?void 0:t.id)!==(null==e?void 0:null===(r=e.currentReference)||void 0===r?void 0:r.id)&&window.scrollTo(0,0)},n.renderLockedModal=function renderLockedModal(){return Object(l.d)(b.a,{transitionName:"LockedContentModal",transitionAppear:!0,transitionEnterTimeout:500,transitionAppearTimeout:500,transitionLeaveTimeout:500},Object(l.d)(g.a,{key:"LockedContentModal"}))},n.render=function render(){var e=this,n=!p.a.isAuthenticatedUser()||!this.props.isEnrolled;if(this.props.currentReference)return Object(l.d)(ThemeRenderProp,null,function(t){var r=t.theme;return Object(l.d)("section",{className:"rc-CourseReferencesPage"},n&&e.renderLockedModal(),Object(l.d)(R.a,{color:"body",variant:"h1semibold",css:Object(l.c)(_templateObject(),r.spacing(48,0,32,0))},e.props.currentReference.name),Object(l.d)(f.a,{key:e.props.currentReference.id,cml:e.props.currentReference.content,isCdsEnabled:!0}))});return null},CourseReferencesPage}(d.a.Component);O.propTypes={currentReference:s.a.shape({id:s.a.string.isRequired,content:s.a.object.isRequired,name:s.a.string.isRequired,shortId:s.a.string.isRequired}),courseId:s.a.string.isRequired,isEnrolled:s.a.bool,theme:s.a.object},O.contextTypes={executeAction:s.a.func.isRequired,router:s.a.object.isRequired},O.defaultProps={currentReference:null},e.default=Object(u.a)(O,["CourseStore","CourseReferencesStore","CourseMembershipStore"],function(e){var n=e.CourseStore,t=e.CourseReferencesStore,r=e.CourseMembershipStore;return{currentReference:t.getCurrentReference(),isEnrolled:r.isEnrolled(),courseId:n.getCourseId()}})},KmGn:function(module,exports,e){var n=e("yn2A"),t;"string"==typeof n&&(n=[[module.i,n,""]]);var r={transform:void 0},o=e("aET+")(n,r);n.locals&&(module.exports=n.locals)},epGu:function(module,e,n){"use strict";var t=n("VkAN"),r=n.n(t),o=n("VbXa"),a=n.n(o),c=n("pVnL"),s=n.n(c),i=n("q1tI"),d=n.n(i),u=n("AeFk"),l=n("w/1P"),p=n.n(l),f=n("17x9"),m=n.n(f),h=n("763+"),b=n("8Hdl"),v=n("fAYU"),R=n("+2ZD"),g=n("8cuT"),C=n("Ys1X"),j=n("eJMc"),O=n.n(j),k=n("37kS"),y=n.n(k),P=n("KmGn"),I=n.n(P);function _templateObject(){var e=r()(["\n              margin: ",";\n            "]);return _templateObject=function _templateObject(){return e},e}var w=function LockedContentModal(e){var n=Object(h.a)();return Object(u.d)(T,s()({},e,{theme:n}))},T=function(e){function LockedContentModalContent(){return e.apply(this,arguments)||this}var n;return a()(LockedContentModalContent,e),LockedContentModalContent.prototype.render=function render(){var e=this.props,n=e.courseRootPath,t=e.theme,r=e.className,o=p()("rc-LockedContentModal","styleguide",r);return Object(u.d)("div",{className:o},Object(u.d)(R.a,{allowClose:!1,modalName:y()("This content is locked")},Object(u.d)(b.a,{variant:"h1",css:Object(u.c)(_templateObject(),t.spacing(0,0,16,0))},y()("This content is locked.")),Object(u.d)(b.a,null,y()("You can access this content by enrolling in an active session of this course. To enroll or check active dates for upcoming sessions, visit the course description page.")),Object(u.d)("div",{className:"horizontal-box align-items-right wrap"},Object(u.d)(v.a,{to:Object(C.a)(n),component:O.a,variant:"standard",typographyVariant:"body1"},y()("View sessions")))))},LockedContentModalContent}(d.a.Component);T.propTypes={className:m.a.string,courseRootPath:m.a.string,theme:m.a.object},e.a=Object(g.a)(w,["CourseStore"],function(e){var n;return{courseRootPath:e.CourseStore.getCourseRootPath()}})},yn2A:function(module,exports,e){}}]);
//# sourceMappingURL=133.af56bc0f15cb89e07f22.js.map