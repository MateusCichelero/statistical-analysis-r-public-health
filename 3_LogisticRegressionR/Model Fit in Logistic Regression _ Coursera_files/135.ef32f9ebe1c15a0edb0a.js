(window.webpackJsonp=window.webpackJsonp||[]).push([[135],{"++4Y":function(module,t,e){"use strict";e.r(t),e.d(t,"styles",function(){return q}),e.d(t,"AttemptPage",function(){return R}),e.d(t,"withRedirectToCover",function(){return z});var n=e("VbXa"),i=e.n(n),o=e("q1tI"),r=e.n(o),a=e("MnCE"),u=e("AeFk"),s=e("+LJP"),c=e("8Hdl"),d=e("usGt"),m=e("LHEl"),l=e("3AF4"),p=e("rD/6"),b=e("tcH/"),f=e("s3XC"),S=e("aBK/"),v=e("uAMc"),g=e("RH4a"),O=e("zaiP"),j=e("wIYG"),h=e("kYu0"),y=e("FtgG"),D=e("8ec0"),I=e("l0R+"),T=e("RLfs"),C=e("pJaO"),A=e("kvW3"),P=e("v4A+"),k=e("kIii"),w=e("GZYn"),x=e("VtNW"),L=e.n(x),q={header:function header(t){return Object(u.c)({marginBottom:t.spacing(48)})},points:function points(t){return Object(u.c)({marginTop:t.spacing(8)})},submissionGrade:function submissionGrade(t){return Object(u.c)({marginTop:t.spacing(12)})}},R=function(t){function AttemptPage(){for(var e,n=arguments.length,i=new Array(n),o=0;o<n;o++)i[o]=arguments[o];return(e=t.call.apply(t,[this].concat(i))||this).state={showSkillTaggingDialog:!1},e.handleKeepLearningClick=function(){e.setState({showSkillTaggingDialog:!0})},e}i()(AttemptPage,t);var e=AttemptPage.prototype;return e.componentDidMount=function componentDidMount(){var t=this.props,e=t.shouldRedirectToCover,n=t.redirectToCover;e&&n()},e.render=function render(){var t=this,e=this.props,n=e.redirectToCover,i=e.redirectToNextItem,o=e.addRedirectToCoverToRouteParams,r=e.examSessionId,a=this.state.showSkillTaggingDialog;return Object(u.d)(O.a,null,function(e){var s=e.item;if(!s)return null;return Object(u.d)(h.a,{standardProctorConfigurationId:s.contentSummary&&"exam"===s.contentSummary.typeName&&s.contentSummary.definition.standardProctorConfigurationId||null},function(e){var d=e.shouldShowTimer,O=e.secondsLeftInLatestAttempt,h=e.refetch,x=e.remainingAttempts;return Object(u.d)(f.b,{onClose:n,backbuttonAriaLabel:L()("Back"),headerLeft:Object(u.d)(l.a,{headerText:s.name,itemTypeText:L()("Graded Quiz"),timeCommitment:s.timeCommitment}),headerRight:Object(u.d)(S.a,{courseId:s.courseId,itemId:s.id,examSessionId:r},function(t){var e=t.isSubmitted;return Object(u.d)(p.a,{deadline:s.deadline,remainingTimeInMs:"number"==typeof O?1e3*O:null,showTimer:d&&!e,timerId:Object(D.a)(s.id)})}),topBanner:Object(u.d)(S.a,{courseId:s.courseId,itemId:s.id,examSessionId:r},function(e){var i=e.isSubmitted,o,r=(s.contentSummary&&"exam"===s.contentSummary.typeName&&s.contentSummary.definition||{}).passingFraction;if(i&&s.itemGrade){var a,c=(s.itemGrade||{}).isPassed,d=Object(w.b)(c,s.isCumulativeGraded,x);return Object(u.d)(m.a,{courseId:s.courseId,itemId:s.id},function(e){var i=e.lockingConfigurationSummary,o=e.bestEvaluation;return Object(u.d)("div",null,Object(u.d)(y.a,{itemGrade:s.itemGrade,computedScore:(o||{}).score,maxScore:(o||{}).maxScore,passingFraction:r,isCumulativeGraded:s.isCumulativeGraded,onKeepLearningClick:t.handleKeepLearningClick,remainingAttempts:x,lockingConfigurationSummary:i,onTryAgainClick:n}),Object(C.b)()&&d===w.a.PASSED&&Object(u.d)(C.a,{courseId:s.courseId,itemId:s.id}))})}return null})},Object(u.d)(P.a,{courseId:s.courseId,itemId:s.id},function(t){var e=t.postQuizSuggestions,n=t.loading;return Object(u.d)(S.a,{courseId:s.courseId,itemId:s.id,onQuizSessionQueryCompleted:function onQuizSessionQueryCompleted(){return h()},examSessionId:r},function(t){var i=t.quizFormData,r=t.sessionId,a=t.nextSubmissionDraftId,m=t.attemptScore,l=t.totalPoints,p=t.quizQuestions,f=t.isSubmitted,S=t.hasDraft,O=t.isLimitedFeedback;if(!i||n||!p||!r)return Object(u.d)(j.a,null);if(O)return null;var h=p.map(function(t){return t.prompt.id});return Object(u.d)(g.a,{itemId:s.id,courseId:s.courseId},function(t){var n=t.stepState,g=t.setStepState;return Object(u.d)("div",null,Object(u.d)("div",{css:q.header},Object(u.d)(c.a,{variant:"h1semibold",component:"h2"},s.name),f&&"number"==typeof m?Object(u.d)("div",{css:q.submissionGrade},Object(u.d)(c.a,{variant:"h2semibold",component:"span"},L()("Latest Submission Grade #{grade}%",{grade:Object(I.a)(m)}))):Object(u.d)("div",{css:q.points},Object(u.d)(c.a,{variant:"h4bold",component:"span","aria-label":L()("Total available points #{totalPoints}",{totalPoints:l})},Object(u.d)("span",{"aria-hidden":!0},Object(u.d)(A.b,{message:L()("Total points {totalPoints}"),totalPoints:Object(u.d)(c.a,{variant:"body2",component:"span"},l)}))))),Object(u.d)("div",null,Object(u.d)(k.a,{quizFormData:i,questions:p,isSubmitted:f,postQuizSuggestions:e,stepState:n})),Object(u.d)(v.a,{ids:h,sessionId:r,nextSubmissionDraftId:a},function(t){var e=t.hasUnfilledResponses,i=t.saveDraft,r=t.autoSaveDraft,a=t.submitDraft,c=t.submitLatestSubmissionDraft;return Object(u.d)(b.a,{hasUnfilledResponses:e,itemId:s.id,courseId:s.courseId,saveDraft:i,autoSaveDraft:r,isSubmitted:f,submitDraft:function submitDraft(){return a?a().then(function(){o()}):Promise.reject()},submitLatestSubmissionDraft:function submitLatestSubmissionDraft(){return c?c().then(function(){o()}):Promise.reject()},hasTimer:d,hasDraft:S,stepState:n,setStepState:g})}))})})}),a&&Object(u.d)(T.b,{hideDialogDuringLoading:!1,scrollLockQuerySelector:".rc-TunnelVisionWrapper__content",dialogTitle:L()("Help us identify the right topics"),dialogDescription:L()("Select the skills that are covered in this quiz"),onClose:i,courseId:s.courseId,itemId:s.id}))})})},AttemptPage}(r.a.Component),z=Object(a.compose)(d.a,Object(s.a)(function(t,e){var n=e.nextItemUrl,i=void 0===n?"":n,o=e.refetchCoverPageData,r=e.refreshProgress,a=function redirectToCover(){o&&o(),t.push({name:"quiz-cover",params:t.params,query:t.location.query}),r()};return{redirectToCover:a,redirectToNextItem:function redirectToNextItem(){i?(t.push(i),r()):a()},addRedirectToCoverToRouteParams:function addRedirectToCoverToRouteParams(){t.push({name:"quiz-attempt",params:t.params,query:{redirectToCover:!0}})},shouldRedirectToCover:t.location.query.redirectToCover}}));t.default=z(R)},kIii:function(module,t,e){"use strict";var n=e("q1tI"),i=e.n(n),o=e("muFd"),r=e("9iH2"),a=e("LXPe"),u=function QuizPartRenderer(t){var e=t.quizFormData,u=t.questions,s=t.isSubmitted,c=t.postQuizSuggestions,d=t.stepState,m=Object(a.a)();return i.a.createElement(n.Fragment,null,e.map(function(t){if(m&&"textSection"===(null==t?void 0:t.typeName))return i.a.createElement(o.a,{textSection:null==t?void 0:t.definition});var e=m&&(null==t?void 0:t.definition)||t;return i.a.createElement(r.a,{key:null==e?void 0:e.prompt.id,quizQuestion:e,index:u.findIndex(function(t){var n;return(null==t?void 0:null===(n=t.prompt)||void 0===n?void 0:n.id)===(null==e?void 0:e.prompt.id)}),postQuizSuggestions:(s&&c||[]).find(function(t){return t.id===e.prompt.id}),isReadOnly:!!s,isDisabled:!!(null==d?void 0:d.isSaving)||!!(null==d?void 0:d.isSubmitting)})}))};t.a=u},muFd:function(module,t,e){"use strict";var n=e("q1tI"),i=e.n(n),o=e("AeFk"),r=e("4Ss+"),a=e("HDKb"),u=e("5mFy"),s=e("7Dw/"),c=e("8Hdl"),d=e("/96O"),m={root:function root(t){return{marginBottom:t.spacing(24),".cds-Accordion-container":{border:"1px solid ".concat(t.palette.gray[300]),'button[aria-expanded="true"]':{background:t.palette.gray[100],borderBottomLeftRadius:0,borderBottomRightRadius:0},"&.cds-Accordion-hover button":{background:t.palette.gray[100]}}}},textSectionBody:function textSectionBody(t){return{marginTop:t.spacing(16)}}},l=function TextSection(t){var e=t.textSection;return Object(o.d)(r.a,{css:m.root},Object(o.d)(a.a,{variant:"standard"},Object(o.d)(u.a,{label:null==e?void 0:e.title}),Object(o.d)(s.a,null,Object(o.d)(c.a,{color:"supportText",css:m.textSectionBody,variant:"body2"},Object(o.d)(d.a,{value:null==e?void 0:e.body})))))};t.a=l},"tcH/":function(module,t,e){"use strict";var n=e("lSNA"),i=e.n(n),o=e("VbXa"),r=e.n(o),a=e("q1tI"),u=e.n(a),s=e("d3Ej"),c=e.n(s),d=e("AeFk"),m=e("HOoY"),l=e("zaiP"),p=e("YmkS"),b=e("qJwm"),f=e("UUQQ"),S=e("CnKM"),v=e("8ec0"),g=e("KvdX"),O=e("rQpo"),j=e("Cqp/"),h=e("ZJgU"),y=e("8xbv"),D=e("PB6g");function ownKeys(t,e){var n=Object.keys(t);if(Object.getOwnPropertySymbols){var i=Object.getOwnPropertySymbols(t);e&&(i=i.filter(function(e){return Object.getOwnPropertyDescriptor(t,e).enumerable})),n.push.apply(n,i)}return n}function _objectSpread(t){for(var e=1;e<arguments.length;e++){var n=null!=arguments[e]?arguments[e]:{};e%2?ownKeys(Object(n),!0).forEach(function(e){i()(t,e,n[e])}):Object.getOwnPropertyDescriptors?Object.defineProperties(t,Object.getOwnPropertyDescriptors(n)):ownKeys(Object(n)).forEach(function(e){Object.defineProperty(t,e,Object.getOwnPropertyDescriptor(n,e))})}return t}var I={root:function root(t){return Object(d.c)({margin:t.spacing(48,0)})},buttonsContainer:function buttonsContainer(t){return Object(d.c)({marginTop:t.spacing(32),paddingBottom:t.spacing(48),borderBottom:"1px solid ".concat(t.palette.gray[300]),marginBottom:t.spacing(12)})},saveButton:function saveButton(t){return Object(d.c)({marginLeft:t.spacing(24)})}},T=function(t){function SubmissionControls(){for(var e,n=arguments.length,i=new Array(n),o=0;o<n;o++)i[o]=arguments[o];return(e=t.call.apply(t,[this].concat(i))||this).state={canSubmit:!1,announcement:""},e.onSubmitClick=function(){var t=e.props,n=t.hasUnfilledResponses,i=t.updateAndSubmitDraft,o=t.showModal,r=function trackedUpdateAndSubmitDraft(){return i&&i().then(function(){m.a.trackComponent({namespace:{app:"open_course",page:"item_page"}},{},"submit","quiz")})};n?o({type:g.a.unansweredQuestions,props:{onPrimaryButtonClick:r}}):i&&(e.setAnnouncement(c()("Submitting assignment")),r())},e.enableSubmit=function(){e.setState({canSubmit:!0})},e.disableSubmit=function(){e.setState({canSubmit:!1})},e.autoSubmit=function(){var t=e.props,n=t.showModal,i=t.hideModal,o=t.submitLatestSubmissionDraft;n({type:g.a.timeExpiredModal,props:{onPrimaryButtonClick:i}}),o&&o()},e.startAutoSubmitTimeout=function(){var t=e.props.expiresAt;"number"==typeof t&&(e.autoSubmitTimeout=window.setTimeout(e.autoSubmit,t-Date.now()))},e.setAnnouncement=function(t){e.setState({announcement:t},function(){setTimeout(function(){e.setState({announcement:""})},150)})},e.updateDraft=function(){var t=e.props.updateDraft;t&&(e.setAnnouncement(c()("Saving draft submission")),t().then(function(){e.setAnnouncement(c()("Draft submission saved successfully"))}))},e}r()(SubmissionControls,t);var e=SubmissionControls.prototype;return e.componentDidMount=function componentDidMount(){var t=this.props,e=t.hasDraft,n=t.autoSaveDraft,i=t.submitLatestSubmissionDraft;!e&&n&&n(),i&&this.startAutoSubmitTimeout()},e.componentDidUpdate=function componentDidUpdate(t){var e=this.props.expiresAt;t.expiresAt!==e&&this.autoSubmitTimeout&&(clearTimeout(this.autoSubmitTimeout),this.autoSubmitTimeout=null,this.startAutoSubmitTimeout())},e.componentWillUnmount=function componentWillUnmount(){this.autoSubmitTimeout&&clearTimeout(this.autoSubmitTimeout)},e.render=function render(){var t=this,e=this.props,n=e.stepState,i=n.isSaving,o=n.isSubmitting,r=n.isAutoSaving,a=e.stepState,u=e.setStepState,s=e.updateDraft,m=e.updateAndSubmitDraft,S=this.state,v=S.canSubmit,g=S.announcement,T=i||r,C=T||o;return Object(d.d)(l.a,null,function(e){var n=e.item;if(n&&n.isPremiumGradingLocked)return Object(d.d)("div",null,Object(d.d)(O.a,{courseId:n.courseId}));return Object(d.d)("div",{css:I.root},Object(d.d)("div",null,Object(d.d)(f.a,{onAgreementComplete:t.enableSubmit,onAgreementIncomplete:t.disableSubmit})),Object(d.d)("div",{css:I.buttonsContainer},m&&Object(d.d)(h.a,{iconPosition:"before",icon:o?Object(d.d)(y.a,null):void 0,onClick:t.onSubmitClick,disabled:C||!v,variant:"primary"},o?c()("Submitting…"):c()("Submit")),s&&Object(d.d)(h.a,{iconPosition:"before",icon:T?Object(d.d)(y.a,null):void 0,onClick:t.updateDraft,disabled:!!C,variant:"secondary",css:I.saveButton},T?c()("Saving..."):c()("Save draft"))),Object(d.d)(p.a,{computedItem:n,itemFeedbackType:b.c.Quiz}),Object(d.d)(D.b,{tagName:"span",role:"region","aria-live":"assertive","aria-atomic":!0},g&&Object(d.d)("span",null,g)),Object(d.d)(j.a,{stepState:a,setStepState:u}))})},SubmissionControls}(u.a.Component),C=function SubmissionControlsContainer(t){var e=t.saveDraft,n=t.submitDraft,i=t.hasTimer,o=t.itemId,r=t.isSubmitted;if(!e||!n)return null;if(!i)return Object(d.d)(g.b,null,function(i){var o=i.showModal,a=i.hideModal;return r?null:Object(d.d)(T,_objectSpread(_objectSpread({},t),{},{updateDraft:e,updateAndSubmitDraft:n,showModal:o,hideModal:a}))});return Object(d.d)(S.a,{id:Object(v.a)(o)},function(i){var o=i.expiresAt;return Object(d.d)(g.b,null,function(i){var a=i.showModal,u=i.hideModal;return r?null:Object(d.d)(T,_objectSpread(_objectSpread({},t),{},{updateDraft:e,updateAndSubmitDraft:n,showModal:a,hideModal:u,expiresAt:o}))})})};t.a=C}}]);
//# sourceMappingURL=135.ef32f9ebe1c15a0edb0a.js.map