(window.webpackJsonp=window.webpackJsonp||[]).push([[135],{"++4Y":function(module,t,e){"use strict";e.r(t),e.d(t,"styles",function(){return q}),e.d(t,"AttemptPage",function(){return M}),e.d(t,"withRedirectToCover",function(){return z});var n=e("VbXa"),i=e.n(n),o=e("q1tI"),a=e.n(o),r=e("MnCE"),s=e("AeFk"),u=e("+LJP"),c=e("8Hdl"),d=e("usGt"),m=e("LHEl"),l=e("3AF4"),b=e("rD/6"),p=e("tcH/"),f=e("s3XC"),v=e("aBK/"),S=e("uAMc"),g=e("RH4a"),O=e("zaiP"),j=e("wIYG"),h=e("kYu0"),y=e("FtgG"),C=e("8ec0"),D=e("l0R+"),I=e("RLfs"),T=e("pJaO"),k=e("kvW3"),A=e("v4A+"),P=e("kIii"),w=e("GZYn"),x=e("pH37"),L=e("VtNW"),B=e.n(L),q={header:function header(t){return Object(s.c)({marginBottom:t.spacing(48)})},points:function points(t){return Object(s.c)({marginTop:t.spacing(8)})},submissionGrade:function submissionGrade(t){return Object(s.c)({marginTop:t.spacing(12)})}},M=function(t){function AttemptPage(){for(var e,n=arguments.length,i=new Array(n),o=0;o<n;o++)i[o]=arguments[o];return(e=t.call.apply(t,[this].concat(i))||this).state={showSkillTaggingDialog:!1},e.handleKeepLearningClick=function(t){Object(x.b)(t)?e.props.redirectToNextItem():e.setState({showSkillTaggingDialog:!0})},e}i()(AttemptPage,t);var e=AttemptPage.prototype;return e.componentDidMount=function componentDidMount(){var t=this.props,e=t.shouldRedirectToCover,n=t.redirectToCover;e&&n()},e.render=function render(){var t=this,e=this.props,n=e.redirectToCover,i=e.redirectToNextItem,o=e.addRedirectToCoverToRouteParams,a=e.examSessionId,r=this.state.showSkillTaggingDialog;return Object(s.d)(O.a,null,function(e){var u=e.item;if(!u)return null;return Object(s.d)(h.a,{standardProctorConfigurationId:u.contentSummary&&"exam"===u.contentSummary.typeName&&u.contentSummary.definition.standardProctorConfigurationId||null},function(e){var d=e.shouldShowTimer,O=e.secondsLeftInLatestAttempt,h=e.refetch,x=e.remainingAttempts;return Object(s.d)(f.b,{onClose:n,backbuttonAriaLabel:B()("Back"),headerLeft:Object(s.d)(l.a,{headerText:u.name,itemTypeText:B()("Graded Quiz"),timeCommitment:u.timeCommitment}),headerRight:Object(s.d)(v.a,{courseId:u.courseId,itemId:u.id,examSessionId:a},function(t){var e=t.isSubmitted;return Object(s.d)(b.a,{deadline:u.deadline,remainingTimeInMs:"number"==typeof O?1e3*O:null,showTimer:d&&!e,timerId:Object(C.a)(u.id)})}),topBanner:Object(s.d)(v.a,{courseId:u.courseId,itemId:u.id,examSessionId:a},function(e){var i=e.isSubmitted,o,a=(u.contentSummary&&"exam"===u.contentSummary.typeName&&u.contentSummary.definition||{}).passingFraction;if(i&&u.itemGrade){var r,c=(u.itemGrade||{}).isPassed,d=Object(w.b)(c,u.isCumulativeGraded,x);return Object(s.d)(m.a,{courseId:u.courseId,itemId:u.id},function(e){var i=e.lockingConfigurationSummary,o=e.bestEvaluation;return Object(s.d)("div",null,Object(s.d)(y.a,{itemGrade:u.itemGrade,computedScore:(o||{}).score,maxScore:(o||{}).maxScore,passingFraction:a,isCumulativeGraded:u.isCumulativeGraded,onKeepLearningClick:function onKeepLearningClick(){return t.handleKeepLearningClick(u.courseId)},remainingAttempts:x,lockingConfigurationSummary:i,onTryAgainClick:n}),Object(T.b)()&&d===w.a.PASSED&&Object(s.d)(T.a,{courseId:u.courseId,itemId:u.id}))})}return null})},Object(s.d)(A.a,{courseId:u.courseId,itemId:u.id},function(t){var e=t.postQuizSuggestions,n=t.loading;return Object(s.d)(v.a,{courseId:u.courseId,itemId:u.id,onQuizSessionQueryCompleted:function onQuizSessionQueryCompleted(){return h()},examSessionId:a},function(t){var i=t.quizFormData,a=t.sessionId,r=t.nextSubmissionDraftId,m=t.attemptScore,l=t.totalPoints,b=t.quizQuestions,f=t.isSubmitted,v=t.hasDraft,O=t.isLimitedFeedback;if(!i||n||!b||!a)return Object(s.d)(j.a,null);if(O)return null;var h=b.map(function(t){return t.prompt.id});return Object(s.d)(g.a,{itemId:u.id,courseId:u.courseId},function(t){var n=t.stepState,b=t.setStepState;return Object(s.d)("div",null,Object(s.d)("div",{css:q.header},Object(s.d)(c.a,{variant:"h1semibold",component:"h2"},u.name),f&&"number"==typeof m?Object(s.d)("div",{css:q.submissionGrade},Object(s.d)(c.a,{variant:"h2semibold",component:"span"},B()("Latest Submission Grade #{grade}%",{grade:Object(D.a)(m)}))):Object(s.d)("div",{css:q.points},Object(s.d)(c.a,{variant:"h4bold",component:"span","aria-label":B()("Total available points #{totalPoints}",{totalPoints:l})},Object(s.d)("span",{"aria-hidden":!0},Object(s.d)(k.b,{message:B()("Total points {totalPoints}"),totalPoints:Object(s.d)(c.a,{variant:"body2",component:"span"},l)}))))),Object(s.d)("div",null,Object(s.d)(P.a,{quizFormData:i,isSubmitted:f,postQuizSuggestions:e,stepState:n})),Object(s.d)(S.a,{ids:h,sessionId:a,nextSubmissionDraftId:r},function(t){var e=t.hasUnfilledResponses,i=t.saveDraft,a=t.autoSaveDraft,r=t.submitDraft,c=t.submitLatestSubmissionDraft;return Object(s.d)(p.a,{hasUnfilledResponses:e,itemId:u.id,courseId:u.courseId,saveDraft:i,autoSaveDraft:a,isSubmitted:f,submitDraft:function submitDraft(){return r?r().then(function(){o()}):Promise.reject()},submitLatestSubmissionDraft:function submitLatestSubmissionDraft(){return c?c().then(function(){o()}):Promise.reject()},hasTimer:d,hasDraft:v,stepState:n,setStepState:b})}))})})}),r&&Object(s.d)(I.b,{hideDialogDuringLoading:!1,scrollLockQuerySelector:".rc-TunnelVisionWrapper__content",dialogTitle:B()("Help us identify the right topics"),dialogDescription:B()("Select the skills that are covered in this quiz"),onClose:i,courseId:u.courseId,itemId:u.id}))})})},AttemptPage}(a.a.Component),z=Object(r.compose)(d.a,Object(u.a)(function(t,e){var n=e.nextItemUrl,i=void 0===n?"":n,o=e.refetchCoverPageData,a=e.refreshProgress,r=function redirectToCover(){o&&o(),t.push({name:"quiz-cover",params:t.params,query:t.location.query}),a()};return{redirectToCover:r,redirectToNextItem:function redirectToNextItem(){i?(t.push(i),a()):r()},addRedirectToCoverToRouteParams:function addRedirectToCoverToRouteParams(){t.push({name:"quiz-attempt",params:t.params,query:{redirectToCover:!0}})},shouldRedirectToCover:t.location.query.redirectToCover}}));t.default=z(M)},PvFy:function(module,t,e){"use strict";var n=e("q1tI"),i=e.n(n),o=e("AeFk"),a=e("8Hdl"),r=e("HDKb"),s=e("5mFy"),u=e("7Dw/"),c=e("/96O"),d=e("kvW3"),m=e("VtNW"),l=e.n(m),b={root:function root(t){return{display:"flex",margin:t.spacing(48,0),".cds-Accordion-focusContainer":{padding:"0 !important"},".cds-AccordionHeader-button":{padding:"0 !important"},".css-1rj0txn-AccordionPanel":{marginLeft:"0 !important"},".cds-Accordion-container":{border:"none !important"},".cds-AccordionHeader-button:hover":{backgroundColor:"transparent !important"}}},textBlockBody:function textBlockBody(t,e){return{marginTop:e?t.spacing(16):0,".rc-CML":{display:"inline-block"}}},numberCell:{width:"26px"},contentCell:{width:"calc(100% - 26px)"}},p=function TextBlock(t){var e=t.textBlock,n=t.isCollapsible,i=void 0!==n&&n,m=t.index,p=Object(o.d)(a.a,{css:function css(t){return b.textBlockBody(t,i)},variant:"body1"},Object(o.d)(c.a,{value:null==e?void 0:e.body}));return Object(o.d)("div",{css:b.root},Object(o.d)("div",{css:b.numberCell},Object(o.d)(a.a,{variant:"h3bold","aria-hidden":!0},Object(o.d)(d.c,{value:m+1,"aria-hidden":!0}),"."),Object(o.d)("span",{className:"screenreader-only"},Object(o.d)(d.b,{message:l()("Question {questionNumber}"),questionNumber:m+1}))),Object(o.d)("div",{css:b.contentCell},i?Object(o.d)(r.a,{variant:"silent"},Object(o.d)(s.a,{label:null==e?void 0:e.title}),Object(o.d)(u.a,null,p)):p))};t.a=p},kIii:function(module,t,e){"use strict";var n=e("q1tI"),i=e.n(n),o=e("PvFy"),a=e("9iH2"),r=e("LXPe"),s=e("uFBg"),u=function QuizPartRenderer(t){var e=t.quizFormData,u=t.isSubmitted,c=t.postQuizSuggestions,d=t.stepState,m=Object(r.a)();return i.a.createElement(n.Fragment,null,e.map(function(t,e){if(m&&(null==t?void 0:t.typeName)===s.b||(null==t?void 0:t.typeName)===s.a)return i.a.createElement(o.a,{textBlock:null==t?void 0:t.definition,isCollapsible:(null==t?void 0:t.typeName)===s.a,index:e});var n=m&&(null==t?void 0:t.definition)||t;return i.a.createElement(a.a,{key:null==n?void 0:n.prompt.id,quizQuestion:n,index:e,postQuizSuggestions:(u&&c||[]).find(function(t){return t.id===n.prompt.id}),isReadOnly:!!u,isDisabled:!!(null==d?void 0:d.isSaving)||!!(null==d?void 0:d.isSubmitting)})}))};t.a=u},"tcH/":function(module,t,e){"use strict";var n=e("lSNA"),i=e.n(n),o=e("VbXa"),a=e.n(o),r=e("q1tI"),s=e.n(r),u=e("d3Ej"),c=e.n(u),d=e("AeFk"),m=e("HOoY"),l=e("zaiP"),b=e("YmkS"),p=e("qJwm"),f=e("UUQQ"),v=e("CnKM"),S=e("8ec0"),g=e("KvdX"),O=e("rQpo"),j=e("Cqp/"),h=e("ZJgU"),y=e("8xbv"),C=e("PB6g");function ownKeys(t,e){var n=Object.keys(t);if(Object.getOwnPropertySymbols){var i=Object.getOwnPropertySymbols(t);e&&(i=i.filter(function(e){return Object.getOwnPropertyDescriptor(t,e).enumerable})),n.push.apply(n,i)}return n}function _objectSpread(t){for(var e=1;e<arguments.length;e++){var n=null!=arguments[e]?arguments[e]:{};e%2?ownKeys(Object(n),!0).forEach(function(e){i()(t,e,n[e])}):Object.getOwnPropertyDescriptors?Object.defineProperties(t,Object.getOwnPropertyDescriptors(n)):ownKeys(Object(n)).forEach(function(e){Object.defineProperty(t,e,Object.getOwnPropertyDescriptor(n,e))})}return t}var D={root:function root(t){return Object(d.c)({margin:t.spacing(48,0)})},buttonsContainer:function buttonsContainer(t){return Object(d.c)({marginTop:t.spacing(32),paddingBottom:t.spacing(48),borderBottom:"1px solid ".concat(t.palette.gray[300]),marginBottom:t.spacing(12)})},saveButton:function saveButton(t){return Object(d.c)({marginLeft:t.spacing(24)})}},I=function(t){function SubmissionControls(){for(var e,n=arguments.length,i=new Array(n),o=0;o<n;o++)i[o]=arguments[o];return(e=t.call.apply(t,[this].concat(i))||this).state={canSubmit:!1,announcement:""},e.onSubmitClick=function(){var t=e.props,n=t.hasUnfilledResponses,i=t.updateAndSubmitDraft,o=t.showModal,a=function trackedUpdateAndSubmitDraft(){return i&&i().then(function(){m.a.trackComponent({namespace:{app:"open_course",page:"item_page"}},{},"submit","quiz")})};n?o({type:g.a.unansweredQuestions,props:{onPrimaryButtonClick:a}}):i&&(e.setAnnouncement(c()("Submitting assignment")),a())},e.enableSubmit=function(){e.setState({canSubmit:!0})},e.disableSubmit=function(){e.setState({canSubmit:!1})},e.autoSubmit=function(){var t=e.props,n=t.showModal,i=t.hideModal,o=t.submitLatestSubmissionDraft;n({type:g.a.timeExpiredModal,props:{onPrimaryButtonClick:i}}),o&&o()},e.startAutoSubmitTimeout=function(){var t=e.props.expiresAt;"number"==typeof t&&(e.autoSubmitTimeout=window.setTimeout(e.autoSubmit,t-Date.now()))},e.setAnnouncement=function(t){e.setState({announcement:t},function(){setTimeout(function(){e.setState({announcement:""})},150)})},e.updateDraft=function(){var t=e.props.updateDraft;t&&(e.setAnnouncement(c()("Saving draft submission")),t().then(function(){e.setAnnouncement(c()("Draft submission saved successfully"))}))},e}a()(SubmissionControls,t);var e=SubmissionControls.prototype;return e.componentDidMount=function componentDidMount(){var t=this.props,e=t.hasDraft,n=t.autoSaveDraft,i=t.submitLatestSubmissionDraft;!e&&n&&n(),i&&this.startAutoSubmitTimeout()},e.componentDidUpdate=function componentDidUpdate(t){var e=this.props.expiresAt;t.expiresAt!==e&&this.autoSubmitTimeout&&(clearTimeout(this.autoSubmitTimeout),this.autoSubmitTimeout=null,this.startAutoSubmitTimeout())},e.componentWillUnmount=function componentWillUnmount(){this.autoSubmitTimeout&&clearTimeout(this.autoSubmitTimeout)},e.render=function render(){var t=this,e=this.props,n=e.stepState,i=n.isSaving,o=n.isSubmitting,a=n.isAutoSaving,r=e.stepState,s=e.setStepState,u=e.updateDraft,m=e.updateAndSubmitDraft,v=this.state,S=v.canSubmit,g=v.announcement,I=i||a,T=I||o;return Object(d.d)(l.a,null,function(e){var n=e.item;if(n&&n.isPremiumGradingLocked)return Object(d.d)("div",null,Object(d.d)(O.a,{courseId:n.courseId}));return Object(d.d)("div",{css:D.root},Object(d.d)("div",null,Object(d.d)(f.a,{onAgreementComplete:t.enableSubmit,onAgreementIncomplete:t.disableSubmit})),Object(d.d)("div",{css:D.buttonsContainer},m&&Object(d.d)(h.a,{iconPosition:"before",icon:o?Object(d.d)(y.a,null):void 0,onClick:t.onSubmitClick,disabled:T||!S,variant:"primary"},o?c()("Submitting…"):c()("Submit")),u&&Object(d.d)(h.a,{iconPosition:"before",icon:I?Object(d.d)(y.a,null):void 0,onClick:t.updateDraft,disabled:!!T,variant:"secondary",css:D.saveButton},I?c()("Saving..."):c()("Save draft"))),Object(d.d)(b.a,{computedItem:n,itemFeedbackType:p.c.Quiz}),Object(d.d)(C.b,{tagName:"span",role:"region","aria-live":"assertive","aria-atomic":!0},g&&Object(d.d)("span",null,g)),Object(d.d)(j.a,{stepState:r,setStepState:s}))})},SubmissionControls}(s.a.Component),T=function SubmissionControlsContainer(t){var e=t.saveDraft,n=t.submitDraft,i=t.hasTimer,o=t.itemId,a=t.isSubmitted;if(!e||!n)return null;if(!i)return Object(d.d)(g.b,null,function(i){var o=i.showModal,r=i.hideModal;return a?null:Object(d.d)(I,_objectSpread(_objectSpread({},t),{},{updateDraft:e,updateAndSubmitDraft:n,showModal:o,hideModal:r}))});return Object(d.d)(v.a,{id:Object(S.a)(o)},function(i){var o=i.expiresAt;return Object(d.d)(g.b,null,function(i){var r=i.showModal,s=i.hideModal;return a?null:Object(d.d)(I,_objectSpread(_objectSpread({},t),{},{updateDraft:e,updateAndSubmitDraft:n,showModal:r,hideModal:s,expiresAt:o}))})})};t.a=T}}]);
//# sourceMappingURL=135.45959eb4fe59b70e7972.js.map