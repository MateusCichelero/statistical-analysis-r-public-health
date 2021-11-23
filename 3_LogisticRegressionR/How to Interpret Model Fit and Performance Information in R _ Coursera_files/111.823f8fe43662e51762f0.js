(window.webpackJsonp=window.webpackJsonp||[]).push([[111],{"2S0G":function(module,e,t){"use strict";var n=t("q1tI"),i=t.n(n),a=t("AeFk"),c=t("kvW3"),r=t("/Fji"),o=t("TpV6"),s=t("8Hdl"),d=t("vrZb"),u=t.n(d),l={titleContainer:function titleContainer(e){return Object(a.c)({margin:e.spacing(12,0)})},description:function description(e){return Object(a.c)({marginBottom:e.spacing(4)})}},b=function LearningObjectivesWithItemsList(e){var t=e.learningObjectives;return Object(a.d)("div",null,null==t?void 0:t.map(function(e,t){var n=e.id,i=e.description,d=e.computedItems;return Object(a.d)("div",{key:"learning-objective-".concat(n)},Object(a.d)(r.a,{key:n,title:Object(a.d)("div",{css:l.titleContainer},Object(a.d)(s.a,{variant:"h2semibold",component:"h3",css:l.description},i),Object(a.d)(s.a,{variant:"body1"},Object(a.d)(c.b,{message:u()("{numItems, plural, =0 {Nothing} one {# material} other {# materials}} to review"),numItems:(null==d?void 0:d.length)||0}))),isCollapsible:!!(null==d?void 0:d.length),collapsibleIconTitle:d&&(null==d?void 0:d.length)>1?u()("Materials to review"):u()("Material to review"),startCollapsed:t>0||!(null==d?void 0:d.length)},null==d?void 0:d.map(function(e){return Object(a.d)("div",{key:"learning-objective-item-".concat(n,"-").concat(e.id)},Object(a.d)(o.a,{computedItem:e}))})))}))};e.a=b},"2iEy":function(module,e,t){"use strict";t.d(e,"a",function(){return g}),t.d(e,"d",function(){return f}),t.d(e,"c",function(){return h});var n=t("lSNA"),i=t.n(n),a=t("J2iB"),c=t.n(a),r=t("q1tI"),o=t.n(r),s=t("AeFk"),d=t("763+"),u=t("8Hdl"),l=t("PSZr"),b=t("IXUt"),m=t("d3Ej"),p=t.n(m),j=function getRootStyles(e,t){return Object(s.c)(i()({padding:e.spacing(24,0),borderBottom:"1px solid ".concat(e.palette.gray[300]),display:"flex",justifyContent:"space-between",position:"relative",flexWrap:"wrap",flexDirection:t?"row-reverse":"row",":last-child":{paddingBottom:e.spacing(48)}},e.breakpoints.down("sm"),{flexDirection:"column"}))},v=function statusIcon(e){return Object(s.c)({marginRight:e.spacing(8),lineHeight:2})},O=function stepTitle(e){return Object(s.c)({display:"flex",marginBottom:e.spacing(8)})},g={check:"check",warning:"warning"},f=function getStatusIconTypeForGradeRow(e,t,n){if(c()(e))return null;if(!e&&(!t||n&&t))return g.warning;return g.check},h=function getStatusIconTitleForGradeRow(e,t){if(c()(e))return;if(!e&&!t)return p()("Activity Incomplete");return p()("Activity Completed")},y=function CoverPageRow(e){var t=e.stepTitle,n=e.stepDetails,i=e.rightSideView,a=e.statusIconType,c=e.statusIconTitle,r=Object(d.a)();return Object(s.d)("div",{css:j(r,!t)},t&&Object(s.d)("div",null,Object(s.d)("div",{css:O(r)},a&&Object(s.d)("div",{css:v(r),"aria-hidden":"true"},a===g.check&&Object(s.d)(l.a,{size:"medium",color:"success",title:c}),a===g.warning&&Object(s.d)(b.a,{size:"medium",color:"error",title:c})),Object(s.d)(u.a,{variant:"h3bold"},t)),Object(s.d)("div",null,n)),Object(s.d)("div",null,i))};e.b=y},"71Lh":function(module,e,t){"use strict";t.r(e),t.d(e,"PracticeQuizCoverPage",function(){return T});var n=t("q1tI"),i=t.n(n),a=t("AeFk"),c=t("MnCE"),r=t("sBWo"),o=t("8mOo"),s=t("YdC/"),d=t("iMVg"),u=t("+LJP"),l=t("6/Gu"),b=t("2iEy"),m=t("hgsf"),p=t("jXJP"),j=t("NXzb"),v=t("YmkS"),O=t("qJwm"),g=t("zaiP"),f=t("oJmH"),h=t.n(f),y=t("fHLu"),w=t("JEIr"),S=t("VtNW"),x=t.n(S),I=t("VMeS"),k=t("sQ/U"),T=function PracticeQuizCoverPage(e){var t=e.nextItemUrl,n=e.children,c=e.openPracticeQuizAttemptPage,r=e.openSubmittedPracticeQuizAttemptPage;return Object(a.d)(g.a,null,function(e){var o=e.item;if(!o||!o.contentSummary)return Object(a.d)(I.a,null);return Object(a.d)(w.a,{slug:o.courseSlug,itemId:o.id,userId:k.a.get().id},function(e){var u=e.loading,g=e.bestSessionId,h=e.bestEvaluation,w=e.lastSessionId,S=e.unsubmittedSessionId,k=e.refetch;if(!o||!o.contentSummary||u)return Object(a.d)(I.a,null);if("quiz"===o.contentSummary.typeName){var T,C,A=(o.contentSummary.definition||{}).passingFraction,P=h||{},L=P.score,R=void 0===L?0:L,z=P.maxScore,F=void 0===z?0:z,B=h?F&&R/F:void 0,D=B?B>=A:void 0,J=o.isCumulativeGraded;return Object(a.d)(l.a,{container:!0},Object(a.d)(l.a,{item:!0,xs:12},Object(a.d)(s.a,{assignmentTypeName:x()("Practice Quiz"),item:o,subHeader:Object(a.d)(d.a,{itemId:null==o?void 0:o.id,courseId:null==o?void 0:o.courseId,courseSlug:null==o?void 0:o.courseSlug,learningObjectiveIds:null==o?void 0:o.learningObjectiveIds})})),Object(a.d)(l.a,{item:!0,xs:12},Object(a.d)(b.b,{stepTitle:x()("Submit your assignment"),rightSideView:Object(a.d)(f.ApolloConsumer,null,function(e){return Object(a.d)(p.a,{startAttempt:w||S?void 0:c,resumeAttempt:S?function(){Object(y.a)(e),c()}:void 0,retryAttempt:function retryAttempt(){Object(y.a)(e),c()}})}),statusIconType:g?b.a.check:void 0,statusIconTitle:g?x()("Activity Completed"):void 0}),Object(a.d)(b.b,{stepTitle:x()("Receive grade"),stepDetails:Object(a.d)(m.a,{passingFraction:A}),rightSideView:Object(a.d)(f.ApolloConsumer,null,function(e){return Object(a.d)(j.a,{itemGrade:"number"==typeof B?{grade:B,isPassed:D||!1,isOverridden:!1,latePenaltyRatio:null}:null,viewFeedback:w?function(){Object(y.a)(e),r()}:void 0,isViewFeedbackButtonLinkStyle:!!S})}),statusIconType:Object(b.d)(D,J),statusIconTitle:Object(b.c)(D,J)})),Object(a.d)("div",{css:function css(e){return Object(a.c)({marginTop:e.spacing(12)})}},Object(a.d)(v.a,{computedItem:o,itemFeedbackType:O.c.Quiz})),n&&k&&i.a.cloneElement(n,{refetchPracticeQuizCoverPageData:function refetchPracticeQuizCoverPageData(){return k().then(function(){return o.refetch()})},nextItemUrl:t}))}return null})})},C=Object(c.compose)(o.a,Object(u.a)(function(e){return{openPracticeQuizAttemptPage:function openPracticeQuizAttemptPage(){e.push({name:"practice-quiz-attempt",params:e.params})},openSubmittedPracticeQuizAttemptPage:function openSubmittedPracticeQuizAttemptPage(){e.push({name:"practice-quiz-view-attempt",params:e.params})}}}))(T);e.default=Object(r.a)(C)},"8mOo":function(module,e,t){"use strict";var n=t("8cuT"),i=t("MnCE"),a=Object(i.compose)(Object(n.a)(["CourseStore"],function(e,t){var n,i=e.CourseStore,a=t.itemMetadata,c,r,o=i.getMaterials().getNeighbors(a).nextItemMetadataOrItemGroup;return{nextItemUrl:null!==(n=null==o?void 0:o.getLink())&&void 0!==n?n:""}}));e.a=a},NXzb:function(module,e,t){"use strict";var n=t("VkAN"),i=t.n(n),a=t("UB5X"),c=t.n(a),r=t("q1tI"),o=t.n(r),s=t("AeFk"),d=t("763+"),u=t("8Hdl"),l=t("ZJgU"),b=t("hbUA"),m=t("kvW3"),p=t("bOpO"),j=t("d3Ej"),v=t.n(j),O=t("PB6g");function _templateObject5(){var e=i()(["\n        padding: ",";\n      "]);return _templateObject5=function _templateObject5(){return e},e}function _templateObject4(){var e=i()(["\n          display: flex;\n        "]);return _templateObject4=function _templateObject4(){return e},e}function _templateObject3(){var e=i()(["\n    margin-right: ",";\n    "," {\n      margin-bottom: ",";\n      margin-top: ",";\n    }\n  "]);return _templateObject3=function _templateObject3(){return e},e}function _templateObject2(){var e=i()(["\n    display: flex;\n    flex-direction: column;\n    align-items: flex-start;\n    justify-content: space-between;\n    "," {\n      margin-top: ",";\n    }\n    margin-bottom: ",";\n  "]);return _templateObject2=function _templateObject2(){return e},e}function _templateObject(){var e=i()(["\n    display: flex;\n    justify-content: space-between;\n    min-width: 300px;\n    min-height: 48px;\n    padding-left: ",";\n    border-left: 1px solid ",";\n    "," {\n      border: none;\n      padding-left: 0;\n      flex-direction: column;\n      margin-left: 0;\n      margin-top: ",";\n    }\n  "]);return _templateObject=function _templateObject(){return e},e}var g=function root(e){return Object(s.c)(_templateObject(),e.spacing(24),e.palette.gray[300],e.breakpoints.down("sm"),e.spacing(24))},f=function viewFeedbackButton(e){return Object(s.c)(_templateObject2(),e.breakpoints.down("sm"),e.spacing(24),e.spacing(8))},h=function gradeContainer(e){return Object(s.c)(_templateObject3(),e.spacing(16),e.breakpoints.down("sm"),e.spacing(8),e.spacing(8))},y=function CoverPageRowRightSideGrade(e){var t=e.itemGrade,n=e.computedScore,i=e.maxScore,a=e.viewFeedback,r=e.isViewFeedbackButtonLinkStyle,o=e.isCumulativeGraded,b=e.reviewsExpected,m=void 0===b?0:b,p=e.reviewsReceived,j=void 0===p?0:p,O=e.showKeepHighScoreMsg,y=void 0===O||O,S=e.isRequired,x=void 0!==S&&S,I=Object(d.a)(),k=t||{},T=k.grade,C=k.isPassed,A=c()(T)&&!!C&&(!o||x&&o);return Object(s.d)("div",{css:g(I)},Object(s.d)("div",{css:h(I)},Object(s.d)(u.a,{variant:"h3bold"},v()("Your grade")),Object(s.d)(u.a,{variant:"h1",component:"div",color:A?"success":"error","data-test":"grade-percent"},Object(s.d)(w,{itemGrade:t,computedScore:n,maxScore:i,reviewsExpected:m,reviewsReceived:j}))),c()(T)&&a&&Object(s.d)("div",{css:f(I)},Object(s.d)(l.a,{variant:r?"ghost":"primary",onClick:a,"data-test":"feedback-button"},v()("View Feedback")),y&&Object(s.d)(u.a,{variant:"body2",color:"supportText"},v()("We keep your highest score"))))},w=function CoverPageRowGradeDetail(e){var t=e.itemGrade,n=e.computedScore,i=e.maxScore,a=e.reviewsExpected,r=void 0===a?0:a,o=e.reviewsReceived,l=void 0===o?0:o,j,g=(t||{}).grade,f=Object(d.a)();if(c()(g))return Object(s.d)("div",{css:Object(s.c)(_templateObject4())},Object(b.b)(g),Object(s.d)(p.a,{itemGrade:t,computedScore:n,maxScore:i}));if(r>0&&l>0)return Object(s.d)(u.a,{variant:"body2"},Object(s.d)(m.b,{message:v()("In progress, {reviewsReceived, number} of {reviewsExpected, number} received"),reviewsExpected:r,reviewsReceived:l}));return Object(s.d)(u.a,{variant:"h1",color:"supportText",css:Object(s.c)(_templateObject5(),f.spacing(8,0,0,0)),"data-test":"no-grade-text"},Object(s.d)("span",{"aria-hidden":"true"},"-"),Object(s.d)(O.b,{tagName:"span"},v()("Not available")))};e.a=y},"YdC/":function(module,e,t){"use strict";var n=t("q1tI"),i=t.n(n),a=t("AeFk"),c=t("JJfJ"),r=t("8Hdl"),o=t("PB6g"),s={quizName:Object(a.c)({marginBottom:"4px"}),subHeader:Object(a.c)({marginTop:"4px"})},d=function CoverPageHeader(e){var t=e.assignmentTypeName,i=e.item,d=(i=void 0===i?{}:i).timeCommitment,u=i.name,l=e.subHeader;return Object(a.d)("div",null,Object(a.d)(r.a,{variant:"d2",component:"h1",css:s.quizName},u),Object(a.d)(r.a,{variant:"body2",component:"div"},t,!!d&&Object(a.d)(n.Fragment,null,Object(a.d)(o.b,{tagName:"span"},Object(c.t)(d,!0)),Object(a.d)("span",{"aria-hidden":!0}," • ",Object(c.t)(d)))),l&&Object(a.d)("div",{css:s.subHeader},l))};e.a=d},fHLu:function(module,e,t){"use strict";t.d(e,"a",function(){return a});var n=t("S0QZ"),i=function clearQuizApolloCache(e){Object(n.a)(e.cache,["RestQuizSessionMetadataElement","RestQuizQuestionDataElement","LocalTimerState","LocalChangedResponse","LocalStepState","$ROOT_QUERY.ChangedResponse"])},a=function clearPracticeQuizApolloCache(e){Object(n.a)(e.cache,["LocalChangedResponse","LocalStepState"])};e.b=i},hgsf:function(module,e,t){"use strict";var n=t("lSNA"),i=t.n(n),a=t("UB5X"),c=t.n(a),r=t("q1tI"),o=t.n(r),s=t("AeFk"),d=t("JJfJ"),u=t("kvW3"),l=t("ryMZ"),b=t("izmZ"),m=t("d3Ej"),p=t.n(m),j=t("763+"),v=function root(e){return Object(s.c)(i()({display:"flex",flexDirection:"column"},e.breakpoints.only("xs"),{flexDirection:"column"}))},O=function row(e){return Object(s.c)(i()({display:"flex",flexDirection:"row",marginBottom:e.spacing(8)},e.breakpoints.only("xs"),{flexDirection:"column",marginBottom:0}))},g=function CoverPageDetails(e){var t=e.deadline,n=e.attemptsCompleted,i=void 0===n?0:n,a=e.attemptsLeft,r=e.attemptsRateCount,o=e.attemptsRateInterval,m=e.passingFraction,g=e.isCumulativeGraded,f=e.lastSubmittedAt,h=Object(j.a)(),y=c()(a)?a+i:null;return Object(s.d)("div",{css:v(h)},Object(s.d)("div",{css:O(h)},t&&Object(s.d)(l.a,{label:p()("Due"),details:Object(d.s)(t,d.g),ariaDetails:Object(d.s)(t,d.b)}),c()(a)&&Object(s.d)(l.a,{label:p()("Attempts"),details:Object(s.d)(u.b,{message:p()("{attemptsLeft, number} left ({attemptsMax, number} total attempts)"),attemptsLeft:Math.max(a,0),attemptsMax:y})}),c()(r)&&c()(o)&&Object(s.d)(l.a,{label:p()("Attempts"),details:Object(s.d)(u.b,{message:p()("{count, number} every {duration}"),count:r,duration:Object(d.t)(o,!0)})}),c()(m)&&!g&&Object(s.d)(l.a,{label:p()("To Pass"),details:Object(s.d)(u.a,{message:p()("{percent} or higher"),percent:Object(s.d)(b.a,{value:m,maxFractionDigits:0})})})),f&&Object(s.d)("div",{css:O(h)},Object(s.d)(l.a,{label:p()("Submitted"),details:Object(d.s)(f,d.g),ariaDetails:Object(d.s)(f,d.b)})))};e.a=g},iMVg:function(module,e,t){"use strict";var n=t("J4zp"),i=t.n(n),a=t("q1tI"),c=t.n(a),r=t("AeFk"),o=t("ZJgU"),s=t("s3XC"),d=t("3AF4"),u=t("mDy0"),l=t("vrZb"),b=t.n(l),m={linkButton:Object(r.c)({marginLeft:-16})},p=function LearningObjectivesButton(e){var t=e.itemId,n=e.courseSlug,c=e.learningObjectiveIds,l=Object(a.useState)(),p=i()(l,2),j=p[0],v=p[1],O=function toggleTunnelVision(){return v(!j)};if(!(null==c?void 0:c.length))return null;return Object(r.d)("div",null,Object(r.d)(o.a,{variant:"ghost",css:m.linkButton,onClick:O,"data-test":"review-button"},b()("Review Learning Objectives")),j&&Object(r.d)(s.b,{"data-test":"tunnel-vision-wrapper",onClose:O,headerLeft:Object(r.d)(d.b,{headerText:b()("Review Learning Objectives"),itemTypeText:b()("Review materials")}),headerRight:Object(r.d)("div",null)},Object(r.d)(u.a,{"data-test":"learning-objectives-content",itemId:t,courseSlug:n,learningObjectiveIds:c})))};e.a=p},jXJP:function(module,e,t){"use strict";var n=t("VkAN"),i=t.n(n),a=t("lSNA"),c=t.n(a),r=t("UB5X"),o=t.n(r),s=t("q1tI"),d=t.n(s),u=t("AeFk"),l=t("ZJgU"),b=t("763+"),m=t("8Hdl"),p=t("H7bS"),j=t("d3Ej"),v=t.n(j),O=t("kWps"),g=t("kvW3"),f=t("JJfJ"),h=t("d2Ft");function _templateObject(){var e=i()(["\n                transform: translateY(4px);\n              "]);return _templateObject=function _templateObject(){return e},e}var y=function styles(e){return{submissionTime:Object(u.c)({marginTop:e.spacing(8),marginBottom:-8}),root:Object(u.c)(c()({},e.breakpoints.down("sm"),{marginTop:e.spacing(12)})),actionButtonStyle:Object(u.c)(c()({display:"flex",justifyContent:"flex-end"},e.breakpoints.down("sm"),{justifyContent:"flex-start"})),retryInfo:Object(u.c)({marginTop:e.spacing(12)}),timeLimitText:Object(u.c)({marginTop:e.spacing(12)})}},w=function renderActionButton(e,t){return Object(u.d)(l.a,{size:"medium",variant:"primary",onClick:t,"aria-labelledby":e,"data-test":"action-button"},e)},S=function CoverPageRowRightSideActions(e){var t=e.startAttempt,n=e.restartAttempt,i=e.resumeAttempt,a=e.retryAttempt,c=e.submissionTime,r=e.showTimer,s=e.timeLimit,d=e.timerId,j=e.attemptsLeft,S=e.secondsLeftInLatestAttempt,x=e.attemptLimitTimeLeft,I=null,k=Object(b.a)(),T=y(k),C=o()(x)||"number"==typeof j&&j<=0;return t?I=w(v()("Start assignment"),t):n?I=w(v()("Restart assignment"),n):i?I=w(v()("Resume assignment"),i):a&&(I=Object(u.d)(l.a,{size:"medium",variant:"ghost","data-test":"action-button","aria-labelledby":v()("Try again"),component:"a",onClick:a,disabled:C},v()("Try again"))),Object(u.d)("div",{css:T.root},Object(u.d)("div",{css:T.actionButtonStyle},I),C&&o()(x)&&Object(u.d)("div",{css:T.retryInfo,"data-test":"retry-info"},Object(u.d)(m.a,{variant:"body2",component:"span",color:"supportText"},Object(u.d)(g.b,{message:v()("Retake the quiz in {attemptLimitTimeLeft}"),attemptLimitTimeLeft:Object(u.d)("strong",null,Object(f.t)(x))}))),c&&Object(u.d)("div",{css:T.submissionTime,"data-test":"submission-time-detail"},Object(u.d)(m.a,{variant:"h4bold",component:"span",color:"supportText"},v()("Submitted"))," ",Object(u.d)(m.a,{variant:"body2",component:"span",color:"supportText"},O.a.getFormattedDeadline(c))),o()(s)&&t&&Object(u.d)("div",null,Object(u.d)(m.a,{variant:"body2"},Object(u.d)(p.a,{size:"medium",color:"default",css:Object(u.c)(_templateObject())}),Object(u.d)("span",{css:T.timeLimitText},Object(u.d)(g.b,{message:v()("You will have {timeLimit} to finish"),timeLimit:Object(f.t)(s,!0)})))),r&&o()(S)&&Object(u.d)(h.a,{timerId:d,remainingTimeInMs:1e3*S,displayRemaining:!0}))};e.a=S},mDy0:function(module,e,t){"use strict";var n=t("q1tI"),i=t.n(n),a=t("AeFk"),c=t("Euzd"),r=t("2S0G"),o=t("8Hdl"),s=t("vrZb"),d=t.n(s),u={root:function root(e){return Object(a.c)({marginBottom:e.spacing(48)})},instructions:function instructions(e){return Object(a.c)({marginTop:e.spacing(8)})},learningObjectiveContainer:function learningObjectiveContainer(e){return Object(a.c)({marginTop:e.spacing(32)})}},l=function LearningObjectivesContent(e){var t=e.itemId,n=e.courseSlug,i=void 0===n?"":n,s=e.learningObjectiveIds;return Object(a.d)("div",{css:u.root},Object(a.d)(o.a,{variant:"h1semibold",component:"h2"},d()("Review Learning Objectives")),Object(a.d)(o.a,{variant:"body2",css:u.instructions},d()("Review concepts related to your current learning objectives")),Object(a.d)("div",{css:u.learningObjectiveContainer},Object(a.d)(c.a,{itemId:t,courseSlug:i,learningObjectiveIds:s,removeParentItem:!0,removeFutureItems:!0},function(e){var t=e.learningObjectives;return Object(a.d)(r.a,{learningObjectives:t})})))};e.a=l},ryMZ:function(module,e,t){"use strict";var n=t("lSNA"),i=t.n(n),a=t("q1tI"),c=t.n(a),r=t("AeFk"),o=t("763+"),s=t("8Hdl"),d=t("PB6g"),u=function root(e){return Object(r.c)({whiteSpace:"nowrap","& + &":i()({marginLeft:e.spacing(16)},e.breakpoints.down("xs"),{marginLeft:0})})},l=function label(e){return Object(r.c)({marginRight:e.spacing(8)})},b=function CoverPagePhaseDetail(e){var t=e.label,n=e.details,i=e.ariaDetails,a=Object(o.a)();return Object(r.d)("div",{css:u(a)},Object(r.d)(s.a,{variant:"h4bold",component:"span",css:l(a),color:"supportText","data-test":"label"},t)," ",Object(r.d)(s.a,{variant:"body2",component:"span",color:"supportText","data-test":"content"},i&&Object(r.d)(d.b,{tagName:"span"},i),Object(r.d)("span",{"aria-hidden":Boolean(i)},n)))};e.a=b}}]);
//# sourceMappingURL=111.823f8fe43662e51762f0.js.map