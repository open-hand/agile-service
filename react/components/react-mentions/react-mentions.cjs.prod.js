"use strict";

function _interopDefault(ex) {
  return ex && "object" == typeof ex && "default" in ex ? ex.default : ex;
}

Object.defineProperty(exports, "__esModule", {
  value: !0
});

var _toConsumableArray = _interopDefault(require("@babel/runtime/helpers/toConsumableArray")), _extends = _interopDefault(require("@babel/runtime/helpers/extends")), _objectSpread = _interopDefault(require("@babel/runtime/helpers/objectSpread")), _classCallCheck = _interopDefault(require("@babel/runtime/helpers/classCallCheck")), _createClass = _interopDefault(require("@babel/runtime/helpers/createClass")), _possibleConstructorReturn = _interopDefault(require("@babel/runtime/helpers/possibleConstructorReturn")), _getPrototypeOf = _interopDefault(require("@babel/runtime/helpers/getPrototypeOf")), _assertThisInitialized = _interopDefault(require("@babel/runtime/helpers/assertThisInitialized")), _inherits = _interopDefault(require("@babel/runtime/helpers/inherits")), _defineProperty = _interopDefault(require("@babel/runtime/helpers/defineProperty")), isEqual = _interopDefault(require("lodash/isEqual")), isNumber = _interopDefault(require("lodash/isNumber")), keys = _interopDefault(require("lodash/keys")), omit = _interopDefault(require("lodash/omit")), values = _interopDefault(require("lodash/values")), PropTypes = _interopDefault(require("prop-types")), React = require("react"), React__default = _interopDefault(React), ReactDOM = _interopDefault(require("react-dom")), substyle = require("substyle"), substyle__default = _interopDefault(substyle), invariant = _interopDefault(require("invariant")), _slicedToArray = _interopDefault(require("@babel/runtime/helpers/slicedToArray")), escapeRegex = function(str) {
  return str.replace(/[-[\]{}()*+?.,\\^$|#\s]/g, "\\$&");
}, PLACEHOLDERS = {
  id: "__id__",
  display: "__display__"
}, findPositionOfCapturingGroup = function(markup, parameterName) {
  invariant("id" === parameterName || "display" === parameterName, 'Second arg must be either "id" or "display", got: "'.concat(parameterName, '"'));
  var indexDisplay = markup.indexOf(PLACEHOLDERS.display), indexId = markup.indexOf(PLACEHOLDERS.id);
  return indexDisplay < 0 && (indexDisplay = null), indexId < 0 && (indexId = null), 
  invariant(null !== indexDisplay || null !== indexId, "The markup '".concat(markup, "' does not contain either of the placeholders '__id__' or '__display__'")), 
  null !== indexDisplay && null !== indexId ? "id" === parameterName && indexId <= indexDisplay || "display" === parameterName && indexDisplay <= indexId ? 0 : 1 : 0;
}, combineRegExps = function(regExps) {
  var serializedRegexParser = /^\/(.+)\/(\w+)?$/;
  return new RegExp(regExps.map(function(regex) {
    var _serializedRegexParse = serializedRegexParser.exec(regex.toString()), _serializedRegexParse2 = _slicedToArray(_serializedRegexParse, 3), regexString = _serializedRegexParse2[1], regexFlags = _serializedRegexParse2[2];
    return invariant(!regexFlags, "RegExp flags are not supported. Change /".concat(regexString, "/").concat(regexFlags, " into /").concat(regexString, "/")), 
    "(".concat(regexString, ")");
  }).join("|"), "g");
}, countPlaceholders = function(markup) {
  var count = 0;
  return markup.indexOf("__id__") >= 0 && count++, markup.indexOf("__display__") >= 0 && count++, 
  count;
}, emptyFn = function() {}, iterateMentionsMarkup = function(value, config, markupIteratee) {
  for (var match, textIteratee = arguments.length > 3 && void 0 !== arguments[3] ? arguments[3] : emptyFn, regex = combineRegExps(config.map(function(c) {
    return c.regex;
  })), accOffset = 2, captureGroupOffsets = config.map(function(_ref) {
    var markup = _ref.markup, result = accOffset;
    return accOffset += countPlaceholders(markup) + 1, result;
  }), start = 0, currentPlainTextIndex = 0; null !== (match = regex.exec(value)); ) {
    var offset = captureGroupOffsets.find(function(o) {
      return !!match[o];
    }), mentionChildIndex = captureGroupOffsets.indexOf(offset), _config$mentionChildI = config[mentionChildIndex], markup = _config$mentionChildI.markup, displayTransform = _config$mentionChildI.displayTransform, idPos = offset + findPositionOfCapturingGroup(markup, "id"), displayPos = offset + findPositionOfCapturingGroup(markup, "display"), id = match[idPos], display = displayTransform(id, match[displayPos]), substr = value.substring(start, match.index);
    textIteratee(substr, start, currentPlainTextIndex), currentPlainTextIndex += substr.length, 
    markupIteratee(match[0], match.index, currentPlainTextIndex, id, display, mentionChildIndex, start), 
    currentPlainTextIndex += display.length, start = regex.lastIndex;
  }
  start < value.length && textIteratee(value.substring(start), start, currentPlainTextIndex);
}, getPlainText = function(value, config) {
  var result = "";
  return iterateMentionsMarkup(value, config, function(match, index, plainTextIndex, id, display) {
    result += display;
  }, function(plainText) {
    result += plainText;
  }), result;
}, mapPlainTextIndex = function(value, config, indexInPlainText) {
  var result, inMarkupCorrection = arguments.length > 3 && void 0 !== arguments[3] ? arguments[3] : "START";
  if ("number" != typeof indexInPlainText) return indexInPlainText;
  return iterateMentionsMarkup(value, config, function(markup, index, mentionPlainTextIndex, id, display, childIndex, lastMentionEndIndex) {
    void 0 === result && mentionPlainTextIndex + display.length > indexInPlainText && (result = "NULL" === inMarkupCorrection ? null : index + ("END" === inMarkupCorrection ? markup.length : 0));
  }, function(substr, index, substrPlainTextIndex) {
    void 0 === result && substrPlainTextIndex + substr.length >= indexInPlainText && (result = index + indexInPlainText - substrPlainTextIndex);
  }), void 0 === result ? value.length : result;
}, spliceString = function(str, start, end, insert) {
  return str.substring(0, start) + insert + str.substring(end);
}, applyChangeToValue = function(value, plainTextValue, _ref, config) {
  var selectionStartBefore = _ref.selectionStartBefore, selectionEndBefore = _ref.selectionEndBefore, selectionEndAfter = _ref.selectionEndAfter, oldPlainTextValue = getPlainText(value, config), lengthDelta = oldPlainTextValue.length - plainTextValue.length;
  "undefined" === selectionStartBefore && (selectionStartBefore = selectionEndAfter + lengthDelta), 
  "undefined" === selectionEndBefore && (selectionEndBefore = selectionStartBefore), 
  selectionStartBefore === selectionEndBefore && selectionEndBefore === selectionEndAfter && oldPlainTextValue.length === plainTextValue.length && (selectionStartBefore -= 1);
  var insert = plainTextValue.slice(selectionStartBefore, selectionEndAfter), spliceStart = Math.min(selectionStartBefore, selectionEndAfter), spliceEnd = selectionEndBefore;
  selectionStartBefore === selectionEndAfter && (spliceEnd = Math.max(selectionEndBefore, selectionStartBefore + lengthDelta));
  var mappedSpliceStart = mapPlainTextIndex(value, config, spliceStart, "START"), mappedSpliceEnd = mapPlainTextIndex(value, config, spliceEnd, "END"), controlSpliceStart = mapPlainTextIndex(value, config, spliceStart, "NULL"), controlSpliceEnd = mapPlainTextIndex(value, config, spliceEnd, "NULL"), willRemoveMention = null === controlSpliceStart || null === controlSpliceEnd, newValue = spliceString(value, mappedSpliceStart, mappedSpliceEnd, insert);
  if (!willRemoveMention) {
    var controlPlainTextValue = getPlainText(newValue, config);
    if (controlPlainTextValue !== plainTextValue) {
      for (spliceStart = 0; plainTextValue[spliceStart] === controlPlainTextValue[spliceStart]; ) spliceStart++;
      insert = plainTextValue.slice(spliceStart, selectionEndAfter), spliceEnd = oldPlainTextValue.lastIndexOf(plainTextValue.substring(selectionEndAfter)), 
      mappedSpliceStart = mapPlainTextIndex(value, config, spliceStart, "START"), mappedSpliceEnd = mapPlainTextIndex(value, config, spliceEnd, "END"), 
      newValue = spliceString(value, mappedSpliceStart, mappedSpliceEnd, insert);
    }
  }
  return newValue;
}, findStartOfMentionInPlainText = function(value, config, indexInPlainText) {
  var result = indexInPlainText, foundMention = !1;
  if (iterateMentionsMarkup(value, config, function(markup, index, mentionPlainTextIndex, id, display, childIndex, lastMentionEndIndex) {
    mentionPlainTextIndex <= indexInPlainText && mentionPlainTextIndex + display.length > indexInPlainText && (result = mentionPlainTextIndex, 
    foundMention = !0);
  }), foundMention) return result;
}, getMentions = function(value, config) {
  var mentions = [];
  return iterateMentionsMarkup(value, config, function(match, index, plainTextIndex, id, display, childIndex, start) {
    mentions.push({
      id: id,
      display: display,
      childIndex: childIndex,
      index: index,
      plainTextIndex: plainTextIndex
    });
  }), mentions;
}, countSuggestions = function(suggestions) {
  return Object.values(suggestions).reduce(function(acc, _ref) {
    return acc + _ref.results.length;
  }, 0);
}, getEndOfLastMention = function(value, config) {
  var mentions = getMentions(value, config), lastMention = mentions[mentions.length - 1];
  return lastMention ? lastMention.plainTextIndex + lastMention.display.length : 0;
}, markupToRegex = function(markup) {
  var escapedMarkup = escapeRegex(markup), charAfterDisplay = markup[markup.indexOf(PLACEHOLDERS.display) + PLACEHOLDERS.display.length], charAfterId = markup[markup.indexOf(PLACEHOLDERS.display) + PLACEHOLDERS.display.length];
  return new RegExp(escapedMarkup.replace(PLACEHOLDERS.display, "([^".concat(escapeRegex(charAfterDisplay || ""), "]+?)")).replace(PLACEHOLDERS.id, "([^".concat(escapeRegex(charAfterId || ""), "]+?)")));
}, readConfigFromChildren = function(children) {
  return React.Children.toArray(children).map(function(_ref) {
    var _ref$props = _ref.props, markup = _ref$props.markup, regex = _ref$props.regex, displayTransform = _ref$props.displayTransform;
    return {
      markup: markup,
      regex: regex ? coerceCapturingGroups(regex, markup) : markupToRegex(markup),
      displayTransform: displayTransform || function(id, display) {
        return display || id;
      }
    };
  });
}, coerceCapturingGroups = function(regex, markup) {
  var numberOfGroups = new RegExp(regex.toString() + "|").exec("").length - 1, numberOfPlaceholders = countPlaceholders(markup);
  return invariant(numberOfGroups === numberOfPlaceholders, "Number of capturing groups in RegExp ".concat(regex.toString(), " (").concat(numberOfGroups, ") does not match the number of placeholders in the markup '").concat(markup, "' (").concat(numberOfPlaceholders, ")")), 
  regex;
}, makeMentionsMarkup = function(markup, id, display) {
  return markup.replace(PLACEHOLDERS.id, id).replace(PLACEHOLDERS.display, display);
}, _generateComponentKey = function(usedKeys, id) {
  return usedKeys.hasOwnProperty(id) ? usedKeys[id]++ : usedKeys[id] = 0, id + "_" + usedKeys[id];
}, Highlighter = function(_Component) {
  function Highlighter() {
    var _this;
    return _classCallCheck(this, Highlighter), (_this = _possibleConstructorReturn(this, _getPrototypeOf(Highlighter).apply(this, arguments))).state = {
      lastPosition: {}
    }, _this;
  }
  return _inherits(Highlighter, _Component), _createClass(Highlighter, [ {
    key: "componentDidMount",
    value: function() {
      this.notifyCaretPosition();
    }
  }, {
    key: "componentDidUpdate",
    value: function() {
      this.notifyCaretPosition();
    }
  }, {
    key: "notifyCaretPosition",
    value: function() {
      if (this.caretRef) {
        var position = {
          left: this.caretRef.offsetLeft,
          top: this.caretRef.offsetTop
        }, lastPosition = this.state.lastPosition;
        isEqual(lastPosition, position) || (this.setState({
          lastPosition: position
        }), this.props.onCaretPositionChange(position));
      }
    }
  }, {
    key: "render",
    value: function() {
      var caretPositionInMarkup, _this2 = this, _this$props = this.props, selection = _this$props.selection, value = _this$props.value, style = _this$props.style, inputStyle = _this$props.inputStyle, children = _this$props.children, config = readConfigFromChildren(children);
      selection.start === selection.end && (caretPositionInMarkup = mapPlainTextIndex(value, config, selection.start, "START"));
      var resultComponents = [], componentKeys = {}, components = resultComponents, substringComponentKey = 0;
      return iterateMentionsMarkup(value, config, function(markup, index, indexInPlainText, id, display, mentionChildIndex, lastMentionEndIndex) {
        var key = _generateComponentKey(componentKeys, id);
        components.push(_this2.getMentionComponentForMatch(id, display, mentionChildIndex, key));
      }, function(substr, index, indexInPlainText) {
        if (isNumber(caretPositionInMarkup) && caretPositionInMarkup >= index && caretPositionInMarkup <= index + substr.length) {
          var splitIndex = caretPositionInMarkup - index;
          components.push(_this2.renderSubstring(substr.substring(0, splitIndex), substringComponentKey)), 
          components = [ _this2.renderSubstring(substr.substring(splitIndex), substringComponentKey) ];
        } else components.push(_this2.renderSubstring(substr, substringComponentKey));
        substringComponentKey++;
      }), components.push(" "), components !== resultComponents && resultComponents.push(this.renderHighlighterCaret(components)), 
      React__default.createElement("div", _extends({}, style, {
        style: _objectSpread({}, inputStyle, style.style)
      }), resultComponents);
    }
  }, {
    key: "renderSubstring",
    value: function(string, key) {
      return React__default.createElement("span", _extends({}, this.props.style("substring"), {
        key: key
      }), string);
    }
  }, {
    key: "getMentionComponentForMatch",
    value: function(id, display, mentionChildIndex, key) {
      var props = {
        id: id,
        display: display,
        key: key
      }, child = React.Children.toArray(this.props.children)[mentionChildIndex];
      return React__default.cloneElement(child, props);
    }
  }, {
    key: "renderHighlighterCaret",
    value: function(children) {
      var _this3 = this;
      return React__default.createElement("span", _extends({}, this.props.style("caret"), {
        ref: function(el) {
          _this3.caretRef = el;
        },
        key: "caret"
      }), children);
    }
  } ]), Highlighter;
}(React.Component);

_defineProperty(Highlighter, "propTypes", {
  selection: PropTypes.shape({
    start: PropTypes.number,
    end: PropTypes.number
  }).isRequired,
  value: PropTypes.string.isRequired,
  onCaretPositionChange: PropTypes.func.isRequired,
  inputStyle: PropTypes.object,
  children: PropTypes.oneOfType([ PropTypes.element, PropTypes.arrayOf(PropTypes.element) ]).isRequired
}), _defineProperty(Highlighter, "defaultProps", {
  value: "",
  inputStyle: {}
});

var styled = substyle.defaultStyle({
  position: "relative",
  width: "inherit",
  color: "transparent",
  overflow: "hidden",
  whiteSpace: "pre-wrap",
  wordWrap: "break-word",
  "&singleLine": {
    whiteSpace: "pre",
    wordWrap: null
  },
  substring: {
    visibility: "hidden"
  }
}, function(props) {
  return {
    "&singleLine": props.singleLine
  };
}), Highlighter$1 = styled(Highlighter), Suggestion = function(_Component) {
  function Suggestion() {
    return _classCallCheck(this, Suggestion), _possibleConstructorReturn(this, _getPrototypeOf(Suggestion).apply(this, arguments));
  }
  return _inherits(Suggestion, _Component), _createClass(Suggestion, [ {
    key: "render",
    value: function() {
      var rest = omit(this.props, "style", keys(Suggestion.propTypes));
      return React__default.createElement("li", _extends({}, rest, this.props.style), this.renderContent());
    }
  }, {
    key: "renderContent",
    value: function() {
      var _this$props = this.props, query = _this$props.query, renderSuggestion = _this$props.renderSuggestion, suggestion = _this$props.suggestion, index = _this$props.index, focused = _this$props.focused, display = this.getDisplay(), highlightedDisplay = this.renderHighlightedDisplay(display, query);
      return renderSuggestion ? renderSuggestion(suggestion, query, highlightedDisplay, index, focused) : highlightedDisplay;
    }
  }, {
    key: "getDisplay",
    value: function() {
      var suggestion = this.props.suggestion;
      if (suggestion instanceof String) return suggestion;
      var id = suggestion.id, display = suggestion.display;
      return void 0 !== id && display ? display : id;
    }
  }, {
    key: "renderHighlightedDisplay",
    value: function(display) {
      var _this$props2 = this.props, query = _this$props2.query, style = _this$props2.style, i = display.toLowerCase().indexOf(query.toLowerCase());
      return -1 === i ? React__default.createElement("span", style("display"), display) : React__default.createElement("span", style("display"), display.substring(0, i), React__default.createElement("b", style("highlight"), display.substring(i, i + query.length)), display.substring(i + query.length));
    }
  } ]), Suggestion;
}(React.Component);

_defineProperty(Suggestion, "propTypes", {
  id: PropTypes.oneOfType([ PropTypes.string, PropTypes.number ]).isRequired,
  query: PropTypes.string.isRequired,
  index: PropTypes.number.isRequired,
  suggestion: PropTypes.oneOfType([ PropTypes.string, PropTypes.shape({
    id: PropTypes.oneOfType([ PropTypes.string, PropTypes.number ]).isRequired,
    display: PropTypes.string
  }) ]).isRequired,
  renderSuggestion: PropTypes.func,
  focused: PropTypes.bool
});

var styled$1 = substyle.defaultStyle({
  cursor: "pointer"
}, function(props) {
  return {
    "&focused": props.focused
  };
}), Suggestion$1 = styled$1(Suggestion);

function LoadingIndicator(_ref) {
  var style = _ref.style, spinnerStyle = style("spinner");
  return React__default.createElement("div", style, React__default.createElement("div", spinnerStyle, React__default.createElement("div", spinnerStyle([ "element", "element1" ])), React__default.createElement("div", spinnerStyle([ "element", "element2" ])), React__default.createElement("div", spinnerStyle([ "element", "element3" ])), React__default.createElement("div", spinnerStyle([ "element", "element4" ])), React__default.createElement("div", spinnerStyle([ "element", "element5" ]))));
}

var LoadingIndicator$1 = substyle__default(LoadingIndicator), SuggestionsOverlay = function(_Component) {
  function SuggestionsOverlay() {
    return _classCallCheck(this, SuggestionsOverlay), _possibleConstructorReturn(this, _getPrototypeOf(SuggestionsOverlay).apply(this, arguments));
  }
  return _inherits(SuggestionsOverlay, _Component), _createClass(SuggestionsOverlay, [ {
    key: "componentDidUpdate",
    value: function() {
      if (this.suggestionsRef && !(this.suggestionsRef.offsetHeight >= this.suggestionsRef.scrollHeight) && this.props.scrollFocusedIntoView) {
        var scrollTop = this.suggestionsRef.scrollTop, _this$suggestionsRef$ = this.suggestionsRef.children[this.props.focusIndex].getBoundingClientRect(), top = _this$suggestionsRef$.top, bottom = _this$suggestionsRef$.bottom, topContainer = this.suggestionsRef.getBoundingClientRect().top;
        bottom = bottom - topContainer + scrollTop, (top = top - topContainer + scrollTop) < scrollTop ? this.suggestionsRef.scrollTop = top : bottom > this.suggestionsRef.offsetHeight && (this.suggestionsRef.scrollTop = bottom - this.suggestionsRef.offsetHeight);
      }
    }
  }, {
    key: "render",
    value: function() {
      var _this = this, _this$props = this.props, suggestions = _this$props.suggestions, isLoading = _this$props.isLoading, style = _this$props.style, onMouseDown = _this$props.onMouseDown;
      return 0 !== countSuggestions(suggestions) || isLoading ? React__default.createElement("div", _extends({}, style, {
        onMouseDown: onMouseDown
      }), React__default.createElement("ul", _extends({
        ref: function(el) {
          _this.suggestionsRef = el;
        }
      }, style("list")), this.renderSuggestions()), this.renderLoadingIndicator()) : null;
    }
  }, {
    key: "renderSuggestions",
    value: function() {
      var _this2 = this;
      return Object.values(this.props.suggestions).reduce(function(accResults, _ref) {
        var results = _ref.results, queryInfo = _ref.queryInfo;
        return [].concat(_toConsumableArray(accResults), _toConsumableArray(results.map(function(result, index) {
          return _this2.renderSuggestion(result, queryInfo, accResults.length + index);
        })));
      }, []);
    }
  }, {
    key: "renderSuggestion",
    value: function(result, queryInfo, index) {
      var _this3 = this, id = this.getID(result), isFocused = index === this.props.focusIndex, childIndex = queryInfo.childIndex, query = queryInfo.query, renderSuggestion = React.Children.toArray(this.props.children)[childIndex].props.renderSuggestion;
      return React__default.createElement(Suggestion$1, {
        style: this.props.style("item"),
        key: "".concat(childIndex, "-").concat(id),
        id: id,
        query: query,
        index: index,
        renderSuggestion: renderSuggestion,
        suggestion: result,
        focused: isFocused,
        onClick: function() {
          return _this3.select(result, queryInfo);
        },
        onMouseEnter: function() {
          return _this3.handleMouseEnter(index);
        }
      });
    }
  }, {
    key: "getID",
    value: function(suggestion) {
      return suggestion instanceof String ? suggestion : suggestion.id;
    }
  }, {
    key: "renderLoadingIndicator",
    value: function() {
      if (this.props.isLoading) return React__default.createElement(LoadingIndicator$1, {
        style: this.props.style("loadingIndicator")
      });
    }
  }, {
    key: "handleMouseEnter",
    value: function(index, ev) {
      this.props.onMouseEnter && this.props.onMouseEnter(index);
    }
  }, {
    key: "select",
    value: function(suggestion, queryInfo) {
      this.props.onSelect(suggestion, queryInfo);
    }
  } ]), SuggestionsOverlay;
}(React.Component);

_defineProperty(SuggestionsOverlay, "propTypes", {
  suggestions: PropTypes.object.isRequired,
  focusIndex: PropTypes.number,
  scrollFocusedIntoView: PropTypes.bool,
  isLoading: PropTypes.bool,
  onSelect: PropTypes.func,
  children: PropTypes.oneOfType([ PropTypes.element, PropTypes.arrayOf(PropTypes.element) ]).isRequired
}), _defineProperty(SuggestionsOverlay, "defaultProps", {
  suggestions: {},
  onSelect: function() {
    return null;
  }
});

var styled$2 = substyle.defaultStyle(function(_ref2) {
  var position = _ref2.position;
  return _objectSpread({
    position: "absolute",
    zIndex: 1,
    backgroundColor: "white",
    marginTop: 14,
    minWidth: 100
  }, position, {
    list: {
      margin: 0,
      padding: 0,
      listStyleType: "none"
    }
  });
}), SuggestionsOverlay$1 = styled$2(SuggestionsOverlay), makeTriggerRegex = function(trigger) {
  var options = arguments.length > 1 && void 0 !== arguments[1] ? arguments[1] : {};
  if (trigger instanceof RegExp) return trigger;
  var allowSpaceInQuery = options.allowSpaceInQuery, escapedTriggerChar = escapeRegex(trigger);
  return new RegExp("(?:^|[^])(".concat(escapedTriggerChar, "([^").concat(allowSpaceInQuery ? "" : "\\s").concat(escapedTriggerChar, "]*))$"));
}, getDataProvider = function(data) {
  return data instanceof Array ? function(query, callback) {
    for (var results = [], i = 0, l = data.length; i < l; ++i) {
      (data[i].display || data[i].id).toLowerCase().indexOf(query.toLowerCase()) >= 0 && results.push(data[i]);
    }
    return results;
  } : data;
}, KEY = {
  TAB: 9,
  RETURN: 13,
  ESC: 27,
  UP: 38,
  DOWN: 40
}, isComposing = !1, propTypes = {
  singleLine: PropTypes.bool,
  allowSpaceInQuery: PropTypes.bool,
  EXPERIMENTAL_cutCopyPaste: PropTypes.bool,
  allowSuggestionsAboveCursor: PropTypes.bool,
  value: PropTypes.string,
  onKeyDown: PropTypes.func,
  onSelect: PropTypes.func,
  onBlur: PropTypes.func,
  onChange: PropTypes.func,
  suggestionsPortalHost: "undefined" == typeof Element ? PropTypes.any : PropTypes.PropTypes.instanceOf(Element),
  inputRef: PropTypes.oneOfType([ PropTypes.func, PropTypes.shape({
    current: "undefined" == typeof Element ? PropTypes.any : PropTypes.instanceOf(Element)
  }) ]),
  children: PropTypes.oneOfType([ PropTypes.element, PropTypes.arrayOf(PropTypes.element) ]).isRequired
}, MentionsInput = function(_React$Component) {
  function MentionsInput(_props) {
    var _this;
    return _classCallCheck(this, MentionsInput), _this = _possibleConstructorReturn(this, _getPrototypeOf(MentionsInput).call(this, _props)), 
    _defineProperty(_assertThisInitialized(_this), "getInputProps", function(isTextarea) {
      var _this$props = _this.props, readOnly = _this$props.readOnly, disabled = _this$props.disabled, style = _this$props.style, props = omit(_this.props, "style", keys(propTypes));
      return _objectSpread({}, props, style("input"), {
        value: _this.getPlainText()
      }, !readOnly && !disabled && {
        onChange: _this.handleChange,
        onSelect: _this.handleSelect,
        onKeyDown: _this.handleKeyDown,
        onBlur: _this.handleBlur,
        onCompositionStart: _this.handleCompositionStart,
        onCompositionEnd: _this.handleCompositionEnd,
        onScroll: _this.updateHighlighterScroll
      });
    }), _defineProperty(_assertThisInitialized(_this), "renderControl", function() {
      var _this$props2 = _this.props, singleLine = _this$props2.singleLine, style = _this$props2.style, inputProps = _this.getInputProps(!singleLine);
      return React__default.createElement("div", style("control"), _this.renderHighlighter(inputProps.style), singleLine ? _this.renderInput(inputProps) : _this.renderTextarea(inputProps));
    }), _defineProperty(_assertThisInitialized(_this), "renderInput", function(props) {
      return React__default.createElement("input", _extends({
        type: "text",
        ref: _this.setInputRef
      }, props));
    }), _defineProperty(_assertThisInitialized(_this), "renderTextarea", function(props) {
      return React__default.createElement("textarea", _extends({
        ref: _this.setInputRef
      }, props));
    }), _defineProperty(_assertThisInitialized(_this), "setInputRef", function(el) {
      _this.inputRef = el;
      var inputRef = _this.props.inputRef;
      "function" == typeof inputRef ? inputRef(el) : inputRef && (inputRef.current = el);
    }), _defineProperty(_assertThisInitialized(_this), "renderSuggestionsOverlay", function() {
      if (!isNumber(_this.state.selectionStart)) return null;
      var suggestionsNode = React__default.createElement(SuggestionsOverlay$1, {
        style: _this.props.style("suggestions"),
        position: _this.state.suggestionsPosition,
        focusIndex: _this.state.focusIndex,
        scrollFocusedIntoView: _this.state.scrollFocusedIntoView,
        ref: function(el) {
          _this.suggestionsRef = el;
        },
        suggestions: _this.state.suggestions,
        onSelect: _this.addMention,
        onMouseDown: _this.handleSuggestionsMouseDown,
        onMouseEnter: function(focusIndex) {
          return _this.setState({
            focusIndex: focusIndex,
            scrollFocusedIntoView: !1
          });
        },
        isLoading: _this.isLoading()
      }, _this.props.children);
      return _this.props.suggestionsPortalHost ? ReactDOM.createPortal(suggestionsNode, _this.props.suggestionsPortalHost) : suggestionsNode;
    }), _defineProperty(_assertThisInitialized(_this), "renderHighlighter", function(inputStyle) {
      var _this$state = _this.state, selectionStart = _this$state.selectionStart, selectionEnd = _this$state.selectionEnd, _this$props3 = _this.props, singleLine = _this$props3.singleLine, children = _this$props3.children, value = _this$props3.value, style = _this$props3.style;
      return React__default.createElement(Highlighter$1, {
        ref: function(el) {
          _this.highlighterRef = el;
        },
        style: style("highlighter"),
        inputStyle: inputStyle,
        value: value,
        singleLine: singleLine,
        selection: {
          start: selectionStart,
          end: selectionEnd
        },
        onCaretPositionChange: function(position) {
          return _this.setState({
            caretPosition: position
          });
        }
      }, children);
    }), _defineProperty(_assertThisInitialized(_this), "getPlainText", function() {
      return getPlainText(_this.props.value || "", readConfigFromChildren(_this.props.children));
    }), _defineProperty(_assertThisInitialized(_this), "executeOnChange", function(event) {
      for (var _len = arguments.length, args = new Array(_len > 1 ? _len - 1 : 0), _key = 1; _key < _len; _key++) args[_key - 1] = arguments[_key];
      var _this$props4, _this$props$valueLink;
      return _this.props.onChange ? (_this$props4 = _this.props).onChange.apply(_this$props4, [ event ].concat(args)) : _this.props.valueLink ? (_this$props$valueLink = _this.props.valueLink).requestChange.apply(_this$props$valueLink, [ event.target.value ].concat(args)) : void 0;
    }), _defineProperty(_assertThisInitialized(_this), "handleChange", function(ev) {
      if ((document.activeElement && document.activeElement.contentDocument || document).activeElement === ev.target) {
        var value = _this.props.value || "", config = readConfigFromChildren(_this.props.children), newPlainTextValue = ev.target.value, newValue = applyChangeToValue(value, newPlainTextValue, {
          selectionStartBefore: _this.state.selectionStart,
          selectionEndBefore: _this.state.selectionEnd,
          selectionEndAfter: ev.target.selectionEnd
        }, config);
        newPlainTextValue = getPlainText(newValue, config);
        var selectionStart = ev.target.selectionStart, selectionEnd = ev.target.selectionEnd, setSelectionAfterMentionChange = !1, startOfMention = findStartOfMentionInPlainText(value, config, selectionStart);
        void 0 !== startOfMention && _this.state.selectionEnd > startOfMention && (selectionEnd = selectionStart = startOfMention, 
        setSelectionAfterMentionChange = !0), _this.setState({
          selectionStart: selectionStart,
          selectionEnd: selectionEnd,
          setSelectionAfterMentionChange: setSelectionAfterMentionChange
        });
        var mentions = getMentions(newValue, config), eventMock = {
          target: {
            value: newValue
          }
        };
        _this.executeOnChange(eventMock, newValue, newPlainTextValue, mentions);
      }
    }), _defineProperty(_assertThisInitialized(_this), "handleSelect", function(ev) {
      if (_this.setState({
        selectionStart: ev.target.selectionStart,
        selectionEnd: ev.target.selectionEnd
      }), !isComposing) {
        var el = _this.inputRef;
        ev.target.selectionStart === ev.target.selectionEnd ? _this.updateMentionsQueries(el.value, ev.target.selectionStart) : _this.clearSuggestions(), 
        _this.updateHighlighterScroll(), _this.props.onSelect(ev);
      }
    }), _defineProperty(_assertThisInitialized(_this), "handleKeyDown", function(ev) {
      var suggestionsCount = countSuggestions(_this.state.suggestions), suggestionsComp = _this.suggestionsRef;
      if (0 !== suggestionsCount && suggestionsComp) switch (values(KEY).indexOf(ev.keyCode) >= 0 && ev.preventDefault(), 
      ev.keyCode) {
       case KEY.ESC:
        return void _this.clearSuggestions();

       case KEY.DOWN:
        return void _this.shiftFocus(1);

       case KEY.UP:
        return void _this.shiftFocus(-1);

       case KEY.RETURN:
       case KEY.TAB:
        return void _this.selectFocused();

       default:
        return;
      } else _this.props.onKeyDown(ev);
    }), _defineProperty(_assertThisInitialized(_this), "shiftFocus", function(delta) {
      var suggestionsCount = countSuggestions(_this.state.suggestions);
      _this.setState({
        focusIndex: (suggestionsCount + _this.state.focusIndex + delta) % suggestionsCount,
        scrollFocusedIntoView: !0
      });
    }), _defineProperty(_assertThisInitialized(_this), "selectFocused", function() {
      var _this$state2 = _this.state, suggestions = _this$state2.suggestions, focusIndex = _this$state2.focusIndex, _Object$values$reduce = Object.values(suggestions).reduce(function(acc, _ref) {
        var results = _ref.results, queryInfo = _ref.queryInfo;
        return [].concat(_toConsumableArray(acc), _toConsumableArray(results.map(function(result) {
          return {
            result: result,
            queryInfo: queryInfo
          };
        })));
      }, [])[focusIndex], result = _Object$values$reduce.result, queryInfo = _Object$values$reduce.queryInfo;
      _this.addMention(result, queryInfo), _this.setState({
        focusIndex: 0
      });
    }), _defineProperty(_assertThisInitialized(_this), "handleBlur", function(ev) {
      var clickedSuggestion = _this._suggestionsMouseDown;
      _this._suggestionsMouseDown = !1, clickedSuggestion || _this.setState({
        selectionStart: null,
        selectionEnd: null
      }), window.setTimeout(function() {
        _this.updateHighlighterScroll();
      }, 1), _this.props.onBlur(ev, clickedSuggestion);
    }), _defineProperty(_assertThisInitialized(_this), "handleSuggestionsMouseDown", function(ev) {
      _this._suggestionsMouseDown = !0;
    }), _defineProperty(_assertThisInitialized(_this), "updateSuggestionsPosition", function() {
      var caretPosition = _this.state.caretPosition, _this$props5 = _this.props, suggestionsPortalHost = _this$props5.suggestionsPortalHost, allowSuggestionsAboveCursor = _this$props5.allowSuggestionsAboveCursor;
      if (caretPosition && _this.suggestionsRef) {
        var suggestions = ReactDOM.findDOMNode(_this.suggestionsRef), highlighter = ReactDOM.findDOMNode(_this.highlighterRef), caretOffsetParentRect = highlighter.getBoundingClientRect(), caretHeight = getComputedStyleLengthProp(highlighter, "font-size"), viewportRelative = {
          left: caretOffsetParentRect.left + caretPosition.left,
          top: caretOffsetParentRect.top + caretPosition.top + caretHeight
        }, viewportHeight = Math.max(document.documentElement.clientHeight, window.innerHeight || 0);
        if (suggestions) {
          var position = {};
          if (suggestionsPortalHost) {
            position.position = "fixed";
            var left = viewportRelative.left, top = viewportRelative.top;
            left -= getComputedStyleLengthProp(suggestions, "margin-left"), top -= getComputedStyleLengthProp(suggestions, "margin-top"), 
            left -= highlighter.scrollLeft, top -= highlighter.scrollTop;
            var viewportWidth = Math.max(document.documentElement.clientWidth, window.innerWidth || 0);
            left + suggestions.offsetWidth > viewportWidth ? position.left = Math.max(0, viewportWidth - suggestions.offsetWidth) : position.left = left, 
            allowSuggestionsAboveCursor && top + suggestions.offsetHeight > viewportHeight && suggestions.offsetHeight < top - caretHeight ? position.top = Math.max(0, top - suggestions.offsetHeight - caretHeight) : position.top = top;
          } else {
            var _left = caretPosition.left - highlighter.scrollLeft, _top = caretPosition.top - highlighter.scrollTop;
            _left + suggestions.offsetWidth > _this.containerRef.offsetWidth ? position.right = 0 : position.left = _left, 
            allowSuggestionsAboveCursor && viewportRelative.top - highlighter.scrollTop + suggestions.offsetHeight > viewportHeight && suggestions.offsetHeight < caretOffsetParentRect.top - caretHeight - highlighter.scrollTop ? position.top = _top - suggestions.offsetHeight - caretHeight : position.top = _top;
          }
          isEqual(position, _this.state.suggestionsPosition) || _this.setState({
            suggestionsPosition: position
          });
        }
      }
    }), _defineProperty(_assertThisInitialized(_this), "updateHighlighterScroll", function() {
      if (_this.inputRef && _this.highlighterRef) {
        var input = _this.inputRef, highlighter = ReactDOM.findDOMNode(_this.highlighterRef);
        highlighter.scrollLeft = input.scrollLeft, highlighter.scrollTop = input.scrollTop, 
        highlighter.height = input.height;
      }
    }), _defineProperty(_assertThisInitialized(_this), "handleCompositionStart", function() {
      isComposing = !0;
    }), _defineProperty(_assertThisInitialized(_this), "handleCompositionEnd", function() {
      isComposing = !1;
    }), _defineProperty(_assertThisInitialized(_this), "setSelection", function(selectionStart, selectionEnd) {
      if (null !== selectionStart && null !== selectionEnd) {
        var el = _this.inputRef;
        if (el.setSelectionRange) el.setSelectionRange(selectionStart, selectionEnd); else if (el.createTextRange) {
          var range = el.createTextRange();
          range.collapse(!0), range.moveEnd("character", selectionEnd), range.moveStart("character", selectionStart), 
          range.select();
        }
      }
    }), _defineProperty(_assertThisInitialized(_this), "updateMentionsQueries", function(plainTextValue, caretPosition) {
      _this._queryId++, _this.suggestions = {}, _this.setState({
        suggestions: {}
      });
      var value = _this.props.value || "", children = _this.props.children, config = readConfigFromChildren(children), positionInValue = mapPlainTextIndex(value, config, caretPosition, "NULL");
      if (null !== positionInValue) {
        var substringStartIndex = getEndOfLastMention(value.substring(0, positionInValue), config), substring = plainTextValue.substring(substringStartIndex, caretPosition);
        React__default.Children.forEach(children, function(child, childIndex) {
          if (child) {
            var regex = makeTriggerRegex(child.props.trigger, _this.props), match = substring.match(regex);
            if (match) {
              var querySequenceStart = substringStartIndex + substring.indexOf(match[1], match.index);
              _this.queryData(match[2], childIndex, querySequenceStart, querySequenceStart + match[1].length, plainTextValue);
            }
          }
        });
      }
    }), _defineProperty(_assertThisInitialized(_this), "clearSuggestions", function() {
      _this._queryId++, _this.suggestions = {}, _this.setState({
        suggestions: {},
        focusIndex: 0
      });
    }), _defineProperty(_assertThisInitialized(_this), "queryData", function(query, childIndex, querySequenceStart, querySequenceEnd, plainTextValue) {
      var mentionChild = React.Children.toArray(_this.props.children)[childIndex], syncResult = getDataProvider(mentionChild.props.data)(query, _this.updateSuggestions.bind(null, _this._queryId, childIndex, query, querySequenceStart, querySequenceEnd, plainTextValue));
      syncResult instanceof Array && _this.updateSuggestions(_this._queryId, childIndex, query, querySequenceStart, querySequenceEnd, plainTextValue, syncResult);
    }), _defineProperty(_assertThisInitialized(_this), "updateSuggestions", function(queryId, childIndex, query, querySequenceStart, querySequenceEnd, plainTextValue, results) {
      if (queryId === _this._queryId) {
        _this.suggestions = _objectSpread({}, _this.suggestions, _defineProperty({}, childIndex, {
          queryInfo: {
            childIndex: childIndex,
            query: query,
            querySequenceStart: querySequenceStart,
            querySequenceEnd: querySequenceEnd,
            plainTextValue: plainTextValue
          },
          results: results
        }));
        var focusIndex = _this.state.focusIndex, suggestionsCount = countSuggestions(_this.suggestions);
        _this.setState({
          suggestions: _this.suggestions,
          focusIndex: focusIndex >= suggestionsCount ? Math.max(suggestionsCount - 1, 0) : focusIndex
        });
      }
    }), _defineProperty(_assertThisInitialized(_this), "addMention", function(_ref2, _ref3) {
      var id = _ref2.id, display = _ref2.display, childIndex = _ref3.childIndex, querySequenceStart = _ref3.querySequenceStart, querySequenceEnd = _ref3.querySequenceEnd, plainTextValue = _ref3.plainTextValue, value = _this.props.value || "", config = readConfigFromChildren(_this.props.children), _mentionsChild$props = React.Children.toArray(_this.props.children)[childIndex].props, markup = _mentionsChild$props.markup, displayTransform = _mentionsChild$props.displayTransform, appendSpaceOnAdd = _mentionsChild$props.appendSpaceOnAdd, onAdd = _mentionsChild$props.onAdd, start = mapPlainTextIndex(value, config, querySequenceStart, "START"), end = start + querySequenceEnd - querySequenceStart, insert = makeMentionsMarkup(markup, id, display);
      appendSpaceOnAdd && (insert += " ");
      var newValue = spliceString(value, start, end, insert);
      _this.inputRef.focus();
      var displayValue = displayTransform(id, display);
      appendSpaceOnAdd && (displayValue += " ");
      var newCaretPosition = querySequenceStart + displayValue.length;
      _this.setState({
        selectionStart: newCaretPosition,
        selectionEnd: newCaretPosition,
        setSelectionAfterMentionChange: !0
      });
      var eventMock = {
        target: {
          value: newValue
        }
      }, mentions = getMentions(newValue, config), newPlainTextValue = spliceString(plainTextValue, querySequenceStart, querySequenceEnd, displayValue);
      _this.executeOnChange(eventMock, newValue, newPlainTextValue, mentions), onAdd && onAdd(id, display), 
      _this.clearSuggestions();
    }), _defineProperty(_assertThisInitialized(_this), "isLoading", function() {
      var isLoading = !1;
      return React__default.Children.forEach(_this.props.children, function(child) {
        isLoading = isLoading || child && child.props.isLoading;
      }), isLoading;
    }), _defineProperty(_assertThisInitialized(_this), "_queryId", 0), _this.suggestions = {}, 
    _this.handleCopy = _this.handleCopy.bind(_assertThisInitialized(_this)), _this.handleCut = _this.handleCut.bind(_assertThisInitialized(_this)), 
    _this.handlePaste = _this.handlePaste.bind(_assertThisInitialized(_this)), _this.state = {
      focusIndex: 0,
      selectionStart: null,
      selectionEnd: null,
      suggestions: {},
      caretPosition: null,
      suggestionsPosition: null
    }, _this;
  }
  return _inherits(MentionsInput, _React$Component), _createClass(MentionsInput, [ {
    key: "componentDidMount",
    value: function() {
      this.props.EXPERIMENTAL_cutCopyPaste && (document.addEventListener("copy", this.handleCopy), 
      document.addEventListener("cut", this.handleCut), document.addEventListener("paste", this.handlePaste)), 
      this.updateSuggestionsPosition();
    }
  }, {
    key: "componentDidUpdate",
    value: function(prevProps, prevState) {
      prevState.suggestionsPosition === this.state.suggestionsPosition && this.updateSuggestionsPosition(), 
      this.state.setSelectionAfterMentionChange && (this.setState({
        setSelectionAfterMentionChange: !1
      }), this.setSelection(this.state.selectionStart, this.state.selectionEnd));
    }
  }, {
    key: "componentWillUnmount",
    value: function() {
      this.props.EXPERIMENTAL_cutCopyPaste && (document.removeEventListener("copy", this.handleCopy), 
      document.removeEventListener("cut", this.handleCut), document.removeEventListener("paste", this.handlePaste));
    }
  }, {
    key: "render",
    value: function() {
      var _this2 = this;
      return React__default.createElement("div", _extends({
        ref: function(el) {
          _this2.containerRef = el;
        }
      }, this.props.style), this.renderControl(), this.renderSuggestionsOverlay());
    }
  }, {
    key: "handlePaste",
    value: function(event) {
      if (event.target === this.inputRef) {
        event.preventDefault();
        var _this$state3 = this.state, selectionStart = _this$state3.selectionStart, selectionEnd = _this$state3.selectionEnd, _this$props6 = this.props, value = _this$props6.value, children = _this$props6.children, config = readConfigFromChildren(children), markupStartIndex = mapPlainTextIndex(value, config, selectionStart, "START"), markupEndIndex = mapPlainTextIndex(value, config, selectionEnd, "END"), pastedMentions = event.clipboardData.getData("text/react-mentions"), pastedData = event.clipboardData.getData("text/plain"), newValue = spliceString(value, markupStartIndex, markupEndIndex, pastedMentions || pastedData), newPlainTextValue = getPlainText(newValue, config), eventMock = {
          target: _objectSpread({}, event.target, {
            value: newValue
          })
        };
        this.executeOnChange(eventMock, newValue, newPlainTextValue, getMentions(newValue, config));
      }
    }
  }, {
    key: "saveSelectionToClipboard",
    value: function(event) {
      var _this$state4 = this.state, selectionStart = _this$state4.selectionStart, selectionEnd = _this$state4.selectionEnd, _this$props7 = this.props, children = _this$props7.children, value = _this$props7.value, config = readConfigFromChildren(children), markupStartIndex = mapPlainTextIndex(value, config, selectionStart, "START"), markupEndIndex = mapPlainTextIndex(value, config, selectionEnd, "END");
      event.clipboardData.setData("text/plain", event.target.value.slice(selectionStart, selectionEnd)), 
      event.clipboardData.setData("text/react-mentions", value.slice(markupStartIndex, markupEndIndex));
    }
  }, {
    key: "handleCopy",
    value: function(event) {
      event.target === this.inputRef && (event.preventDefault(), this.saveSelectionToClipboard(event));
    }
  }, {
    key: "handleCut",
    value: function(event) {
      if (event.target === this.inputRef) {
        event.preventDefault(), this.saveSelectionToClipboard(event);
        var _this$state5 = this.state, selectionStart = _this$state5.selectionStart, selectionEnd = _this$state5.selectionEnd, _this$props8 = this.props, children = _this$props8.children, value = _this$props8.value, config = readConfigFromChildren(children), markupStartIndex = mapPlainTextIndex(value, config, selectionStart, "START"), markupEndIndex = mapPlainTextIndex(value, config, selectionEnd, "END"), newValue = [ value.slice(0, markupStartIndex), value.slice(markupEndIndex) ].join(""), newPlainTextValue = getPlainText(newValue, config), eventMock = {
          target: _objectSpread({}, event.target, {
            value: newPlainTextValue
          })
        };
        this.executeOnChange(eventMock, newValue, newPlainTextValue, getMentions(value, config));
      }
    }
  } ]), MentionsInput;
}(React__default.Component);

_defineProperty(MentionsInput, "propTypes", propTypes), _defineProperty(MentionsInput, "defaultProps", {
  singleLine: !1,
  allowSuggestionsAboveCursor: !1,
  onKeyDown: function() {
    return null;
  },
  onSelect: function() {
    return null;
  },
  onBlur: function() {
    return null;
  }
});

var getComputedStyleLengthProp = function(forElement, propertyName) {
  var length = parseFloat(window.getComputedStyle(forElement, null).getPropertyValue(propertyName));
  return isFinite(length) ? length : 0;
}, isMobileSafari = "undefined" != typeof navigator && /iPhone|iPad|iPod/i.test(navigator.userAgent), styled$3 = substyle.defaultStyle({
  position: "relative",
  overflowY: "visible",
  input: {
    display: "block",
    position: "absolute",
    top: 0,
    boxSizing: "border-box",
    backgroundColor: "transparent",
    width: "inherit",
    fontFamily: "inherit",
    fontSize: "inherit"
  },
  "&multiLine": {
    input: _objectSpread({
      width: "100%",
      height: "100%",
      bottom: 0,
      overflow: "hidden",
      resize: "none"
    }, isMobileSafari ? {
      marginTop: 1,
      marginLeft: -3
    } : null)
  }
}, function(_ref4) {
  var singleLine = _ref4.singleLine;
  return {
    "&singleLine": singleLine,
    "&multiLine": !singleLine
  };
}), MentionsInput$1 = styled$3(MentionsInput), styled$4 = substyle.defaultStyle({
  fontWeight: "inherit"
}), Mention = styled$4(function(_ref) {
  var display = _ref.display, style = _ref.style;
  return React__default.createElement("strong", style, display);
});

Mention.propTypes = {
  onAdd: PropTypes.func,
  onRemove: PropTypes.func,
  renderSuggestion: PropTypes.func,
  trigger: PropTypes.oneOfType([ PropTypes.string, PropTypes.instanceOf(RegExp) ]),
  markup: PropTypes.string,
  displayTransform: PropTypes.func,
  allowSpaceInQuery: PropTypes.bool,
  isLoading: PropTypes.bool
}, Mention.defaultProps = {
  trigger: "@",
  markup: "@[__display__](__id__)",
  displayTransform: function(id, display) {
    return display || id;
  },
  onAdd: function() {
    return null;
  },
  onRemove: function() {
    return null;
  },
  renderSuggestion: null,
  isLoading: !1,
  appendSpaceOnAdd: !1
}, exports.Mention = Mention, exports.MentionsInput = MentionsInput$1;
