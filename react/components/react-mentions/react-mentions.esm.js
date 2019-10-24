import _toConsumableArray from '@babel/runtime/helpers/esm/toConsumableArray';
import _extends from '@babel/runtime/helpers/esm/extends';
import _objectSpread from '@babel/runtime/helpers/esm/objectSpread';
import _classCallCheck from '@babel/runtime/helpers/esm/classCallCheck';
import _createClass from '@babel/runtime/helpers/esm/createClass';
import _possibleConstructorReturn from '@babel/runtime/helpers/esm/possibleConstructorReturn';
import _getPrototypeOf from '@babel/runtime/helpers/esm/getPrototypeOf';
import _assertThisInitialized from '@babel/runtime/helpers/esm/assertThisInitialized';
import _inherits from '@babel/runtime/helpers/esm/inherits';
import _defineProperty from '@babel/runtime/helpers/esm/defineProperty';
import isEqual from 'lodash/isEqual';
import isNumber from 'lodash/isNumber';
import keys from 'lodash/keys';
import omit from 'lodash/omit';
import values from 'lodash/values';
import PropTypes from 'prop-types';
import React, { Children, Component } from 'react';
import ReactDOM from 'react-dom';
import substyle, { defaultStyle } from 'substyle';
import invariant from 'invariant';
import _slicedToArray from '@babel/runtime/helpers/esm/slicedToArray';

// escape RegExp special characters https://stackoverflow.com/a/9310752/5142490
var escapeRegex = function escapeRegex(str) {
  return str.replace(/[-[\]{}()*+?.,\\^$|#\s]/g, '\\$&');
};

var PLACEHOLDERS = {
  id: '__id__',
  display: '__display__'
};

var findPositionOfCapturingGroup = function findPositionOfCapturingGroup(markup, parameterName) {
  invariant(parameterName === 'id' || parameterName === 'display', "Second arg must be either \"id\" or \"display\", got: \"".concat(parameterName, "\"")); // find positions of placeholders in the markup

  var indexDisplay = markup.indexOf(PLACEHOLDERS.display);
  var indexId = markup.indexOf(PLACEHOLDERS.id); // set indices to null if not found

  if (indexDisplay < 0) indexDisplay = null;
  if (indexId < 0) indexId = null; // markup must contain one of the mandatory placeholders

  invariant(indexDisplay !== null || indexId !== null, "The markup '".concat(markup, "' does not contain either of the placeholders '__id__' or '__display__'"));

  if (indexDisplay !== null && indexId !== null) {
    // both placeholders are used, return 0 or 1 depending on the position of the requested parameter
    return parameterName === 'id' && indexId <= indexDisplay || parameterName === 'display' && indexDisplay <= indexId ? 0 : 1;
  } // just one placeholder is being used, we'll use the captured string for both parameters


  return 0;
};

var combineRegExps = function combineRegExps(regExps) {
  var serializedRegexParser = /^\/(.+)\/(\w+)?$/;
  return new RegExp(regExps.map(function (regex) {
    var _serializedRegexParse = serializedRegexParser.exec(regex.toString()),
        _serializedRegexParse2 = _slicedToArray(_serializedRegexParse, 3),
        regexString = _serializedRegexParse2[1],
        regexFlags = _serializedRegexParse2[2];

    invariant(!regexFlags, "RegExp flags are not supported. Change /".concat(regexString, "/").concat(regexFlags, " into /").concat(regexString, "/"));
    return "(".concat(regexString, ")");
  }).join('|'), 'g');
};

var countPlaceholders = function countPlaceholders(markup) {
  var count = 0;
  if (markup.indexOf('__id__') >= 0) count++;
  if (markup.indexOf('__display__') >= 0) count++;
  return count;
};

var emptyFn = function emptyFn() {}; // Finds all occurrences of the markup in the value and calls the `markupIteratee` callback for each of them.
// The optional `textIteratee` callback is called for each plain text ranges in between these markup occurrences.


var iterateMentionsMarkup = function iterateMentionsMarkup(value, config, markupIteratee) {
  var textIteratee = arguments.length > 3 && arguments[3] !== undefined ? arguments[3] : emptyFn;
  var regex = combineRegExps(config.map(function (c) {
    return c.regex;
  }));
  var accOffset = 2; // first is whole match, second is the for the capturing group of first regexp component

  var captureGroupOffsets = config.map(function (_ref) {
    var markup = _ref.markup;
    var result = accOffset; // + 1 is for the capturing group we add around each regexp component in combineRegExps

    accOffset += countPlaceholders(markup) + 1;
    return result;
  });
  var match;
  var start = 0;
  var currentPlainTextIndex = 0; // detect all mention markup occurrences in the value and iterate the matches

  while ((match = regex.exec(value)) !== null) {
    var offset = captureGroupOffsets.find(function (o) {
      return !!match[o];
    }); // eslint-disable-line no-loop-func

    var mentionChildIndex = captureGroupOffsets.indexOf(offset);
    var _config$mentionChildI = config[mentionChildIndex],
        markup = _config$mentionChildI.markup,
        displayTransform = _config$mentionChildI.displayTransform;
    var idPos = offset + findPositionOfCapturingGroup(markup, 'id');
    var displayPos = offset + findPositionOfCapturingGroup(markup, 'display');
    var id = match[idPos];
    var display = displayTransform(id, match[displayPos]);
    var substr = value.substring(start, match.index);
    textIteratee(substr, start, currentPlainTextIndex);
    currentPlainTextIndex += substr.length;
    markupIteratee(match[0], match.index, currentPlainTextIndex, id, display, mentionChildIndex, start);
    currentPlainTextIndex += display.length;
    start = regex.lastIndex;
  }

  if (start < value.length) {
    textIteratee(value.substring(start), start, currentPlainTextIndex);
  }
};

var getPlainText = function getPlainText(value, config) {
  var result = '';
  iterateMentionsMarkup(value, config, function (match, index, plainTextIndex, id, display) {
    result += display;
  }, function (plainText) {
    result += plainText;
  });
  return result;
};

// in the marked up value string.
// If the passed character index lies inside a mention, the value of `inMarkupCorrection` defines the
// correction to apply:
//   - 'START' to return the index of the mention markup's first char (default)
//   - 'END' to return the index after its last char
//   - 'NULL' to return null

var mapPlainTextIndex = function mapPlainTextIndex(value, config, indexInPlainText) {
  var inMarkupCorrection = arguments.length > 3 && arguments[3] !== undefined ? arguments[3] : 'START';

  if (typeof indexInPlainText !== 'number') {
    return indexInPlainText;
  }

  var result;

  var textIteratee = function textIteratee(substr, index, substrPlainTextIndex) {
    if (result !== undefined) return;

    if (substrPlainTextIndex + substr.length >= indexInPlainText) {
      // found the corresponding position in the current plain text range
      result = index + indexInPlainText - substrPlainTextIndex;
    }
  };

  var markupIteratee = function markupIteratee(markup, index, mentionPlainTextIndex, id, display, childIndex, lastMentionEndIndex) {
    if (result !== undefined) return;

    if (mentionPlainTextIndex + display.length > indexInPlainText) {
      // found the corresponding position inside current match,
      // return the index of the first or after the last char of the matching markup
      // depending on whether the `inMarkupCorrection`
      if (inMarkupCorrection === 'NULL') {
        result = null;
      } else {
        result = index + (inMarkupCorrection === 'END' ? markup.length : 0);
      }
    }
  };

  iterateMentionsMarkup(value, config, markupIteratee, textIteratee); // when a mention is at the end of the value and we want to get the caret position
  // at the end of the string, result is undefined

  return result === undefined ? value.length : result;
};

var spliceString = function spliceString(str, start, end, insert) {
  return str.substring(0, start) + insert + str.substring(end);
};

// guided by the textarea text selection ranges before and after the change

var applyChangeToValue = function applyChangeToValue(value, plainTextValue, _ref, config) {
  var selectionStartBefore = _ref.selectionStartBefore,
      selectionEndBefore = _ref.selectionEndBefore,
      selectionEndAfter = _ref.selectionEndAfter;
  var oldPlainTextValue = getPlainText(value, config);
  var lengthDelta = oldPlainTextValue.length - plainTextValue.length;

  if (selectionStartBefore === 'undefined') {
    selectionStartBefore = selectionEndAfter + lengthDelta;
  }

  if (selectionEndBefore === 'undefined') {
    selectionEndBefore = selectionStartBefore;
  } // Fixes an issue with replacing combined characters for complex input. Eg like acented letters on OSX


  if (selectionStartBefore === selectionEndBefore && selectionEndBefore === selectionEndAfter && oldPlainTextValue.length === plainTextValue.length) {
    selectionStartBefore = selectionStartBefore - 1;
  } // extract the insertion from the new plain text value


  var insert = plainTextValue.slice(selectionStartBefore, selectionEndAfter); // handling for Backspace key with no range selection

  var spliceStart = Math.min(selectionStartBefore, selectionEndAfter);
  var spliceEnd = selectionEndBefore;

  if (selectionStartBefore === selectionEndAfter) {
    // handling for Delete key with no range selection
    spliceEnd = Math.max(selectionEndBefore, selectionStartBefore + lengthDelta);
  }

  var mappedSpliceStart = mapPlainTextIndex(value, config, spliceStart, 'START');
  var mappedSpliceEnd = mapPlainTextIndex(value, config, spliceEnd, 'END');
  var controlSpliceStart = mapPlainTextIndex(value, config, spliceStart, 'NULL');
  var controlSpliceEnd = mapPlainTextIndex(value, config, spliceEnd, 'NULL');
  var willRemoveMention = controlSpliceStart === null || controlSpliceEnd === null;
  var newValue = spliceString(value, mappedSpliceStart, mappedSpliceEnd, insert);

  if (!willRemoveMention) {
    // test for auto-completion changes
    var controlPlainTextValue = getPlainText(newValue, config);

    if (controlPlainTextValue !== plainTextValue) {
      // some auto-correction is going on
      // find start of diff
      spliceStart = 0;

      while (plainTextValue[spliceStart] === controlPlainTextValue[spliceStart]) {
        spliceStart++;
      } // extract auto-corrected insertion


      insert = plainTextValue.slice(spliceStart, selectionEndAfter); // find index of the unchanged remainder

      spliceEnd = oldPlainTextValue.lastIndexOf(plainTextValue.substring(selectionEndAfter)); // re-map the corrected indices

      mappedSpliceStart = mapPlainTextIndex(value, config, spliceStart, 'START');
      mappedSpliceEnd = mapPlainTextIndex(value, config, spliceEnd, 'END');
      newValue = spliceString(value, mappedSpliceStart, mappedSpliceEnd, insert);
    }
  }

  return newValue;
};

// returns a the index of of the first char of the mention in the plain text.
// If indexInPlainText does not lie inside a mention, returns indexInPlainText.

var findStartOfMentionInPlainText = function findStartOfMentionInPlainText(value, config, indexInPlainText) {
  var result = indexInPlainText;
  var foundMention = false;

  var markupIteratee = function markupIteratee(markup, index, mentionPlainTextIndex, id, display, childIndex, lastMentionEndIndex) {
    if (mentionPlainTextIndex <= indexInPlainText && mentionPlainTextIndex + display.length > indexInPlainText) {
      result = mentionPlainTextIndex;
      foundMention = true;
    }
  };

  iterateMentionsMarkup(value, config, markupIteratee);

  if (foundMention) {
    return result;
  }
};

var getMentions = function getMentions(value, config) {
  var mentions = [];
  iterateMentionsMarkup(value, config, function (match, index, plainTextIndex, id, display, childIndex, start) {
    mentions.push({
      id: id,
      display: display,
      childIndex: childIndex,
      index: index,
      plainTextIndex: plainTextIndex
    });
  });
  return mentions;
};

var countSuggestions = function countSuggestions(suggestions) {
  return Object.values(suggestions).reduce(function (acc, _ref) {
    var results = _ref.results;
    return acc + results.length;
  }, 0);
};

var getEndOfLastMention = function getEndOfLastMention(value, config) {
  var mentions = getMentions(value, config);
  var lastMention = mentions[mentions.length - 1];
  return lastMention ? lastMention.plainTextIndex + lastMention.display.length : 0;
};

var markupToRegex = function markupToRegex(markup) {
  var escapedMarkup = escapeRegex(markup);
  var charAfterDisplay = markup[markup.indexOf(PLACEHOLDERS.display) + PLACEHOLDERS.display.length];
  var charAfterId = markup[markup.indexOf(PLACEHOLDERS.display) + PLACEHOLDERS.display.length];
  return new RegExp(escapedMarkup.replace(PLACEHOLDERS.display, "([^".concat(escapeRegex(charAfterDisplay || ''), "]+?)")).replace(PLACEHOLDERS.id, "([^".concat(escapeRegex(charAfterId || ''), "]+?)")));
};

var readConfigFromChildren = function readConfigFromChildren(children) {
  return Children.toArray(children).map(function (_ref) {
    var _ref$props = _ref.props,
        markup = _ref$props.markup,
        regex = _ref$props.regex,
        displayTransform = _ref$props.displayTransform;
    return {
      markup: markup,
      regex: regex ? coerceCapturingGroups(regex, markup) : markupToRegex(markup),
      displayTransform: displayTransform || function (id, display) {
        return display || id;
      }
    };
  });
}; // make sure that the custom regex defines the correct number of capturing groups


var coerceCapturingGroups = function coerceCapturingGroups(regex, markup) {
  var numberOfGroups = new RegExp(regex.toString() + '|').exec('').length - 1;
  var numberOfPlaceholders = countPlaceholders(markup);
  invariant(numberOfGroups === numberOfPlaceholders, "Number of capturing groups in RegExp ".concat(regex.toString(), " (").concat(numberOfGroups, ") does not match the number of placeholders in the markup '").concat(markup, "' (").concat(numberOfPlaceholders, ")"));
  return regex;
};

var makeMentionsMarkup = function makeMentionsMarkup(markup, id, display) {
  return markup.replace(PLACEHOLDERS.id, id).replace(PLACEHOLDERS.display, display);
};

var _generateComponentKey = function _generateComponentKey(usedKeys, id) {
  if (!usedKeys.hasOwnProperty(id)) {
    usedKeys[id] = 0;
  } else {
    usedKeys[id]++;
  }

  return id + '_' + usedKeys[id];
};

var Highlighter =
/*#__PURE__*/
function (_Component) {
  _inherits(Highlighter, _Component);

  function Highlighter() {
    var _this;

    _classCallCheck(this, Highlighter);

    _this = _possibleConstructorReturn(this, _getPrototypeOf(Highlighter).apply(this, arguments));
    _this.state = {
      lastPosition: {}
    };
    return _this;
  }

  _createClass(Highlighter, [{
    key: "componentDidMount",
    value: function componentDidMount() {
      this.notifyCaretPosition();
    }
  }, {
    key: "componentDidUpdate",
    value: function componentDidUpdate() {
      this.notifyCaretPosition();
    }
  }, {
    key: "notifyCaretPosition",
    value: function notifyCaretPosition() {
      if (!this.caretRef) {
        return;
      }

      var position = {
        left: this.caretRef.offsetLeft,
        top: this.caretRef.offsetTop
      };
      var lastPosition = this.state.lastPosition;

      if (isEqual(lastPosition, position)) {
        return;
      }

      this.setState({
        lastPosition: position
      });
      this.props.onCaretPositionChange(position);
    }
  }, {
    key: "render",
    value: function render() {
      var _this2 = this;

      var _this$props = this.props,
          selection = _this$props.selection,
          value = _this$props.value,
          style = _this$props.style,
          inputStyle = _this$props.inputStyle,
          children = _this$props.children;
      var config = readConfigFromChildren(children); // If there's a caret (i.e. no range selection), map the caret position into the marked up value

      var caretPositionInMarkup;

      if (selection.start === selection.end) {
        caretPositionInMarkup = mapPlainTextIndex(value, config, selection.start, 'START');
      }

      var resultComponents = [];
      var componentKeys = {}; // start by appending directly to the resultComponents

      var components = resultComponents;
      var substringComponentKey = 0;

      var textIteratee = function textIteratee(substr, index, indexInPlainText) {
        // check whether the caret element has to be inserted inside the current plain substring
        if (isNumber(caretPositionInMarkup) && caretPositionInMarkup >= index && caretPositionInMarkup <= index + substr.length) {
          // if yes, split substr at the caret position and insert the caret component
          var splitIndex = caretPositionInMarkup - index;
          components.push(_this2.renderSubstring(substr.substring(0, splitIndex), substringComponentKey)); // add all following substrings and mention components as children of the caret component

          components = [_this2.renderSubstring(substr.substring(splitIndex), substringComponentKey)];
        } else {
          // otherwise just push the plain text substring
          components.push(_this2.renderSubstring(substr, substringComponentKey));
        }

        substringComponentKey++;
      };

      var mentionIteratee = function mentionIteratee(markup, index, indexInPlainText, id, display, mentionChildIndex, lastMentionEndIndex) {
        // generate a component key based on the id
        var key = _generateComponentKey(componentKeys, id);

        components.push(_this2.getMentionComponentForMatch(id, display, mentionChildIndex, key));
      };

      iterateMentionsMarkup(value, config, mentionIteratee, textIteratee); // append a span containing a space, to ensure the last text line has the correct height

      components.push(' ');

      if (components !== resultComponents) {
        // if a caret component is to be rendered, add all components that followed as its children
        resultComponents.push(this.renderHighlighterCaret(components));
      }

      return React.createElement("div", _extends({}, style, {
        style: _objectSpread({}, inputStyle, style.style)
      }), resultComponents);
    }
  }, {
    key: "renderSubstring",
    value: function renderSubstring(string, key) {
      // set substring span to hidden, so that Emojis are not shown double in Mobile Safari
      return React.createElement("span", _extends({}, this.props.style('substring'), {
        key: key
      }), string);
    } // Returns a clone of the Mention child applicable for the specified type to be rendered inside the highlighter

  }, {
    key: "getMentionComponentForMatch",
    value: function getMentionComponentForMatch(id, display, mentionChildIndex, key) {
      var props = {
        id: id,
        display: display,
        key: key
      };
      var child = Children.toArray(this.props.children)[mentionChildIndex];
      return React.cloneElement(child, props);
    } // Renders an component to be inserted in the highlighter at the current caret position

  }, {
    key: "renderHighlighterCaret",
    value: function renderHighlighterCaret(children) {
      var _this3 = this;

      return React.createElement("span", _extends({}, this.props.style('caret'), {
        ref: function ref(el) {
          _this3.caretRef = el;
        },
        key: "caret"
      }), children);
    }
  }]);

  return Highlighter;
}(Component);

_defineProperty(Highlighter, "propTypes", {
  selection: PropTypes.shape({
    start: PropTypes.number,
    end: PropTypes.number
  }).isRequired,
  value: PropTypes.string.isRequired,
  onCaretPositionChange: PropTypes.func.isRequired,
  inputStyle: PropTypes.object,
  children: PropTypes.oneOfType([PropTypes.element, PropTypes.arrayOf(PropTypes.element)]).isRequired
});

_defineProperty(Highlighter, "defaultProps", {
  value: '',
  inputStyle: {}
});

var styled = defaultStyle({
  position: 'relative',
  width: 'inherit',
  color: 'transparent',
  overflow: 'hidden',
  whiteSpace: 'pre-wrap',
  wordWrap: 'break-word',
  '&singleLine': {
    whiteSpace: 'pre',
    wordWrap: null
  },
  substring: {
    visibility: 'hidden'
  }
}, function (props) {
  return {
    '&singleLine': props.singleLine
  };
});
var Highlighter$1 = styled(Highlighter);

var Suggestion =
/*#__PURE__*/
function (_Component) {
  _inherits(Suggestion, _Component);

  function Suggestion() {
    _classCallCheck(this, Suggestion);

    return _possibleConstructorReturn(this, _getPrototypeOf(Suggestion).apply(this, arguments));
  }

  _createClass(Suggestion, [{
    key: "render",
    value: function render() {
      var rest = omit(this.props, 'style', keys(Suggestion.propTypes));
      return React.createElement("li", _extends({}, rest, this.props.style), this.renderContent());
    }
  }, {
    key: "renderContent",
    value: function renderContent() {
      var _this$props = this.props,
          query = _this$props.query,
          renderSuggestion = _this$props.renderSuggestion,
          suggestion = _this$props.suggestion,
          index = _this$props.index,
          focused = _this$props.focused;
      var display = this.getDisplay();
      var highlightedDisplay = this.renderHighlightedDisplay(display, query);

      if (renderSuggestion) {
        return renderSuggestion(suggestion, query, highlightedDisplay, index, focused);
      }

      return highlightedDisplay;
    }
  }, {
    key: "getDisplay",
    value: function getDisplay() {
      var suggestion = this.props.suggestion;

      if (suggestion instanceof String) {
        return suggestion;
      }

      var id = suggestion.id,
          display = suggestion.display;

      if (id === undefined || !display) {
        return id;
      }

      return display;
    }
  }, {
    key: "renderHighlightedDisplay",
    value: function renderHighlightedDisplay(display) {
      var _this$props2 = this.props,
          query = _this$props2.query,
          style = _this$props2.style;
      var i = display.toLowerCase().indexOf(query.toLowerCase());

      if (i === -1) {
        return React.createElement("span", style('display'), display);
      }

      return React.createElement("span", style('display'), display.substring(0, i), React.createElement("b", style('highlight'), display.substring(i, i + query.length)), display.substring(i + query.length));
    }
  }]);

  return Suggestion;
}(Component);

_defineProperty(Suggestion, "propTypes", {
  id: PropTypes.oneOfType([PropTypes.string, PropTypes.number]).isRequired,
  query: PropTypes.string.isRequired,
  index: PropTypes.number.isRequired,
  suggestion: PropTypes.oneOfType([PropTypes.string, PropTypes.shape({
    id: PropTypes.oneOfType([PropTypes.string, PropTypes.number]).isRequired,
    display: PropTypes.string
  })]).isRequired,
  renderSuggestion: PropTypes.func,
  focused: PropTypes.bool
});

var styled$1 = defaultStyle({
  cursor: 'pointer'
}, function (props) {
  return {
    '&focused': props.focused
  };
});
var Suggestion$1 = styled$1(Suggestion);

function LoadingIndicator(_ref) {
  var style = _ref.style;
  var spinnerStyle = style('spinner');
  return React.createElement("div", style, React.createElement("div", spinnerStyle, React.createElement("div", spinnerStyle(['element', 'element1'])), React.createElement("div", spinnerStyle(['element', 'element2'])), React.createElement("div", spinnerStyle(['element', 'element3'])), React.createElement("div", spinnerStyle(['element', 'element4'])), React.createElement("div", spinnerStyle(['element', 'element5']))));
}

var LoadingIndicator$1 = substyle(LoadingIndicator);

var SuggestionsOverlay =
/*#__PURE__*/
function (_Component) {
  _inherits(SuggestionsOverlay, _Component);

  function SuggestionsOverlay() {
    _classCallCheck(this, SuggestionsOverlay);

    return _possibleConstructorReturn(this, _getPrototypeOf(SuggestionsOverlay).apply(this, arguments));
  }

  _createClass(SuggestionsOverlay, [{
    key: "componentDidUpdate",
    value: function componentDidUpdate() {
      if (!this.suggestionsRef || this.suggestionsRef.offsetHeight >= this.suggestionsRef.scrollHeight || !this.props.scrollFocusedIntoView) {
        return;
      }

      var scrollTop = this.suggestionsRef.scrollTop;

      var _this$suggestionsRef$ = this.suggestionsRef.children[this.props.focusIndex].getBoundingClientRect(),
          top = _this$suggestionsRef$.top,
          bottom = _this$suggestionsRef$.bottom;

      var _this$suggestionsRef$2 = this.suggestionsRef.getBoundingClientRect(),
          topContainer = _this$suggestionsRef$2.top;

      top = top - topContainer + scrollTop;
      bottom = bottom - topContainer + scrollTop;

      if (top < scrollTop) {
        this.suggestionsRef.scrollTop = top;
      } else if (bottom > this.suggestionsRef.offsetHeight) {
        this.suggestionsRef.scrollTop = bottom - this.suggestionsRef.offsetHeight;
      }
    }
  }, {
    key: "render",
    value: function render() {
      var _this = this;

      var _this$props = this.props,
          suggestions = _this$props.suggestions,
          isLoading = _this$props.isLoading,
          style = _this$props.style,
          onMouseDown = _this$props.onMouseDown; // do not show suggestions until there is some data

      if (countSuggestions(suggestions) === 0 && !isLoading) {
        return null;
      }

      return React.createElement("div", _extends({}, style, {
        onMouseDown: onMouseDown
      }), React.createElement("ul", _extends({
        ref: function ref(el) {
          _this.suggestionsRef = el;
        }
      }, style('list')), this.renderSuggestions()), this.renderLoadingIndicator());
    }
  }, {
    key: "renderSuggestions",
    value: function renderSuggestions() {
      var _this2 = this;

      return Object.values(this.props.suggestions).reduce(function (accResults, _ref) {
        var results = _ref.results,
            queryInfo = _ref.queryInfo;
        return [].concat(_toConsumableArray(accResults), _toConsumableArray(results.map(function (result, index) {
          return _this2.renderSuggestion(result, queryInfo, accResults.length + index);
        })));
      }, []);
    }
  }, {
    key: "renderSuggestion",
    value: function renderSuggestion(result, queryInfo, index) {
      var _this3 = this;

      var id = this.getID(result);
      var isFocused = index === this.props.focusIndex;
      var childIndex = queryInfo.childIndex,
          query = queryInfo.query;
      var renderSuggestion = Children.toArray(this.props.children)[childIndex].props.renderSuggestion;
      return React.createElement(Suggestion$1, {
        style: this.props.style('item'),
        key: "".concat(childIndex, "-").concat(id),
        id: id,
        query: query,
        index: index,
        renderSuggestion: renderSuggestion,
        suggestion: result,
        focused: isFocused,
        onClick: function onClick() {
          return _this3.select(result, queryInfo);
        },
        onMouseEnter: function onMouseEnter() {
          return _this3.handleMouseEnter(index);
        }
      });
    }
  }, {
    key: "getID",
    value: function getID(suggestion) {
      if (suggestion instanceof String) {
        return suggestion;
      }

      return suggestion.id;
    }
  }, {
    key: "renderLoadingIndicator",
    value: function renderLoadingIndicator() {
      if (!this.props.isLoading) {
        return;
      }

      return React.createElement(LoadingIndicator$1, {
        style: this.props.style('loadingIndicator')
      });
    }
  }, {
    key: "handleMouseEnter",
    value: function handleMouseEnter(index, ev) {
      if (this.props.onMouseEnter) {
        this.props.onMouseEnter(index);
      }
    }
  }, {
    key: "select",
    value: function select(suggestion, queryInfo) {
      this.props.onSelect(suggestion, queryInfo);
    }
  }]);

  return SuggestionsOverlay;
}(Component);

_defineProperty(SuggestionsOverlay, "propTypes", {
  suggestions: PropTypes.object.isRequired,
  focusIndex: PropTypes.number,
  scrollFocusedIntoView: PropTypes.bool,
  isLoading: PropTypes.bool,
  onSelect: PropTypes.func,
  children: PropTypes.oneOfType([PropTypes.element, PropTypes.arrayOf(PropTypes.element)]).isRequired
});

_defineProperty(SuggestionsOverlay, "defaultProps", {
  suggestions: {},
  onSelect: function onSelect() {
    return null;
  }
});

var styled$2 = defaultStyle(function (_ref2) {
  var position = _ref2.position;
  return _objectSpread({
    position: 'absolute',
    zIndex: 1,
    backgroundColor: 'white',
    marginTop: 14,
    minWidth: 100
  }, position, {
    list: {
      margin: 0,
      padding: 0,
      listStyleType: 'none'
    }
  });
});
var SuggestionsOverlay$1 = styled$2(SuggestionsOverlay);

var makeTriggerRegex = function makeTriggerRegex(trigger) {
  var options = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : {};

  if (trigger instanceof RegExp) {
    return trigger;
  } else {
    var allowSpaceInQuery = options.allowSpaceInQuery;
    var escapedTriggerChar = escapeRegex(trigger); // first capture group is the part to be replaced on completion
    // second capture group is for extracting the search query

    return new RegExp("(?:^|[^])(".concat(escapedTriggerChar, "([^").concat(allowSpaceInQuery ? '' : '\\s').concat(escapedTriggerChar, "]*))$"));
  }
};

var getDataProvider = function getDataProvider(data) {
  if (data instanceof Array) {
    // if data is an array, create a function to query that
    return function (query, callback) {
      var results = [];

      for (var i = 0, l = data.length; i < l; ++i) {
        var display = data[i].display || data[i].id;

        if (display.toLowerCase().indexOf(query.toLowerCase()) >= 0) {
          results.push(data[i]);
        }
      }

      return results;
    };
  } else {
    // expect data to be a query function
    return data;
  }
};

var KEY = {
  TAB: 9,
  RETURN: 13,
  ESC: 27,
  UP: 38,
  DOWN: 40
};
var isComposing = false;
var propTypes = {
  /**
   * If set to `true` a regular text input element will be rendered
   * instead of a textarea
   */
  singleLine: PropTypes.bool,
  allowSpaceInQuery: PropTypes.bool,
  EXPERIMENTAL_cutCopyPaste: PropTypes.bool,
  allowSuggestionsAboveCursor: PropTypes.bool,
  value: PropTypes.string,
  onKeyDown: PropTypes.func,
  onSelect: PropTypes.func,
  onBlur: PropTypes.func,
  onChange: PropTypes.func,
  suggestionsPortalHost: typeof Element === 'undefined' ? PropTypes.any : PropTypes.PropTypes.instanceOf(Element),
  inputRef: PropTypes.oneOfType([PropTypes.func, PropTypes.shape({
    current: typeof Element === 'undefined' ? PropTypes.any : PropTypes.instanceOf(Element)
  })]),
  children: PropTypes.oneOfType([PropTypes.element, PropTypes.arrayOf(PropTypes.element)]).isRequired
};

var MentionsInput =
/*#__PURE__*/
function (_React$Component) {
  _inherits(MentionsInput, _React$Component);

  function MentionsInput(_props) {
    var _this;

    _classCallCheck(this, MentionsInput);

    _this = _possibleConstructorReturn(this, _getPrototypeOf(MentionsInput).call(this, _props));

    _defineProperty(_assertThisInitialized(_this), "getInputProps", function (isTextarea) {
      var _this$props = _this.props,
          readOnly = _this$props.readOnly,
          disabled = _this$props.disabled,
          style = _this$props.style; // pass all props that we don't use through to the input control

      var props = omit(_this.props, 'style', keys(propTypes));
      return _objectSpread({}, props, style('input'), {
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
    });

    _defineProperty(_assertThisInitialized(_this), "renderControl", function () {
      var _this$props2 = _this.props,
          singleLine = _this$props2.singleLine,
          style = _this$props2.style;

      var inputProps = _this.getInputProps(!singleLine);

      return React.createElement("div", style('control'), _this.renderHighlighter(inputProps.style), singleLine ? _this.renderInput(inputProps) : _this.renderTextarea(inputProps));
    });

    _defineProperty(_assertThisInitialized(_this), "renderInput", function (props) {
      return React.createElement("input", _extends({
        type: "text",
        ref: _this.setInputRef
      }, props));
    });

    _defineProperty(_assertThisInitialized(_this), "renderTextarea", function (props) {
      return React.createElement("textarea", _extends({
        ref: _this.setInputRef
      }, props));
    });

    _defineProperty(_assertThisInitialized(_this), "setInputRef", function (el) {
      _this.inputRef = el;
      var inputRef = _this.props.inputRef;

      if (typeof inputRef === 'function') {
        inputRef(el);
      } else if (inputRef) {
        inputRef.current = el;
      }
    });

    _defineProperty(_assertThisInitialized(_this), "renderSuggestionsOverlay", function () {
      if (!isNumber(_this.state.selectionStart)) {
        // do not show suggestions when the input does not have the focus
        return null;
      }

      var suggestionsNode = React.createElement(SuggestionsOverlay$1, {
        style: _this.props.style('suggestions'),
        position: _this.state.suggestionsPosition,
        focusIndex: _this.state.focusIndex,
        scrollFocusedIntoView: _this.state.scrollFocusedIntoView,
        ref: function ref(el) {
          _this.suggestionsRef = el;
        },
        suggestions: _this.state.suggestions,
        onSelect: _this.addMention,
        onMouseDown: _this.handleSuggestionsMouseDown,
        onMouseEnter: function onMouseEnter(focusIndex) {
          return _this.setState({
            focusIndex: focusIndex,
            scrollFocusedIntoView: false
          });
        },
        isLoading: _this.isLoading()
      }, _this.props.children);

      if (_this.props.suggestionsPortalHost) {
        return ReactDOM.createPortal(suggestionsNode, _this.props.suggestionsPortalHost);
      } else {
        return suggestionsNode;
      }
    });

    _defineProperty(_assertThisInitialized(_this), "renderHighlighter", function (inputStyle) {
      var _this$state = _this.state,
          selectionStart = _this$state.selectionStart,
          selectionEnd = _this$state.selectionEnd;
      var _this$props3 = _this.props,
          singleLine = _this$props3.singleLine,
          children = _this$props3.children,
          value = _this$props3.value,
          style = _this$props3.style;
      return React.createElement(Highlighter$1, {
        ref: function ref(el) {
          _this.highlighterRef = el;
        },
        style: style('highlighter'),
        inputStyle: inputStyle,
        value: value,
        singleLine: singleLine,
        selection: {
          start: selectionStart,
          end: selectionEnd
        },
        onCaretPositionChange: function onCaretPositionChange(position) {
          return _this.setState({
            caretPosition: position
          });
        }
      }, children);
    });

    _defineProperty(_assertThisInitialized(_this), "getPlainText", function () {
      return getPlainText(_this.props.value || '', readConfigFromChildren(_this.props.children));
    });

    _defineProperty(_assertThisInitialized(_this), "executeOnChange", function (event) {
      for (var _len = arguments.length, args = new Array(_len > 1 ? _len - 1 : 0), _key = 1; _key < _len; _key++) {
        args[_key - 1] = arguments[_key];
      }

      if (_this.props.onChange) {
        var _this$props4;

        return (_this$props4 = _this.props).onChange.apply(_this$props4, [event].concat(args));
      }

      if (_this.props.valueLink) {
        var _this$props$valueLink;

        return (_this$props$valueLink = _this.props.valueLink).requestChange.apply(_this$props$valueLink, [event.target.value].concat(args));
      }
    });

    _defineProperty(_assertThisInitialized(_this), "handleChange", function (ev) {
      // if we are inside iframe, we need to find activeElement within its contentDocument
      var currentDocument = document.activeElement && document.activeElement.contentDocument || document;

      if (currentDocument.activeElement !== ev.target) {
        // fix an IE bug (blur from empty input element with placeholder attribute trigger "input" event)
        return;
      }

      var value = _this.props.value || '';
      var config = readConfigFromChildren(_this.props.children);
      var newPlainTextValue = ev.target.value; // Derive the new value to set by applying the local change in the textarea's plain text

      var newValue = applyChangeToValue(value, newPlainTextValue, {
        selectionStartBefore: _this.state.selectionStart,
        selectionEndBefore: _this.state.selectionEnd,
        selectionEndAfter: ev.target.selectionEnd
      }, config); // In case a mention is deleted, also adjust the new plain text value

      newPlainTextValue = getPlainText(newValue, config); // Save current selection after change to be able to restore caret position after rerendering

      var selectionStart = ev.target.selectionStart;
      var selectionEnd = ev.target.selectionEnd;
      var setSelectionAfterMentionChange = false; // Adjust selection range in case a mention will be deleted by the characters outside of the
      // selection range that are automatically deleted

      var startOfMention = findStartOfMentionInPlainText(value, config, selectionStart);

      if (startOfMention !== undefined && _this.state.selectionEnd > startOfMention) {
        // only if a deletion has taken place
        selectionStart = startOfMention;
        selectionEnd = selectionStart;
        setSelectionAfterMentionChange = true;
      }

      _this.setState({
        selectionStart: selectionStart,
        selectionEnd: selectionEnd,
        setSelectionAfterMentionChange: setSelectionAfterMentionChange
      });

      var mentions = getMentions(newValue, config); // Propagate change
      // let handleChange = this.getOnChange(this.props) || emptyFunction;

      var eventMock = {
        target: {
          value: newValue
        } // this.props.onChange.call(this, eventMock, newValue, newPlainTextValue, mentions);

      };

      _this.executeOnChange(eventMock, newValue, newPlainTextValue, mentions);
    });

    _defineProperty(_assertThisInitialized(_this), "handleSelect", function (ev) {
      // keep track of selection range / caret position
      _this.setState({
        selectionStart: ev.target.selectionStart,
        selectionEnd: ev.target.selectionEnd
      }); // do nothing while a IME composition session is active


      if (isComposing) return; // refresh suggestions queries

      var el = _this.inputRef;

      if (ev.target.selectionStart === ev.target.selectionEnd) {
        _this.updateMentionsQueries(el.value, ev.target.selectionStart);
      } else {
        _this.clearSuggestions();
      } // sync highlighters scroll position


      _this.updateHighlighterScroll();

      _this.props.onSelect(ev);
    });

    _defineProperty(_assertThisInitialized(_this), "handleKeyDown", function (ev) {
      // do not intercept key events if the suggestions overlay is not shown
      var suggestionsCount = countSuggestions(_this.state.suggestions);
      var suggestionsComp = _this.suggestionsRef;

      if (suggestionsCount === 0 || !suggestionsComp) {
        _this.props.onKeyDown(ev);

        return;
      }

      if (values(KEY).indexOf(ev.keyCode) >= 0) {
        ev.preventDefault();
      }

      switch (ev.keyCode) {
        case KEY.ESC:
          {
            _this.clearSuggestions();

            return;
          }

        case KEY.DOWN:
          {
            _this.shiftFocus(+1);

            return;
          }

        case KEY.UP:
          {
            _this.shiftFocus(-1);

            return;
          }

        case KEY.RETURN:
          {
            _this.selectFocused();

            return;
          }

        case KEY.TAB:
          {
            _this.selectFocused();

            return;
          }

        default:
          {
            return;
          }
      }
    });

    _defineProperty(_assertThisInitialized(_this), "shiftFocus", function (delta) {
      var suggestionsCount = countSuggestions(_this.state.suggestions);

      _this.setState({
        focusIndex: (suggestionsCount + _this.state.focusIndex + delta) % suggestionsCount,
        scrollFocusedIntoView: true
      });
    });

    _defineProperty(_assertThisInitialized(_this), "selectFocused", function () {
      var _this$state2 = _this.state,
          suggestions = _this$state2.suggestions,
          focusIndex = _this$state2.focusIndex;
      var _Object$values$reduce = Object.values(suggestions).reduce(function (acc, _ref) {
        var results = _ref.results,
            queryInfo = _ref.queryInfo;
        return [].concat(_toConsumableArray(acc), _toConsumableArray(results.map(function (result) {
          return {
            result: result,
            queryInfo: queryInfo
          };
        })));
      }, [])[focusIndex],
          result = _Object$values$reduce.result,
          queryInfo = _Object$values$reduce.queryInfo;

      _this.addMention(result, queryInfo);

      _this.setState({
        focusIndex: 0
      });
    });

    _defineProperty(_assertThisInitialized(_this), "handleBlur", function (ev) {
      var clickedSuggestion = _this._suggestionsMouseDown;
      _this._suggestionsMouseDown = false; // only reset selection if the mousedown happened on an element
      // other than the suggestions overlay

      if (!clickedSuggestion) {
        _this.setState({
          selectionStart: null,
          selectionEnd: null
        });
      }

      window.setTimeout(function () {
        _this.updateHighlighterScroll();
      }, 1);

      _this.props.onBlur(ev, clickedSuggestion);
    });

    _defineProperty(_assertThisInitialized(_this), "handleSuggestionsMouseDown", function (ev) {
      _this._suggestionsMouseDown = true;
    });

    _defineProperty(_assertThisInitialized(_this), "updateSuggestionsPosition", function () {
      var caretPosition = _this.state.caretPosition;
      var _this$props5 = _this.props,
          suggestionsPortalHost = _this$props5.suggestionsPortalHost,
          allowSuggestionsAboveCursor = _this$props5.allowSuggestionsAboveCursor;

      if (!caretPosition || !_this.suggestionsRef) {
        return;
      }

      var suggestions = ReactDOM.findDOMNode(_this.suggestionsRef);
      var highlighter = ReactDOM.findDOMNode(_this.highlighterRef); // first get viewport-relative position (highlighter is offsetParent of caret):

      var caretOffsetParentRect = highlighter.getBoundingClientRect();
      var caretHeight = getComputedStyleLengthProp(highlighter, 'font-size');
      var viewportRelative = {
        left: caretOffsetParentRect.left + caretPosition.left,
        top: caretOffsetParentRect.top + caretPosition.top + caretHeight
      };
      var viewportHeight = Math.max(document.documentElement.clientHeight, window.innerHeight || 0);

      if (!suggestions) {
        return;
      }

      var position = {}; // if suggestions menu is in a portal, update position to be releative to its portal node

      if (suggestionsPortalHost) {
        position.position = 'fixed';
        var left = viewportRelative.left;
        var top = viewportRelative.top; // absolute/fixed positioned elements are positioned according to their entire box including margins; so we remove margins here:

        left -= getComputedStyleLengthProp(suggestions, 'margin-left');
        top -= getComputedStyleLengthProp(suggestions, 'margin-top'); // take into account highlighter/textinput scrolling:

        left -= highlighter.scrollLeft;
        top -= highlighter.scrollTop; // guard for mentions suggestions list clipped by right edge of window

        var viewportWidth = Math.max(document.documentElement.clientWidth, window.innerWidth || 0);

        if (left + suggestions.offsetWidth > viewportWidth) {
          position.left = Math.max(0, viewportWidth - suggestions.offsetWidth);
        } else {
          position.left = left;
        } // guard for mentions suggestions list clipped by bottom edge of window if allowSuggestionsAboveCursor set to true.
        // Move the list up above the caret if it's getting cut off by the bottom of the window, provided that the list height
        // is small enough to NOT cover up the caret


        if (allowSuggestionsAboveCursor && top + suggestions.offsetHeight > viewportHeight && suggestions.offsetHeight < top - caretHeight) {
          position.top = Math.max(0, top - suggestions.offsetHeight - caretHeight);
        } else {
          position.top = top;
        }
      } else {
        var _left = caretPosition.left - highlighter.scrollLeft;

        var _top = caretPosition.top - highlighter.scrollTop; // guard for mentions suggestions list clipped by right edge of window


        if (_left + suggestions.offsetWidth > _this.containerRef.offsetWidth) {
          position.right = 0;
        } else {
          position.left = _left;
        } // guard for mentions suggestions list clipped by bottom edge of window if allowSuggestionsAboveCursor set to true.
        // move the list up above the caret if it's getting cut off by the bottom of the window, provided that the list height
        // is small enough to NOT cover up the caret


        if (allowSuggestionsAboveCursor && viewportRelative.top - highlighter.scrollTop + suggestions.offsetHeight > viewportHeight && suggestions.offsetHeight < caretOffsetParentRect.top - caretHeight - highlighter.scrollTop) {
          position.top = _top - suggestions.offsetHeight - caretHeight;
        } else {
          position.top = _top;
        }
      }

      if (isEqual(position, _this.state.suggestionsPosition)) {
        return;
      }

      _this.setState({
        suggestionsPosition: position
      });
    });

    _defineProperty(_assertThisInitialized(_this), "updateHighlighterScroll", function () {
      if (!_this.inputRef || !_this.highlighterRef) {
        // since the invocation of this function is deferred,
        // the whole component may have been unmounted in the meanwhile
        return;
      }

      var input = _this.inputRef;
      var highlighter = ReactDOM.findDOMNode(_this.highlighterRef);
      highlighter.scrollLeft = input.scrollLeft;
      highlighter.scrollTop = input.scrollTop;
      highlighter.height = input.height;
    });

    _defineProperty(_assertThisInitialized(_this), "handleCompositionStart", function () {
      isComposing = true;
    });

    _defineProperty(_assertThisInitialized(_this), "handleCompositionEnd", function () {
      isComposing = false;
    });

    _defineProperty(_assertThisInitialized(_this), "setSelection", function (selectionStart, selectionEnd) {
      if (selectionStart === null || selectionEnd === null) return;
      var el = _this.inputRef;

      if (el.setSelectionRange) {
        el.setSelectionRange(selectionStart, selectionEnd);
      } else if (el.createTextRange) {
        var range = el.createTextRange();
        range.collapse(true);
        range.moveEnd('character', selectionEnd);
        range.moveStart('character', selectionStart);
        range.select();
      }
    });

    _defineProperty(_assertThisInitialized(_this), "updateMentionsQueries", function (plainTextValue, caretPosition) {
      // Invalidate previous queries. Async results for previous queries will be neglected.
      _this._queryId++;
      _this.suggestions = {};

      _this.setState({
        suggestions: {}
      });

      var value = _this.props.value || '';
      var children = _this.props.children;
      var config = readConfigFromChildren(children);
      var positionInValue = mapPlainTextIndex(value, config, caretPosition, 'NULL'); // If caret is inside of mention, do not query

      if (positionInValue === null) {
        return;
      } // Extract substring in between the end of the previous mention and the caret


      var substringStartIndex = getEndOfLastMention(value.substring(0, positionInValue), config);
      var substring = plainTextValue.substring(substringStartIndex, caretPosition); // Check if suggestions have to be shown:
      // Match the trigger patterns of all Mention children on the extracted substring

      React.Children.forEach(children, function (child, childIndex) {
        if (!child) {
          return;
        }

        var regex = makeTriggerRegex(child.props.trigger, _this.props);
        var match = substring.match(regex);

        if (match) {
          var querySequenceStart = substringStartIndex + substring.indexOf(match[1], match.index);

          _this.queryData(match[2], childIndex, querySequenceStart, querySequenceStart + match[1].length, plainTextValue);
        }
      });
    });

    _defineProperty(_assertThisInitialized(_this), "clearSuggestions", function () {
      // Invalidate previous queries. Async results for previous queries will be neglected.
      _this._queryId++;
      _this.suggestions = {};

      _this.setState({
        suggestions: {},
        focusIndex: 0
      });
    });

    _defineProperty(_assertThisInitialized(_this), "queryData", function (query, childIndex, querySequenceStart, querySequenceEnd, plainTextValue) {
      var mentionChild = Children.toArray(_this.props.children)[childIndex];
      var provideData = getDataProvider(mentionChild.props.data);
      var syncResult = provideData(query, _this.updateSuggestions.bind(null, _this._queryId, childIndex, query, querySequenceStart, querySequenceEnd, plainTextValue));

      if (syncResult instanceof Array) {
        _this.updateSuggestions(_this._queryId, childIndex, query, querySequenceStart, querySequenceEnd, plainTextValue, syncResult);
      }
    });

    _defineProperty(_assertThisInitialized(_this), "updateSuggestions", function (queryId, childIndex, query, querySequenceStart, querySequenceEnd, plainTextValue, results) {
      // neglect async results from previous queries
      if (queryId !== _this._queryId) return; // save in property so that multiple sync state updates from different mentions sources
      // won't overwrite each other

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
      var focusIndex = _this.state.focusIndex;
      var suggestionsCount = countSuggestions(_this.suggestions);

      _this.setState({
        suggestions: _this.suggestions,
        focusIndex: focusIndex >= suggestionsCount ? Math.max(suggestionsCount - 1, 0) : focusIndex
      });
    });

    _defineProperty(_assertThisInitialized(_this), "addMention", function (_ref2, _ref3) {
      var id = _ref2.id,
          display = _ref2.display;
      var childIndex = _ref3.childIndex,
          querySequenceStart = _ref3.querySequenceStart,
          querySequenceEnd = _ref3.querySequenceEnd,
          plainTextValue = _ref3.plainTextValue;
      // Insert mention in the marked up value at the correct position
      var value = _this.props.value || '';
      var config = readConfigFromChildren(_this.props.children);
      var mentionsChild = Children.toArray(_this.props.children)[childIndex];
      var _mentionsChild$props = mentionsChild.props,
          markup = _mentionsChild$props.markup,
          displayTransform = _mentionsChild$props.displayTransform,
          appendSpaceOnAdd = _mentionsChild$props.appendSpaceOnAdd,
          onAdd = _mentionsChild$props.onAdd;
      var start = mapPlainTextIndex(value, config, querySequenceStart, 'START');
      var end = start + querySequenceEnd - querySequenceStart;
      var insert = makeMentionsMarkup(markup, id, display);

      if (appendSpaceOnAdd) {
        insert += ' ';
      }

      var newValue = spliceString(value, start, end, insert); // Refocus input and set caret position to end of mention

      _this.inputRef.focus();

      var displayValue = displayTransform(id, display);

      if (appendSpaceOnAdd) {
        displayValue += ' ';
      }

      var newCaretPosition = querySequenceStart + displayValue.length;

      _this.setState({
        selectionStart: newCaretPosition,
        selectionEnd: newCaretPosition,
        setSelectionAfterMentionChange: true
      }); // Propagate change


      var eventMock = {
        target: {
          value: newValue
        }
      };
      var mentions = getMentions(newValue, config);
      var newPlainTextValue = spliceString(plainTextValue, querySequenceStart, querySequenceEnd, displayValue);

      _this.executeOnChange(eventMock, newValue, newPlainTextValue, mentions);

      if (onAdd) {
        onAdd(id, display);
      } // Make sure the suggestions overlay is closed


      _this.clearSuggestions();
    });

    _defineProperty(_assertThisInitialized(_this), "isLoading", function () {
      var isLoading = false;
      React.Children.forEach(_this.props.children, function (child) {
        isLoading = isLoading || child && child.props.isLoading;
      });
      return isLoading;
    });

    _defineProperty(_assertThisInitialized(_this), "_queryId", 0);

    _this.suggestions = {};
    _this.handleCopy = _this.handleCopy.bind(_assertThisInitialized(_this));
    _this.handleCut = _this.handleCut.bind(_assertThisInitialized(_this));
    _this.handlePaste = _this.handlePaste.bind(_assertThisInitialized(_this));
    _this.state = {
      focusIndex: 0,
      selectionStart: null,
      selectionEnd: null,
      suggestions: {},
      caretPosition: null,
      suggestionsPosition: null
    };
    return _this;
  }

  _createClass(MentionsInput, [{
    key: "componentDidMount",
    value: function componentDidMount() {
      var EXPERIMENTAL_cutCopyPaste = this.props.EXPERIMENTAL_cutCopyPaste;

      if (EXPERIMENTAL_cutCopyPaste) {
        document.addEventListener('copy', this.handleCopy);
        document.addEventListener('cut', this.handleCut);
        document.addEventListener('paste', this.handlePaste);
      }

      this.updateSuggestionsPosition();
    }
  }, {
    key: "componentDidUpdate",
    value: function componentDidUpdate(prevProps, prevState) {
      // Update position of suggestions unless this componentDidUpdate was
      // triggered by an update to suggestionsPosition.
      if (prevState.suggestionsPosition === this.state.suggestionsPosition) {
        this.updateSuggestionsPosition();
      } // maintain selection in case a mention is added/removed causing
      // the cursor to jump to the end


      if (this.state.setSelectionAfterMentionChange) {
        this.setState({
          setSelectionAfterMentionChange: false
        });
        this.setSelection(this.state.selectionStart, this.state.selectionEnd);
      }
    }
  }, {
    key: "componentWillUnmount",
    value: function componentWillUnmount() {
      var EXPERIMENTAL_cutCopyPaste = this.props.EXPERIMENTAL_cutCopyPaste;

      if (EXPERIMENTAL_cutCopyPaste) {
        document.removeEventListener('copy', this.handleCopy);
        document.removeEventListener('cut', this.handleCut);
        document.removeEventListener('paste', this.handlePaste);
      }
    }
  }, {
    key: "render",
    value: function render() {
      var _this2 = this;

      return React.createElement("div", _extends({
        ref: function ref(el) {
          _this2.containerRef = el;
        }
      }, this.props.style), this.renderControl(), this.renderSuggestionsOverlay());
    }
  }, {
    key: "handlePaste",
    value: function handlePaste(event) {
      if (event.target !== this.inputRef) {
        return;
      }

      event.preventDefault();
      var _this$state3 = this.state,
          selectionStart = _this$state3.selectionStart,
          selectionEnd = _this$state3.selectionEnd;
      var _this$props6 = this.props,
          value = _this$props6.value,
          children = _this$props6.children;
      var config = readConfigFromChildren(children);
      var markupStartIndex = mapPlainTextIndex(value, config, selectionStart, 'START');
      var markupEndIndex = mapPlainTextIndex(value, config, selectionEnd, 'END');
      var pastedMentions = event.clipboardData.getData('text/react-mentions');
      var pastedData = event.clipboardData.getData('text/plain');
      var newValue = spliceString(value, markupStartIndex, markupEndIndex, pastedMentions || pastedData);
      var newPlainTextValue = getPlainText(newValue, config);
      var eventMock = {
        target: _objectSpread({}, event.target, {
          value: newValue
        })
      };
      this.executeOnChange(eventMock, newValue, newPlainTextValue, getMentions(newValue, config));
    }
  }, {
    key: "saveSelectionToClipboard",
    value: function saveSelectionToClipboard(event) {
      var _this$state4 = this.state,
          selectionStart = _this$state4.selectionStart,
          selectionEnd = _this$state4.selectionEnd;
      var _this$props7 = this.props,
          children = _this$props7.children,
          value = _this$props7.value;
      var config = readConfigFromChildren(children);
      var markupStartIndex = mapPlainTextIndex(value, config, selectionStart, 'START');
      var markupEndIndex = mapPlainTextIndex(value, config, selectionEnd, 'END');
      event.clipboardData.setData('text/plain', event.target.value.slice(selectionStart, selectionEnd));
      event.clipboardData.setData('text/react-mentions', value.slice(markupStartIndex, markupEndIndex));
    }
  }, {
    key: "handleCopy",
    value: function handleCopy(event) {
      if (event.target !== this.inputRef) {
        return;
      }

      event.preventDefault();
      this.saveSelectionToClipboard(event);
    }
  }, {
    key: "handleCut",
    value: function handleCut(event) {
      if (event.target !== this.inputRef) {
        return;
      }

      event.preventDefault();
      this.saveSelectionToClipboard(event);
      var _this$state5 = this.state,
          selectionStart = _this$state5.selectionStart,
          selectionEnd = _this$state5.selectionEnd;
      var _this$props8 = this.props,
          children = _this$props8.children,
          value = _this$props8.value;
      var config = readConfigFromChildren(children);
      var markupStartIndex = mapPlainTextIndex(value, config, selectionStart, 'START');
      var markupEndIndex = mapPlainTextIndex(value, config, selectionEnd, 'END');
      var newValue = [value.slice(0, markupStartIndex), value.slice(markupEndIndex)].join('');
      var newPlainTextValue = getPlainText(newValue, config);
      var eventMock = {
        target: _objectSpread({}, event.target, {
          value: newPlainTextValue
        })
      };
      this.executeOnChange(eventMock, newValue, newPlainTextValue, getMentions(value, config));
    } // Handle input element's change event

  }]);

  return MentionsInput;
}(React.Component);
/**
 * Returns the computed length property value for the provided element.
 * Note: According to spec and testing, can count on length values coming back in pixels. See https://developer.mozilla.org/en-US/docs/Web/CSS/used_value#Difference_from_computed_value
 */


_defineProperty(MentionsInput, "propTypes", propTypes);

_defineProperty(MentionsInput, "defaultProps", {
  singleLine: false,
  allowSuggestionsAboveCursor: false,
  onKeyDown: function onKeyDown() {
    return null;
  },
  onSelect: function onSelect() {
    return null;
  },
  onBlur: function onBlur() {
    return null;
  }
});

var getComputedStyleLengthProp = function getComputedStyleLengthProp(forElement, propertyName) {
  var length = parseFloat(window.getComputedStyle(forElement, null).getPropertyValue(propertyName));
  return isFinite(length) ? length : 0;
};

var isMobileSafari = typeof navigator !== 'undefined' && /iPhone|iPad|iPod/i.test(navigator.userAgent);
var styled$3 = defaultStyle({
  position: 'relative',
  overflowY: 'visible',
  input: {
    display: 'block',
    position: 'absolute',
    top: 0,
    boxSizing: 'border-box',
    backgroundColor: 'transparent',
    width: 'inherit',
    fontFamily: 'inherit',
    fontSize: 'inherit'
  },
  '&multiLine': {
    input: _objectSpread({
      width: '100%',
      height: '100%',
      bottom: 0,
      overflow: 'hidden',
      resize: 'none'
    }, isMobileSafari ? {
      marginTop: 1,
      marginLeft: -3
    } : null)
  }
}, function (_ref4) {
  var singleLine = _ref4.singleLine;
  return {
    '&singleLine': singleLine,
    '&multiLine': !singleLine
  };
});
var MentionsInput$1 = styled$3(MentionsInput);

var styled$4 = defaultStyle({
  fontWeight: 'inherit'
});
var Mention = styled$4(function (_ref) {
  var display = _ref.display,
      style = _ref.style;
  return React.createElement("strong", style, display);
});
Mention.propTypes = {
  /**
   * Called when a new mention is added in the input
   *
   * Example:
   *
   * ```js
   * function(id, display) {
   *   console.log("user " + display + " was mentioned!");
   * }
   * ```
   */
  onAdd: PropTypes.func,
  onRemove: PropTypes.func,
  renderSuggestion: PropTypes.func,
  trigger: PropTypes.oneOfType([PropTypes.string, PropTypes.instanceOf(RegExp)]),
  markup: PropTypes.string,
  displayTransform: PropTypes.func,

  /**
   * If set to `true` spaces will not interrupt matching suggestions
   */
  allowSpaceInQuery: PropTypes.bool,
  isLoading: PropTypes.bool
};
Mention.defaultProps = {
  trigger: '@',
  markup: '@[__display__](__id__)',
  displayTransform: function displayTransform(id, display) {
    return display || id;
  },
  onAdd: function onAdd() {
    return null;
  },
  onRemove: function onRemove() {
    return null;
  },
  renderSuggestion: null,
  isLoading: false,
  appendSpaceOnAdd: false
};

export { Mention, MentionsInput$1 as MentionsInput };
