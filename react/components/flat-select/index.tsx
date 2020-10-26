/* eslint-disable react/static-property-placement */
/* eslint-disable max-classes-per-file */
import React, { isValidElement, ReactNode, CSSProperties } from 'react';
import { observer } from 'mobx-react';
import { Tooltip, Icon } from 'choerodon-ui/pro';
import isNil from 'lodash/isNil';
import isString from 'lodash/isString';
import noop from 'lodash/noop';
import { Select, SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { pxToRem } from 'choerodon-ui/lib/_util/UnitConvertor';
import measureTextWidth from 'choerodon-ui/pro/lib/_util/measureTextWidth';
import { getWidth } from '@/components/issue-search/custom-fields/utils';
import './index.less';

const { Option, OptGroup } = Select;

class FlatSelect<T extends SelectProps> extends Select<T> {
  static defaultProps = {
    ...Select.defaultProps,
    dropdownMatchSelectWidth: false,
  };

  // @ts-ignore
  getWrapperClassNames(...args): string {
    const { prefixCls, multiple, range } = this;
    const suffix = this.getSuffix();
    const prefix = this.getPrefix();
    return super.getWrapperClassNames(
      {
        'flat-select': true,
        [`${prefixCls}-empty`]: this.isEmpty(),
        // @ts-ignore
        [`${prefixCls}-suffix-button`]: isValidElement<{ onClick; }>(suffix),
        [`${prefixCls}-multiple`]: multiple,
        [`${prefixCls}-range`]: range,
        // @ts-ignore
        [`${prefixCls}-prefix-button`]: isValidElement<{ onClick; }>(prefix),
      },
      ...args,
    );
  }

  getInnerSpanButton(): ReactNode {
    const {
      props: { clearButton },
      prefixCls,
    } = this;
    if (clearButton && !this.isReadOnly()) {
      return this.wrapperInnerSpanButton(
        <Icon type="close" onClick={this.handleClearButtonClick} />,
        {
          className: `${prefixCls}-clear-button`,
        },
      );
    }
    return null;
  }

  // renderMultipleEditor(props: T) {
  //   const { style } = this.props;
  //   const { text } = this;
  //   const editorStyle = {} as CSSProperties;
  //   if (!this.editable) {
  //     editorStyle.position = 'absolute';
  //     editorStyle.left = 0;
  //     editorStyle.top = 0;
  //     editorStyle.zIndex = -1;
  //     // eslint-disable-next-line no-param-reassign
  //     props.readOnly = true;
  //   } else if (text) {
  //     editorStyle.width = pxToRem(measureTextWidth(text, style));
  //   }
  //   return (
  //     <li key="text">
  //       <input {...(props as Object)} value={text || ''} style={editorStyle} />
  //     </li>
  //   );
  // }
  renderMultipleHolder() {
    const { name, multiple } = this;
    const hasValue = this.getValue() !== undefined && this.getValue() !== null;
    const placeholder = this.hasFloatLabel ? undefined : this.getPlaceholders()[0];
    const width = (hasValue ? 0 : measureTextWidth(placeholder || '') + 32);
    if (multiple) {
      return (
        <input
          key="value"
          className={`${this.prefixCls}-multiple-value`}
          value={this.toValueString(this.getValue()) || ''}
          name={name}
          onChange={noop}
          style={{ width }}
        />
      );
    }
    return undefined;
  }

  getEditor(): ReactNode {
    const {
      prefixCls,
      multiple,
    } = this;
    const otherProps = this.getOtherProps();
    if (multiple) {
      return (
        <div key="text" className={otherProps.className} style={{ height: 34 }} ref={this.saveTagContainer}>
          {this.renderMultipleValues()}
          {/* @ts-ignore */}
          {this.renderMultipleEditor({
            ...otherProps,
            className: `${prefixCls}-multiple-input`,
          })}
        </div>
      );
    }
    const text = this.getTextNode();
    const finalText = isString(text) ? text : this.getText(this.getValue());
    const hasValue = this.getValue() !== undefined && this.getValue() !== null;
    const placeholder = this.hasFloatLabel ? undefined : this.getPlaceholders()[0];
    const width = (hasValue ? measureTextWidth(finalText) + 52 : measureTextWidth(placeholder || '') + 32);
    if (isValidElement(text)) {
      otherProps.style = { ...otherProps.style, width, textIndent: -1000 };
    } else {
      otherProps.style = { width, ...otherProps.style };
    }
    return (
      <input
        key="text"
        {...otherProps}
        placeholder={placeholder}
        value={finalText}
        readOnly={!this.editable}
      />
    );
  }

  // @ts-ignore
  renderMultipleValues(readOnly?: boolean) {
    const multipleText = this.getMultipleText();
    return (
      <span className="flat-select-multiple-text">
        <Tooltip title={multipleText}>
          {multipleText}
        </Tooltip>
      </span>
    );
  }

  getMultipleText() {
    const values = this.getValues();
    const valueLength = values.length;
    const {
      range,
      props: { maxTagCount = valueLength },
    } = this;
    const repeats: Map<any, number> = new Map<any, number>();
    const texts = values.slice(0, maxTagCount).map((v) => {
      const key = this.getValueKey(v);
      const repeat = repeats.get(key) || 0;
      const text = range ? this.renderRangeValue(true, v, repeat) : this.processRenderer(v, repeat);
      repeats.set(key, repeat + 1);
      if (!isNil(text)) {
        return text;
      }
      return undefined;
    });
    return texts.join(',');
  }
}

@observer
export default class ObserverFlatSelect extends FlatSelect<SelectProps> {
  static defaultProps = FlatSelect.defaultProps;

  static Option = Option;

  static OptGroup = OptGroup;
}
