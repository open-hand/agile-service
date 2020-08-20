/* eslint-disable max-classes-per-file */
import React, { isValidElement, ReactNode } from 'react';
import { observer } from 'mobx-react';
import isNil from 'lodash/isNil';
import isString from 'lodash/isString';
import { Select, SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import './index.less';

const { Option, OptGroup } = Select;

class FlatSelect<T extends SelectProps> extends Select<T> {
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

    if (isValidElement(text)) {
      otherProps.style = { ...otherProps.style, textIndent: -1000 };
    }
    return (
      <input
        key="text"
        {...otherProps}
        placeholder={this.hasFloatLabel ? undefined : this.getPlaceholders()[0]}
        value={isString(text) ? text : this.getText(this.getValue())}
        readOnly={!this.editable}
      />
    );
  }

  // @ts-ignore
  renderMultipleValues(readOnly?: boolean) {
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
    return <span className="flat-select-multiple-text">{texts}</span>;
  }
}

@observer
export default class ObserverFlatSelect extends FlatSelect<SelectProps> {
  static Option = Option;

  static OptGroup = OptGroup;
}
