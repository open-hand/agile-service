/* eslint-disable react/static-property-placement */
/* eslint-disable max-classes-per-file */
import React from 'react';
import { observer } from 'mobx-react';
import { isNil } from 'lodash';
import { Select, SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { Tooltip } from 'choerodon-ui/pro/lib';
import CloseButton from 'choerodon-ui/pro/lib/field/CloseButton';
import classNames from 'classnames';
import './select-disabled.less';

const { Option, OptGroup } = Select;

class SelectU<T extends SelectProps> extends Select<T> {
  // @ts-ignore
  static defaultProps = {
    ...Select.defaultProps,
    popupCls: 'agile-select-drop-menu',
  };

  // @ts-ignore
  getWrapperClassNames(...args): string {
    const { prefixCls } = this;
    return super.getWrapperClassNames(
      {
        'agile-select': true,
      },
      ...args,
    );
  }

  renderMultipleValues(readOnly?: boolean) {
    const values = this.getValues();
    const valueLength = values.length;
    const {
      prefixCls,
      range,
      options,
      props: { maxTagCount = valueLength, maxTagPlaceholder, onOption },
    } = this;
    const { validationResults } = this.validator;
    const repeats: Map<any, number> = new Map<any, number>();
    const blockClassName = classNames(
      {
        [`${prefixCls}-multiple-block-disabled`]: this.isDisabled(),
      },
      `${prefixCls}-multiple-block`,
    );
    const tags = values.slice(0, maxTagCount).map((v) => {
      const key = this.getValueKey(v);
      const repeat = repeats.get(key) || 0;
      const record = this.findByValue(v);
      const optionProps = record ? onOption({ dataSet: options, record }) : {}; // 待优化
      const text = range ? this.renderRangeValue(true, v, repeat) : this.processRenderer(v, repeat); // 待优化
      repeats.set(key, repeat + 1);
      if (!isNil(text)) {
        const validationResult = validationResults.find((error) => error.value === v);
        const className = classNames(
          {
            [`${prefixCls}-multiple-block-invalid`]: validationResult,
          },
          blockClassName,
          {
            [`${prefixCls}-multiple-block-option-disabled`]: optionProps.disabled,
          },
        );
        const validationMessage = validationResult && this.renderValidationMessage(validationResult);
        const closeBtn = !optionProps.disabled && !this.isDisabled() && !this.isReadOnly() && (
          <CloseButton onClose={this.handleMutipleValueRemove} value={v} index={repeat} />
        );
        const inner = readOnly ? (
          <span className={className}>{text}</span>
        ) : (
          <li className={className}>
            <div>{text}</div>
            {closeBtn}
          </li>
        );
        return (
          <Tooltip
            suffixCls="form-tooltip c7n-pro-tooltip"
            key={`${key}-${repeat}`}
            title={validationMessage}
            theme="light"
            placement="bottomLeft"
            hidden={this.isValidationMessageHidden(validationMessage)}
          >
            {inner}
          </Tooltip>
        );
      }
      return undefined;
    });

    if (valueLength > maxTagCount) {
      let content: React.ReactNode = `+ ${valueLength - maxTagCount} ...`;
      if (maxTagPlaceholder) {
        const omittedValues = values.slice(maxTagCount, valueLength);
        content = typeof maxTagPlaceholder === 'function'
          ? (maxTagPlaceholder as any)(omittedValues)
          : maxTagPlaceholder;
      }
      tags.push(
        <li key="maxTagPlaceholder" className={blockClassName}>
          <div>{content}</div>
        </li>,
      );
    }

    return tags;
  }
}

@observer
export default class ObserverSelectU extends SelectU<SelectProps> {
  static defaultProps = SelectU.defaultProps;

  static Option = Option;

  static OptGroup = OptGroup;
}
