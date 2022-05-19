import React from 'react';
import { merge, set } from 'lodash';
import { toJS } from 'mobx';
import { getMenuType } from '@/utils/common';
import { IFieldType } from '@/common/types';
import SelectPickDate from './select-date-pick';
import { InjectedRenderComponent } from '../page-issue-type/components/sort-table/injectComponent';
import { getAgileFields } from '@/components/field-pro';
import { isCodeInSystemComponents } from '@/components/field-pro/base/utils';
import { FORMAT_FIELDS, MINUTE } from '@/constants/DATE_FORMAT';

interface IRenderFieldProps {
  data: { fieldCode: string, fieldType: IFieldType, defaultValue?: any, fieldOptions?: Array<any>, extraConfig?: boolean }
  dataRef?: React.MutableRefObject<any>
  style?: React.CSSProperties,
  // otherProps?: SelectProps | any
  [propsName: string]: any
}
/**
 * @deprecated 后续将迁移到 field-pro
 * @param param0
 * @returns
 */
function renderEditor({
  data, dataRef, style, ...otherProps
}: IRenderFieldProps): React.ReactElement {
  const { fieldType, fieldCode, defaultValue: propsDefaultValue } = data;
  const defaultValue = toJS(propsDefaultValue);
  // 系统字段的时间类字段、人员类这里不取用
  if (!['date', 'time', 'datetime', 'multiMember', 'member'].includes(fieldType) && isCodeInSystemComponents(fieldCode)) {
    const el = getAgileFields([], {
      code: fieldCode as any,
      fieldType: fieldType as any,
      outputs: ['element'],
      props: {
        style,
        dataRef,
        valueField: fieldCode.toLowerCase().includes('version') ? 'versionId' : undefined,
        ...otherProps,
      },
    })[0][0] as React.ReactElement;
    return React.cloneElement(el, { ...el.props, dataRef });
  }
  switch (fieldCode) {
    case 'backlogType':
      // @ts-ignore
      return (
        <InjectedRenderComponent.backlogType
          // @ts-ignore
          style={style}
          // @ts-ignore
          multiple={['checkbox', 'multiple'].includes(fieldType)}
          // @ts-ignore
          afterLoad={(list) => dataRef && set(dataRef, 'current', list)}
          // @ts-ignore
          {...otherProps}
        />
      );
    case 'backlogClassification':
      // @ts-ignore
      return <InjectedRenderComponent.backlogClassification style={style} multiple={['checkbox', 'multiple'].includes(fieldType)} afterLoad={(list) => dataRef && set(dataRef, 'current', list)} {...otherProps} />;
    case 'progressFeedback':
      // @ts-ignore
      return <InjectedRenderComponent.progressFeedback style={style} multiple={['checkbox', 'multiple'].includes(fieldType)} afterLoad={(list) => dataRef && set(dataRef, 'current', list)} {...otherProps} />;
    default:

      break;
  }

  if (['date', 'time', 'datetime'].includes(fieldType)) {
    // 预计开始/结束时间、实际开始/结束时间精确到分
    const isFormatField = FORMAT_FIELDS.includes(fieldCode);
    const format = isFormatField ? MINUTE : undefined;
    return <SelectPickDate format={format} isCustomDateTimeView={isFormatField} dateType={fieldType as any} style={style} defaultValue={defaultValue} {...otherProps} />;
  }

  if (['checkbox', 'multiple', 'radio', 'single'].includes(fieldType)) {
    const fieldOptions = data.fieldOptions || [];
    const el = getAgileFields([], [], {
      fieldType: fieldType as 'checkbox' | 'multiple' | 'radio' | 'single',
      outputs: ['element'],
      props: {
        key: data.fieldCode,
        dataRef,
        selected: data.defaultValue,
        style,
        disabledRuleConfig: true,
        fieldOptions: fieldOptions.map((item: any) => ({ ...item, id: item.id ?? item.tempKey })),
        ...otherProps,
      } as any,
    })[0][0] as React.ReactElement;
    return React.cloneElement(el, { ...el.props, dataRef });
  }
  switch (fieldType) {
    case 'multiMember':
    case 'member':
    {
      const type = getMenuType();
      const el = getAgileFields([], [], {
        fieldType,
        outputs: ['element'],
        props: {
          level: type === 'project' ? 'project' : 'org',
          dataRef,
          selectedUser: typeof (defaultValue) === 'object' ? defaultValue : undefined,
          selected: typeof (defaultValue) === 'string' ? defaultValue.split(',') : undefined,
          style,
          clearButton: true,
          ...otherProps,
        },
      })[0][0] as React.ReactElement;
      return React.cloneElement(el, { ...el.props, dataRef });
    }

    case 'number': {
      const { extraConfig } = data;
      return getAgileFields([], [], { fieldType, outputs: ['element'], props: { step: extraConfig ? 0.1 : 1, ...otherProps } })[0][0] as React.ReactElement;
    }
    default:
      break;
  }
  return getAgileFields([], [], [
    {
      fieldType: fieldType as any,
      outputs: ['element'],
      props: {
        ...otherProps,
      },
    },
  ])[0][0] as React.ReactElement;
}

export default renderEditor;
