import React from 'react';
import { set } from 'lodash';
import { toJS } from 'mobx';
import { getMenuType } from '@/utils/common';
import { IFieldType } from '@/common/types';
import SelectPickDate from './select-date-pick';
import { InjectedRenderComponent } from '../page-issue-type/components/sort-table/injectComponent';
import { getAgileFields } from '@/components/field-pro';

interface IRenderFieldProps {
  data: { fieldCode: string, fieldType: IFieldType, defaultValue?: any, fieldOptions?: Array<any>, extraConfig?: boolean }
  dataRef?: React.MutableRefObject<any>
  style?: React.CSSProperties,
  // otherProps?: SelectProps | any
  [propsName: string]: any
}

function renderEditor({
  data, dataRef, style, ...otherProps
}: IRenderFieldProps):React.ReactElement {
  const { fieldType, fieldCode, defaultValue: propsDefaultValue } = data;
  const defaultValue = toJS(propsDefaultValue);
  switch (fieldCode) {
    case 'component':
      return getAgileFields([], {
        code: fieldCode,
        outputs: ['element'],
        props: {
          style,
          dataRef,
          ...otherProps,
        },
      })[0][0] as React.ReactElement;
    case 'label':
      return getAgileFields([], {
        code: fieldCode,
        outputs: ['element'],
        fieldType,
        props: {
          style,
          dataRef,
          ...otherProps,
        },
      })[0][0] as React.ReactElement;
    case 'influenceVersion':
    case 'fixVersion':
      return getAgileFields([], {
        code: fieldCode,
        outputs: ['element'],
        fieldType,
        props: {
          style,
          valueField: 'versionId',
          dataRef,
          ...otherProps,
        },
      })[0][0] as React.ReactElement;
    case 'sprint':
      return getAgileFields([], {
        code: fieldCode,
        outputs: ['element'],
        fieldType,
        props: {
          style,
          dataRef,
          ...otherProps,
        },
      })[0][0] as React.ReactElement;
    case 'epic':
      return getAgileFields([], {
        code: fieldCode,
        outputs: ['element'],
        fieldType,
        props: {
          style,
          dataRef,
          ...otherProps,
        },
      })[0][0] as React.ReactElement;
    case 'environment':
      return getAgileFields([], {
        code: fieldCode,
        outputs: ['element'],
        fieldType,
        props: {
          style,
          afterLoad: (list) => dataRef && set(dataRef, 'current', list),
          ...otherProps,
        },
      })[0][0] as React.ReactElement;
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
    return <SelectPickDate dateType={fieldType as any} style={style} defaultValue={defaultValue} {...otherProps} />;
  }

  if (['checkbox', 'multiple', 'radio', 'single'].includes(fieldType)) {
    const fieldOptions = data.fieldOptions || [];
    return getAgileFields([], [], {
      fieldType: fieldType as 'checkbox' | 'multiple' | 'radio' | 'single',
      outputs: ['element'],
      props: { style, fieldOptions: fieldOptions.map((item: any) => ({ ...item, id: item.id ?? item.tempKey })) },
    })[0][0] as React.ReactElement;
  }
  switch (fieldType) {
    case 'multiMember':
    case 'member':
    {
      const type = getMenuType();
      return getAgileFields([], [], {
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
