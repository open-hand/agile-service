import React from 'react';
import {
  Select, TextField, TextArea, NumberField,
} from 'choerodon-ui/pro/lib';
import { set } from 'lodash';
import { toJS } from 'mobx';
import SelectUser from '@/components/select/select-user';
import { getMenuType } from '@/utils/common';
import { userApi } from '@/api';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import SelectComponent from '@/components/select/select-component';
import SelectLabel from '@/components/select/select-label';
import SelectVersion from '@/components/select/select-version';
import SelectSprint from '@/components/select/select-sprint';
import SelectEpic from '@/components/select/select-epic';
import { IField, IFieldType } from '@/common/types';
import SelectEnvironment from '@/components/select/select-environment';
import SelectPickDate from './select-date-pick';
import { InjectedRenderComponent } from '../page-issue-type/components/sort-table/injectComponent';

interface IRenderFieldProps {
  data: { fieldCode: string, fieldType: IFieldType, defaultValue?: any, fieldOptions?: Array<any>, extraConfig?: boolean }
  dataRef?: React.MutableRefObject<any>
  style?: React.CSSProperties,
  // otherProps?: SelectProps | any
  [propsName: string]: any
}
function renderEditor({
  data, dataRef, style, ...otherProps
}: IRenderFieldProps) {
  const { fieldType, fieldCode, defaultValue: propsDefaultValue } = data;
  const defaultValue = toJS(propsDefaultValue);
  switch (fieldCode) {
    case 'component':
      return (
        <SelectComponent
          // trigger={['click'] as any}
          style={style}
          multiple={['checkbox', 'multiple'].includes(fieldType)}
          dataRef={dataRef}
          {...otherProps}

        />
      );
    case 'label':
      return <SelectLabel multiple={['checkbox', 'multiple'].includes(fieldType)} dataRef={dataRef} style={style} {...otherProps} />;
    case 'influenceVersion':
    case 'fixVersion':
      return <SelectVersion valueField="versionId" multiple={['checkbox', 'multiple'].includes(fieldType)} dataRef={dataRef} style={style} {...otherProps} />;
    case 'sprint':
      return <SelectSprint multiple={['checkbox', 'multiple'].includes(fieldType)} dataRef={dataRef} style={style} {...otherProps} />;
    case 'epic':
      return <SelectEpic multiple={['checkbox', 'multiple'].includes(fieldType)} dataRef={dataRef} style={style} {...otherProps} />;
    case 'environment':
      return <SelectEnvironment multiple={['checkbox', 'multiple'].includes(fieldType)} afterLoad={(list) => dataRef && set(dataRef, 'current', list)} style={style} {...otherProps} />;
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
    console.log('ssssss', data);
    return (
      <Select
        multiple={['checkbox', 'multiple'].includes(fieldType)}
        style={style}
        {...otherProps}
      >
        {fieldOptions.map((item: any) => <Select.Option value={item.id || item.tempKey}>{item.value}</Select.Option>)}
      </Select>
    );
  }
  switch (fieldType) {
    case 'input':
      return <TextField maxLength={100} {...otherProps} />;
    case 'text':
      return <TextArea rows={3} maxLength={255} {...otherProps} />;
    case 'multiMember':
    case 'member':
    {
      const type = getMenuType();
      return (
        <SelectUser
          selectedUser={typeof (defaultValue) === 'object' ? defaultValue : undefined}
          autoQueryConfig={typeof (defaultValue) === 'string' ? {
            selectedUserIds: defaultValue.split(','),
            queryUserRequest: async (userId: string | number) => (type === 'project' ? userApi.getAllInProject('', undefined, userId as number) : userApi.getAllInOrg('', undefined, userId as number)),
          } : undefined}
          multiple={fieldType === 'multiMember'}
          clearButton
          dataRef={dataRef}
          style={style}
          request={({ filter, page }) => (type === 'project' ? userApi.getAllInProject(filter, page) : userApi.getAllInOrg(filter, page))}
          {...otherProps}
        />
      );
    }
    case 'number': {
      const { extraConfig } = data;
      return <NumberField step={extraConfig ? 0.1 : 1} {...otherProps} />;
    }
    default:
      break;
  }
  return <TextField {...otherProps} />;
}

export default renderEditor;
