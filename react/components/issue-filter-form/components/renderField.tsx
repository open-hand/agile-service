import React, { useEffect, useRef } from 'react';
import {
  TextField, Select, DatePicker, TimePicker, DateTimePicker, NumberField, TextArea, UrlField, DataSet,
} from 'choerodon-ui/pro';
import { toJS } from 'mobx';
import { find } from 'lodash';
import SelectUser from '@/components/select/select-user';
import SelectSprint from '@/components/select/select-sprint';
import SelectIssueType from '@/components/select/select-issue-type';
import SelectEpic from '@/components/select/select-epic';
import SelectPriority from '@/components/select/select-priority';
import SelectLabel from '@/components/select/select-label';
import SelectComponent from '@/components/select/select-component';
import SelectVersion from '@/components/select/select-version';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { IChosenFieldField } from '@/components/chose-field/types';
import SelectSubProject from '@/components/select/select-sub-project';
import SelectStatus from './field/StatusField';
import FeatureProjectField from './field/FeatureProjectField';
import PIField from './field/pi-field';

const { Option } = Select;
const singleList = ['radio', 'single'];

export default function renderField<T extends Partial<SelectProps>>(field: IChosenFieldField, selectOtherProps: T, { dataSet }: {
  dataSet: DataSet,
}) {
  const {
    code, fieldType, name, fieldOptions, value, id,
  } = field;
  const defaultValue = toJS(value);
  if (!id) {
    switch (code) {
      case 'sprint':
      case 'sprintList':
        return (
          <SelectSprint
            name={code}
            statusList={[]}
            isProgram={code === 'sprintList'}
            multiple
            afterLoad={code === 'sprintList' ? () => { } : (sprints) => {
              if (!defaultValue && Array.isArray(sprints) && sprints.length > 0) {
                const data = find<any>(sprints, { statusCode: 'sprint_planning' }) ?? sprints[0];
                dataSet.current?.set(field.code, [data.sprintId]);
              }
            }}
            selectSprints={value}
            {...selectOtherProps}
          />
        );
      case 'statusId':
      case 'statusList':
        return <SelectStatus name={code} isProgram={code === 'statusList'} multiple {...selectOtherProps} />;
      case 'issueTypeId':
      case 'issueTypeList':
        return <SelectIssueType name={code} filterList={code === 'issueTypeList' ? [] : undefined} multiple {...selectOtherProps} />;
      case 'epic':
      case 'epicList':
        // @ts-ignore
        return <SelectEpic name={code} isProgram={code === 'epicList'} multiple {...selectOtherProps} />;
      case 'priorityId':
        // @ts-ignore
        return <SelectPriority name={code} multiple {...selectOtherProps} />;
      case 'label':
        // @ts-ignore
        return <SelectLabel name={code} multiple {...selectOtherProps} />;
      case 'component':
        // @ts-ignore
        return <SelectComponent name={code} multiple {...selectOtherProps} />;
      case 'version':
        // @ts-ignore
        return <SelectVersion name={code} {...selectOtherProps} />;
      case 'feature': {
        // @ts-ignore
        return <FeatureProjectField name={code} multiple featureIds={defaultValue} {...selectOtherProps} />;// label={name} style={{ width: '100%' }}
      }
      case 'teamProjectList': {
        return <SelectSubProject name={code} multiple {...selectOtherProps} />;// label={name} style={{ width: '100%' }}
      }
      case 'piList': {
        return (
          <PIField
            name={code}
            multiple
            afterLoad={(piList) => {
              if (!defaultValue && Array.isArray(piList) && piList.length > 0) {
                const data = find(piList, { statusCode: 'doing' }) ?? piList[0];
                dataSet.current?.set(field.code, [data.id]);
              }
            }}
            {...selectOtherProps}
          />
        );// label={name} style={{ width: '100%' }}
      }
      default:
        break;
    }
  }

  switch (fieldType) {
    case 'time':
      return (
        <TimePicker
          label={name}
          name={code}
          style={{ width: '100%' }}
        />
      );
    case 'datetime':
      return (
        <DateTimePicker
          name={code}
          label={name}
          style={{ width: '100%' }}
        />
      );
    case 'date':
      return (
        <DatePicker
          name={code}
          label={name}
          style={{ width: '100%' }}
        />
      );
    case 'number':
      return (
        <div>
          <NumberField
            name={code}
            label={name}
            style={{ width: '100%' }}
          // step={isCheck ? 0.1 : 1}
          />
        </div>
      );
    case 'input':
      return (
        <TextField
          name={code}
          maxLength={100}
          label={name}
          style={{ width: '100%' }}
        />
      );
    case 'text':
      return (
        <TextArea
          name={code}
          rows={3}
          maxLength={255}
          label={name}
          style={{ width: '100%' }}
        />
      );
    case 'url':
      return (
        <UrlField
          label={name}
          name={code}
        />
      );
    case 'radio': case 'single': case 'checkbox': case 'multiple':
      return (
        <Select
          name={code}
          label={name}
          style={{ width: '100%' }}
          multiple
        >
          {fieldOptions
            && fieldOptions.length > 0
            && fieldOptions.map((item) => {
              return (
                <Option
                  value={item.id}
                  key={item.id}
                >
                  {item.value}
                </Option>
              );
              return [];
            })}
        </Select>
      );
    case 'member':
    {
      return (
        <SelectUser
          label="user"
          multiple
            // @ts-ignore
          selectedUser={defaultValue ? defaultValue.map((item: string) => ({ id: item })) : undefined}
            // request={(({ filter, page }) => userApi.getAllInProject(filter, page).then((res) => {
            //   if (res.list && Array.isArray(res.list)) {
            //   }
            // }))}
          style={{ width: '100%' }}
          name={code}
        />
      );
    }
    default:
      return null;
  }
}
