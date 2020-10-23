import React, { useRef } from 'react';
import {
  Select, DatePicker, TimePicker, DateTimePicker, TextArea, TextField, NumberField, DataSet,
} from 'choerodon-ui/pro';
import { stores } from '@choerodon/boot';
import SelectIssueType from '@/components/select/select-issue-type';
import SelectStatus from '@/components/issue-filter-form/components/field/StatusField';
import SelectPriority from '@/components/select/select-priority';
import SelectComponent from '@/components/select/select-component';
import SelectLabel from '@/components/select/select-label';
import SelectVersion from '@/components/select/select-version';
import SelectEpic from '@/components/select/select-epic';
import SelectSprint from '@/components/select/select-sprint';
import SelectUser from '@/components/select/select-user';
// import SelectDemandType from '@choerodon/agile-pro/lib/components/select/select-demand-type';
// import SelectTreeDemandClassification from '@choerodon/agile-pro/lib/components/select/select-demand-classification';
// import SelectUrgent from '@choerodon/agile-pro/lib/components/select/select-priority';

const { Option } = Select;
const { AppState } = stores;

export type Operation = 'in' | 'not_in' | 'is' | 'is_not' | 'eq' | 'not_eq' | 'gt' | 'gte' | 'lt' | 'lte' | 'like' | 'not_like' | '';
export type IFieldType = 'radio' | 'checkbox' | 'single' | 'multiple' | 'date' | 'datetime' | 'time' | 'number' | 'member' | 'text' | 'input';
export type IMiddleFieldType = 'option' | 'date_hms' | 'date' | 'number' | 'string' | 'text';
export interface Rule {
  ao?: 'and' | 'or',
  code: string,
  operation: Operation,
  value: any,
}

interface FieldOption {
  id: string,
  fieldId: string,
  code: string,
  value: string,
  enabled: boolean,
}

export interface IField {
  code: string,
  fieldOptions?: FieldOption[],
  fieldType: IFieldType,
  fieldTypeName?: string,
  id: string,
  name: string,
  system: boolean,
  extraConfig?: boolean,
}

export interface IFieldWithType extends IField {
  type: IMiddleFieldType,
}

const renderRule = (dataset: DataSet, fieldK: { key: number }, fieldData: IField[], systemDataRefMap: React.MutableRefObject<Map<string, any>>, getFieldValue: { (name: any): any; (arg0: string): string; }) => {
  const isProgram = AppState.currentMenuType.category === 'PROGRAM';
  const { key } = fieldK;
  const field = fieldData.find((item: IField) => item.code === dataset?.current?.get(`${key}-code`));
  const operation = dataset?.current?.get(`${key}-operation`);
  if (operation === 'is' || operation === 'is_not') {
    return (
      <Select
        name={`${key}-value`}
        label="值"
        style={{
          width: '100%',
        }}
      >
        <Option value="empty">空</Option>
      </Select>
    );
  }
  if (field) {
    const {
      fieldType, system, code, fieldOptions, extraConfig,
    } = field;
    if (system) {
      switch (code) {
        case 'issueType': {
          return (
            <SelectIssueType
              name={`${key}-value`}
              isProgram={isProgram}
              label="值"
              valueField="typeCode"
              style={{
                width: '100%',
              }}
              afterLoad={(data) => {
                systemDataRefMap.current.set(code, data || []);
              }}
            />
          );
        }
        case 'status': {
          return (
            <SelectStatus
              name={`${key}-value`}
              isProgram={isProgram}
              label="值"
              style={{
                width: '100%',
              }}
              afterLoad={(data) => {
                systemDataRefMap.current.set(code, data || []);
              }}
            />
          );
        }
        case 'priority': {
          return (
            <SelectPriority
              name={`${key}-value`}
              label="值"
              style={{
                width: '100%',
              }}
              afterLoad={(data) => {
                systemDataRefMap.current.set(code, data || []);
              }}
            />
          );
        }
        case 'component': {
          return (
            <SelectComponent
              valueField="componentId"
              multiple
              name={`${key}-value`}
              label="值"
              maxTagCount={2}
              maxTagTextLength={10}
              style={{
                width: '100%',
              }}
              afterLoad={(data) => {
                systemDataRefMap.current.set(code, data || []);
              }}
            />
          );
        }
        case 'label': {
          return (
            <SelectLabel
              valueField="labelId"
              multiple
              name={`${key}-value`}
              label="值"
              maxTagCount={2}
              maxTagTextLength={10}
              style={{
                width: '100%',
              }}
              afterLoad={(data) => {
                systemDataRefMap.current.set(code, data || []);
              }}
            />
          );
        }
        case 'influence_version':
        case 'fix_version': {
          return (
            <SelectVersion
              valueField="versionId"
              multiple
              name={`${key}-value`}
              label="值"
              maxTagCount={2}
              maxTagTextLength={10}
              style={{
                width: '100%',
              }}
              afterLoad={(data) => {
                systemDataRefMap.current.set(code, data || []);
              }}
            />
          );
        }
        case 'epic': {
          return (
            <SelectEpic
              name={`${key}-value`}
              isProgram={isProgram}
              label="值"
              style={{
                width: '100%',
              }}
              afterLoad={(data) => {
                systemDataRefMap.current.set(code, data || []);
              }}
            />
          );
        }
        case 'sprint': {
          return (
            <SelectSprint
              name={`${key}-value`}
              label="值"
              style={{
                width: '100%',
              }}
              afterLoad={(data) => {
                systemDataRefMap.current.set(code, data || []);
              }}
            />
          );
        }
        case 'reporter':
        case 'assignee': {
          return (
            <SelectUser
              name={`${key}-value`}
              label="值"
              style={{
                width: '100%',
              }}
              afterLoad={(data) => {
                systemDataRefMap.current.set(code, data || []);
              }}
              // @ts-ignore
              autoQueryConfig={{
                selectedUserIds: getFieldValue(`${key}-value`) ? [getFieldValue(`${key}-value`)] : [],
              }}
            />
          );
        }
              // case 'backlogType': {
              //   return <SelectDemandType required name={`${key}-value`} label="值" />
              // }
              // case 'backlogClassification': {
              //   return <SelectTreeDemandClassification required name={`${key}-value`} label="值" />
              // }
              // case 'urgent': {
              //   return <SelectUrgent required name={`${key}-value`} label="值" />
              // }
      }
    }
    switch (fieldType) {
      case 'radio':
      case 'checkbox':
      case 'multiple':
      case 'single': {
        return (
          <Select
            key={code}
            label="值"
            name={`${key}-value`}
            multiple={fieldType === 'checkbox' || fieldType === 'multiple'}
            maxTagCount={2}
            maxTagTextLength={10}
            style={{
              width: '100%',
            }}
          >
            {(fieldOptions || []).map((item: FieldOption) => {
              if (item.enabled) {
                return (
                  <Option
                    value={item.id}
                    key={item.id}
                  >
                    {item.value}
                  </Option>
                );
              }
              return [];
            })}
          </Select>
        );
      }
      case 'member': {
        return (
          <SelectUser
            name={`${key}-value`}
            label="值"
            afterLoad={(data) => {
              systemDataRefMap.current.set(code, data || []);
            }}
            style={{
              width: '100%',
            }}
            // @ts-ignore
            autoQueryConfig={{
              selectedUserIds: getFieldValue(`${key}-value`) ? [getFieldValue(`${key}-value`)] : [],
            }}
          />
        );
      }
      case 'text': {
        return (
          <TextArea
            name={`${key}-value`}
            rows={3}
            maxLength={255}
            style={{ width: '100%' }}
            label="值"
          />
        );
      }
      case 'input': {
        return (
          <TextField
            name={`${key}-value`}
            maxLength={100}
            label="值"
            style={{
              width: '100%',
            }}
          />
        );
      }
      case 'number': {
        // remain_time, story_point
        return (
          <NumberField
            name={`${key}-value`}
            label="值"
            style={{
              width: '100%',
            }}
          />
        );
      }
      case 'time': {
        return (
          <TimePicker
            name={`${key}-value`}
            label="值"
            style={{
              width: '100%',
            }}
          />
        );
      }
      case 'datetime': {
        // creationDate, lastUpdateDate,estimatedStartTime,estimatedEndTime,
        return (
          <DateTimePicker
            name={`${key}-value`}
            label="值"
            style={{
              width: '100%',
            }}
          />
        );
      }
      case 'date': {
        return (
          <DatePicker
            name={`${key}-value`}
            label="值"
            style={{
              width: '100%',
            }}
          />
        );
      }
      default:
        return (
          <Select
            name={`${key}-value`}
            label="值"
            style={{
              width: '100%',
            }}
          />
        );
    }
  }

  return (
    <Select
      name={`${key}-value`}
      label="值"
      style={{
        width: '100%',
      }}
    />
  );
};

export default renderRule;
