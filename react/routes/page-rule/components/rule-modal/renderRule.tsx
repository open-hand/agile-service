import React from 'react';
import {
  Select, DatePicker, TimePicker, DateTimePicker, TextArea, TextField, NumberField, DataSet, Row, Col,
} from 'choerodon-ui/pro';
import { stores } from '@choerodon/boot';
import SelectStatus from '@/components/issue-filter-form/components/field/StatusField';
import SelectPriority from '@/components/select/select-priority';
import SelectComponent from '@/components/select/select-component';
import SelectLabel from '@/components/select/select-label';
import SelectVersion from '@/components/select/select-version';
import SelectEpic from '@/components/select/select-epic';
import SelectSprint from '@/components/select/select-sprint';
import SelectUser from '@/components/select/select-user';
import { InjectedComponent } from './injectComponent';

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
  const middleValue = dataset?.current?.get(`${key}-middleValue`);
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
              multiple
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
              multiple
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
        case 'influenceVersion':
        case 'fixVersion': {
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
              multiple
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
              multiple
            />
          );
        }
        case 'reporter': {
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
                selectedUserIds: getFieldValue(`${key}-value`) ? getFieldValue(`${key}-value`) : [],
              }}
              multiple
            />
          );
        }
        case 'backlogType': {
          return (
            <InjectedComponent.BacklogType
              // @ts-ignore
              name={`${key}-value`}
              label="值"
              afterLoad={(data: {id: string, name: string}) => {
                systemDataRefMap.current.set(code, data || []);
              }}
              multiple
            />
          );
        }
        case 'backlogClassification': {
          return (
            <InjectedComponent.BacklogClassification
              // @ts-ignore
              name={`${key}-value`}
              placeholder="值"
              afterLoad={(data: {id: string, name: string}) => {
                systemDataRefMap.current.set(code, data || []);
              }}
              multiple
            />
          );
        }
        case 'urgent': {
          return (
            <InjectedComponent.Urgent
              // @ts-ignore
              name={`${key}-value`}
              label="值"
              afterLoad={(data: {id: string, name: string}) => {
                systemDataRefMap.current.set(code, data || []);
              }}
              multiple
            />
          );
        }
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
            multiple
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
              selectedUserIds: getFieldValue(`${key}-value`) ? getFieldValue(`${key}-value`) : [],
            }}
            multiple
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
        // remainingTime, storyPoints
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
          <Row gutter={20}>
            <Col span={middleValue === 'specified' ? 12 : 24}>
              <Select name={`${key}-middleValue`} label="值" clearButton={false}>
                <Option value="now">当前时间</Option>
                <Option value="specified">指定值</Option>
              </Select>
            </Col>

            {
              middleValue === 'specified' && (
                <Col span={12}>
                  <TimePicker
                    name={`${key}-value`}
                    label="值"
                    style={{
                      width: '100%',
                    }}
                  />
                </Col>
              )
            }
          </Row>
        );
      }
      case 'datetime': {
        // creationDate, lastUpdateDate,estimatedStartTime,estimatedEndTime,
        return (
          <Row gutter={20}>
            <Col span={middleValue === 'specified' ? 12 : 24}>
              <Select name={`${key}-middleValue`} label="值" clearButton={false}>
                <Option value="now">当前时间</Option>
                <Option value="specified">指定值</Option>
              </Select>
            </Col>

            {
              middleValue === 'specified' && (
                <Col span={12}>
                  <DateTimePicker
                    name={`${key}-value`}
                    label="值"
                    style={{
                      width: '100%',
                    }}
                  />
                </Col>
              )
            }
          </Row>
        );
      }
      case 'date': {
        return (
          <Row gutter={20}>
            <Col span={middleValue === 'specified' ? 12 : 24}>
              <Select name={`${key}-middleValue`} label="值" clearButton={false}>
                <Option value="now">当前时间</Option>
                <Option value="specified">指定值</Option>
              </Select>
            </Col>

            {
              middleValue === 'specified' && (
                <Col span={12}>
                  <DatePicker
                    name={`${key}-value`}
                    label="值"
                    style={{
                      width: '100%',
                    }}
                  />
                </Col>
              )
            }
          </Row>
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
