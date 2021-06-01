import React from 'react';

import {
  TextField, Select, DatePicker, TimePicker, DateTimePicker,
  NumberField, TextArea, Col, Row,
} from 'choerodon-ui/pro';
import SelectUser from '@/components/select/select-user';
import SelectEnvironment from '@/components/select/select-environment';
import SelectProgramVersion from '@/components/select/select-program-version';
import { userApi } from '@/api';
import SelectCustomField from '@/components/select/select-custom-field';
import styles from './index.less';

const { Option } = Select;
const singleList = ['radio', 'single'];

const clearIdMap = new Map([
  ['label', 'labelId'],
  ['component', 'componentId'],
  ['fixVersion', 'versionId'],
  ['version', 'versionId'],
]);

const extraOptionsMap = new Map();
export default function renderField({
  code, fieldType, fieldOptions, required,
}, data, selectUserMap, isProgram, isOrganization) {
  switch (code) {
    case 'component': {
      return (
        <Select
          style={{ width: '100%' }}
          multiple
          name={code}
          searchable
          searchMatcher="name"
          maxTagCount={2}
          maxTagTextLength={10}
          onOption={({ record }) => ({
            disabled: data[code].value && ((data[code].value.indexOf('clear') > -1 && record.get(clearIdMap.get(code) || 'value') !== 'clear') || (data[code].value.indexOf('clear') === -1 && record.get(clearIdMap.get(code) || 'value') === 'clear')),
          })}
        />
      );
    }
    case 'environment': {
      return (
        <SelectEnvironment
          label="环境"
          style={{ width: '100%' }}
          name={code}
          onOption={({ record }) => ({
            disabled: data[code].value && ((data[code].value.indexOf('clear') > -1 && record.get(clearIdMap.get(code) || 'value') !== 'clear') || (data[code].value.indexOf('clear') === -1 && record.get(clearIdMap.get(code) || 'value') === 'clear')),
          })}
        />
      );
    }
    case 'programVersion': {
      return (
        <SelectProgramVersion
          label="版本"
          style={{ width: '100%' }}
          name={code}
          multiple
          onOption={({ record }) => ({
            disabled: data[code].value && ((data[code].value.indexOf('clear') > -1 && record.get(clearIdMap.get(code) || 'value') !== 'clear') || (data[code].value.indexOf('clear') === -1 && record.get(clearIdMap.get(code) || 'value') === 'clear')),
          })}
        />
      );
    }
    default: break;
  }
  switch (fieldType) {
    case 'time': {
      extraOptionsMap.time = [
        { id: 'specifier', label: '自定义指定时间' },
        { id: 'current_time', label: '当前时间' },
      ];
      if (!required) {
        extraOptionsMap.time.unshift({ id: 'clear', label: '清空' });
      }
      return (
        <Row gutter={20}>
          <Col span={12}>
            <Select placeholder="字段值" label="字段值" name={`${code}-select`} className={styles.timeSelect}>
              {
                  extraOptionsMap.time.map((item) => (
                    <Option value={item.id} key={item.id}>{item.label}</Option>
                  ))
                }
            </Select>
          </Col>
          {
            data[code].selected && data[code].selected === 'specifier' && (
              <Col span={12}>
                <TimePicker
                  label="字段值"
                  name={code}
                  style={{ width: '100%' }}
                />
              </Col>
            )
          }
        </Row>
      );
    }
    case 'datetime': {
      extraOptionsMap.datetime = [
        { id: 'specifier', label: '自定义指定时间' },
        { id: 'current_time', label: '当前时间' },
        { id: 'add', label: '流转后几天' },
      ];
      if (!required) {
        extraOptionsMap.datetime.unshift({ id: 'clear', label: '清空' });
      }
      return (
        <Row gutter={20}>
          <Col span={12}>
            <Select placeholder="字段值" label="字段值" name={`${code}-select`} className={styles.dateTimeSelect}>
              {
                extraOptionsMap.datetime.map((item) => (
                  <Option value={item.id} key={item.id}>{item.label}</Option>
                ))
              }
            </Select>
          </Col>
          {
            data[code].selected && (data[code].selected === 'specifier' || data[code].selected === 'add') && (
            <Col span={12}>
              {
                data[code].selected === 'specifier' ? (
                  <DateTimePicker
                    name={code}
                    label="字段值"
                    style={{ width: '100%' }}
                  />
                ) : (
                  <NumberField
                    name={code}
                    label="字段值"
                    style={{ width: '100%' }}
                    step={1}
                    min={1}
                  />
                )
              }
            </Col>
            )
          }
        </Row>
      );
    }
    case 'date': {
      extraOptionsMap.date = [
        { id: 'specifier', label: '自定义指定时间' },
        { id: 'current_time', label: '当前时间' },
        { id: 'add', label: '流转后几天' },
      ];
      if (!required) {
        extraOptionsMap.date.unshift({ id: 'clear', label: '清空' });
      }
      return (
        <Row gutter={20}>
          <Col span={12}>
            <Select placeholder="字段值" label="字段值" name={`${code}-select`} className={styles.dateSelect}>
              {
                extraOptionsMap.date.map((item) => (
                  <Option value={item.id} key={item.id}>{item.label}</Option>
                ))
              }
            </Select>
          </Col>
          {
            data[code].selected && (data[code].selected === 'specifier' || data[code].selected === 'add') && (
            <Col span={12}>
              {
                data[code].selected === 'specifier' ? (
                  <DatePicker
                    name={code}
                    label="字段值"
                    style={{ width: '100%' }}
                  />
                ) : (
                  <NumberField
                    name={code}
                    label="字段值"
                    style={{ width: '100%' }}
                    step={1}
                    min={1}
                  />
                )
              }
            </Col>
            )
          }
        </Row>
      );
    }

    case 'number': {
      extraOptionsMap.number = [
        { id: 'specifier', label: '指定数值' },
        { id: 'add', label: '当前数值+指定数值' },
      ];
      if (!required) {
        extraOptionsMap.number.unshift({ id: 'clear', label: '清空' });
      }
      return (
        <Row gutter={20}>
          <Col span={16}>
            <Select placeholder="字段值" label="字段值" name={`${code}-select`} className={styles.numberSelect}>
              {
                extraOptionsMap.number.map((item) => (
                  <Option value={item.id} key={item.id}>{item.label}</Option>
                ))
              }
            </Select>
          </Col>
          {
            data[code].selected && (data[code].selected === 'specifier' || data[code].selected === 'add') && (
            <Col span={8}>
              <NumberField
                label="字段值"
                name={code}
                style={{ width: '100%' }}
              />
            </Col>
            )
          }
        </Row>
      );
    }
    case 'input': {
      extraOptionsMap.input = [
        { id: 'specifier', label: '指定文本' },
      ];
      if (!required) {
        extraOptionsMap.input.unshift({ id: 'clear', label: '清空' });
      }
      return (
        <Row gutter={20}>
          <Col span={12}>
            <Select placeholder="字段值" label="字段值" name={`${code}-select`} className={styles.inputSelect}>
              {
                extraOptionsMap.input.map((item) => (
                  <Option value={item.id} key={item.id}>{item.label}</Option>
                ))
              }
            </Select>
          </Col>
          {
            data[code].selected && data[code].selected === 'specifier' && (
              <Col span={12}>
                <TextField
                  label="字段值"
                  name={code}
                  maxLength={100}
                  valueChangeAction="input"
                  style={{ width: '100%' }}
                />
              </Col>
            )
          }
        </Row>
      );
    }
    case 'text': {
      extraOptionsMap.text = [
        { id: 'specifier', label: '指定文本' },
      ];
      if (!required) {
        extraOptionsMap.text.unshift({ id: 'clear', label: '清空' });
      }
      return (
        <Row gutter={20}>
          <Col span={12}>
            <Select placeholder="字段值" label="字段值" name={`${code}-select`} className={styles.textSelect}>
              {
                extraOptionsMap.text.map((item) => (
                  <Option value={item.id} key={item.id}>{item.label}</Option>
                ))
              }
            </Select>
          </Col>
          <Col span={12}>
            {
              data[code].selected && data[code].selected === 'specifier' && (
              <TextArea
                label="字段值"
                name={code}
                rows={3}
                maxLength={255}
                valueChangeAction="input"
                style={{ width: '100%' }}
              />
              )
            }
          </Col>
        </Row>
      );
    }
    case 'radio': case 'single': case 'checkbox': case 'multiple': {
      const options = [...(!required ? [{ id: 'clear', value: '清空', enabled: true }] : []), ...(fieldOptions || [])];
      const isMultiple = !(singleList.indexOf(fieldType) !== -1);
      if (fieldOptions) {
        return (
          <SelectCustomField
            searchable
            placeholder="字段值"
            label="字段值"
            name={code}
            style={{ width: '100%' }}
            multiple={isMultiple}
            maxTagCount={2}
            maxTagTextLength={10}
            onOption={({ record }) => ({
              disabled: isMultiple && data[code].value && ((data[code].value.indexOf('clear') > -1 && record.get(clearIdMap.get(code) || 'value') !== 'clear') || (data[code].value.indexOf('clear') === -1 && record.get(clearIdMap.get(code) || 'value') === 'clear')),
            })}
            fieldOptions={options.map((item) => ({ ...item, id: item.tempKey ?? item.id }))}
          />
        );
      }
      return (
        <Select
          searchable
          placeholder="字段值"
          label="字段值"
          name={code}
          style={{ width: '100%' }}
          multiple={isMultiple}
          maxTagCount={2}
          maxTagTextLength={10}
          onOption={({ record }) => ({
            disabled: isMultiple && data[code].value && ((data[code].value.indexOf('clear') > -1 && record.get(clearIdMap.get(code) || 'value') !== 'clear') || (data[code].value.indexOf('clear') === -1 && record.get(clearIdMap.get(code) || 'value') === 'clear')),
          })}
        >
          {options.map((item) => {
            if (item.enabled) {
              return (
                <Option
                  value={item.tempKey || item.id}
                  key={item.tempKey || item.id}
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
    case 'multiMember':
    case 'member': {
      if (code === 'assignee') {
        extraOptionsMap.member = [
          { id: 'reportor', realName: '报告人' },
          { id: 'creator', realName: '创建人' },
          { id: 'operator', realName: '当前操作人' },
        ];
      } else if (code === 'reporter') {
        extraOptionsMap.member = [
          { id: 'creator', realName: '创建人' },
          { id: 'operator', realName: '当前操作人' },

        ];
      } else if (code === 'mainResponsible') {
        extraOptionsMap.member = [
          { id: 'reportor', realName: '报告人' },
          { id: 'creator', realName: '创建人' },
          { id: 'operator', realName: '当前操作人' },

        ];
      } else {
        extraOptionsMap.member = [
          { id: 'reportor', realName: '报告人' },
          { id: 'creator', realName: '创建人' },
          { id: 'operator', realName: '当前操作人' },
        ];
      }

      if (!isProgram && code !== 'mainResponsible') {
        extraOptionsMap.member.push({
          id: 'mainResponsible', realName: '主要负责人',
        });
      }

      if (!isProgram && code !== 'assignee') {
        extraOptionsMap.member.unshift({
          id: 'assignee', realName: '经办人',
        });
      }

      if (!required || code !== 'reporter') {
        extraOptionsMap.member.unshift({ id: 'clear', realName: '清空' });
      }

      return (
        <SelectUser
          style={{ width: '100%' }}
          name={code}
          extraOptions={extraOptionsMap.member}
          selectedUser={selectUserMap.get(code)}
          maxTagCount={2}
          maxTagTextLength={10}
          onOption={({ record }) => ({
            disabled: fieldType === 'multiMember' && data[code].value.length && ((data[code].value.indexOf('clear') > -1 && record.get(clearIdMap.get(code) || 'value') !== 'clear') || (data[code].value.indexOf('clear') === -1 && record.get(clearIdMap.get(code) || 'value') === 'clear')),
          })}
          request={({ filter, page }) => (!isOrganization ? userApi.getAllInProject(filter, page) : userApi.getAllInOrg(filter, page))}
        />
      );
    }
    default:
      return null;
  }
}
