/* eslint-disable no-nested-ternary */

import React from 'react';

import {
  TextField, Select, DatePicker, TimePicker, DateTimePicker,
  NumberField, TextArea, Col, Row,
} from 'choerodon-ui/pro';
import SelectUser from '@/components/select/select-user';
import SelectEnvironment from '@/components/select/select-environment';
import SelectProgramVersion from '@/components/select/select-program-version';
import { userApi } from '@/api';
import styles from './index.less';
import { getAgileFields } from '@/components/field-pro';

const { Option } = Select;
const singleList = ['radio', 'single'];

const clearIdMap = new Map([
  ['label', 'labelId'],
  ['component', 'componentId'],
  ['fixVersion', 'versionId'],
  ['influenceVersion', 'versionId'],
]);

const extraOptionsMap = new Map();
export default function renderField({
  code, fieldType, fieldOptions, required, system,
}, data, selectUserMap, isProgram, isOrganization, colSpan = 12) {
  switch (code) {
    case 'component': {
      return (
        <Select
          multiple
          name={code}
          searchable
          searchMatcher="name"
          maxTagCount={2}
          maxTagTextLength={10}
          colSpan={colSpan}
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
          colSpan={colSpan}
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
          colSpan={colSpan}
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
      return ([
        <Select
          placeholder="字段值"
          label="字段值"
          name={`${code}-select`}
          className={styles.timeSelect}
          colSpan={colSpan / 2}
        >
          {
              extraOptionsMap.time.map((item) => (
                <Option value={item.id} key={item.id}>{item.label}</Option>
              ))
            }
        </Select>,
        data[code].selected && data[code].selected === 'specifier' ? (
          <TimePicker
            label="字段值"
            name={code}
            style={{ width: '100%' }}
            colSpan={colSpan / 2}
          />
        ) : null,
      ]);
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
      return ([
        <Select
          placeholder="字段值"
          label="字段值"
          name={`${code}-select`}
          className={styles.dateTimeSelect}
          colSpan={colSpan / 2}
        >
          {
              extraOptionsMap.datetime.map((item) => (
                <Option value={item.id} key={item.id}>{item.label}</Option>
              ))
            }
        </Select>,
        data[code].selected && (data[code].selected === 'specifier' || data[code].selected === 'add') ? (
          data[code].selected === 'specifier' ? (
            <DateTimePicker
              name={code}
              label="字段值"
              colSpan={colSpan / 2}
            />
          ) : (
            <NumberField
              name={code}
              label="字段值"
              colSpan={colSpan / 2}
              step={1}
              min={1}
            />
          )
        ) : null,
      ]);
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
      return ([
        <Select
          placeholder="字段值"
          label="字段值"
          name={`${code}-select`}
          className={styles.dateSelect}
          colSpan={colSpan / 2}
        >
          {
              extraOptionsMap.date.map((item) => (
                <Option value={item.id} key={item.id}>{item.label}</Option>
              ))
            }
        </Select>,
        data[code].selected && (data[code].selected === 'specifier' || data[code].selected === 'add') ? (
          data[code].selected === 'specifier' ? (
            <DatePicker
              name={code}
              label="字段值"
              colSpan={colSpan / 2}
            />
          ) : (
            <NumberField
              name={code}
              label="字段值"
              colSpan={colSpan / 2}
              step={1}
              min={1}
            />
          )
        ) : null,
      ]);
    }

    case 'number': {
      extraOptionsMap.number = [
        { id: 'specifier', label: '指定数值' },
        { id: 'add', label: '当前数值+指定数值' },
      ];
      if (!required) {
        extraOptionsMap.number.unshift({ id: 'clear', label: '清空' });
      }
      return ([
        <Select
          placeholder="字段值"
          label="字段值"
          name={`${code}-select`}
          className={styles.numberSelect}
          colSpan={colSpan / 3 * 2}
        >
          {
              extraOptionsMap.number.map((item) => (
                <Option value={item.id} key={item.id}>{item.label}</Option>
              ))
            }
        </Select>,
        data[code].selected && (data[code].selected === 'specifier' || data[code].selected === 'add') ? (
          <NumberField
            label="字段值"
            name={code}
            colSpan={colSpan / 3}
          />
        ) : null,
      ]
      );
    }
    case 'input': {
      extraOptionsMap.input = [
        { id: 'specifier', label: '指定文本' },
      ];
      if (!required) {
        extraOptionsMap.input.unshift({ id: 'clear', label: '清空' });
      }
      return ([
        <Select
          placeholder="字段值"
          label="字段值"
          name={`${code}-select`}
          className={styles.inputSelect}
          colSpan={colSpan / 2}
        >
          {
              extraOptionsMap.input.map((item) => (
                <Option value={item.id} key={item.id}>{item.label}</Option>
              ))
            }
        </Select>,
        data[code].selected && data[code].selected === 'specifier' ? (
          <TextField
            label="字段值"
            name={code}
            maxLength={100}
            valueChangeAction="input"
            colSpan={colSpan / 2}
          />
        ) : null,
      ]
      );
    }
    case 'text': {
      extraOptionsMap.text = [
        { id: 'specifier', label: '指定文本' },
      ];
      if (!required) {
        extraOptionsMap.text.unshift({ id: 'clear', label: '清空' });
      }
      return ([
        <Select
          placeholder="字段值"
          label="字段值"
          name={`${code}-select`}
          className={styles.textSelect}
          colSpan={colSpan / 2}
        >
          {
              extraOptionsMap.text.map((item) => (
                <Option value={item.id} key={item.id}>{item.label}</Option>
              ))
            }
        </Select>,
        data[code].selected && data[code].selected === 'specifier' ? (
          <TextArea
            label="字段值"
            name={code}
            rows={3}
            maxLength={255}
            valueChangeAction="input"
            colSpan={colSpan / 2}
          />
        ) : null,
      ]
      );
    }
    case 'radio': case 'single': case 'checkbox': case 'multiple': {
      const options = [...(!required ? [{ id: 'clear', value: '清空', enabled: true }] : []), ...(fieldOptions || [])];
      const isMultiple = !(singleList.indexOf(fieldType) !== -1);
      if (fieldOptions?.length && !system) {
        return getAgileFields([], [], {
          fieldType,
          outputs: ['element'],
          props: {
            searchable: true,
            // style: { width: '100%' },
            colSpan,
            placeholder: '字段值',
            label: '字段值',
            maxTagCount: 2,
            maxTagTextLength: 10,
            onOption: ({ record }) => ({
              disabled: isMultiple && data[code].value && ((data[code].value.indexOf('clear') > -1 && record.get(clearIdMap.get(code) || 'value') !== 'clear') || (data[code].value.indexOf('clear') === -1 && record.get(clearIdMap.get(code) || 'value') === 'clear')),
            }),
            fieldOptions: options.map((item) => ({ ...item, id: item.tempKey ?? item.id })),
          },
        });
      }
      return (
        <Select
          searchable
          placeholder="字段值"
          label="字段值"
          name={code}
          colSpan={colSpan}
          multiple={isMultiple}
          maxTagCount={2}
          maxTagTextLength={10}
          key={code}
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
          colSpan={colSpan}
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
