import React from 'react';

import {
  Select, Input, InputNumber,
  Checkbox, TimePicker, Row, Col, Radio, DatePicker,
} from 'choerodon-ui';
import { findIndex } from 'lodash';
import moment from 'moment';
import SelectFocusLoad from '../SelectFocusLoad';

const { TextArea } = Input;
const { Option } = Select;
export default function renderField(field) {
  const {
    fieldOptions, fieldType, required, fieldName,
  } = field;
  if (fieldType === 'radio') {
    if (fieldOptions && fieldOptions.length > 0) {
      return (
        <Radio.Group
          label={fieldName}
        >
          {fieldOptions && fieldOptions.length > 0
            && fieldOptions.filter((option) => option.enabled).map((item) => (
              <Radio
                className="radioStyle"
                value={item.id}
                key={item.id}
              >
                {item.value}
              </Radio>
            ))}
        </Radio.Group>
      );
    }
    return (
      <Radio.Group
        label={fieldName}
      >
        <span style={{ color: '#D50000' }}>暂无选项，请联系管理员</span>
      </Radio.Group>
    );
  } if (field.fieldType === 'checkbox') {
    if (fieldOptions && fieldOptions.length > 0) {
      return (
        <Checkbox.Group
          label={fieldName}
        >
          <Row>
            {fieldOptions && fieldOptions.length > 0
              && fieldOptions.filter((option) => option.enabled).map((item) => (
                <Col
                  span={24}
                  key={item.id}
                >
                  <Checkbox
                    value={item.id}
                    key={item.id}
                    className="checkboxStyle"
                  >
                    {item.value}
                  </Checkbox>
                </Col>
              ))}
          </Row>
        </Checkbox.Group>
      );
    }
    return (
      <Checkbox.Group
        label={fieldName}
      >
        <span style={{ color: '#D50000' }}>暂无选项，请联系管理员</span>
      </Checkbox.Group>
    );
  } if (field.fieldType === 'time') {
    return (
      <TimePicker
        label={fieldName}
        placeholder={fieldName}
        style={{ display: 'block', width: 330 }}
        defaultOpenValue={moment('00:00:00', 'HH:mm:ss')}
        allowEmpty={!required}
      />
    );
  } if (field.fieldType === 'datetime') {
    return (
      <DatePicker
        showTime
        label={fieldName}
        placeholder={fieldName}
        format="YYYY-MM-DD HH:mm:ss"
        style={{ display: 'block', width: 330 }}
        allowClear={!required}
      />
    );
  } if (field.fieldType === 'date') {
    return (
      <DatePicker
        label={fieldName}
        placeholder={fieldName}
        format="YYYY-MM-DD"
        style={{ display: 'block', width: 330 }}
        allowClear={!required}
      />
    );
  } if (field.fieldType === 'single') {
    return (
      <Select
        filter
        filterOption={(input, option) => option.props.children.toLowerCase().indexOf(input.toLowerCase()) >= 0}
        label={fieldName}
        allowClear={!required}
        getPopupContainer={(triggerNode) => triggerNode.parentNode}

      >
        {field.fieldOptions && field.fieldOptions.length > 0
          && field.fieldOptions.filter((option) => option.enabled).map((item) => (
            <Option
              value={item.id}
              key={item.id}
            >
              {item.value}
            </Option>
          ))}
      </Select>
    );
  } if (field.fieldType === 'multiple') {
    return (
      <Select
        filter
        filterOption={(input, option) => option.props.children.toLowerCase().indexOf(input.toLowerCase()) >= 0}
        label={fieldName}
        mode="multiple"
        getPopupContainer={(triggerNode) => triggerNode.parentNode}

      >
        {field.fieldOptions && field.fieldOptions.length > 0
          && field.fieldOptions.filter((option) => option.enabled).map((item) => (
            <Option
              value={item.id}
              key={item.id}
            >
              {item.value}
            </Option>
          ))}
      </Select>
    );
  } if (field.fieldType === 'number') {
    return (
      <InputNumber
        label={fieldName}
        step={field.extraConfig ? 0.01 : 1}
        maxLength={8}
      />
    );
  } if (field.fieldType === 'text') {
    return (
      <TextArea
        autosize
        label={fieldName}
        maxLength={255}
      />
    );
  } if (['member', 'multiMember'].includes(field.fieldType)) {
    return (
      <SelectFocusLoad
        label={fieldName}
        allowClear
        type="user"
        mode={field.fieldType === 'multiMember' ? 'multiple' : undefined}
        saveList={(v) => {
          // 假如本项目无此人，则将默认值手动增添进去
          if (field.defaultValue && field.defaultValueObj) {
            const index = findIndex(v, { id: field.defaultValue });
            if (index === -1) {
              v.push(field.defaultValueObj);
            }
          }
        }}
        loadWhenMount
      />
    );
  }
  return (
    <Input
      label={fieldName}
      maxLength={100}
    />
  );
}
