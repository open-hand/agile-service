import React, {
  Fragment, useState, useContext, useEffect, useMemo,
} from 'react';
import { observer } from 'mobx-react-lite';
import {
  Form, TextField, Select, DatePicker, TimePicker, DateTimePicker, CheckBox, NumberField, TextArea, UrlField,
} from 'choerodon-ui/pro';
import { Choerodon } from '@choerodon/boot';
import { debounce } from 'lodash';


import UserInfo from '@/components/UserInfo';
import { randomString } from '@/common/utils';
import Store from './stores';
import DragList from '../drag-list';

import './index.less';
import * as images from '../../images';

const { Option } = Select;
const singleList = ['radio', 'single'];
const multipleList = ['checkbox', 'multiple'];


function CreateField() {
  const ctx = useContext(Store);
  const {
    formDataSet, formatMessage, modal, AppState: { currentMenuType: { type, id, organizationId } }, schemeCode, isEdit, handleRefresh, userOptionDataSet,
  } = ctx;
  const [fieldOptions, setFieldOptions] = useState([]);

  useEffect(() => {
    if (isEdit && formDataSet.status === 'ready') {
      setFieldOptions(formDataSet.current.get('fieldOptions') || []);
    }
  }, [formDataSet && formDataSet.status]);

  const fieldTypeOptionRender = ({ text, value }) => (
    <Fragment>
      <img src={images[value]} alt="" className="issue-field-img" />
      <span>
        {text}
      </span>
    </Fragment>
  );

  const contextOptionSetter = ({ record }) => {
    const contextValue = formDataSet.current.get('context');
    const currentValue = record.get('valueCode');
    return {
      disabled: currentValue === 'global' ? contextValue.length > 0 && contextValue.indexOf('global') < 0 : contextValue.indexOf('global') >= 0,
    };
  };

  // 创建或者编辑的提交操作
  async function handleOk() {
    const { current } = formDataSet;
    const obj = {
      fieldType: current.get('fieldType'),
      defaultValue: current.get('defaultValue'),
    };
    if (singleList.indexOf(obj.fieldType) !== -1) {
      if (fieldOptions.length === 0) {
        Choerodon.prompt('字段列表不能为空');
        return false;
      }
      obj.fieldOptions = fieldOptions.map((o) => {
        if (obj.defaultValue && (o.tempKey === obj.defaultValue || o.id === obj.defaultValue)) {
          return { ...o, isDefault: true };
        } else {
          return { ...o, isDefault: false };
        }
      });
    } else if (multipleList.indexOf(obj.fieldType) !== -1) {
      if (fieldOptions.length === 0) {
        Choerodon.prompt('字段列表不能为空');
        return false;
      }
      obj.fieldOptions = fieldOptions.map((o) => {
        if (obj.defaultValue.indexOf(String(o.tempKey)) !== -1
          || obj.defaultValue.indexOf(String(o.id)) !== -1) {
          return { ...o, isDefault: true };
        } else {
          return { ...o, isDefault: false };
        }
      });
      if (obj.defaultValue.length) {
        obj.defaultValue = obj.defaultValue.join(',');
      }
    }


    const url = isEdit ? `/agile/v1/${type}s/${id}/object_scheme_field/${current.get('id')}?organizationId=${organizationId}` : `/agile/v1/${type}s/${id}/object_scheme_field?organizationId=${organizationId}`;
    const method = isEdit ? 'put' : 'post';
    formDataSet.transport[isEdit ? 'update' : 'create'] = ({ data: [data] }) => ({
      url,
      method,
      transformRequest: () => {
        const prefix = type === 'project' ? 'pro_' : 'org_';
        const { name, check } = data;
        let { context } = data;
        if (context && context.length === formDataSet.getField('context').options.length) {
          context = ['global'];
        }
        const postData = {
          context,
          code: `${prefix}${data.code}`,
          name,
          ...obj,
          schemeCode,
          extraConfig: check,
        };
        if (isEdit) {
          postData.objectVersionNumber = current.get('objectVersionNumber');
        }
        return JSON.stringify(postData);
      },
    });


    try {
      if ((await formDataSet.submit()) !== false) {
        handleRefresh();
        return true;
      } else {
        return false;
      }
    } catch (e) {
      return false;
    }
  }
  modal.handleOk(handleOk);

  const onTreeChange = (newFieldOptions) => {
    setFieldOptions(newFieldOptions);
  };

  const onTreeCreate = (code, value) => {
    setFieldOptions([...fieldOptions,
      {
        enabled: true,
        status: 'add',
        code,
        value,
        tempKey: randomString(5),
      }]);
  };

  const onTreeDelete = (tempKey) => {
    const { current } = formDataSet;
    const newDefaultValue = current.get('defaultValue');
    const fieldType = current.get('fieldType');

    if (multipleList.indexOf(fieldType) !== -1) {
      const newValue = newDefaultValue.filter(v => v !== String(tempKey));
      current.set('defaultValue', newValue);
    } else if (singleList.indexOf(fieldType) !== -1) {
      if (newDefaultValue === tempKey) {
        current.set('defaultValue', '');
      }
    }
  };

  function memberOptionRender({ record }) {
    return <UserInfo name={record.get('realName') || ''} id={record.get('loginName')} avatar={record.get('imageUrl')} />;
  }

  function loadUserData(value) {
    const userId = formDataSet.current.get('defaultValue');
    userOptionDataSet.setQueryParameter('param', value);
    userOptionDataSet.setQueryParameter('userId', userId);
    userOptionDataSet.query();
  }

  const searchData = useMemo(() => debounce((value) => {
    loadUserData(value);
  }, 500), []);


  function getAttachFields() {
    const { current } = formDataSet;
    const isCheck = current.get('check');
    const fieldType = current.get('fieldType');
    switch (fieldType) {
      case 'time':
        return (
          <Fragment>
            <TimePicker
              name="defaultValue"
              disabled={isCheck}
              className="form-field-full-row"
            />
            <CheckBox
              name="check"
            >
              {formatMessage({ id: 'field.useCurrentTime' })}
            </CheckBox>
          </Fragment>
        );
      case 'datetime':
        return (
          <Fragment>
            <DateTimePicker
              name="defaultValue"
              disabled={isCheck}
              className="form-field-full-row"
            />
            <CheckBox
              name="check"
            >
              {formatMessage({ id: 'field.useCurrentDate' })}
            </CheckBox>
          </Fragment>
        );
      case 'date':
        return (
          <Fragment>
            <DatePicker
              name="defaultValue"
              disabled={isCheck}
              className="form-field-full-row"
            />
            <CheckBox
              name="check"
            >
              {formatMessage({ id: 'field.useCurrentDate' })}
            </CheckBox>
          </Fragment>
        );
      case 'number':
        return (
          <div>
            <NumberField
              name="defaultValue"
              step={isCheck ? 0.1 : 1}
              className="form-field-full-row"
            />
            <CheckBox
              name="check"
            >
              {formatMessage({ id: 'field.decimal' })}
            </CheckBox>
          </div>
        );
      case 'input':
        return (
          <TextField
            name="defaultValue"
            maxLength={100}
          />
        );
      case 'text':
        return (
          <TextArea
            name="defaultValue"
            rows={3}
            maxLength={255}
          />
        );
      case 'url':
        return (
          <UrlField
            name="defaultValue"
          />
        );
      case 'radio': case 'single': case 'checkbox': case 'multiple':
        return (
          <Fragment>
            <Select
              name="defaultValue"
              style={{ width: '100%', marginBottom: '20px' }}
              multiple={!(singleList.indexOf(fieldType) !== -1)}
            >
              {fieldOptions
                && fieldOptions.length > 0
                && fieldOptions.map((item) => {
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
            <DragList
              title={formatMessage({ id: `field.${fieldType}` })}
              data={fieldOptions}
              tips={formatMessage({ id: 'field.dragList.tips' })}
              formatMessage={formatMessage}
              onChange={onTreeChange}
              onCreate={onTreeCreate}
              onDelete={onTreeDelete}
              onInvalid={onTreeDelete}
            />
          </Fragment>
        );
      case 'member':
        return (
          <Select
            name="defaultValue"
            searchable
            searchMatcher="param"
            onBlur={(e) => { e.persist(); loadUserData(''); }}
            onInput={(e) => { e.persist(); searchData(e.target.value); }}
            optionRenderer={memberOptionRender}
          />
        );
      default:
        return null;
    }
  }
  return (
    <div className="create-field-form-wrap">
      <Form
        dataSet={formDataSet}
      >
        {isEdit ? null
          : (
            <TextField
              name="code"
            />
          )
        }
        <TextField
          name="name"
        />
        <Select
          name="fieldType"
          disabled={isEdit}
          optionRenderer={fieldTypeOptionRender}
        />
        <Select
          name="context"
          onOption={contextOptionSetter}
        />
        {getAttachFields()}
      </Form>
    </div>
  );
}

export default observer(CreateField);
