import React, {
  useState, useContext, useEffect, useRef, useLayoutEffect,
} from 'react';
import { observer } from 'mobx-react-lite';
import {
  Form, TextField, Select as S1, DatePicker, TimePicker, DateTimePicker,
  CheckBox, NumberField, TextArea, UrlField,
} from 'choerodon-ui/pro';
import { Choerodon } from '@choerodon/boot';
import SelectUser from '@/components/select/select-user';
import moment from 'moment';
import { User } from '@/common/types';
import { toJS } from 'mobx';
import { set, uniq } from 'lodash';
import { randomString } from '@/utils/random';
import { RenderProps } from 'choerodon-ui/pro/lib/field/FormField';
import { pageConfigApi, userApi } from '@/api';
import Store from './stores';
import DragList from '../drag-list';
import './index.less';
import * as images from '../../images';
import beforeSubmitProcessData from './util';
import Select from './SelectU';

const { Option } = Select;
const singleList = ['radio', 'single'];
const multipleList = ['checkbox', 'multiple'];
interface FiledOptions {
  fieldOptions?: any,
  fieldType: string,
  defaultValue: string,
}
interface IFieldPostData extends FiledOptions {
  id?: string,
  context: any
  code: string,
  name: string,
  schemeCode: string,
  check?: boolean,
  objectVersionNumber?: number,
  issueTypeIds: string[] // 问题类型id
  extraConfig: any,
  syncIssueType?: string[] // 同步主默认值的问题类型
}
export type IFieldPostDataProps = IFieldPostData;
function CreateField() {
  const ctx = useContext(Store);
  const {
    formDataSet, formatMessage, modal, onSubmitLocal,
    AppState: { currentMenuType: { type, id, organizationId } }, store,
    schemeCode, isEdit, handleRefresh,
  } = ctx;
  const [fieldOptions, setFieldOptions] = useState<Array<any>>([]);
  const userDataRef = useRef<User[] | undefined>();
  useEffect(() => {
    if (isEdit && formDataSet.status === 'ready') {
      setFieldOptions(formDataSet.current?.get('fieldOptions') || []);
    }
  }, [formDataSet && formDataSet.status]);

  const fieldTypeOptionRender = ({ text, value }: RenderProps) => (
    <>
      <img
        // @ts-ignore
        src={images[value]}
        alt=""
        className="issue-field-img"
      />
      <span>
        {text}
      </span>
    </>
  );

  const contextOptionSetter = ({ record }: RenderProps) => {
    const contextValue = formDataSet.current?.get('context');
    const currentValue = record?.get('valueCode');
    return { disabled: !record?.get('enabled') };
    return {
      disabled: currentValue === 'global' ? contextValue.length > 0 && contextValue.indexOf('global') < 0 : contextValue.indexOf('global') >= 0,
    };
  };

  // 创建或者编辑的提交操作
  async function handleOk() {
    const { current } = formDataSet;
    const fieldType = current?.get('fieldType');
    const originDefaultValue = toJS(current?.get('defaultValue'));
    if (singleList.indexOf(fieldType) !== -1) {
      if (fieldOptions.length === 0) {
        Choerodon.prompt('字段列表不能为空');
        return false;
      }
    } else if (multipleList.indexOf(fieldType) !== -1) {
      if (fieldOptions.length === 0) {
        Choerodon.prompt('字段列表不能为空');
        return false;
      }
    }
    const postData = beforeSubmitProcessData(current!, { fieldOptions, schemeCode });

    // 防止使用dataSet提交时 忽略filedOptions
    formDataSet.current?.set('updateFieldOptions', postData.fieldOptions);
    if (onSubmitLocal) {
      const validResult = await formDataSet.validate();
      if (['member', 'multiMember'].includes(postData.fieldType) && postData?.defaultValue !== '') {
        const userIds = Array.isArray(originDefaultValue) ? originDefaultValue : [originDefaultValue];
        set(postData, 'localDefaultObj', userDataRef.current?.filter((item) => userIds.includes(item.id)) || {});
      }
      return validResult && onSubmitLocal({ ...postData, defaultValue: originDefaultValue, issueTypeIds: postData.context });
    }
    const fieldId = formDataSet.current?.get('id');
    const url = isEdit ? `/agile/v1/${type}s/${id}/object_scheme_field/${fieldId}?organizationId=${organizationId}` : `/agile/v1/${type}s/${id}/object_scheme_field?organizationId=${organizationId}`;
    const method = isEdit ? 'put' : 'post';
    formDataSet.transport[isEdit ? 'update' : 'create'] = ({ data: [data] }) => ({
      url,
      method,
      transformRequest: () => {
        if (isEdit) {
          postData.objectVersionNumber = formDataSet.current?.get('objectVersionNumber');
        }
        return JSON.stringify(postData);
      },
    });
    const syncIssueTypeArr = isEdit ? [...current?.get('syncIssueType')] : [];
    const extraConfig = isEdit ? current?.get('extraConfig') : undefined;
    try {
      if ((await formDataSet.submit()) !== false) {
        syncIssueTypeArr.length > 0 && await pageConfigApi.syncDefaultValue(fieldId,
          {
            issueTypeIds: syncIssueTypeArr,
            extraConfig,
            custom: true,
          });
        handleRefresh && handleRefresh();
        return true;
      }
      return false;
    } catch (e) {
      return false;
    }
  }
  modal.handleOk(handleOk);

  const onTreeChange = (newFieldOptions: any) => {
    setFieldOptions(newFieldOptions);
  };

  const onTreeCreate = (code: string, value: string) => {
    setFieldOptions([...fieldOptions,
      {
        enabled: true,
        status: 'add',
        code,
        value,
        tempKey: randomString(5),
      }]);
  };

  const onTreeDelete = (tempKey: string) => {
    const { current } = formDataSet;
    const newDefaultValue = current?.get('defaultValue');
    if (!newDefaultValue) {
      return;
    }
    const fieldType = current?.get('fieldType');
    if (multipleList.indexOf(fieldType) !== -1) {
      const newValue = newDefaultValue.filter((v: string) => v !== String(tempKey));
      current?.set('defaultValue', newValue);
    } else if (singleList.indexOf(fieldType) !== -1) {
      if (newDefaultValue === String(tempKey)) {
        current?.set('defaultValue', undefined);
      }
    }
  };

  function getAttachFields() {
    const { current } = formDataSet;
    const isCheck = current?.get('check');
    const fieldType = current?.get('fieldType');
    switch (fieldType) {
      case 'time':
        return (
          <>
            <TimePicker
              name="defaultValue"
              disabled={isCheck}
              format="HH:mm:ss"
              className="form-field-full-row"
            />
            <CheckBox
              name="check"
            >
              {formatMessage({ id: 'field.useCurrentTime' })}
            </CheckBox>
          </>
        );
      case 'datetime':
        return (
          <>
            <DateTimePicker
              name="defaultValue"
              disabled={isCheck}
              format="YYYY-MM-DD HH:mm:ss"
              className="form-field-full-row"
            />
            <CheckBox
              name="check"
            >
              {formatMessage({ id: 'field.useCurrentDate' })}
            </CheckBox>
          </>
        );
      case 'date':
        return (
          <>
            <DatePicker
              name="defaultValue"
              disabled={isCheck}
              format="YYYY-MM-DD"
              className="form-field-full-row"
            />
            <CheckBox
              name="check"
            >
              {formatMessage({ id: 'field.useCurrentDate' })}
            </CheckBox>
          </>
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
      case 'radio': case 'single': case 'checkbox': case 'multiple': {
        return (
          <>
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
            <Select
              name="defaultValue"
              key={`${singleList.indexOf(fieldType) !== -1 ? 'single' : 'multiple'}-defaultValue-select`}
              style={{ width: '100%', marginTop: '20px' }}
              multiple={!(singleList.indexOf(fieldType) !== -1)}
            >
              {fieldOptions
                && fieldOptions.length > 0
                && fieldOptions.map((item) => {
                  if (item.enabled) {
                    return (
                      <Option
                        value={item.id || item.tempKey}
                        key={item.id || item.tempKey}
                      >
                        {item.value}
                      </Option>
                    );
                  }
                  return [];
                })}
            </Select>
          </>
        );
      }
      case 'multiMember':
      case 'member':
        return (
          <SelectUser
            key={`page-config-create-or-edit-member-${fieldType}`}
            name="defaultValue"
            selectedUser={current?.get('defaultValueObj')}
            // autoQueryConfig={{
            //   selectedUserIds: current?.get('defaultValue'),
            //   // @ts-ignore
            //   queryUserRequest: async (userId: number) => (type === 'project' ? userApi.getAllInProject('', undefined, userId) : userApi.getAllInOrg('', undefined, userId)),
            // }}
            dataRef={userDataRef}
            multiple={fieldType === 'multiMember'}
            request={({ filter, page }) => (type === 'project' ? userApi.getAllInProject(filter, page) : userApi.getAllInOrg(filter, page))}
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
          )}
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
          onChange={(val) => {
            formDataSet.current?.set('context', uniq([...store.eternalContext, ...(val || [])]));
          }}
          onOption={contextOptionSetter}
        />
        {getAttachFields()}
        {isEdit ? <Select name="syncIssueType" /> : null}
      </Form>
    </div>
  );
}

export default observer(CreateField);
