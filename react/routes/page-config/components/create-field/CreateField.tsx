import React, {
  useState, useContext, useEffect,
} from 'react';
import { observer } from 'mobx-react-lite';
import {
  Form, TextField, Select, DatePicker, TimePicker, DateTimePicker,
  CheckBox, NumberField, TextArea, UrlField,
} from 'choerodon-ui/pro';
import { Choerodon } from '@choerodon/boot';
import SelectUser from '@/components/select/select-user';
import moment from 'moment';
import { toJS } from 'mobx';
import { randomString } from '@/utils/random';
import { RenderProps } from 'choerodon-ui/pro/lib/field/FormField';
import { pageConfigApi, userApi } from '@/api';
import Store from './stores';
import DragList from '../drag-list';
import './index.less';
import * as images from '../../images';

const { Option } = Select;
const singleList = ['radio', 'single'];
const multipleList = ['checkbox', 'multiple'];
interface FiledOptions {
  fieldOptions: any,
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
  extraConfig: any,
  syncIssueType?: string[] // 同步主默认值的问题类型
}
export type IFieldPostDataProps = IFieldPostData;
function CreateField() {
  const ctx = useContext(Store);
  const {
    formDataSet, formatMessage, modal, onSubmitLocal,
    AppState: { currentMenuType: { type, id, organizationId } },
    schemeCode, isEdit, handleRefresh,
  } = ctx;
  const [fieldOptions, setFieldOptions] = useState<Array<any>>([]);

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
    return {
      disabled: currentValue === 'global' ? contextValue.length > 0 && contextValue.indexOf('global') < 0 : contextValue.indexOf('global') >= 0,
    };
  };
  const dataTransformPostData = (fieldOption: FiledOptions): IFieldPostData => {
    const data = formDataSet.toData()[0] as IFieldPostData;
    const dateList = ['date', 'datetime', 'time'];
    const prefix = type === 'project' ? 'pro_' : 'org_';
    const { name, check } = data;
    const { context } = data;
    // if (context && context.length === formDataSet.getField('context')?.options?.length) {
    //   context = ['global'];
    // }
    const transformTime = {} as { defaultValue: string };
    const dateFormat = ['YYYY-MM-DD', 'YYYY-MM-DD HH:mm:ss', 'HH:mm:ss'];
    const dateIndex = dateList.indexOf(data.fieldType);
    if (dateIndex !== -1 && fieldOption?.defaultValue !== '') {
      const dateFormatVal = moment(fieldOption.defaultValue);
      transformTime.defaultValue = dateFormatVal.isValid() ? dateFormatVal.format(dateFormat[1]) : moment(fieldOption.defaultValue, dateFormat).format(dateFormat[1]);
    }
    const postData: IFieldPostData = {
      context,
      code: `${prefix}${data.code}`,
      name,
      ...fieldOption,
      ...transformTime,
      schemeCode,
      extraConfig: check,
    };
    return postData;
  };
  // 创建或者编辑的提交操作
  async function handleOk() {
    const { current } = formDataSet;
    const obj: FiledOptions & { localDefaultObj?: any } = {
      fieldOptions: null,
      fieldType: current?.get('fieldType'),
      defaultValue: String(current?.get('defaultValue') || ''),
    };
    if (singleList.indexOf(obj.fieldType) !== -1) {
      if (fieldOptions.length === 0) {
        Choerodon.prompt('字段列表不能为空');
        return false;
      }
      obj.fieldOptions = fieldOptions.map((o) => {
        if (obj.defaultValue
          && (o.id === obj.defaultValue || o.code === obj.defaultValue
            || o.tempKey === obj.defaultValue)) {
          return { ...o, isDefault: true };
        }
        return { ...o, isDefault: false };
      });
    } else if (multipleList.indexOf(obj.fieldType) !== -1) {
      if (fieldOptions.length === 0) {
        Choerodon.prompt('字段列表不能为空');
        return false;
      }
      const defaultValueArr = toJS(current?.get('defaultValue'));
      obj.fieldOptions = fieldOptions.map((o) => {
        if (Array.isArray(defaultValueArr) && defaultValueArr.some((v) => v === o.id || v === o.tempKey || v === o.code)) {
          return { ...o, isDefault: true };
        }
        return { ...o, isDefault: false };
      });
      // if (obj.defaultValue && Array.isArray(obj.defaultValue)) {
      //   obj.defaultValue = obj.defaultValue.join(',');
      // }
    }
    // 防止使用dataSet提交时 忽略filedOptions
    formDataSet.current?.set('updateFieldOptions', obj.fieldOptions);
    if (onSubmitLocal) {
      const validResult = await formDataSet.validate();
      if (obj.fieldType === 'member' && obj?.defaultValue !== '') {
        const { list: userInfoList } = await userApi.getById(obj.defaultValue);
        obj.localDefaultObj = userInfoList && userInfoList.length > 0 ? userInfoList[0] : {};
      }
      return validResult && onSubmitLocal(dataTransformPostData(obj));
    }
    const fieldId = formDataSet.current?.get('id');
    const url = isEdit ? `/agile/v1/${type}s/${id}/object_scheme_field/${fieldId}?organizationId=${organizationId}` : `/agile/v1/${type}s/${id}/object_scheme_field?organizationId=${organizationId}`;
    const method = isEdit ? 'put' : 'post';
    formDataSet.transport[isEdit ? 'update' : 'create'] = ({ data: [data] }) => ({
      url,
      method,
      transformRequest: () => {
        const postData: IFieldPostData = dataTransformPostData(obj);
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
        syncIssueTypeArr.length > 0 && await pageConfigApi.syncDefaultValue(fieldId, String(syncIssueTypeArr), extraConfig);
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
      case 'member':
        return (
          <SelectUser
            name="defaultValue"
            autoQueryConfig={{
              selectedUserIds: current?.get('defaultValue'),
              // @ts-ignore
              queryUserRequest: async (userId: number) => (type === 'project' ? userApi.getAllInProject('', undefined, userId) : userApi.getAllInOrg('', undefined, userId)),
            }}
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
          onOption={contextOptionSetter}
        />
        {getAttachFields()}
        {isEdit ? <Select name="syncIssueType" /> : null}
      </Form>
    </div>
  );
}

export default observer(CreateField);
