import React, {
  useState, useMemo, useEffect, useCallback,
} from 'react';
import { observer } from 'mobx-react-lite';
import {
  DataSet, Select, Form, Button, Row, Col,
  Icon,
} from 'choerodon-ui/pro';

import { FieldProps } from 'choerodon-ui/pro/lib/data-set/Field';
import { find, includes } from 'lodash';
import { FieldType } from 'choerodon-ui/pro/lib/data-set/enum';
import moment from 'moment';
import { getProjectId, getIsOrganization, getOrganizationId } from '@/utils/common';
import useFields from '@/routes/Issue/components/BatchModal/useFields';
import { pageConfigApi, statusTransformApi } from '@/api';
import { Priority, IField } from '@/common/types';
import { OldLoading as Loading } from '@/components/Loading';
import useIsProgram from '@/hooks/useIsProgram';
import renderField from './renderField';
import styles from './index.less';

const { Option } = Select;

interface IFieldK extends IField {
  key: number,
  request: boolean,
}
interface IFieldValue {
  creationDate: string,
  createdBy: number,
  lastUpdateDate: string,
  lastUpdatedBy: number,
  objectVersionNumber: number,
  id: string,
  statusFieldSettingId: string,
  projectId: number,
  optionId: null | string | string[],
  fieldType: 'member' | 'radio' | 'single' | 'checkbox' | 'multiple' | 'text' | 'input' | 'number' | 'date' | 'time' | 'datetime',
  operateType: 'clear' | 'specifier' | 'current_time' | 'add' | 'reportor' | 'creator' | 'operator' | 'participant',
  numberValue: null | number,
  numberAddValue: null | number,
  textValue: null | string,
  dateValue: null | string,
  dateAddValue: null | number,
  userId: null | string,
  name: null | string
}
export interface ISettingField {
  fieldId: string
  fieldValueList: IFieldValue[]
  id: string
  issueTypeId: string
  projectId: number
  statusId: string
}

type ISelectUserMap = Map<string, { id: null | string, realName: null | string }>

const excludeCode = ['summary', 'status', 'issueNum', 'issueType', 'sprint', 'feature', 'epicName', 'epic', 'pi', 'timeTrace', 'lastUpdateDate', 'creationDate', 'created_user', 'last_updated_user', 'tag'];
const orgExcludeCode: string[] = ['component', 'label', 'fixVersion', 'featureType', 'subProject', 'programVersion', 'tag'];
const memberIsNotSpecifier = ['reportor', 'clear', 'operator', 'creator', 'assignee', 'mainResponsible', 'participant'];
// @ts-ignore
const transformUpdateData = (data) => {
  const updateData = [];
  for (const [key, fieldValue] of Object.entries(data)) {
    const {
      // @ts-ignore
      fieldType, fieldId, selected, value,
    } = fieldValue;
    switch (key) {
      case 'environment': {
        if (value) {
          updateData.push({
            fieldId,
            fieldValueList: [{
              operateType: value ? 'specifier' : selected,
              stringValue: value,
              fieldType: 'input',
            }],
          });
        }
        break;
      }
      default: {
        break;
      }
    }
    switch (fieldType) {
      case 'member': {
        const isSpecifier = !includes(memberIsNotSpecifier, value);
        if (value) {
          updateData.push({
            fieldId,
            fieldValueList: [{
              operateType: isSpecifier ? 'specifier' : value,
              userId: isSpecifier ? value : undefined,
              fieldType,
            }],
          });
        }
        break;
      }
      case 'multiMember': {
        if (value && value.length) {
          updateData.push({
            fieldId,
            fieldValueList: (value || []).map((item: string) => {
              const isSpecifier = !includes(memberIsNotSpecifier, item);
              return (
                {
                  operateType: !isSpecifier ? item : 'specifier',
                  userId: isSpecifier ? item : undefined,
                  fieldType,
                }
              );
            }),
          });
        }
        break;
      }
      case 'radio': case 'single': case 'checkbox': case 'multiple': {
        if (key === 'environment') {
          break;
        }
        if (value || value?.length > 0) {
          updateData.push({
            fieldId,
            fieldValueList: (fieldType === 'radio' || fieldType === 'single') ? [{
              operateType: value === 'clear' ? 'clear' : 'specifier',
              optionId: value === 'clear' ? undefined : value,
              fieldType,
            }] : (value || []).map((item: string) => (
              {
                operateType: item === 'clear' ? 'clear' : 'specifier',
                optionId: item === 'clear' ? undefined : item,
                fieldType,
              }
            )),
          });
        }
        break;
      }
      case 'text': {
        if ((selected === 'specifier' && value) || selected === 'clear') {
          updateData.push({
            fieldId,
            fieldValueList: [{
              operateType: value ? 'specifier' : selected,
              textValue: value,
              fieldType,
            }],
          });
        }
        break;
      }
      case 'input': {
        if ((selected === 'specifier' && value) || selected === 'clear') {
          updateData.push({
            fieldId,
            fieldValueList: [{
              operateType: value ? 'specifier' : selected,
              stringValue: value,
              fieldType,
            }],
          });
        }
        break;
      }
      case 'number': {
        if (((selected === 'specifier' || selected === 'add') && (value || value === 0)) || selected === 'clear') {
          updateData.push({
            fieldId,
            fieldValueList: [{
              operateType: selected,
              numberValue: selected === 'specifier' ? value : undefined,
              numberAddValue: selected === 'add' ? value : undefined,
              fieldType,
            }],
          });
        }
        break;
      }
      case 'date': case 'time': case 'datetime': {
        if (((selected === 'specifier' || selected === 'add') && value) || selected === 'clear' || selected === 'current_time') {
          updateData.push({
            fieldId,
            fieldValueList: [{
              operateType: selected,
              dateValue: selected === 'specifier' ? moment(value).format('YYYY-MM-DD HH:mm:ss') : undefined,
              dateAddValue: selected === 'add' ? value : undefined,
              fieldType,
            }],
          });
        }
        break;
      }
      default: {
        break;
      }
    }
  }
  return updateData;
};

// @ts-ignore
const setCurrentByFieldType = (current, fieldValue, fieldCode) => {
  const { fieldValueList } = fieldValue;
  const firstField = (fieldValueList && fieldValueList[0]) || {};
  const { fieldType } = firstField;
  switch (fieldType) {
    case 'member': {
      const { operateType, userId } = firstField;
      const isSpecifier = operateType === 'specifier';
      current.set(fieldCode, isSpecifier ? userId : operateType);
      break;
    }
    case 'multiMember': {
      const { operateType } = firstField;
      const isClear = operateType === 'clear';
      current.set(fieldCode, isClear ? ['clear'] : fieldValueList.map((item: IFieldValue) => {
        const { operateType: fieldValueType } = item;
        const isSpecifierUser = fieldValueType === 'specifier';
        return isSpecifierUser ? item.userId : fieldValueType;
      }));
      break;
    }
    case 'radio': case 'single': case 'checkbox': case 'multiple': {
      const { operateType } = firstField;
      const isClear = operateType === 'clear';
      if (fieldType === 'radio' || fieldType === 'single') {
        current.set(fieldCode, isClear ? 'clear' : firstField.optionId);
      } else {
        // @ts-ignore
        current.set(fieldCode, isClear ? ['clear'] : fieldValueList.map((item) => item.optionId));
      }
      break;
    }
    case 'text': {
      const { operateType, textValue } = firstField;
      current.set(`${fieldCode}-select`, operateType);
      current.set(fieldCode, textValue);
      break;
    }
    case 'input': {
      const { operateType, stringValue } = firstField;
      current.set(`${fieldCode}-select`, operateType);
      current.set(fieldCode, stringValue);
      break;
    }
    case 'number': {
      const { operateType, numberValue, numberAddValue } = firstField;
      current.set(`${fieldCode}-select`, operateType);
      if (operateType !== 'clear') {
        current.set(fieldCode, operateType === 'specifier' ? numberValue : numberAddValue);
      }
      break;
    }
    case 'date': case 'time': case 'datetime': {
      const { operateType, dateValue, dateAddValue } = firstField;
      current.set(`${fieldCode}-select`, operateType);
      current.set(fieldCode, operateType === 'specifier' ? dateValue && moment(dateValue) : dateAddValue);
      break;
    }
    default: {
      break;
    }
  }
};
const memberTypeList = ['member', 'multiMember'];
const UpdateField = ({
  // @ts-ignore
  modal, selectedType, selectedTypeCode, record, customCirculationDataSet,
}) => {
  const { isProgram } = useIsProgram();
  const [fieldData, setFieldData] = useState<IField[]>([]);
  const [updateCount, setUpdateCount] = useState<number>(0);
  const [fields, Field] = useFields();
  const [loading, setLoading] = useState<boolean>(false);
  const [selectUserMap, setSelectUserMap] = useState<ISelectUserMap>(new Map());
  const isOrganization = getIsOrganization();

  const userFields = useMemo(() => fieldData.filter((field) => memberTypeList.includes(field.fieldType)).map((field) => ({
    name: field.code,
    type: 'string' as FieldType,
    textField: 'realName',
    valueField: 'id',
    multiple: field.fieldType === 'multiMember',
    label: '人员',
  })), [fieldData]);

  const numberFields = useMemo(() => fieldData.filter((field) => field.fieldType === 'number').map((field) => {
    if (field.code === 'storyPoints' || field.code === 'remainingTime') {
      return {
        name: field.code,
        type: 'number' as FieldType,
        min: 0,
        max: 100,
        step: 1,
      };
    } if (!field.system && field.extraConfig) {
      return {
        name: field.code,
        type: 'number' as FieldType,
        step: 0.01,
      };
    }
    return {
      name: field.code,
      type: 'number' as FieldType,
      step: 1,
    };
  }), [fieldData]);

  const projectFields = useMemo<FieldProps[]>(() => ([{
    name: 'label',
    type: 'array' as FieldType,
    label: '标签',
    lookupAxiosConfig: () => ({
      url: `/agile/v1/projects/${getProjectId()}/issue_labels`,
      method: 'get',
      transformResponse: (data: any) => (Array.isArray(data) ? data : [{ labelId: 'clear', labelName: '清空' }, ...JSON.parse(data)]),
    }),
    valueField: 'labelId',
    textField: 'labelName',
  }, {
    name: 'component',
    type: 'array' as FieldType,
    label: '模块',
    // @ts-ignore
    lookupAxiosConfig: ({ dataSet: ds, params }) => ({
      url: `/agile/v1/projects/${getProjectId()}/component/query_all`,
      method: 'post',
      data: {
        advancedSearchArgs: {},
        searchArgs: { name: params.name },
      },
      params: {
        size: 999,
        page: 1,
      },
      // @ts-ignore
      transformResponse: (response) => {
        try {
          const data = JSON.parse(response);
          return [{ componentId: 'clear', name: '清空' }, ...data.content];
        } catch (error) {
          return response;
        }
      },
    }),
    valueField: 'componentId',
    textField: 'name',
  }, {
    name: 'fixVersion',
    type: 'array' as FieldType,
    label: '修复的版本',
    lookupAxiosConfig: () => ({
      url: `/agile/v1/projects/${getProjectId()}/product_version/names`,
      method: 'post',
      data: ['version_planning'],
      transformResponse: (data) => (Array.isArray(data) ? data : [{ versionId: 'clear', name: '清空' }, ...JSON.parse(data)]),
    }),
    valueField: 'versionId',
    textField: 'name',
  }, {
    name: 'influenceVersion',
    type: 'array' as FieldType,
    label: '影响的版本',
    lookupAxiosConfig: () => ({
      url: `/agile/v1/projects/${getProjectId()}/product_version/names`,
      method: 'post',
      data: [],
      transformResponse: (data) => (Array.isArray(data) ? data : [{ versionId: 'clear', name: '清空' }, ...JSON.parse(data)]),
    }),
    valueField: 'versionId',
    textField: 'name',
  }]), []);
  const dataSet = useMemo(() => new DataSet({
    autoCreate: true,
    fields: [
      {
        name: 'priority',
        type: 'string' as FieldType,
        label: '优先级',
        lookupAxiosConfig: () => ({
          url: isOrganization ? `/agile/v1/organizations/${getOrganizationId()}/priority` : `/agile/v1/projects/${getProjectId()}/priority/list_by_org`,
          method: 'get',
          // @ts-ignore
          transformResponse: (response) => {
            try {
              if (typeof response === 'string') {
                const data = JSON.parse(response);
                return data.filter((v: Priority) => v.enable);
              }
              return response;
            } catch (error) {
              console.log(error);
              return response;
            }
          },
        }),
        valueField: 'id',
        textField: 'name',
      }, ...(isOrganization ? [] : projectFields), ...userFields, ...numberFields],
    events: {
      update: ({
        // @ts-ignore
        // eslint-disable-next-line no-shadow
        dataSet, record, name, value, oldValue,
      }) => {
        if (value && Array.isArray(value) && value.length > 1) {
          dataSet.current.set(name, value.filter((item) => item !== 'clear'));
        }
        setUpdateCount((count) => count + 1);
      },
    },
  }), [isOrganization, numberFields, projectFields, userFields]);

  useEffect(() => {
    pageConfigApi[isOrganization ? 'loadByIssueType' : 'loadFieldsByType'](selectedType).then((res: IField[] | { fields: IField[] }) => {
      // @ts-ignore
      const data = ((isOrganization ? res.fields.map((item) => ({
        ...item, code: item.fieldCode, name: item.fieldName, id: item.fieldId,
      })) : res) || []).filter((item: IField) => !find([...excludeCode, ...(isOrganization ? orgExcludeCode : [])], (code) => code === item.code));
      setFieldData(data);
    });
  }, [isOrganization, selectedType]);

  useEffect(() => {
    if (fieldData && fieldData.length) {
      setLoading(true);
      // @ts-ignore
      statusTransformApi.getUpdateFieldInfo(selectedType, record.get('id')).then((res) => {
        setLoading(false);
        const initFields = fieldData.filter((
          f,
        ) => res.find((item: ISettingField) => item.fieldId === f.id));
        if (initFields.length) {
          Field.init(initFields);
          const { current } = dataSet;
          if (current) {
            (res || []).forEach((item: ISettingField) => {
              const field = find(fieldData, { id: item.fieldId }) || {} as any;
              // @ts-ignore
              const fieldCode = field?.code;
              const { fieldType } = field;
              const { fieldValueList } = item;
              const firstField = (fieldValueList && fieldValueList[0]) || {};
              const { operateType, userId, name } = firstField;
              const isSpecifier = operateType === 'specifier';
              if (fieldType === 'member' && isSpecifier) {
                // current.set(`${fieldCode}-name`, userId);
                selectUserMap?.set(fieldCode, {
                  // @ts-ignore
                  id: userId,
                  realName: name,
                });
                setSelectUserMap(selectUserMap);
              }
              if (fieldType === 'multiMember') {
                const selectedUsers: any = [];
                (fieldValueList || []).forEach((fieldValue: IFieldValue) => {
                  const { operateType: fieldValueType, userId: id, name: userName } = fieldValue;
                  const isSpecifierUser = fieldValueType === 'specifier';
                  if (isSpecifierUser) {
                    selectedUsers.push({
                      id,
                      realName: userName,
                    });
                  }
                });
                selectUserMap?.set(fieldCode, selectedUsers);
                setSelectUserMap(selectUserMap);
              }
              setCurrentByFieldType(current, item, fieldCode);
            });
          }
        } else {
          Field.add();
        }
      }).catch(() => {
        setLoading(false);
      });
    }
  }, [Field, dataSet, fieldData, record, selectUserMap, selectedType]);

  const getData = useCallback(() => {
    const temp = dataSet.current ? dataSet.current.toData() : {} as any;
    const obj: any = {};
    fields.forEach((field: IFieldK) => {
      if (field.code) {
        const fieldObj = {
          // @ts-ignore
          selected: temp[`${field.code}-select`],
          // @ts-ignore
          value: temp[field.code],
          fieldId: field.id,
          fieldType: field.fieldType,
        };
        obj[field.code] = { ...fieldObj, ...(field.fieldType === 'member' ? { userName: temp[`${field.code}-name`] } : {}) };
      }
    });
    return obj;
  }, [dataSet, fields]);

  useEffect(() => {
    const submit = async () => {
      const validate = await dataSet.validate();
      if (validate) {
        const data = getData();
        const updateData = transformUpdateData(data);
        await statusTransformApi[isOrganization ? 'orgUpdateField' : 'updateField'](selectedType, record.get('id'), record.get('objectVersionNumber'), updateData);
        customCirculationDataSet.query(customCirculationDataSet.currentPage);
        return true;
      }
      return false;
    };
    modal.handleOk(submit);
  }, [customCirculationDataSet, getData, modal, record, selectedType, isOrganization, dataSet]);

  const data = getData();

  const render = () => (
    <Form
      className={styles.form}
      disabled={Boolean(loading)}
      dataSet={dataSet}
      columns={25}
      style={{
        overflowY: 'auto', overflowX: 'hidden', marginLeft: '-0.05rem',
      }}
    >
      {fields.map((f: IFieldK) => {
        const { key, id, code } = f;
        return ([
          <Select
            colSpan={12}
            newLine
            style={{ width: '100%' }}
            placeholder="字段"
            label="字段"
            value={f.id}
            onChange={(value) => {
              const field = find(fieldData, { id: value });
              // @ts-ignore
              Field.set(key, field);
            }}
          >
            {
              fieldData.filter((field: IField) => (
                id === field.id
              ) || !find(fields, {
                id: field.id,
              })).map((field) => (
                <Option value={field.id}>
                  {field.name}
                </Option>
              ))
            }
          </Select>,
          // @ts-ignore
          renderField(f, data, selectedTypeCode, selectUserMap, isProgram, isOrganization),
          <Icon
            onClick={() => {
              // @ts-ignore
              Field.remove(key);
              // @ts-ignore
              dataSet.current.init(f.code);
            }}
            type="delete_sweep-o"
            style={{
              cursor: 'pointer',
              color: 'var(--primary-color)',
              lineHeight: '0.48rem',
            }}
          />,
        ]);
      })}
      <div
        // @ts-ignore
        newLine
      >
        <Button
          // @ts-ignore
          onClick={Field.add}
          icon="add"
        >
          添加字段
        </Button>
      </div>
    </Form>
  );
  return (
    <div className={styles.updateField}>
      <Loading loading={loading} />
      <div className={styles.content}>
        <div className={styles.tip}>工作项流转到当前状态后，将自动执行下面设置的字段值变更。</div>
        {render()}
      </div>

    </div>
  );
};

export default observer(UpdateField);
