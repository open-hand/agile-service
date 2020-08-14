import React, {
  useState, useMemo, useEffect, useCallback,
} from 'react';
import { observer } from 'mobx-react-lite';
import {
  DataSet, Select, Form, Button, Row, Col,
} from 'choerodon-ui/pro';
import { getProjectId } from '@/utils/common';
import { find } from 'lodash';
import useFields from '@/routes/Issue/components/BatchModal/useFields';
import { pageConfigApi, statusTransformApi } from '@/api';
import { ButtonColor } from 'choerodon-ui/pro/lib/button/enum';
import { FieldType } from 'choerodon-ui/pro/lib/data-set/enum';
import { Priority, IField } from '@/common/types';
import Loading from '@/components/Loading';
import moment from 'moment';
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
  fieldType: 'member' | 'radio' | 'single' |'checkbox' | 'multiple' | 'text' | 'input' | 'number' | 'date' | 'time' | 'datetime',
  operateType: 'clear' | 'specifier' | 'current_time' | 'add' | 'reportor' | 'creator' | 'operator',
  numberValue: null | number,
  numberAddValue: null | number,
  textValue: null | string,
  dateValue: null | string,
  dateAddValue: null | number,
  userId: null | string,
  name: null | string
}
interface ISettingField {
  fieldId: string
  fieldValueList: IFieldValue[]
  id: string
  issueTypeId: string
  projectId: number
  statusId: string
}

const excludeCode = ['summary', 'status', 'issueNum', 'issueType', 'sprint', 'feature', 'epicName', 'epic', 'pi', 'timeTrace', 'lastUpdateDate', 'creationDate'];
const dateTransform = (fieldType: string, d: Date) => {
  let transformed = '';
  switch (fieldType) {
    case 'time': {
      transformed = moment(d).format('HH:mm:ss');
      break;
    }
    case 'date': {
      transformed = moment(d).format('YYYY-MM-DD');
      break;
    }
    case 'datetime': {
      transformed = moment(d).format('YYYY-MM-DD HH:mm:ss');
      break;
    }
    default: {
      break;
    }
  }
  return transformed;
};
// @ts-ignore
const transformUpdateData = (data) => {
  const updateData = [];
  for (const [key, fieldValue] of Object.entries(data)) {
    const {
    // @ts-ignore
      fieldType, fieldId, selected, value,
    } = fieldValue;
    switch (fieldType) {
      case 'member': {
        const isSpecifier = value !== 'reportor' && value !== 'creator' && value !== 'operator' && value !== 'clear';
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
      case 'radio': case 'single': case 'checkbox': case 'multiple': {
        if (value || value.length > 0) {
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
        if (((selected === 'specifier' || selected === 'add') && value) || selected === 'clear') {
          updateData.push({
            fieldId,
            fieldValueList: [{
              operateType: selected,
              numberValue: selected === 'specifier' ? value : undefined,
              numAddValue: selected === 'add' ? value : undefined,
              fieldType,
            }],
          });
        }
        break;
      }
      case 'date': case 'time': case 'datetime': {
        if (((selected === 'specifier' || selected === 'add') && value) || selected === 'clear') {
          updateData.push({
            fieldId,
            fieldValueList: [{
              operateType: selected,
              dateValue: selected === 'specifier' ? dateTransform(fieldType, value) : undefined,
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
      const { operateType, numberValue, numAddValue } = firstField;
      current.set(`${fieldCode}-select`, operateType);
      if (operateType !== 'clear') {
        current.set(fieldCode, operateType === 'specifier' ? numberValue : numAddValue);
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

const UpdateField = ({
  // @ts-ignore
  modal, selectedType, record, customCirculationDataSet,
}) => {
  const [fieldData, setFieldData] = useState<IField[]>([]);
  const [updateCount, setUpdateCount] = useState<number>(0);
  const [fields, Field] = useFields();
  const [loading, setLoading] = useState<boolean>(false);

  const userFields = useMemo(() => fieldData.filter((field) => field.fieldType === 'member').map((field) => ({
    name: field.code,
    type: 'string' as FieldType,
    textField: 'realName',
    valueField: 'id',
    label: '人员',
  })), [fieldData]);

  const dataSet = useMemo(() => new DataSet({
    autoCreate: true,
    fields: [
      {
        name: 'priority',
        type: 'string' as FieldType,
        label: '优先级',
        lookupAxiosConfig: () => ({
          url: `/agile/v1/projects/${getProjectId()}/priority/list_by_org`,
          method: 'get',
          transformResponse: (response) => {
            try {
              const data = JSON.parse(response);
              return data.filter((v: Priority) => v.enable);
            } catch (error) {
              return response;
            }
          },
        }),
        valueField: 'id',
        textField: 'name',
      }, {
        name: 'label',
        type: 'array' as FieldType,
        label: '标签',
        lookupAxiosConfig: () => ({
          url: `/agile/v1/projects/${getProjectId()}/issue_labels`,
          method: 'get',
          transformResponse: (data) => (Array.isArray(data) ? data : [{ labelId: 'clear', labelName: '清空' }, ...JSON.parse(data)]),
        }),
        valueField: 'labelId',
        textField: 'labelName',
      }, {
        name: 'component',
        type: 'array' as FieldType,
        label: '模块',
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
        name: 'version',
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
      }, ...userFields],
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
  }), [userFields]);

  useEffect(() => {
    pageConfigApi.loadFieldsByType(selectedType).then((res: IField[]) => {
      const data = res.filter((item) => !find(excludeCode, (code) => code === item.code));
      setFieldData(data);
    });
  }, [selectedType]);

  useEffect(() => {
    if (fieldData && fieldData.length) {
      setLoading(true);
      // @ts-ignore
      statusTransformApi.getUpdateFieldInfo(selectedType, record.get('id')).then((res) => {
        setLoading(false);
        const initFields = fieldData.filter((
          f,
        ) => res.find((item: ISettingField) => item.fieldId === f.id));
        Field.init(initFields);
        const { current } = dataSet;
        if (current) {
          (res || []).forEach((item: ISettingField) => {
            const field = find(fieldData, { id: item.fieldId }) || {};
            // @ts-ignore
            const fieldCode = field?.code;
            setCurrentByFieldType(current, item, fieldCode);
          });
        }
      }).catch(() => {
        setLoading(false);
      });
    }
  }, [dataSet, fieldData, record, selectedType]);

  const getData = useCallback(() => {
    const temp = dataSet.current ? dataSet.current.data : {};
    const obj: any = {};
    fields.forEach((field: IFieldK) => {
      if (field.code) {
        obj[field.code] = {
          // @ts-ignore
          selected: temp[`${field.code}-select`],
          // @ts-ignore
          value: temp[field.code],
          fieldId: field.id,
          fieldType: field.fieldType,
        };
      }
    });
    return obj;
  }, [dataSet, fields]);

  useEffect(() => {
    const submit = async () => {
      const data = getData();
      const updateData = transformUpdateData(data);
      await statusTransformApi.updateField(selectedType, record.get('id'), record.get('objectVersionNumber'), updateData);
      customCirculationDataSet.query();
      return true;
    };
    modal.handleOk(submit);
  }, [customCirculationDataSet, getData, modal, record, selectedType]);
  const data = getData();

  const render = () => (
    <Form
      className={styles.form}
      disabled={Boolean(loading)}
      dataSet={dataSet}
      style={{
        overflowY: 'auto', overflowX: 'hidden',
      }}
    >
      {
        fields.map((f: IFieldK) => {
          const { key, id } = f;
          return (
            <Row key={key} gutter={20}>
              <Col span={11}>
                <Select
                  style={{ width: '100%' }}
                  placeholder="字段"
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
                </Select>
              </Col>
              {id && (
                <Col span={11} key={id}>
                  {
                  // @ts-ignore
                  renderField(f, data)
                  }
                </Col>
              )}
              <Col span={2}>
                <Button
                  onClick={() => {
                    // @ts-ignore
                    Field.remove(key);
                    // @ts-ignore
                    dataSet.current.init(f.code);
                  }}
                  icon="delete"
                />
              </Col>
            </Row>
          );
        })
      }
      <div>
        <Button
            // @ts-ignore
          onClick={Field.add}
          icon="add"
          color={'blue' as ButtonColor}
        >
          添加字段
        </Button>
      </div>
    </Form>
  );
  return (
    <div className={styles.updateField}>
      <Loading loading={loading} />
      {render()}
    </div>
  );
};

export default observer(UpdateField);
