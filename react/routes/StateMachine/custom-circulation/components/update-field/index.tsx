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
import { fieldApi, pageConfigApi } from '@/api';
import { ButtonColor } from 'choerodon-ui/pro/lib/button/enum';
import { FieldType } from 'choerodon-ui/pro/lib/data-set/enum';
import { Priority, IField } from '@/common/types';
import renderField from './renderField';
import styles from './index.less';

const { Option } = Select;

const systemFields = new Map([
  ['assigneeId', {
    id: 'assigneeId',
    code: 'assigneeId',
    name: '经办人',
    fieldType: 'member',
  }],
  ['reporterId', {
    id: 'reporterId',
    code: 'reporterId',
    name: '报告人',
    fieldType: 'member',
  }],
  ['priorityId', {
    id: 'priorityId',
    code: 'priorityId',
    name: '优先级',
    fieldType: 'single',
  }],
  ['labelIssueRelVOList', {
    id: 'labelIssueRelVOList',
    code: 'labelIssueRelVOList',
    name: '标签',
    fieldType: 'multiple',
  }],
  ['componentIssueRelVOList', {
    id: 'componentIssueRelVOList',
    code: 'componentIssueRelVOList',
    name: '模块',
    fieldType: 'multiple',
  }],
  ['influenceVersion', {
    id: 'influenceVersion',
    code: 'influenceVersion',
    name: '影响的版本',
    fieldType: 'multiple',
  }],
  ['fixVersion', {
    id: 'fixVersion',
    code: 'fixVersion',
    name: '修复的版本',
    fieldType: 'multiple',
  }],
  ['storyPoints', {
    id: 'storyPoints',
    code: 'storyPoints',
    name: '故事点',
    fieldType: 'number',
  }],
  ['remainingTime', {
    id: 'remainingTime',
    code: 'remainingTime',
    name: '预估时间',
    fieldType: 'number',
  }],
]);

const programSystemFields = new Map([
  ['reporterId', {
    id: 'reporterId',
    code: 'reporterId',
    name: '报告人',
    fieldType: 'member',
  }],
  ['teamProjectIds', {
    id: 'teamProjectIds',
    code: 'teamProjectIds',
    name: '负责的子项目',
    fieldType: 'multiple',
  }],

  ['benfitHypothesis', {
    id: 'benfitHypothesis',
    code: 'benfitHypothesis',
    name: '特性价值',
    fieldType: 'input',
  }],
  ['acceptanceCritera', {
    id: 'acceptanceCritera',
    code: 'acceptanceCritera',
    name: '验收标准',
    fieldType: 'input',
  }],
]);
// @ts-ignore
function transformValue(dataSet, key, value, format) {
  if (!value || !format) {
    return value;
  }
  function transform(v: string) {
    const lookup = dataSet.getField(key).getLookupData(v);
    return format(v, lookup);
  }
  if (Array.isArray(value)) {
    return value.map((v) => transform(v));
  }
  return transform(value);
}
// @ts-ignore
function formatFields(fieldData, data, dataSet) {
  const temp = {
    predefinedFields: {},
    customFields: [],
  };
  for (const key of Object.keys(data)) {
    if (systemFields.get(key)) {
      // @ts-ignore
      // eslint-disable-next-line max-len
      temp.predefinedFields[key] = transformValue(dataSet, key, data[key], systemFields.get(key).format);
    } else {
      const customField = find(fieldData, { code: key });
      // @ts-ignore
      // eslint-disable-next-line max-len
      temp.customFields.push({ fieldId: customField.id, fieldType: customField.fieldType, value: data[key] });
    }
  }
  return temp;
}
// @ts-ignore
const UpdateField = ({ modal, isProgram, selectedType }) => {
  console.log('isProgram, selectedType：');
  console.log(isProgram, selectedType);
  const [fieldData, setFieldData] = useState<IField[]>([]);
  const [updateCount, setUpdateCount] = useState<number>(0);
  useEffect(() => {
    pageConfigApi.loadFieldsByType(selectedType).then((res: IField[]) => {
      console.log('res：');
      console.log(res);
    });
  }, [selectedType]);
  useEffect(() => {
    const getCustomFields = async () => {
      const fields = await fieldApi.getCustomFields();
      console.log('field Data：');
      console.log([...(
        isProgram ? programSystemFields.values() : systemFields.values()
      ), ...fields]);
      setFieldData([...(
        isProgram ? programSystemFields.values() : systemFields.values()
      ), ...fields]);
    };
    getCustomFields();
  }, [isProgram]);
  const [fields, Field] = useFields();
  const [loading, setLoading] = useState(false);

  const userFields = useMemo(() => fieldData.filter((field) => field.fieldType === 'member').map((field) => ({
    name: field.code,
    type: 'string' as FieldType,
    textField: 'realName',
    valueField: 'id',
    label: '人员',
  })), [fieldData]);

  const dataSet = useMemo(() => new DataSet({
    fields: [
      {
        name: 'priorityId',
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
        name: 'labelIssueRelVOList',
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
        name: 'componentIssueRelVOList',
        type: 'array' as FieldType,
        label: '模块',
        lookupAxiosConfig: ({ record, dataSet: ds, params }) => ({
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

  const getData = useCallback(() => {
    const temp = dataSet.current ? dataSet.current.data : {};
    const obj: any = {};
    // @ts-ignore
    fields.forEach((field) => {
      if (field.code) {
        obj[field.code] = {
          // @ts-ignore
          selected: temp[`${field.code}-select`],
          // @ts-ignore
          value: temp[field.code],
        };
      }
    });
    return obj;
  }, [dataSet, fields]);

  useEffect(() => {
    const submit = async () => {
      const data = getData();
      console.log('data：');
      console.log(data);
      // const issueIds = tableDataSet.selected.map((record) => record.get('issueId'));
      // const res = { issueIds, ...formatFields(fieldData, data, dataSet) };
      // await fieldApi.batchUpdateIssue(res);
      return false;
      // setLoading(true);
    };
    modal.handleOk(submit);
  }, [dataSet, fields, getData, modal]);

  const data = getData();
  const render = () => (
    <Form
      className={styles.form}
        // disabled={Boolean(loading)}
      dataSet={dataSet}
      style={{
        maxHeight: 400, overflowY: 'auto', overflowX: 'hidden',
      }}
    >
      {
          // @ts-ignore
        fields.map((f) => {
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

                    // @ts-ignore
                    fieldData.filter((field: IField) => (
                      id === field.id
                    ) || !find(fields, {
                      // @ts-ignore
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
                  { renderField(f, data) }
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
      {render()}
    </div>
  );
};

export default observer(UpdateField);
