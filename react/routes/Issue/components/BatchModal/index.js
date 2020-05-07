import React, { useMemo } from 'react';
import {
  Form, Button, Select, DataSet, Row, Col,
} from 'choerodon-ui/pro';
import { find } from 'lodash';
import { getProjectId } from '@/common/utils';
import useFields from './useFields';
import renderField from './renderField';

const systemFields = new Map([
  ['status', {
    id: 'status',
    code: 'status',
    name: '状态',
    fieldType: 'single',
  }],
  ['assignee', {
    id: 'assignee',
    code: 'assignee',
    name: '经办人',
    fieldType: 'member',
  }],
  ['reporter', {
    id: 'reporter',
    code: 'reporter',
    name: '报告人',
    fieldType: 'member',
  }],
  ['sprint', {
    id: 'sprint',
    code: 'sprint',
    name: '冲刺',
    fieldType: 'single',
  }],
  ['epic', {
    id: 'epic',
    code: 'epic',
    name: '所属史诗',
    fieldType: 'single',
  }],
  // ['feature', {
  //   id: 'feature',
  //   code: 'feature',
  //   name: '所属特性',
  //   fieldType: 'single',
  // }],
  ['priority', {
    id: 'priority',
    code: 'priority',
    name: '优先级',
    fieldType: 'single',
  }],
  ['label', {
    id: 'label',
    code: 'label',
    name: '标签',
    fieldType: 'multiple',
    format: (value, label) => label,
  }],
  ['componentIssueRelVOList', {
    id: 'componentIssueRelVOList',
    code: 'componentIssueRelVOList',
    name: '模块',
    fieldType: 'multiple',
    format: (value, component) => component,
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
const { Option } = Select;

function transformValue(dataSet, key, value, format) {
  if (!value || !format) {
    return value;
  }
  function transform(v) {
    const lookup = dataSet.getField(key).getLookupData(v);
    return format(v, lookup);
  }
  if (Array.isArray(value)) {
    return value.map(v => transform(v));
  }
  return transform(value);
}

function formatFields(fieldData, data, dataSet) {
  const temp = {
    predefinedFields: {},
    customFields: [],
  };
  for (const key of Object.keys(data)) {
    if (systemFields.get(key)) {
      temp.predefinedFields[key] = transformValue(dataSet, key, data[key], systemFields.get(key).format);
    } else {
      const customField = find(fieldData, { code: key });
      temp.customFields.push({
        fieldId: customField.id,
        fieldType: customField.fieldType,
        value: data[key],
      });
    }
  }
  return temp;
}

function BatchModal({ dataSet: tableDataSet, fields: customFields }) {
  const fieldData = [...systemFields.values(), ...customFields]; 
  const [fields, Field] = useFields();

  const dataSet = useMemo(() => new DataSet({
    autoCreate: true,
    fields: [{
      name: 'priority',
      type: 'number',
      label: '优先级',
      lookupAxiosConfig: () => ({
        url: `/agile/v1/projects/${getProjectId()}/priority/list_by_org`,
        method: 'get',
        transformResponse: (response) => {
          try {
            const data = JSON.parse(response);         
            return data.filter(v => v.enable);
          } catch (error) {
            return response;
          }
        },
      }),
      valueField: 'id',
      textField: 'name',
    }, {
      name: 'status',
      type: 'number',
      label: '状态',
      lookupAxiosConfig: () => ({
        url: `/agile/v1/projects/${getProjectId()}/schemes/query_status_by_project_id?apply_type=${'agile'}`,
        method: 'get',
      }),
      valueField: 'id',
      textField: 'name',
    }, {
      name: 'componentIssueRelVOList',
      type: 'array',
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
            return data.list;
          } catch (error) {
            return response;
          }
        },
      }),
      valueField: 'componentId',
      textField: 'name',
    }, {
      name: 'label',
      type: 'array',
      label: '标签',
      lookupAxiosConfig: () => ({
        url: `/agile/v1/projects/${getProjectId()}/issue_labels`,
        method: 'get',
      }),
      valueField: 'labelId',
      textField: 'labelName',
    }, {
      name: 'fixVersion',
      type: 'array',
      label: '修复的版本',
      lookupAxiosConfig: () => ({
        url: `/agile/v1/projects/${getProjectId()}/product_version/names`,
        method: 'post',
        data: ['version_planning'],
      }),
      valueField: 'versionId',
      textField: 'name',
    }, {
      name: 'influenceVersion',
      type: 'array',
      label: '影响的版本',
      lookupAxiosConfig: () => ({
        url: `/agile/v1/projects/${getProjectId()}/product_version/names`,
        method: 'post',
        data: [],
      }),
      valueField: 'versionId',
      textField: 'name',
    }],
  }), []);
  const data = useMemo(() => {
    const temp = dataSet.current.toData();
    const obj = {};
    fields.forEach((field) => {
      if (field.code) {
        obj[field.code] = temp[field.code];
      }
    });
    return obj;
  }, [fields]);
  const submit = () => {
    const issueIds = tableDataSet.selected.map(record => record.get('issueId'));
    const res = { issueIds, ...formatFields(fieldData, data, dataSet) };
    // eslint-disable-next-line no-console
    console.log(res);
  };
  return (
    <div style={{ padding: 15 }}>
      <Form
        dataSet={dataSet}
        style={{
          maxHeight: 400, overflowY: 'auto', overflowX: 'hidden', 
        }}
      >
        {fields.map((f) => {
          const { key, id } = f;
          return (
            <Row key={key}>
              <Col span={10}>
                <Select
                  placeholder="请选择"
                  onChange={(value) => {
                    const field = find(fieldData, { id: value });
                    Field.set(key, field);
                  }}
                >
                  {fieldData.filter(field => (id === field.id) || !find(fields, { id: field.id })).map(field => (
                    <Option value={field.id}>
                      {field.name}
                    </Option>
                  ))}
                </Select>
              </Col>
              {id && (
                <Col span={10} key={id}>
                  {renderField(f)}
                </Col>
              )}
              <Col span={4}>
                <Button
                  onClick={() => { 
                    Field.remove(key); 
                    dataSet.current.init(f.code);
                  }}
                  icon="delete"
                  style={{ marginLeft: 10 }}
                />
              </Col>
            </Row>
          );
        })}
        <div>
          <Button onClick={Field.add} icon="add" color="blue">添加字段</Button>
        </div>          
      </Form>
      <div>         
        <Button>取消</Button>
        <Button
          disabled={Object.keys(data).length === 0}
          color="blue"
          onClick={() => {
            submit();
          }}
        >
          确定
        </Button>
      </div>      
    </div>
  );
}
export default BatchModal;
