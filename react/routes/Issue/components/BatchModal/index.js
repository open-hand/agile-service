import React, { useReducer, useMemo, Fragment } from 'react';
import {
  Form, Button, Select, DataSet, Row, Col,
} from 'choerodon-ui/pro';
import { find } from 'lodash';
import { getProjectId } from '@/common/utils';
import useFields from './useFields';
import renderField from './renderField';
import fieldMock from './fields.json';


const { Option } = Select;

const initialState = {
  count: 0,
};
function reducer(state, action) {
  switch (action.type) {
    case 'add': {
      return {
        ...state,
        count: state.count + 1,
      };
    }
    default: throw new Error();
  }
}

function BatchModal() {
  const [state, dispatch] = useReducer(reducer, initialState);
  const [fields, Field] = useFields();
  const dataSet = useMemo(() => new DataSet({
    transport: {
      submit: () => ({
        url: 'test',
        method: 'post',
        transformRequest: (data) => {
          // eslint-disable-next-line no-console
          console.log(data);
        },
      }),
    },
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
      name: 'component',
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
                    const field = find(fieldMock, { id: value });
                    Field.set(key, field);
                  }}
                >
                  {fieldMock.filter(field => (id === field.id) || !find(fields, { id: field.id })).map(field => (
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
                <Button onClick={() => { Field.remove(key); }} icon="delete" />
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
          color="blue"
          onClick={() => {
            dataSet.submit();
          }}
        >
          确定
        </Button>
      </div>      
    </div>
  );
}
export default BatchModal;
