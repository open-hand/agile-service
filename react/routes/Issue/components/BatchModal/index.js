import React, { useReducer, useMemo, Fragment } from 'react';
import {
  Form, Button, Select, DataSet, Row, Col,
} from 'choerodon-ui/pro';
import { find } from 'lodash';
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
                <Col span={10}>
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
