import React, {
  useMemo, Fragment, useState,
} from 'react';
import {
  Form, Button, Select, DataSet, Row, Col, Progress,
} from 'choerodon-ui/pro';
import {
  stores, WSHandler, Choerodon,  
} from '@choerodon/boot';
import { find } from 'lodash';
import { getProjectId, getOrganizationId } from '@/common/utils';
import { batchUpdateIssue } from '@/api/NewIssueApi';
import IsInProgramStore from '@/stores/common/program/IsInProgramStore';
import WSProvider from '@choerodon/master/lib/containers/components/c7n/tools/ws/WSProvider';
import { WEBSOCKET_SERVER } from '@choerodon/master/lib/containers/common';
import useFields from './useFields';
import renderField from './renderField';
import styles from './index.less';

const { AppState } = stores;

const systemFields = new Map([
  ['statusId', {
    id: 'statusId',
    code: 'statusId',
    name: '状态',
    fieldType: 'single',
  }],
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
  ['sprintId', {
    id: 'sprintId',
    code: 'sprintId',
    name: '冲刺',
    fieldType: 'single',
  }],
  ['epicId', {
    id: 'epicId',
    code: 'epicId',
    name: '所属史诗',
    fieldType: 'single',
  }],
  ['featureId', {
    id: 'featureId',
    code: 'featureId',
    name: '所属特性',
    fieldType: 'single',
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
    format: (value, labelIssueRelVOList) => labelIssueRelVOList,
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
    format: (value, influenceVersion) => influenceVersion,
  }],
  ['fixVersion', {
    id: 'fixVersion',
    code: 'fixVersion',
    name: '修复的版本',
    fieldType: 'multiple',
    format: (value, fixVersion) => fixVersion,
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

function BatchModal({
  dataSet: tableDataSet, fields: customFields, onCancel, onEdit, 
}) {
  const { isInProgram } = IsInProgramStore;
  const fieldData = [...systemFields.values(), ...customFields].filter((f => (isInProgram ? f.code !== 'epicId' : f.code !== 'featureId')));
  const [fields, Field] = useFields();
  const [loading, setLoading] = useState(false);
  const [progress, setProgress] = useState(0);
  const dataSet = useMemo(() => new DataSet({
    fields: [{
      name: 'statusId',
      type: 'number',
      label: '状态',
      lookupAxiosConfig: () => ({
        url: `/agile/v1/projects/${getProjectId()}/schemes/query_status_by_project_id?apply_type=${'agile'}`,
        method: 'get',
      }),
      valueField: 'id',
      textField: 'name',
    }, {
      name: 'sprintId',
      type: 'number',
      label: '冲刺',
      lookupAxiosConfig: () => ({
        url: `/agile/v1/projects/${getProjectId()}/sprint/names`,
        method: 'post',
        data: ['started', 'sprint_planning'],
      }),
      valueField: 'sprintId',
      textField: 'sprintName',
    },
    ...isInProgram ? [{
      name: 'featureId',
      type: 'number',
      label: '所属特性',
      lookupAxiosConfig: () => ({
        url: `/agile/v1/projects/${getProjectId()}/issues/feature/select_data?organizationId=${getOrganizationId()}`,
        method: 'get',
      }),
      valueField: 'issueId',
      textField: 'summary',
    }] : [{
      name: 'epicId',
      type: 'number',
      label: '所属史诗',
      lookupAxiosConfig: () => ({
        url: `/agile/v1/projects/${getProjectId()}/issues/epics/select_data`,
        method: 'get',
      }),
      valueField: 'issueId',
      textField: 'epicName',
    }], {
      name: 'priorityId',
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
      name: 'labelIssueRelVOList',
      type: 'array',
      label: '标签',
      lookupAxiosConfig: () => ({
        url: `/agile/v1/projects/${getProjectId()}/issue_labels`,
        method: 'get',
      }),
      valueField: 'labelId',
      textField: 'labelName',
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
  const getData = () => {
    const temp = dataSet.current ? dataSet.current.toData() : {};
    const obj = {};
    fields.forEach((field) => {
      if (field.code) {
        obj[field.code] = temp[field.code];
      }
    });
    return obj;
  };
  const submit = async () => {
    const data = getData();
    const issueIds = tableDataSet.selected.map(record => record.get('issueId'));
    const res = { issueIds, ...formatFields(fieldData, data, dataSet) };
    await batchUpdateIssue(res);
    setLoading(true);
  };

  const handleMessage = (message) => {
    if (message === 'ok') {
      return;
    }
    const data = JSON.parse(message);
    if (data) {
      const { status, process } = data;
      switch (status) {
        case 'success': {
          setLoading('success');
          setTimeout(() => {
            onEdit();
          }, 2000); 
          break;
        }
        case 'doing': {
          setProgress(Number(process));
          break;
        }
        case 'failed': {
          Choerodon.prompt(data.error, 'error');
          setLoading(false);
          break;
        }
        default: break;
      }      
    }
  };
  const render = () => (
    <Fragment>
      <Form
        className={styles.form}
        disabled={Boolean(loading)}
        dataSet={dataSet}
        style={{
          maxHeight: 400, overflowY: 'auto', overflowX: 'hidden',
        }}
      >
        {fields.map((f) => {
          const { key, id } = f;
          return (
            <Row key={key} gutter={20}>
              <Col span={11}>
                <Select
                  style={{ width: '100%' }}
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
                <Col span={11} key={id}>
                  {renderField(f)}
                </Col>
              )}
              <Col span={2}>                
                <Button
                  onClick={() => {
                    Field.remove(key);
                    dataSet.current.init(f.code);
                  }}
                  icon="delete"
                />
              </Col>
            </Row>
          );
        })}
        <div>
          <Button onClick={Field.add} icon="add" color="blue">添加字段</Button>
        </div>
      </Form>
      {loading && (
      <div style={{ color: 'rgba(254,71,87,1)', textAlign: 'center' }}>
        {loading === 'success' ? '修改成功' : ['正在修改，请稍等片刻', <span className={styles.dot}>…</span>]}      
        <Progress value={Math.round(progress * 100)} />  
      </div>
      )}
      <div style={{ display: 'flex', justifyContent: 'flex-end' }}>
        <Button onClick={onCancel}>取消</Button>
        <Button
          disabled={Object.keys(getData()).length === 0}
          color="blue"
          loading={loading}
          onClick={() => {
            submit();
          }}
        >
          确定
        </Button>
      </div>
    </Fragment>
  );
  return (
    <div style={{ padding: 15 }}>
      <WSProvider server={WEBSOCKET_SERVER}>
        <WSHandler
          messageKey={`choerodon:msg:agile-batch-update-field:${AppState.userInfo.id}`}
          onMessage={handleMessage}
        >
          {render()}
        </WSHandler>
      </WSProvider>
    </div>
  );
}
export default BatchModal;
