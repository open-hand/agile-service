import React, {
  useEffect,
  useMemo, useState,
} from 'react';
import {
  Form, Button, Select, DataSet, Row, Col, Progress,
  Icon, CheckBox,
} from 'choerodon-ui/pro';
import { WSHandler, Choerodon } from '@choerodon/boot';
import { observer } from 'mobx-react-lite';
import { find, pick } from 'lodash';
import WSProvider from '@choerodon/master/lib/containers/components/c7n/tools/ws/WSProvider';
import { getProjectId } from '@/utils/common';
import useIsInProgram from '@/hooks/useIsInProgram';
import { fieldApi } from '@/api';
import useFields from './useFields';
import { systemFields, formatFields } from './utils';
import renderField from './renderField';
import styles from './Content.less';
import STATUS_COLOR from '../../../../constants/STATUS';

const { Option } = Select;
const DISABLE_EMPTY_DATA = ['priorityId', 'statusId', 'reporterId', 'tags'];

function BatchModal({
  selected, fields: customFields, onCancel, onEdit, issueSearchStore,
}) {
  const { isInProgram } = useIsInProgram();
  const fieldData = [...systemFields.values(), ...customFields].filter(((f) => (isInProgram ? f.code !== 'epicId' : f.code !== 'featureId')));
  const [fields, Field] = useFields();
  const [loading, setLoading] = useState(false);
  const [progress, setProgress] = useState(0);
  const userFields = fieldData.filter((field) => field.fieldType === 'member').map((field) => ({
    name: field.code,
    type: 'string',
    textField: 'realName',
    valueField: 'id',
  }));
  const dataSet = useMemo(() => new DataSet({
    fields: [{
      name: 'statusId',
      label: '状态',
      lookupAxiosConfig: () => ({
        url: `/agile/v1/projects/${getProjectId()}/schemes/query_status_by_project_id?apply_type=${'agile'}`,
        method: 'get',
      }),
      valueField: 'id',
      textField: 'name',
    }, {
      name: 'sprintId',
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
      label: '所属特性',
      valueField: 'issueId',
      textField: 'summary',
    }] : [{
      name: 'epicId',
      label: '所属史诗',
      lookupAxiosConfig: () => ({
        url: `/agile/v1/projects/${getProjectId()}/issues/epics/select_data`,
        method: 'get',
      }),
      valueField: 'issueId',
      textField: 'epicName',
    }], {
      name: 'priorityId',
      label: '优先级',
      lookupAxiosConfig: () => ({
        url: `/agile/v1/projects/${getProjectId()}/priority/list_by_org`,
        method: 'get',
        transformResponse: (response) => {
          try {
            const data = JSON.parse(response);
            return data.filter((v) => v.enable);
          } catch (error) {
            return response;
          }
        },
      }),
      valueField: 'id',
      textField: 'name',
    }, {
      name: 'labelIssueRelVOList',
      label: '标签',
      lookupAxiosConfig: () => ({
        url: `/agile/v1/projects/${getProjectId()}/issue_labels`,
        method: 'get',
      }),
      valueField: 'labelId',
      textField: 'labelName',
    }, {
      name: 'componentIssueRelVOList',
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
            return data.content;
          } catch (error) {
            return response;
          }
        },
      }),
      valueField: 'componentId',
      textField: 'name',
    }, {
      name: 'fixVersion',
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
      label: '影响的版本',
      lookupAxiosConfig: () => ({
        url: `/agile/v1/projects/${getProjectId()}/product_version/names`,
        method: 'post',
        data: [],
      }),
      valueField: 'versionId',
      textField: 'name',
    },
    {
      name: 'tags',
      label: 'Tag',
    },
    ],
    events: {
      update: ({ value, name, dataSet: formDs }) => {
        if (value && !DISABLE_EMPTY_DATA.includes(name)) {
          formDs.setState('setEmptyId', name);
        }
      },
    },
  }), []);

  useEffect(() => {
    if (dataSet.getState('setEmptyId')) {
      const field = fields.find((item) => item.id === dataSet.getState('setEmptyId'));
      field && Field.set(field.key, { ...field, isSetEmpty: false });
    }
  }, [dataSet.getState('setEmptyId')]);

  const getIsDisabled = () => !fields.some((filed) => filed.code);

  const getData = () => {
    const temp = dataSet.current ? dataSet.current.toData() : {};
    const obj = {};
    fields.forEach((field) => {
      if (field.code && (field.isSetEmpty || temp[field.code])) {
        obj[field.code] = temp[field.code];
      }
    });
    return obj;
  };
  const submit = async () => {
    const data = getData();
    const issueIds = selected;
    const res = { issueIds, ...formatFields(fieldData, data, dataSet) };
    await fieldApi.batchUpdateIssue(res);
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
          setProgress(Number(process));
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
    <>
      <Form
        className={styles.form}
        disabled={Boolean(loading)}
        dataSet={dataSet}
        style={{
          maxHeight: 400, overflowY: 'auto', overflowX: 'hidden',
        }}
      >
        {fields.map((f) => {
          const { key, id, isSetEmpty = false } = f;
          return (
            <Row key={key} gutter={18} type="flex" align="middle">
              <Col span={10}>
                <Select
                  style={{ width: '100%' }}
                  placeholder="请选择字段"
                  label="字段"
                  value={id}
                  onChange={(value) => {
                    const field = find(fieldData, { id: value });
                    Field.set(key, field);
                  }}
                >
                  {fieldData.filter((field) => (id === field.id) || !find(fields, { id: field.id })).map((field) => (
                    <Option value={field.id}>
                      {field.name}
                    </Option>
                  ))}
                </Select>
              </Col>
              {id && (
                <Col span={10} key={id}>
                  {React.cloneElement(renderField(f), {
                    label: f.name,
                  })}
                </Col>
              )}
              {id && (
                <Col span={3}>
                  <CheckBox
                    checked={isSetEmpty}
                    onChange={(value) => {
                      if (value) {
                        dataSet.current?.init(f.code);
                      }
                      Field.set(key, { ...f || {}, isSetEmpty: value });
                    }}
                    disabled={DISABLE_EMPTY_DATA.includes(id)}
                  >
                    置空
                  </CheckBox>
                </Col>
              )}
              <Col span={1}>
                <Icon
                  onClick={() => {
                    Field.remove(key);
                    dataSet.current?.init(f.code);
                  }}
                  type="delete_sweep-o"
                  style={{
                    cursor: 'pointer',
                    color: 'var(--primary-color)',
                  }}
                />
              </Col>
            </Row>
          );
        })}
        <div>
          <Button onClick={Field.add} icon="add">选择字段</Button>
        </div>
      </Form>
      {loading && (
      <div style={{ textAlign: 'center' }}>
        {loading === 'success' ? '修改成功' : ['正在修改，请稍等片刻', <span className={styles.dot}>…</span>]}
        <Progress strokeColor={STATUS_COLOR.done} value={Math.round(progress * 100)} />
      </div>
      )}
      <div style={{ display: 'flex', justifyContent: 'flex-end' }}>
        <Button
          onClick={onCancel}
          disabled={loading}
          style={{
            fontWeight: 500,
          }}
        >
          取消
        </Button>
        <Button
          disabled={getIsDisabled()}
          loading={loading}
          color="primary"
          style={{
            fontWeight: 500,
          }}
          onClick={() => {
            submit();
          }}
        >
          确定
        </Button>
      </div>
    </>
  );

  return (
    <>
      {
        issueSearchStore.batchAction === 'edit' && (
        <div style={{ padding: 15 }}>
          <WSProvider server={Choerodon.WEBSOCKET_SERVER}>
            <WSHandler
              messageKey={`agile-batch-update-field-${getProjectId()}`}
              onMessage={handleMessage}
            >
              {render()}
            </WSHandler>
          </WSProvider>
        </div>
        )
      }
    </>
  );
}
export default observer(BatchModal);
