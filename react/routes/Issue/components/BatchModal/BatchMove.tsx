import React, {
  useCallback,
  useEffect,
  useMemo, useRef, useState,
} from 'react';
import {
  Form, Button, Select, DataSet, Row, Col, Progress, Spin, Icon,
} from 'choerodon-ui/pro';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import { toJS } from 'mobx';
import { ButtonColor } from 'choerodon-ui/pro/lib/button/interface';
import { WSHandler, Choerodon } from '@choerodon/boot';
import { observer } from 'mobx-react-lite';
import { includes } from 'lodash';
// @ts-ignore
import WSProvider from '@choerodon/master/lib/components/ws/WSProvider';
import { getProjectId } from '@/utils/common';
import {
  commonApi, issueApi, issueTypeApi, projectApi,
} from '@/api';
import STATUS_COLOR from '../../../../constants/STATUS';
import { IssueSearchStore } from '@/components/issue-search';
import { IIssueType } from '@/common/types';
import SelectProject from '@/components/select/select-project';
import SelectStatus from '@/components/select/select-status';
import SelectIssueType from '@/components/select/select-issue-type';
import useIssueTypes from '@/hooks/data/useIssueTypes';
import styles from './BatchMove.less';

const isDEV = process.env.NODE_ENV === 'development';
// @ts-ignore
const HAS_AGILE_PRO = C7NHasModule('@choerodon/agile-pro');
const shouldRequest = isDEV || HAS_AGILE_PRO;

interface Props {
  selected: string[],
  issueSearchStore: IssueSearchStore
  onCancel: () => void
  onMove: () => void
}
const BatchContent: React.FC<Props> = ({
  selected, onCancel, onMove, issueSearchStore,
}) => {
  const { data: issueTypesList } = useIssueTypes();
  const [loading, setLoading] = useState<string | boolean>(false);
  const [progress, setProgress] = useState(0);
  const projectDsRef = useRef<DataSet>();
  const [fetching, setFetching] = useState<boolean>(false);

  const issueTypeOptionsDs = useMemo(() => new DataSet({
    paging: false,
    data: [],
  }), []);

  const issueTypeDs = useMemo(() => new DataSet({
    fields: [{
      name: 'issueTypeId',
      textField: 'name',
      valueField: 'id',
      label: '目标类型',
      options: issueTypeOptionsDs,
      required: true,
      dynamicProps: {
        disabled: () => !projectDsRef?.current?.current?.get('targetProjectId'),
      },
    }, {
      name: 'originIssueTypeId',
      textField: 'name',
      valueField: 'id',
      label: '源类型',
      options: issueTypeOptionsDs,
      disabled: true,
    }],
    events: {
      update: ({ name, record, value }: { name: string, record: Record, value: string | undefined}) => {
        if (name === 'issueTypeId') {
          const statusDs = record.getState('statusDs');
          statusDs.setState('issueTypeId', value);
          statusDs.records.forEach((statusRd: Record) => {
            if (statusRd.get('statusId')) {
              statusRd.set('statusId', undefined);
            }
          });
        }
      },
    },
  }), [issueTypeOptionsDs]);

  const getStatusDs = useCallback(() => ({
    fields: [{
      name: 'statusId',
      textField: 'name',
      valueField: 'id',
      label: '目标状态',
      required: true,
      dynamicProps: {
        disabled: ({ dataSet }: { dataSet: DataSet}) => !projectDsRef?.current?.current?.get('targetProjectId') || !dataSet.getState('issueTypeId'),
      },
    }, {
      name: 'originStatusId',
      textField: 'name',
      valueField: 'id',
      label: '源状态',
      disabled: true,
    }],
  }), []);

  useEffect(() => {
    const getTypeStatusMap = async () => {
      setFetching(true);
      const res = await issueApi.getBatchMoveMap(toJS(selected));
      const originIssueTypeIds = Object.keys(res);
      originIssueTypeIds.forEach((originIssueType) => {
        const issueTypeRd = issueTypeDs.find((typeRd) => typeRd.get('originIssueTypeId') === originIssueType);
        if (!issueTypeRd) {
          const newRecord = issueTypeDs?.create();
          newRecord?.set('originIssueTypeId', originIssueType);
          newRecord?.setState('statusDs', new DataSet(getStatusDs()));
          const originStatusIds = res[originIssueType];
          originStatusIds.forEach((originStatus: string) => {
            const statusRecord = newRecord?.getState('statusDs')?.create();
            statusRecord?.set('originStatusId', originStatus);
          });
        } else {
          const originStatusIds = res[originIssueType];
          const issueTypeStatusDs = issueTypeRd?.getState('statusDs');
          originStatusIds.forEach((originStatus: string) => {
            const statusRecord = issueTypeStatusDs.find((rd: Record) => rd.get('originStatusId') === originStatus);
            if (!statusRecord) {
              const newStatusRecord = issueTypeStatusDs?.create();
              newStatusRecord?.set('originStatusId', originStatus);
            }
          });
          issueTypeStatusDs.forEach((statusRd: Record) => {
            if (!includes(originStatusIds, statusRd.get('originStatusId'))) {
              issueTypeStatusDs.remove(statusRd);
            }
          });
        }
      });
      issueTypeDs.forEach((rd) => {
        if (!includes(originIssueTypeIds, rd.get('originIssueTypeId'))) {
          issueTypeDs.remove(rd);
        }
      });
      setFetching(false);
    };
    getTypeStatusMap();
  }, [getStatusDs, issueTypeDs, selected]);

  const projectDs = useMemo(() => new DataSet({
    autoCreate: true,
    fields: [{
      name: 'targetProjectId',
      textField: 'name',
      valueField: 'id',
      label: '目标项目',
      required: true,
    }],
    events: {
      update: async ({ value }: { value: string | undefined }) => {
        issueTypeDs.forEach((issueTypeRd) => {
          if (issueTypeRd.get('issueTypeId')) {
            issueTypeRd.set('issueTypeId', undefined);
            const issueTypeStatusDs = issueTypeRd.getState('statusDs');
            issueTypeStatusDs.forEach((statusRd: Record) => {
              if (statusRd.get('statusId')) {
                statusRd.set('statusId', undefined);
              }
            });
          }
        });
        if (value) {
          const targetProjectInfo = await projectApi.loadBasicInfo(value);
          let targetIsInProgram = false;
          const targetIsProgram = (targetProjectInfo.categories || []).find((item: any) => item.code === 'N_PROGRAM');
          if (!targetIsInProgram && shouldRequest) {
            targetIsInProgram = Boolean(await commonApi.getProjectsInProgram(value));
          }
          const issueTypes: IIssueType[] = await issueTypeApi.loadAllWithStateMachineId(targetIsProgram ? 'program' : 'agile', value);
          const excludeTypeCode: string[] = [];
          if (!targetIsProgram) {
            excludeTypeCode.push('feature');
            if (targetIsInProgram) {
              excludeTypeCode.push('issue_epic');
            }
          }
          await issueTypeOptionsDs.loadData(issueTypes.filter((item) => item.enabled && !includes(excludeTypeCode, item.typeCode)));
        } else {
          issueTypeOptionsDs.loadData([]);
        }
      },
    },
  }), [issueTypeDs, issueTypeOptionsDs]);
  projectDsRef.current = projectDs;

  const submit = async () => {
    const projectDsValidate = await projectDs.validate();
    const typeDsValidate = await issueTypeDs.validate();
    const statusDsArr: DataSet[] = [];
    issueTypeDs.records.forEach((typeDd) => {
      statusDsArr.push(typeDd.getState('statusDs'));
    });
    const statusDsValidates = await Promise.all(statusDsArr.map((ds) => ds.validate()));
    if (projectDsValidate && typeDsValidate && statusDsValidates.every((validate) => !!validate)) {
      const issueIds = selected;
      const issueTypeStatusMap = {};
      issueTypeDs.forEach((issueTypeRd) => {
        const statusDs = issueTypeRd.getState('statusDs');
        const statusMap = {};
        statusDs.forEach((statusRd: Record) => {
          Object.assign(statusMap, {
            [statusRd.get('originStatusId')]: statusRd.get('statusId'),
          });
        });
        Object.assign(issueTypeStatusMap, {
          [issueTypeRd.get('originIssueTypeId')]: {
            [issueTypeRd.get('issueTypeId')]: statusMap,
          },
        });
      });
      const data = { issueIds, issueTypeStatusMap };
      await issueApi.batchMove(projectDs.current?.get('targetProjectId'), data);
      setLoading(true);
    }
  };

  const handleMessage = (message: any) => {
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
            onMove();
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
    <Spin spinning={fetching}>
      <div className={styles.tip}>
        <p className={styles.tipText}>
          由于目标项目与源项目的字段设置不同，在不同项目之间移动工作项时，您会丢失项目自定义字段、模块、标签、所属史诗（特性）、版本、tag数据信息，如果目标项目缺少源项目的人员，那么您的人员分配信息也将丢失。即使您移回源项目，也无法恢复这些数据。
        </p>
      </div>
      <Form
        className={styles.form}
        disabled={Boolean(loading) || fetching}
        style={{
          maxHeight: 400, overflowY: 'auto', overflowX: 'hidden',
        }}
      >
        <SelectProject
          dataSet={projectDs}
          name="targetProjectId"
          category="N_AGILE"
          style={{ marginBottom: 8 }}
        />
        {
          issueTypeDs.records.map((record) => (
            <Row key={record.get('originIssueTypeId')} gutter={14} type="flex" align="top">
              <Col span={6}>
                <SelectIssueType name="originIssueTypeId" record={record} />
              </Col>
              <Col span={6}>
                <Select
                  name="issueTypeId"
                  record={record}
                  optionsFilter={(rd) => {
                    // @ts-ignore
                    const originType = issueTypesList.find((item: IIssueType) => item.id === record.get('originIssueTypeId'));
                    if (originType) {
                      if (originType.typeCode === 'sub_task') {
                        return rd.get('typeCode') === 'sub_task';
                      }
                      return rd.get('typeCode') === 'story' || rd.get('typeCode') === 'task' || rd.get('typeCode') === 'bug';
                    }
                    return true;
                  }}
                />
              </Col>
              <Col span={12}>
                {
                  record.getState('statusDs')?.records.map((statusRecord: Record) => (
                    <Row key={statusRecord.get('originStatusId')} gutter={14} style={{ marginBottom: 8 }}>
                      <Col span={12}><SelectStatus name="originStatusId" record={statusRecord} issueTypeId={record.get('originIssueTypeId')} /></Col>
                      <Col span={12}>
                        <SelectStatus name="statusId" record={statusRecord} issueTypeId={record.get('issueTypeId')} projectId={projectDs?.current?.get('targetProjectId')} key={`${projectDs?.current?.get('targetProjectId')}-${record.get('issueTypeId')}`} />
                      </Col>
                    </Row>
                  ))
                }
              </Col>
            </Row>
          ))
        }
      </Form>
      {loading && (
        <div style={{ textAlign: 'center' }}>
          {loading === 'success' ? '批量移动成功' : ['正在移动，请稍等片刻', <span className={styles.dot}>…</span>]}
          <Progress strokeColor={STATUS_COLOR.done} value={Math.round(progress * 100)} />
        </div>
      )}
      <div style={{ display: 'flex', justifyContent: 'flex-end', paddingTop: 15 }}>
        <Button
          onClick={onCancel}
          disabled={loading as boolean || fetching}
          style={{
            fontWeight: 500,
          }}
        >
          取消
        </Button>
        <Button
          // disabled={getIsDisabled()}
          loading={loading as boolean || fetching}
          color={'primary' as ButtonColor}
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
    </Spin>
  );

  return (
    <>
      {
        issueSearchStore.batchAction === 'move' && (
          <div style={{ padding: 15 }}>
            <WSProvider server={Choerodon.WEBSOCKET_SERVER}>
              <WSHandler
                messageKey={`agile-issue-project-batch-move-${getProjectId()}`}
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
};
export default observer(BatchContent);
