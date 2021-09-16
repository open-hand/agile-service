import React, { useCallback, useMemo, useRef } from 'react';
import { observer } from 'mobx-react-lite';
import {
  Table, Modal, DataSet, Form, TextField, Select,
} from 'choerodon-ui/pro';
import { Icon, Tooltip } from 'choerodon-ui';
import { TableColumnTooltip } from 'choerodon-ui/pro/lib/table/enum';
import MODAL_WIDTH from '@/constants/MODAL_WIDTH';
import { statusTransformApiConfig } from '@/api';
import { LINK_URL_TO } from '@/constants/LINK_URL';
import styles from './LogModal.less';

const { Column } = Table;
const { Option } = Select;
interface Props {

}

const StatusMap = new Map([
  ['success', {
    color: '#00BFA5',
    text: '已执行',
  }],
  [
    'stop', {
      color: '#F76776',
      text: '终止',
    },
  ],
  [
    'loop', {
      color: '#F76776',
      text: '循环终止',
    },
  ],
  [
    'error', {
      color: '#F76776',
      text: '失败',
    },
  ],
  ['max_depth', {
    color: '#F76776',
    text: '链路超长',
  }],
]);
const RemarkMap = new Map([
  ['same_status', '联动的问题已处在目标状态'],
  ['sub_bug', '关联的缺陷属于子缺陷'],
  ['condition_limit', '受流转条件限制无法流转到目标状态'],
  ['max_depth', '触发状态联动的链长度将超过最大长度 10 后，系统自动停止执行'],
]);

const LogTable: React.FC<Props> = () => {
  const dataSetRef = useRef<DataSet>();
  const queryDataSet = useMemo(() => new DataSet({
    fields: [
      {
        name: 'params',
        label: '请输入搜索内容',
      },
      {
        name: 'statusCode',
        label: '状态',
        textField: 'label',
        valueField: 'value',
      },
    ],
    events: {
      update() {
        dataSetRef.current?.query();
      },
    },
  }), []);
  const logTableDs = useMemo(() => new DataSet({
    autoQuery: true,
    selection: false,
    transport: {
      read: ({ params, data }) => statusTransformApiConfig.getLogList(params, data),
    },
    fields: [{
      name: 'creationDate',
      label: '时间',
    }, {
      name: 'content',
      label: '状态联动',
    }, {
      name: 'statusCode',
      label: '状态',
    }, {
      name: 'preIssueId',
      label: '源问题',
    }, {
      name: 'curIssueId',
      label: '联动的问题',
    }, {
      name: 'remark',
      label: '备注',
    }],
    queryDataSet,
  }), [queryDataSet]);

  dataSetRef.current = logTableDs;

  const renderStatus = useCallback(({ value }) => (
    <span className={styles.status} style={{ background: StatusMap.get(value)?.color }}>
      {StatusMap.get(value)?.text}
    </span>
  ), []);

  const handleClickIssue = useCallback((issueId: string, issueNum: string) => {
    LINK_URL_TO.issueLinkTo(issueId, issueNum);
  }, []);

  const renderIssue = useCallback(({ record }, issueType: 'pre' | 'cur') => {
    const issue = issueType === 'pre' ? record.get('preIssueInfo') : record.get('curIssueInfo');
    return (
      <Tooltip title={`${issue.issueNum} ${issue.summary}`}>
        <span className={styles.issue} role="none" onClick={() => { handleClickIssue(issue.issueId, issue.issueNum); }}>
          {`${issue.issueNum} ${issue.summary}`}
        </span>
      </Tooltip>
    );
  }, [handleClickIssue]);

  const renderRemark = useCallback(({ value }) => RemarkMap.get(value), []);

  return (
    <div className={styles.logTable}>
      <Table
        dataSet={logTableDs}
        queryBar={() => (
          <Form dataSet={queryDataSet} columns={14} labelLayout={'none' as any} style={{ marginLeft: -5 }}>
            <TextField name="params" prefix={<Icon type="search" />} colSpan={3} valueChangeAction={'input' as any} placeholder="请输入搜索内容" />
            <Select name="statusCode" colSpan={2} placeholder="状态">
              <Option value="success">已执行</Option>
              <Option value="error">失败</Option>
              <Option value="stop">终止</Option>
              <Option value="max_depth">过长终止</Option>
              <Option value="loop">循环终止</Option>
            </Select>
          </Form>
        )}
      >
        <Column name="creationDate" lock width={150} tooltip={'overflow' as TableColumnTooltip} />
        <Column name="content" lock width={480} tooltip={'overflow' as TableColumnTooltip} />
        <Column name="statusCode" width={100} renderer={renderStatus} tooltip={'overflow' as TableColumnTooltip} />
        <Column
          name="preIssueId"
          // @ts-ignore
          renderer={({ record }) => renderIssue({ record }, 'pre')}
          width={200}
        />
        <Column
          name="curIssueId"
          // @ts-ignore
          renderer={({ record }) => renderIssue({ record }, 'cur')}
          width={200}
        />
        <Column
          name="remark"
          // @ts-ignore
          renderer={renderRemark}
          width={200}
        />
      </Table>
    </div>
  );
};

const ObserverLogModal = observer(LogTable);

const openLogModal = (props: Props) => {
  Modal.open({
    key: Modal.key(),
    title: '查看执行日志',
    style: {
      width: MODAL_WIDTH.large,
    },
    drawer: true,
    className: styles.logModal,
    children: <ObserverLogModal {...props} />,
    border: false,
  });
};

export default openLogModal;
