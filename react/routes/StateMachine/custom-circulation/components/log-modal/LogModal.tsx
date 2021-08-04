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
      label: '源',
    }, {
      name: 'curIssueId',
      label: '影响',
    }],
    queryDataSet,
  }), [queryDataSet]);

  dataSetRef.current = logTableDs;

  const renderStatus = useCallback(({ value }) => (
    <span className={styles.status} style={{ background: value === 'SUCCESS' ? '#00BFA5' : '#F76776' }}>
      {value === 'SUCCESS' ? '已执行' : '循环终止'}
    </span>
  ), []);

  const handleClickIssue = useCallback((issueId: string, issueNum: string) => {
    LINK_URL_TO.issueLinkTo('206436126142103552', 'AG-11287');
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

  return (
    <div className={styles.logTable}>
      <Table
        dataSet={logTableDs}
        queryBar={() => (
          <Form dataSet={queryDataSet} columns={7}>
            <TextField name="params" prefix={<Icon type="search" />} colSpan={2} valueChangeAction={'input' as any} />
            <Select name="statusCode" colSpan={1}>
              <Option value="SUCCESS">已执行</Option>
              <Option value="LOOP">循环终止</Option>
            </Select>
          </Form>
        )}
      >
        <Column name="creationDate" width={160} tooltip={'overflow' as TableColumnTooltip} />
        <Column name="content" tooltip={'overflow' as TableColumnTooltip} />
        <Column name="statusCode" width={130} renderer={renderStatus} tooltip={'overflow' as TableColumnTooltip} />
        <Column
          name="preIssueId"
          // @ts-ignore
          renderer={({ record }) => renderIssue({ record }, 'pre')}
        />
        <Column
          name="curIssueId"
          // @ts-ignore
          renderer={({ record }) => renderIssue({ record }, 'cur')}
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
