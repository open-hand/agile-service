import React, { useCallback } from 'react';
import { observer } from 'mobx-react-lite';
import {
  ColumnAlign, TableColumnTooltip, TableQueryBarType,
} from 'choerodon-ui/pro/lib/table/enum';
import { Table } from 'choerodon-ui/pro';
import { Tooltip } from 'choerodon-ui';
import { getIsOrganization } from '@/utils/common';
import UserTag from '@/components/tag/user-tag';
import StatusTag from '@/components/StatusTag';
import DetailContainer, { useDetail } from '@/components/detail-container';
import styles from './LogTable.less';
import { TypeTag } from '@/components';
import { useLogStore } from '../../stores';

const { Column } = Table;

const LogTable = () => {
  const [issueDetailProps] = useDetail();
  const { logDs, loadData } = useLogStore();
  const renderMember = useCallback(({ record }) => {
    const user = record?.get('user');
    const showText = user?.ldap ? `${user?.realName}(${user?.loginName})` : `${user?.realName}(${user?.email})`;
    return (
      <UserTag
        data={{
          ...user || {},
          textShow: showText,
        }}
      />
    );
  }, []);

  const renderWorkTime = useCallback(({ value }) => (value && <span>{`${value}h`}</span>), []);

  const openIssueDetail = useCallback((e, item) => {
    e.stopPropagation();
    issueDetailProps?.open({
      path: 'issue',
      props: {
        issueId: item.issueId,
        projectId: item.projectId,
        applyType: 'agile',
      },
      events: {
        update: () => {
          loadData();
        },
        delete: () => {
          loadData();
        },
        close: () => {
          loadData();
        },
      },
    });
  }, [issueDetailProps, loadData]);

  const renderIssue = useCallback(({ record }) => (
    <Tooltip title={`${record.get('issueNum')} ${record?.get('summary')}`}>
      <div className={styles.issue}>
        <TypeTag data={record?.get('issueTypeVO')} />
        <span
          role="none"
          onClick={() => {}}
          style={{ overflow: 'hidden', textOverflow: 'ellipsis' }}
        >
          {`${record.get('issueNum')}`}
          <span
            className={styles.summary}
            role="none"
            onClick={(e) => openIssueDetail(e, {
              issueId: record.get('issueId'),
              projectId: record.get('projectId'),
            })}
          >
            {record?.get('summary')}
          </span>
        </span>
      </div>
    </Tooltip>
  ), [openIssueDetail]);

  const renderProject = useCallback(({ record }) => record.get('projectVO')?.name, []);

  const renderStatus = useCallback(({ record }) => (
    <Tooltip title={record.get('statusVO')?.name}>
      <div style={{
        display: 'inline-flex',
        overflow: 'hidden',
        maxWidth: 100,
      }}
      >
        <StatusTag
          data={record.get('statusVO')}
          style={{ display: 'inline-block' }}
        />
      </div>
    </Tooltip>
  ), []);

  return (
    <div>
      <Table dataSet={logDs} queryBar={'none' as TableQueryBarType}>
        <Column name="userId" sortable renderer={renderMember} width={150} />
        <Column name="workTime" sortable renderer={renderWorkTime} align={'left' as ColumnAlign} width={110} />
        <Column name="startDate" sortable tooltip={'overflow' as TableColumnTooltip} width={170} />
        <Column name="issueId" sortable renderer={renderIssue} />
        {
          getIsOrganization() && (
            <Column name="projectId" sortable tooltip={'overflow' as TableColumnTooltip} renderer={renderProject} width={170} />
          )
        }
        <Column name="statusId" sortable renderer={renderStatus} width={120} />
      </Table>
      <DetailContainer {...issueDetailProps} />
    </div>
  );
};

export default observer(LogTable);
