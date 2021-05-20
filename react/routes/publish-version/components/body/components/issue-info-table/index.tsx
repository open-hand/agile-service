import React, { useEffect, useState } from 'react';
import {
  Table,
} from 'choerodon-ui/pro';
import UserTag from '@/components/tag/user-tag';
import renderStatus from '@/components/column-renderer/status';
import renderPriority from '@/components/column-renderer/priority';
import { usePublishVersionContext } from '@/routes/publish-version/stores';
import IssueSearch, { useIssueSearchStore } from '@/components/issue-search';
import classnames from 'classnames';
import { getSystemFields } from '@/stores/project/issue/IssueStore';
import { transformFilter } from '@/routes/Issue/stores/utils';

import { publishVersionApi } from '@/api';
import styles from './index.less';
import IssueTypeSwitch from '../switch';

interface TagActionHistory {
  action?: 'add'
  createdBy?: string
  creationDate?: string
  id?: string
  lastUpdateDate?: string
  lastUpdatedBy?: string
  objectVersionNumber: number
  organizationId?: string
  projectId?: string
  publishVersionId?: string
  status?: 'done' | 'doing' | 'failed'
}
const STATUS_MAP_TEXT = {
  done: '完成',
  doing: '进行中',
  failed: '失败',

};
const { Column } = Table;
function IssueInfoTable() {
  const { issueInfoTableDataSet, store, preview } = usePublishVersionContext();
  const [history, setHistory] = useState<Required<TagActionHistory> | undefined>(undefined);
  const issueSearchStore = useIssueSearchStore({
    getSystemFields: () => getSystemFields().filter((i) => i.code !== 'issueTypeId') as any,
    transformFilter,
  });
  useEffect(() => {
    publishVersionApi.loadLatestTagHistory(store.getCurrentData.id).then((res:any) => {
      if (res?.id) {
        setHistory(res);
      }
    });
  }, [store.getCurrentData.id]);
  return (
    <div className={classnames(styles.info, { [styles.preview]: preview })}>
      {
        history && (
        <div className={styles.history}>
          <span className={styles.history_title}>历史记录</span>

          <div className={styles.history_item}>{`状态：${STATUS_MAP_TEXT[history.status]}`}</div>
          <div className={styles.history_item}>{`最近处理时间：${history.lastUpdateDate}`}</div>

        </div>
        )
      }
      <IssueTypeSwitch />
      <div className={styles.body}>
        <IssueSearch
          store={issueSearchStore}
          onClear={() => { }}
          onChange={() => {
            const search = issueSearchStore.getCustomFieldFilters();
            issueInfoTableDataSet.setQueryParameter('search', search);
            issueInfoTableDataSet.query();
          }}
        />
        <Table dataSet={issueInfoTableDataSet} className={styles.table}>
          <Column
            name="summary"
            className={classnames({ 'c7n-agile-table-cell-click': !preview, 'c7n-agile-table-cell': preview })}
            tooltip={'always' as any}
            onCell={({ record }) => ({
              onClick: () => {
                !preview && store.selectIssue(record.get('issueId'));
              },
            })}
            lock={'left' as any}
            width={210}
          />
          <Column name="issueNum" width={120} tooltip={'overflow' as any} className="c7n-agile-table-cell" />
          <Column name="status" renderer={({ record }) => (record?.get('statusVO') ? renderStatus({ record }) : undefined)} />
          <Column name="priority" renderer={renderPriority} />
          <Column
            name="assigneeId"
            className="c7n-agile-table-cell"
            renderer={({ value, record }) => (value ? (
              <UserTag data={{
                imageUrl: record?.get('assigneeImageUrl'),
                loginName: record?.get('assigneeLoginName'),
                realName: record?.get('assigneeRealName'),
                tooltip: record?.get('assigneeName'),
              }}
              />
            ) : '')}
          />
          <Column
            name="tags"
            className="c7n-agile-table-cell"
            width={140}
            tooltip={'always' as any}
            renderer={({ value }) => value?.
              map((i: any) => (`${i.appServiceCode}:${i.tagName}`)).join('、')}
          />
          <Column name="creationDate" className="c7n-agile-table-cell" width={120} renderer={({ value }) => (value ? String(value).split(' ')[0] : '')} />
        </Table>
      </div>
    </div>
  );
}
export default IssueInfoTable;
