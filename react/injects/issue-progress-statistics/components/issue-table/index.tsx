import React, { useMemo } from 'react';
import IssueSearch from '@/components/issue-search';
import IssueTable, { IssueTableProps } from '@/components/issue-table';

import { useIssueProgressStatisticsContext } from '../../stores';
import { getTableColumns } from './utils';
import './index.less';

const IssueProgressStatisticsTable: React.FC = () => {
  const prefixCls = 'c7n-agile-issue-progress-statistics-table';
  const {
    issueSearchStore, tableProps, tableFields, listLayoutColumns,
  } = useIssueProgressStatisticsContext();
  return (
    <div className={prefixCls}>
      <IssueSearch store={issueSearchStore} onClear={() => { }} onChange={() => { }} />
      <div>
        <IssueTable
          fields={tableFields}
          getTableColumns={getTableColumns}
          listLayoutColumns={listLayoutColumns}
          onSummaryClick={() => { }}
          tableProps={tableProps}
          isShowColumnCheck={false}
          createIssue={false}
          headerHeight={45}
        />
      </div>
    </div>
  );
};
export default IssueProgressStatisticsTable;
