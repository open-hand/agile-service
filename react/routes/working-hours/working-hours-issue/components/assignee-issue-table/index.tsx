import React from 'react';
import { observer } from 'mobx-react-lite';
import { Table, DataSet } from 'choerodon-ui/pro';
import { TableQueryBarType, SelectionMode } from 'choerodon-ui/pro/lib/table/enum';
import { ColumnPropsInner } from 'choerodon-ui/pro/lib/table/Column';
import IssueTable from '../issue-table';
import { useIssueStore } from '../../stores';
import columnMap from '../columnMap';
import styles from './index.less';
import WorkingHoursIssuesDataSet from '../../stores/WorkingHoursIssuesDataSet';
import { getProjectId, getOrganizationId } from '@/utils/common';
import { ListLayoutColumnVO } from '@/api';
import SimpleIssueTable from '../simple-issue-table';
import SimpleIssueDataSet from '../../stores/SimpleIssueDataSet';

const { Column } = Table;
interface Props {
  // eslint-disable-next-line react/require-default-props
  projectId?: string
  // eslint-disable-next-line react/require-default-props
  defaultListLayoutColumns?: ListLayoutColumnVO[]
  dataSet: DataSet
}
const AssigneeIssueTable: React.FC<Props> = ({ projectId, defaultListLayoutColumns, dataSet }) => {
  const { isProject, mode } = useIssueStore();

  return (
    <div className={styles.assigneeTable}>
      <Table
        dataSet={dataSet}
        queryBar={'none' as TableQueryBarType}
        rowHeight={mode === 'assignee' ? 29 : 40}
        selectionMode={'none' as SelectionMode}
        expandIconAsCell={false}
        expandedRowRenderer={({ record }) => {
          let recordIssueDs = record.getState('recordDs');
          if (!recordIssueDs) {
            const searchData = {
              projectId: (projectId || getProjectId()) as string,
              organizationId: getOrganizationId() as string,
            };
            // @ts-ignore
            const newDs = new DataSet(!isProject ? SimpleIssueDataSet(searchData) : WorkingHoursIssuesDataSet(searchData));
            recordIssueDs = newDs;
            record.setState('recordDs', recordIssueDs);
            recordIssueDs.query();
          }
          return (
            !isProject ? (
              <SimpleIssueTable dataSet={recordIssueDs} key={`${projectId || getProjectId()}-${record.get('userId')}`} />
            ) : (
              <IssueTable
                // @ts-ignore
                dataSet={recordIssueDs}
                defaultListLayoutColumns={defaultListLayoutColumns}
                className={styles.assigneeModeIssueTable}
                key={`${projectId || getProjectId()}-${record.get('userId')}`}
              />
            )
          );
        }}
      >
        <Column name="userId" {...columnMap.get('userId') as ColumnPropsInner} />
        <Column name="workTime" {...columnMap.get('workTime') as ColumnPropsInner} />
        <Column name="historyWorkTime" {...columnMap.get('historyWorkTime') as ColumnPropsInner} />
        <Column name="estimatedWorkTime" {...columnMap.get('estimatedWorkTime') as ColumnPropsInner} />
        <Column name="rate" {...columnMap.get('rate') as ColumnPropsInner} />
      </Table>
    </div>
  );
};

export default observer(AssigneeIssueTable);
