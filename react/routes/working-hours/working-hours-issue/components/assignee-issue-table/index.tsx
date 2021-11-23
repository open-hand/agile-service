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
  // eslint-disable-next-line react/require-default-props
  expandRecordId?: string
}
const AssigneeIssueTable: React.FC<Props> = ({
  projectId, defaultListLayoutColumns, dataSet, expandRecordId,
}) => {
  const {
    isProject, mode, startTime, endTime, isContain, issueSearchStore, tableFieldsFetched, addCustomFieldToDs,
  } = useIssueStore();

  return ((isProject && tableFieldsFetched) || !isProject) ? (
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
            const newDs = new DataSet(!isProject ? SimpleIssueDataSet({ issueSearchStore, organizationId: getOrganizationId() }) : WorkingHoursIssuesDataSet({ projectId: getProjectId(), organizationId: getOrganizationId(), issueSearchStore }));
            recordIssueDs = newDs;
            if (isProject) {
              addCustomFieldToDs(recordIssueDs);
            }
            record.setState('recordDs', recordIssueDs);
            recordIssueDs.setQueryParameter('startTime', startTime);
            recordIssueDs.setQueryParameter('endTime', endTime);
            recordIssueDs.setQueryParameter('containsSubIssue', isContain);
            recordIssueDs.setQueryParameter('assigneeId', [record.get('userId')]);
            if (mode === 'projectAssignee') {
              recordIssueDs.setQueryParameter('projectIds', [projectId]);
            }
            recordIssueDs.query();
          }
          return (
            !isProject ? (
              <SimpleIssueTable dataSet={recordIssueDs} key={`${projectId || getProjectId()}-${record.get('userId')}`} expandRecordId={expandRecordId || record.get('userId')} />
            ) : (
              <IssueTable
                // @ts-ignore
                dataSet={recordIssueDs}
                defaultListLayoutColumns={defaultListLayoutColumns}
                className={styles.assigneeModeIssueTable}
                key={`${projectId || getProjectId()}-${record.get('userId')}`}
                expandRecordId={record.get('userId')}
              />
            )
          );
        }}
        pagination={!(dataSet.totalCount < 10) as any}
      >
        <Column name="userId" {...columnMap.get('userId') as ColumnPropsInner} width={400} />
        <Column name="workTime" {...columnMap.get('workTime') as ColumnPropsInner} />
        <Column name="cumulativeWorkTime" {...columnMap.get('cumulativeWorkTime') as ColumnPropsInner} />
        <Column name="estimateTime" {...columnMap.get('estimateTime') as ColumnPropsInner} />
        <Column name="deviationRate" {...columnMap.get('deviationRate') as ColumnPropsInner} />
      </Table>
    </div>
  ) : null;
};

export default observer(AssigneeIssueTable);
