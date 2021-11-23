import React from 'react';
import { observer } from 'mobx-react-lite';
import { Table, DataSet } from 'choerodon-ui/pro';
import { TableQueryBarType, SelectionMode } from 'choerodon-ui/pro/lib/table/enum';
import { ColumnPropsInner } from 'choerodon-ui/pro/lib/table/Column';
import { useIssueStore } from '../../stores';
import columnMap from '../columnMap';
import styles from './index.less';
import { getProjectId, getOrganizationId } from '@/utils/common';
import SimpleIssueTable from '../simple-issue-table';
import SimpleIssueDataSet from '../../stores/SimpleIssueDataSet';
import AssigneeDataSet from '../../stores/AssigneeDataSet';
import AssigneeIssueTable from '../assignee-issue-table';

const { Column } = Table;
interface Props {
}
const ProjectIssueTable: React.FC<Props> = () => {
  const {
    workingHoursProjectDs, workingHoursProjectAssigneeDs, mode, startTime, endTime, isContain, issueSearchStore,
  } = useIssueStore();
  const dataSet = mode === 'project' ? workingHoursProjectDs : workingHoursProjectAssigneeDs;
  return (
    <div className={styles.assigneeTable}>
      <Table
        dataSet={dataSet}
        queryBar={'none' as TableQueryBarType}
        rowHeight={29}
        selectionMode={'none' as SelectionMode}
        expandIconAsCell={false}
        expandedRowRenderer={({ record }) => {
          let recordIssueDs = record.getState('recordDs');
          if (!recordIssueDs) {
            // @ts-ignore
            const newDs = mode === 'project' ? new DataSet(SimpleIssueDataSet({
              organizationId: getOrganizationId(),
              issueSearchStore,
            })) : new DataSet(AssigneeDataSet({ organizationId: getOrganizationId(), issueSearchStore }));
            recordIssueDs = newDs;
            record.setState('recordDs', recordIssueDs);
            recordIssueDs.setQueryParameter('startTime', startTime);
            recordIssueDs.setQueryParameter('endTime', endTime);
            recordIssueDs.setQueryParameter('projectIds', [record.get('projectId')]);
            if (mode === 'project') {
              recordIssueDs.setQueryParameter('containsSubIssue', isContain);
            }
            recordIssueDs.query();
          }
          return mode === 'project' ? (
            <SimpleIssueTable dataSet={recordIssueDs} key={record.get('projectId')} />
          ) : (
            <AssigneeIssueTable dataSet={recordIssueDs} projectId={record.get('projectId')} key={record.get('projectId')} />
          );
        }}
        pagination={!(dataSet.totalCount < 10) as any}
      >
        <Column name="projectId" {...columnMap.get('projectId') as ColumnPropsInner} width={400} />
        <Column name="workTime" {...columnMap.get('workTime') as ColumnPropsInner} />
        <Column name="cumulativeWorkTime" {...columnMap.get('cumulativeWorkTime') as ColumnPropsInner} />
        <Column name="estimateTime" {...columnMap.get('estimateTime') as ColumnPropsInner} />
        <Column name="deviationRate" {...columnMap.get('deviationRate') as ColumnPropsInner} />
      </Table>
    </div>
  );
};

export default observer(ProjectIssueTable);
