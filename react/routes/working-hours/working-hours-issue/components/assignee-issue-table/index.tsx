import React from 'react';
import { observer } from 'mobx-react-lite';
import { Table, DataSet } from 'choerodon-ui/pro';
import { TableColumnTooltip, TableQueryBarType, SelectionMode } from 'choerodon-ui/pro/lib/table/enum';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import IssueTable from '../issue-table';
import { useIssueStore } from '../../stores';
import columnMap from '../columnMap';
import styles from './index.less';
import WorkingHoursIssuesDataSet from '../../stores/WorkingHoursIssuesDataSet';
import { getProjectId, getOrganizationId } from '@/utils/common';

const { Column } = Table;
interface Props {
  // eslint-disable-next-line react/require-default-props
  projectId?: string
}
const AssigneeIssueTable: React.FC<Props> = ({ projectId }) => {
  const {
    loading, workingHoursAssigneeDs, tableFields, setLoading,
  } = useIssueStore();
  const handleExpand = (expand: boolean, record: Record) => {
    console.log(expand, record.isExpanded);
  };
  return (
    <div className={styles.assigneeTable}>
      <Table
        dataSet={workingHoursAssigneeDs}
        queryBar={'none' as TableQueryBarType}
        rowHeight={29}
        selectionMode={'none' as SelectionMode}
        expandIconAsCell={false}
        expandedRowRenderer={({ record }) => {
          let recordIssueDs = record.getState('issueDs');
          if (!recordIssueDs) {
            // setLoading(true);
            // @ts-ignore
            const newDs = new DataSet(WorkingHoursIssuesDataSet({
              projectId: (projectId || getProjectId()) as string,
              organizationId: getOrganizationId() as string,
            }));
            recordIssueDs = newDs;
            record.setState('issueDs', recordIssueDs);
            recordIssueDs.query().then(() => {
              // setLoading(false);
            });
          }
          return (
            <IssueTable
              fields={tableFields}
              dataSet={recordIssueDs}
              className={styles.assigneeModeIssueTable}
              key={`${projectId || getProjectId()}-${record.get('userId')}`}
            />
          );
        }}
        onExpand={handleExpand}
      >
        <Column name="userId" {...columnMap.get('userId')} />
        <Column name="workTime" {...columnMap.get('workTime')} />
        <Column name="historyWorkTime" {...columnMap.get('historyWorkTime')} />
        <Column name="estimatedWorkTime" {...columnMap.get('estimatedWorkTime')} />
        <Column name="rate" {...columnMap.get('rate')} />
      </Table>
    </div>
  );
};

export default observer(AssigneeIssueTable);
