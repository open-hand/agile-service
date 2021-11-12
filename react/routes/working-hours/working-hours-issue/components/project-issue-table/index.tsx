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

const { Column } = Table;
interface Props {
}
const ProjectIssueTable: React.FC<Props> = () => {
  const {
    workingHoursProjectDs,
  } = useIssueStore();

  return (
    <div className={styles.assigneeTable}>
      <Table
        dataSet={workingHoursProjectDs}
        queryBar={'none' as TableQueryBarType}
        rowHeight={29}
        selectionMode={'none' as SelectionMode}
        expandIconAsCell={false}
        expandedRowRenderer={({ record }) => {
          let recordIssueDs = record.getState('issueDs');
          if (!recordIssueDs) {
            const searchData = {
              projectId: getProjectId() as string, // 可去掉
              organizationId: getOrganizationId() as string,
            };
            // @ts-ignore
            const newDs = new DataSet(SimpleIssueDataSet(searchData));
            recordIssueDs = newDs;
            record.setState('issueDs', recordIssueDs);
            recordIssueDs.query();
          }
          return (
            <SimpleIssueTable dataSet={recordIssueDs} key={record.get('projectId')} />
          );
        }}
      >
        <Column name="projectId" {...columnMap.get('projectId') as ColumnPropsInner} width={400} />
        <Column name="workTime" {...columnMap.get('workTime') as ColumnPropsInner} />
        <Column name="historyWorkTime" {...columnMap.get('historyWorkTime') as ColumnPropsInner} />
        <Column name="estimatedWorkTime" {...columnMap.get('estimatedWorkTime') as ColumnPropsInner} />
        <Column name="rate" {...columnMap.get('rate') as ColumnPropsInner} />
      </Table>
    </div>
  );
};

export default observer(ProjectIssueTable);
