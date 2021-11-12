import React, { useCallback } from 'react';
import { observer } from 'mobx-react-lite';
import { Table, DataSet } from 'choerodon-ui/pro';
import { get } from 'lodash';
import { ColumnPropsInner } from 'choerodon-ui/pro/lib/table/Column';
import {
  TableColumnTooltip, TableQueryBarType, TableMode, SelectionMode,
} from 'choerodon-ui/pro/lib/table/enum';
import { useIssueStore } from '../../stores';
import columnMap from '../columnMap';
import DetailContainer, { useDetail } from '@/components/detail-container';
import styles from './index.less';

const { Column } = Table;

interface Props {
  dataSet: DataSet
}
const SimpleIssueTable: React.FC<Props> = ({ dataSet }) => {
  const [issueDetailProps] = useDetail();
  const {
    onCloseDetail, mode,
  } = useIssueStore();
  const detailCallback = useCallback((expandRecordId) => {
    onCloseDetail(expandRecordId);
  }, [onCloseDetail]);
  const onSummaryClick = useCallback((record) => {
    issueDetailProps?.open({
      path: 'issue',
      props: {
        issueId: record.get('issueId'),
        projectId: record.get('projectId'),
        applyType: 'agile',
      },
      events: {
        delete: () => detailCallback(record.get('projectId')),
        close: () => detailCallback(record.get('projectId')),
      },
    });
  }, [detailCallback, issueDetailProps]);
  return (
    <>
      <Table
        dataSet={dataSet}
        queryBar={'none' as TableQueryBarType}
        className={styles.workingHoursIssueTable}
        mode={'tree' as TableMode}
        rowHeight={35}
        selectionMode={'none' as SelectionMode}
      >
        <Column
          name="summary"
          {...columnMap.get('summary')}
          // @ts-ignore
          renderer={({ record }) => columnMap.get('summary')?.renderer({ record }, onSummaryClick)}
        />
        <Column name="issueNum" {...columnMap.get('issueNum') as ColumnPropsInner} minWidth={150} tooltip={'overflow' as TableColumnTooltip} />
        <Column name="statusId" {...columnMap.get('statusId') as ColumnPropsInner} />
        {
          mode === 'project' && (
            <Column name="assigneeId" {...columnMap.get('assigneeId') as ColumnPropsInner} />
          )
        }
        {
          mode === 'assignee' && (
            <Column name="projectId" {...columnMap.get('projectId') as ColumnPropsInner} />
          )
        }
        <Column name="workTime" {...columnMap.get('workTime') as ColumnPropsInner} />
        <Column name="historyWorkTime" {...columnMap.get('historyWorkTime') as ColumnPropsInner} />
        <Column name="estimatedWorkTime" {...columnMap.get('estimatedWorkTime') as ColumnPropsInner} />
        <Column name="rate" {...columnMap.get('rate') as ColumnPropsInner} />
      </Table>
      <DetailContainer {...issueDetailProps} />
    </>

  );
};

export default observer(SimpleIssueTable);
