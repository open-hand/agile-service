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
        update: () => {},
      },
    });
  }, [detailCallback, issueDetailProps]);
  return (
    <div className={styles.simpleTable}>
      <Table
        dataSet={dataSet}
        queryBar={'none' as TableQueryBarType}
        className={styles.workingHoursIssueTable}
        mode={'tree' as TableMode}
        rowHeight={35}
        selectionMode={'none' as SelectionMode}
        pagination={!(dataSet.totalCount < 10) as any}
      >
        <Column
          name="summary"
          {...columnMap.get('summary')}
          // @ts-ignore
          renderer={({ record }) => columnMap.get('summary')?.renderer({ record }, onSummaryClick)}
          minWidth={420}
        />
        <Column name="issueNum" {...columnMap.get('issueNum') as ColumnPropsInner} width={110} tooltip={'overflow' as TableColumnTooltip} />
        <Column name="statusId" {...columnMap.get('statusId') as ColumnPropsInner} width={120} />
        {
          mode === 'project' && (
            <Column name="assigneeId" {...columnMap.get('assigneeId') as ColumnPropsInner} width={180} />
          )
        }
        {
          mode === 'assignee' && (
            <Column name="projectId" {...columnMap.get('projectId') as ColumnPropsInner} width={210} />
          )
        }
        <Column name="workTime" {...columnMap.get('workTime') as ColumnPropsInner} width={90} />
        <Column name="cumulativeWorkTime" {...columnMap.get('cumulativeWorkTime') as ColumnPropsInner} width={130} />
        <Column name="estimateTime" {...columnMap.get('estimateTime') as ColumnPropsInner} width={130} />
        <Column name="deviationRate" {...columnMap.get('deviationRate') as ColumnPropsInner} width={110} />
      </Table>
      <DetailContainer {...issueDetailProps} />
    </div>

  );
};

export default observer(SimpleIssueTable);
