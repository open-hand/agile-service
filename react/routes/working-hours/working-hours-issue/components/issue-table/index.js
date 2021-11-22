import React, { useMemo, useCallback } from 'react';
import { Table } from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import { usePersistFn } from 'ahooks';
import { get } from 'lodash';
import { useUpdateColumnMutation } from '@/hooks/data/useTableColumns';
import styles from './index.less';
import { getTableColumns } from '@/components/issue-table/columns';
import getListLayoutColumns from '@/routes/Issue/components/issue-table/utils/getListLayoutColumns';
import DetailContainer, { useDetail } from '@/components/detail-container';
import { useIssueStore } from '../../stores';

const WorkingHoursIssueTable = ({
  dataSet, defaultListLayoutColumns, handleColumnResize, ...otherProps
}) => {
  const [issueDetailProps] = useDetail();
  const {
    tableFields: fields, onCloseDetail, mode, onCloseDetailTableQuery,
  } = useIssueStore();
  const detailCallback = useCallback((expandRecordId) => {
    if (mode === 'issue') {
      onCloseDetailTableQuery(dataSet);
    } else {
      onCloseDetail(expandRecordId);
    }
  }, [dataSet, mode, onCloseDetail, onCloseDetailTableQuery]);
  const onSummaryClick = useCallback((record) => {
    issueDetailProps?.open({
      path: 'issue',
      props: {
        issueId: get(record, 'issueId'),
        projectId: get(record, 'projectId'),
        applyType: 'agile',
      },
      events: {
        delete: () => detailCallback(get(record, 'assigneeId')),
        close: () => detailCallback(get(record, 'assigneeId')),
        update: () => {},
      },
    });
  }, [detailCallback, issueDetailProps]);
  const visibleColumns = useMemo(() => defaultListLayoutColumns.filter((item) => item.display), [defaultListLayoutColumns]);
  const tableColumns = useMemo(() => getTableColumns({
    listLayoutColumns: visibleColumns, fields, onSummaryClick,
  }), [visibleColumns, fields, onSummaryClick]);

  return (
    <div {...otherProps}>
      <Table
        queryBar="none"
        autoHeight={{ type: 'maxHeight', diff: 70 }}
        className={styles.workingHoursIssueTable}
        dataSet={dataSet}
        columns={[...tableColumns].map((column, i) => ({
          ...column,
          renderer: column.render && (({ record }) => column.render({ rowData: record.data })),
          lock: column.fixed,
          name: column.dataIndex,
        }))}
        columnResizable
        onColumnResize={handleColumnResize}
        mode="tree"
        rowHeight={mode === 'issue' ? 29 : 35}
        selectionMode="none"
        pagination={!(dataSet.totalCount < 10)}
      />
      <DetailContainer {...issueDetailProps} />
    </div>
  );
};

const ObserverWorkingHoursIssueTable = observer(WorkingHoursIssueTable);

const NewComponent = observer(({
  dataSet, defaultListLayoutColumns, ...otherProps
}) => {
  const { tableFields: fields } = useIssueStore();

  const mutation = useUpdateColumnMutation('workingHoursIssue');

  const listLayoutColumns = useMemo(() => getListLayoutColumns(defaultListLayoutColumns, fields), [defaultListLayoutColumns, fields]);
  const handleColumnResize = usePersistFn(({ column, width }) => {
    mutation.mutate({
      applyType: 'workingHoursIssue',
      listLayoutColumnRelVOS: listLayoutColumns.map((listLayoutColumn, i) => ({
        ...listLayoutColumn,
        ...listLayoutColumn.columnCode === column.code ? {
          width,
        } : {
          width: listLayoutColumn.width ?? 0,
        },
        sort: i,
      })),
    });
  });
  return (
    <>
      <ObserverWorkingHoursIssueTable
        dataSet={dataSet}
        defaultListLayoutColumns={listLayoutColumns}
        handleColumnResize={handleColumnResize}
        {...otherProps}
      />
    </>
  );
}, []);

export default NewComponent;
