import React, {
  useCallback, useMemo, forwardRef,
  useImperativeHandle,
} from 'react';
import {
  Table, DataSet,
} from 'choerodon-ui/pro';
import { TableColumnTooltip } from 'choerodon-ui/pro/lib/table/enum';
import { TableProps } from 'choerodon-ui/pro/lib/table/interface';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import { quickFilterApi, quickFilterApiConfig } from '@/api';
import TableAction from '@/components/TableAction';
import { transformOperation } from './utils';
import styles from './index.less';

const { Column } = Table;
interface FastSearchTableProps {
  onEditClick: (record: Record) => void
  onMenuClick: (key: string, record: Record) => void
}
const FastSearchTable: React.ForwardRefRenderFunction<any, FastSearchTableProps> = ({ onEditClick, onMenuClick }, ref) => {
  const dataSet = useMemo(() => new DataSet({
    autoQuery: true,
    selection: false,
    transport: {
      read: ({ params, data, ...a }) => quickFilterApiConfig.loadList({ contents: [data.params ?? ''], filterName: data.name ?? '' }, params.page, params.size),
    },
    fields: [{
      name: 'name',
      label: '名称',
    }, {
      name: 'expressQuery',
      label: '筛选器',
    }, {
      name: 'description',
      label: '描述',
    }],
    queryFields: [{
      name: 'name',
      label: '名称',
    }],
  }), []);
  const refresh = useCallback(() => {
    dataSet.query(dataSet.currentPage);
  }, [dataSet]);
  const handleDragEnd: TableProps['onDragEnd'] = useCallback(async (_, columns, result) => {
    const { source, destination } = result;
    if (!destination) {
      return;
    }
    const destinationIndex = destination.index;
    const sourceIndex = source.index;
    if (sourceIndex === destinationIndex) {
      return;
    }
    const sourceItem = dataSet.data[destinationIndex];
    const beforeItem = dataSet.data[destinationIndex - 1];
    const afterItem = dataSet.data[destinationIndex + 1];
    await quickFilterApi.drag({
      afterSequence: afterItem?.get('sequence') ?? null,
      beforeSequence: beforeItem?.get('sequence') ?? null,
      filterId: sourceItem.get('filterId'),
      objectVersionNumber: sourceItem.get('objectVersionNumber'),
    });
    refresh();
  }, [dataSet.data, refresh]);

  useImperativeHandle(ref, () => ({
    refresh,
  }));
  return (
    <Table
      dataSet={dataSet}
      rowDraggable
      onDragEnd={handleDragEnd}
      onRow={() => ({
        className: styles.row,
      })}
    >
      <Column
        name="name"
        tooltip={'overflow' as TableColumnTooltip}
        renderer={({ record, text }) => (
          <TableAction
            onEditClick={() => record && onEditClick(record)}
            onMenuClick={({ key }: { key: string }) => record && onMenuClick(key, record)}
            menus={[{
              key: 'delete',
              text: '删除',
            }]}
            text={text}
          />
        )}
      />
      <Column tooltip={'overflow' as TableColumnTooltip} className="c7n-agile-table-cell" name="expressQuery" renderer={({ text }) => text && transformOperation(text)} />
      <Column tooltip={'overflow' as TableColumnTooltip} className="c7n-agile-table-cell" name="description" renderer={({ text }) => text && text.split('+++')[0]} />
    </Table>
  );
};

export default forwardRef(FastSearchTable);
