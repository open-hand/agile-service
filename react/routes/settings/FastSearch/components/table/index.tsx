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
import TableDropMenu from '@/components/table-drop-menu';
import { transformOperation } from './utils';
import styles from './index.less';
import useFormatMessage from '@/hooks/useFormatMessage';

const { Column } = Table;
interface FastSearchTableProps {
  onEditClick: (record: Record) => void
  onMenuClick: (key: string, record: Record) => void
}
const FastSearchTable: React.ForwardRefRenderFunction<any, FastSearchTableProps> = ({ onEditClick, onMenuClick }, ref) => {
  const formatMessage = useFormatMessage();
  const dataSet = useMemo(() => new DataSet({
    autoQuery: true,
    selection: false,
    transport: {
      read: ({ params, data, ...a }) => quickFilterApiConfig.loadList({ contents: [data.params ?? ''], filterName: data.name ?? '' }, params.page, params.size),
    },
    fields: [{
      name: 'name',
      label: formatMessage({ id: 'agile.common.name' }),
    }, {
      name: 'expressQueryText',
      label: formatMessage({ id: 'agile.setting.filter.column' }),
    }, {
      name: 'descriptionText',
      label: formatMessage({ id: 'agile.common.description' }),
    }],
    queryFields: [{
      name: 'name',
      label: formatMessage({ id: 'agile.common.name' }),
    }],
  }), [formatMessage]);
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
          <TableDropMenu
            onMenuClick={({ key }: { key: string }) => record && onMenuClick(key, record)}
            menuData={[
              { action: () => record && onEditClick(record), text: '编辑' },
              {
                key: 'delete',
                text: '删除',
              }]}
            text={text}
          />
        )}
      />
      <Column
        tooltip={'overflow' as TableColumnTooltip}
        className="c7n-agile-table-cell"
        name="expressQueryText"
        renderer={({ record }) => record?.get('expressQuery') && transformOperation(record?.get('expressQuery'))}
      />
      <Column
        tooltip={'overflow' as TableColumnTooltip}
        className="c7n-agile-table-cell"
        name="descriptionText"
        renderer={({ record }) => record?.get('description') && record?.get('description').split('+++')[0]}
      />
    </Table>
  );
};

export default forwardRef(FastSearchTable);
