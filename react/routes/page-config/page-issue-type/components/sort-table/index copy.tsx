/* eslint-disable no-param-reassign */
import React, {
  useMemo, ReactElement, useEffect, memo, useState, PropsWithChildren,
} from 'react';
import { observer, useObservable } from 'mobx-react-lite';
import {
  Table, DataSet, Icon, CheckBox, Spin, Output, Button,
} from 'choerodon-ui/pro/lib';
import { TableQueryBarType } from 'choerodon-ui/pro/lib/table/enum';
import { RenderProps } from 'choerodon-ui/pro/lib/field/FormField';
import { ColumnProps } from 'choerodon-ui/pro/lib/table/Column';
import {
  DropResult, ResponderProvided, DraggableProvided, DraggableStateSnapshot, DraggableRubric,
} from 'react-beautiful-dnd';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import { IFiledProps } from '@/api';
import OldSortTable from '../../../page/components/SortTable';
import './index.less';
import { usePageIssueTypeStore } from '../../stores';

const { Column } = Table;
interface Props {
  disabled: boolean | undefined,
  org?: number,
  dataStatus: { code: string },
  onDelete?: (data: IFiledProps) => void,
}
interface DragRenderCloneProps {
  provided: DraggableProvided, // DraggableProvided,
  snapshot: DraggableStateSnapshot, // DraggableStateSnapshot,
  rubric: DraggableRubric,
  key: any,
  record: Record,
  column: ColumnProps[],
}
const prefixCls = 'c7n-page-issue-detail';
const SortTable: React.FC<Props> = ({ disabled, dataStatus, onDelete }) => {
  const { sortTableDataSet } = usePageIssueTypeStore();
  // const [dataStatus, setDataStatus] = useState<string>();
  const renderFieldName = ({ value }: RenderProps) => (
    <div>
      {!disabled && <Icon type="baseline-drag_indicator" />}
      <span>{value}</span>
    </div>
  );

  function renderCheckBox({
    value, name, record, dataSet,
  }: RenderProps) {
    return (
      <CheckBox
        disabled={disabled}
        checked={value}
        // value={value}
        onChange={(val) => {
          record?.set(name as String, val);
          // console.log('dataSet?.dirty', dataSet?.dirty);
          if (dataStatus.code !== 'drag_update' && dataSet?.dirty) {
            // setDataStatus('update');
            dataStatus.code = 'update';
          } else if (dataStatus.code !== 'drag_update' && !dataSet?.dirty) {
            // setDataStatus('ready');
            dataStatus.code = 'ready';
          }
        }}
      />
    );
    // return <input type="checkbox" checked={value} />;
  }
  const getColumns = () => {
    console.log('co');
    return [
      {
        title: '字段名称',
        dataIndex: 'fieldName',
        width: '25%',
        render: (value: string) => (
          <div>
            {!disabled && <Icon type="baseline-drag_indicator" />}
            <span>{value}</span>
          </div>
        ),
      },
      {
        title: '默认值',
        dataIndex: 'defaultValue',
        width: '25%',
        // minWidth: '70px',
      },
      {
        title: '是否必填',
        dataIndex: 'require',
        width: '75px',
        render: (value: any) => (
          <CheckBox
            disabled={disabled}
            checked
            // value={value}
            onChange={(val) => {
              console.log('val', val);
              // record?.set(name as String, val);
            }}
          />
        ),
      },
      {
        title: '是否加入到编辑页',
        dataIndex: 'eidt',
        width: '135px',
        render: (value: any) => (
          <CheckBox
            disabled={disabled}
            checked
            value={value}
            onChange={(val) => {
              console.log('val', val);
              // record?.set(name as String, val);
            }}
          />
        ),
      },
      {
        title: '是否加入到创建页',
        dataIndex: 'create',
        width: '135px',
        render: (value: any) => (
          <CheckBox
            disabled={disabled}
            checked
            value={value}
            onChange={(val) => {
              console.log('val', val);
              // record?.set(name as String, val);
            }}
          />
        ),
      },
    ];
  };
  const renderAction = ({
    value, name, record, dataSet,
  }: RenderProps) => (
    <div>
      {renderCheckBox({
        value, name, record, dataSet,
      })}
      <Button
        disabled={disabled}
        style={{ marginLeft: 10 }}
        onClick={() => {
          onDelete && onDelete(record?.toData());
          // dataSet?.delete(record as Record);
        }}
      >
        <Icon type="delete" style={{ fontSize: 18 }} />
      </Button>
    </div>
  );
  const renderClone = ({
    provided,
    snapshot,
    key,
    record,
    column,
  }: DragRenderCloneProps) => (
    <tr
      key={key}
      {...provided.draggableProps}
      {...provided.dragHandleProps}
      style={provided.draggableProps.style}
      className={`${prefixCls}-drag`}
    >
      {column.map((item: any) => (
        <div className={`${prefixCls}-drag-item`}>
          {item.renderer
            ? item.renderer({ record, name: item.name, value: record.get(item.name) })
            : record.get(item.name)}
        </div>
      ))}

    </tr>
  );
  return (
    <div className={prefixCls}>
      {/* <OldSortTable
        pagination={false}
        columns={getColumns()}
        dataSource={sortTableDataSet.toData().slice()}
        filterBar={false}
        handleDrag={() => { }}
        hight={300}
      /> */}
      {/* <Table
        dataSet={sortTableDataSet}
        queryBar={'none' as TableQueryBarType}
        dragRow={!disabled}
        // @ts-ignore
        onDragEnd={(
          dataSet: DataSet, columns: ColumnProps[],
          resultDrag: DropResult, provided: ResponderProvided,
        ) => {
          if (dataStatus.code !== 'drag_update') {
            // setDataStatus('drag_update');
            dataStatus.code = 'drag_update';
          }
        }}
        rowDragRender={{
          // @ts-ignore
          renderClone,
        }}

        border={false}
      >
        <Column name="fieldName" renderer={renderFieldName} />
        <Column name="defaultValue" />
        <Column name="required" width={70} renderer={renderCheckBox} />
        <Column name="edited" width={130} renderer={renderCheckBox} />
        <Column name="created" width={130} renderer={renderAction} />
      </Table> */}
    </div>
  );
};
export default memo(observer(SortTable));
