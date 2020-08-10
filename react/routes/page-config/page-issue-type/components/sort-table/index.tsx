import React, {
  useMemo, ReactElement, useEffect, memo, useState, PropsWithChildren,
} from 'react';
import { observer } from 'mobx-react-lite';
import {
  Table, DataSet, Icon, CheckBox, Spin, Output,
} from 'choerodon-ui/pro/lib';
import { TableQueryBarType } from 'choerodon-ui/pro/lib/table/enum';
import { RenderProps } from 'choerodon-ui/pro/lib/field/FormField';
import OldSortTable from '../../../page/components/SortTable';
import './index.less';
import { usePageIssueTypeStore } from '../../stores';

const { Column } = Table;
interface Props {
  disabled: boolean | undefined
}
const prefixCls = 'c7n-page-issue-detail';
const SortTable: React.FC<Props> = ({ disabled }) => {
  const { sortTableDataSet } = usePageIssueTypeStore();

  const renderFieldName = ({ value }: RenderProps) => (
    <div>
      {!disabled && <Icon type="baseline-drag_indicator" />}
      <span>{value}</span>
    </div>
  );

  function renderCheckBox({ value, name, record }: RenderProps) {
    return (
      <CheckBox
        disabled={disabled}
        checked={value}
        // value={value}
        onChange={(val) => {
          console.log('val', val, name);
          record?.set(name as String, val);
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
  return (
    <div className={prefixCls}>
      {/* <Spin dataSet={sortTableDataSet}>
        <OldSortTable
          pagination={false}
          columns={getColumns()}
          dataSource={sortTableDataSet.toData().slice()}
          filterBar={false}
          handleDrag={() => { }}
          hight={300}
        />
      </Spin> */}
      <Table
        dataSet={sortTableDataSet}
        queryBar={'none' as TableQueryBarType}
        dragRow={!disabled}
        // @ts-ignore
        rowDragRender={{
          renderClone: ({
            provided,
            snapshot,
            key,
            // @ts-ignore
            record,
            // @ts-ignore
            column,
          }) => (
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
          ),
        }}
        // rowDragRender
        //   rowDragRender={{
        //     // droppableProps: DroppableProps;
        //     draggableProps: {
        //       children:(provided: DraggableProvided,
        //         snapshot: DraggableStateSnapshot,
        //         rubric:DraggableRubric)=>(<div {....provided.}></div>)
        //     // renderClone: (dragRenderProps: DragRenderClone) => ReactElement<any>;
        //     // renderIcon: (DragIconRender: any) => ReactElement<any>;
        // }}
        border={false}
      >
        <Column name="fieldName" renderer={renderFieldName} />
        <Column name="defaultValue" />
        <Column name="require" width={70} renderer={renderCheckBox} />
        <Column name="edit" width={130} renderer={renderCheckBox} />
        <Column name="create" width={130} renderer={renderCheckBox} />
      </Table>
    </div>
  );
};
export default memo(observer(SortTable),
  (prevProps: Readonly<PropsWithChildren<Props>>,
    nextProps: Readonly<PropsWithChildren<Props>>) => {
    if (prevProps.disabled !== nextProps.disabled) {
      return false;
    }

    return false;
  });
