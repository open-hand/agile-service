import React, {
  useMemo, ReactElement, useEffect, memo,
} from 'react';
import { observer } from 'mobx-react-lite';
import {
  Table, DataSet, Icon, CheckBox,
} from 'choerodon-ui/pro/lib';
import { TableQueryBarType } from 'choerodon-ui/pro/lib/table/enum';
import { RenderProps } from 'choerodon-ui/pro/lib/field/FormField';
import OldSortTable from '../../../page/components/SortTable';
import './index.less';
import { usePageIssueTypeStore } from '../../stores';

const { Column } = Table;
const SortTable: React.FC<{ type: string, disabled: boolean | undefined }> = (
  { type, disabled, children },
) => {
  const { sortTableDataSet } = usePageIssueTypeStore();

  useEffect(() => {
    console.log(type, 'disabled:', disabled);
  }, [disabled, type]);
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
      },
      {
        title: '默认值',
        dataIndex: 'defaultValue',
        // width: '25%',
      },
      {
        title: '是否必填',
        dataIndex: 'require',
        width: '75px',
      },
      {
        title: '是否加入到编辑页',
        dataIndex: 'eidt',
        width: '135px',
        render: (value:any) => (
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
        title: '是否加入到创建页',
        dataIndex: 'create',
        width: '135px',
      },
    ];
  };
  return (
    <div className="c7n-page-issue-detail">
      {/* <OldSortTable
        pagination={false}
        columns={getColumns()}
        dataSource={sortTableDataSet.toData().slice()}
        filterBar={false}
        handleDrag={() => { }}
        hight={0}
      /> */}
      <Table
        dataSet={sortTableDataSet}
        queryBar={'none' as TableQueryBarType}
        dragRow={!disabled}
        //   rowDragRender={{
        //     droppableProps: DroppableProps;
        //     draggableProps: DraggableProps;
        //     renderClone: (dragRenderProps: DragRenderClone) => ReactElement<any>;
        //     renderIcon: (DragIconRender: any) => ReactElement<any>;
        // }}
        border={false}
      >
        <Column name="fieldName" renderer={renderFieldName} />
        <Column name="defaultValue" />
        <Column name="require" width={75} renderer={renderCheckBox} />
        <Column name="edit" width={135} renderer={renderCheckBox} />
        <Column name="create" width={135} renderer={renderCheckBox} />
      </Table>
    </div>
  );
};
export default memo(observer(SortTable));
