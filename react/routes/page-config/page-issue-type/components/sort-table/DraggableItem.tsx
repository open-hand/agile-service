/* eslint-disable no-param-reassign */
import React from 'react';
import {
  DraggableProvided, Draggable, DraggingStyle, NotDraggingStyle,
} from 'react-beautiful-dnd';
import classnames from 'classnames';
import {
  Button, IconPicker, Icon, Output, CheckBox, Tooltip, DataSet,
} from 'choerodon-ui/pro/lib';
import { RenderProps } from 'choerodon-ui/pro/lib/field/FormField';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import { observer } from 'mobx-react-lite';
import { Modal } from 'choerodon-ui';
import { usePageIssueTypeStore } from '../../stores';
import { PageIssueTypeStoreStatusCode } from '../../stores/PageIssueTypeStore';
import { useSortTableContext } from './stores';

// import CheckBox from './components/Checkbox';

interface Props {
  data: Record,
  provided: DraggableProvided,
  virtualizedStyle?: React.CSSProperties,
  draggingClassName?: string,
  isDragDisabled?: boolean
}
const prefixCls = 'c7n-page-issue-detail-drag';
const updateCodeArr = [
  PageIssueTypeStoreStatusCode.add,
  PageIssueTypeStoreStatusCode.del,
  PageIssueTypeStoreStatusCode.desc,
];
const DraggableItem: React.FC<Props> = ({
  data, isDragDisabled, virtualizedStyle, provided, draggingClassName,
}) => {
  const { pageIssueTypeStore } = usePageIssueTypeStore();
  const { onDelete, showSplitLine } = useSortTableContext();
  const renderFieldName = ({ value }: RenderProps) => (
    <div className={`${prefixCls}-text`}>
      {!isDragDisabled && <Icon type="baseline-drag_indicator" className={`${prefixCls}-text-icon`} />}
      <Tooltip title={value} placement="top">
        <span>{value}</span>
      </Tooltip>
    </div>
  );

  function renderCheckBox({
    name, record, dataSet,
  }: RenderProps) {
    return (
      <CheckBox
        disabled={isDragDisabled}
        checked={record?.get(name)}
        // record={record}
        // name={name!}
        // value={value}
        onChange={(val) => {
          record?.set(name as String, val);
          // if (dataSet?.dirty
          //   && updateCodeArr.every((item) => item !== pageIssueTypeStore.getDataStatusCode)) {
          //   pageIssueTypeStore.setDataStatusCode(PageIssueTypeStoreStatusCode.update);
          // } else if (!dataSet?.dirty
          //   && pageIssueTypeStore.dataStatusCode === PageIssueTypeStoreStatusCode.update) {
          //   pageIssueTypeStore.setDataStatusCode(PageIssueTypeStoreStatusCode.null);
          // }
        }}
      />
    );
    // return <input type="checkbox" checked={value} />;
  }
  const handleDelete = (record: Record, dataSet: DataSet) => {
    onDelete && onDelete(record?.toData());
    console.log('re', record);
    pageIssueTypeStore.addDeleteRecord(record!);
    dataSet?.remove(record!);
  };
  const renderAction = ({
    name, record, dataSet,
  }: RenderProps) => (
    <div className={`${prefixCls}-action`}>
      {renderCheckBox({
        name, record, dataSet,
      })}
      <Button
        className={`${prefixCls}-action-button`}
        disabled={isDragDisabled}
        style={{ marginLeft: 10 }}
        onClick={() => {
          Modal.confirm({
            title: `是否删除【${record?.get('fieldName')}】字段`,
            onOk: handleDelete.bind(this, record, dataSet),
          });

          // dataSet?.splice(index, 1);
        }}
      >
        <Icon type="delete" style={{ fontSize: 18 }} />
      </Button>
    </div>
  );
  const getStyle = (draggableStyle: DraggingStyle | NotDraggingStyle | undefined) => ({
    ...draggableStyle,
    ...virtualizedStyle,
  });
  return (

    <div
      role="none"
      ref={provided.innerRef}
      {...provided.draggableProps}
      {...provided.dragHandleProps}
      style={getStyle(provided.draggableProps.style)}
      className={classnames(`${prefixCls}`, { [`${prefixCls}-split`]: showSplitLine }, draggingClassName)}
      onClick={(e) => { }}
    >
      <div className={`${prefixCls}-item`}>
        {renderFieldName({ value: data.get('fieldName') })}
      </div>
      <Tooltip title={data.get('defaultValue')} placement="top">
        <div className={`${prefixCls}-item ${prefixCls}-item-text`}>
          {data.get('defaultValue')}
        </div>
      </Tooltip>

      <div className={`${prefixCls}-item`}>
        {renderCheckBox({ record: data, name: 'required', dataSet: data.dataSet })}
      </div>
      <div className={`${prefixCls}-item`}>
        {renderCheckBox({ record: data, name: 'edited', dataSet: data.dataSet })}
      </div>
      <div className={`${prefixCls}-item`}>
        {renderAction({ record: data, name: 'created', dataSet: data.dataSet })}
      </div>
    </div>

  );
};
export default observer(DraggableItem);
