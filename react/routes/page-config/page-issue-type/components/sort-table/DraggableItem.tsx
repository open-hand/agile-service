/* eslint-disable no-param-reassign */
import React, {
  useMemo, ReactElement, useEffect, memo, useState, PropsWithChildren, CSSProperties,
} from 'react';
import {
  DraggableProvided, Draggable, DraggingStyle, NotDraggingStyle,
} from 'react-beautiful-dnd';
import {
  Button, IconPicker, Icon, Output, CheckBox,
} from 'choerodon-ui/pro/lib';
import { RenderProps } from 'choerodon-ui/pro/lib/field/FormField';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import { observer } from 'mobx-react-lite';
import { usePageIssueTypeStore } from '../../stores';
import { PageIssueTypeStoreStatusCode } from '../../stores/PageIssueTypeStore';
import { useSortTableContext } from './stores';
// import CheckBox from './components/Checkbox';

interface Props {
  data: Record,
  provided: DraggableProvided,
  virtualizedStyle?: React.CSSProperties,
  index: number,
  isDragDisabled?: boolean
}
const prefixCls = 'c7n-page-issue-detail-drag';
const updateCodeArr = [
  PageIssueTypeStoreStatusCode.add,
  PageIssueTypeStoreStatusCode.del,
  PageIssueTypeStoreStatusCode.desc,
  PageIssueTypeStoreStatusCode.drag,
];
const DraggableItem: React.FC<Props> = ({
  data, isDragDisabled, virtualizedStyle, provided, index,
}) => {
  const { pageIssueTypeStore } = usePageIssueTypeStore();
  const { onDelete } = useSortTableContext();
  const renderFieldName = ({ value }: RenderProps) => (
    <div className={`${prefixCls}-text`}>
      {!isDragDisabled && <Icon type="baseline-drag_indicator" className={`${prefixCls}-text-icon`} />}
      <span>{value}</span>
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
          // console.log('dataSet?.dirty', dataSet?.dirty);

          if (dataSet?.dirty
            && updateCodeArr.every((item) => item !== pageIssueTypeStore.getDataStatusCode)) {
            // setDataStatus('update');
            pageIssueTypeStore.setDataStatusCode(PageIssueTypeStoreStatusCode.update);
          } else if (!dataSet?.dirty
            && pageIssueTypeStore.dataStatusCode === PageIssueTypeStoreStatusCode.update) {
            // setDataStatus('ready');
            pageIssueTypeStore.setDataStatusCode(PageIssueTypeStoreStatusCode.null);
          }
        }}
      />
    );
    // return <input type="checkbox" checked={value} />;
  }
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
          onDelete && onDelete(record?.toData());
            dataSet?.splice(index, 1);
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
      className={`${prefixCls}`}
      onClick={(e) => { }}
    >
      <div className={`${prefixCls}-item`}>
        {renderFieldName({ value: data.get('fieldName') })}
      </div>
      <div className={`${prefixCls}-item`}>
        {data.get('defaultValue')}
      </div>

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
