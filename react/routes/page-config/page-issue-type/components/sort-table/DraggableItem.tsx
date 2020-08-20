/* eslint-disable no-param-reassign */
import React from 'react';
import {
  DraggableProvided, Draggable, DraggingStyle, NotDraggingStyle,
} from 'react-beautiful-dnd';
import classnames from 'classnames';
import { Menu, Modal } from 'choerodon-ui';
import {
  Button, IconPicker, Icon, Output, CheckBox, Tooltip, DataSet,
} from 'choerodon-ui/pro/lib';
import { RenderProps } from 'choerodon-ui/pro/lib/field/FormField';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import { observer } from 'mobx-react-lite';
import moment from 'moment';
import TableDropMenu from '@/common/TableDropMenu';
import { usePageIssueTypeStore } from '../../stores';
import { PageIssueTypeStoreStatusCode } from '../../stores/PageIssueTypeStore';
import { useSortTableContext } from './stores';

// import CheckBox from './components/Checkbox';

interface Props {
  data: Record,
  provided: DraggableProvided,
  virtualizedStyle?: React.CSSProperties,
  draggingClassName?: string,
  isDragDisabled?: boolean,
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
  const pageConfigFieldEdited = data?.get('pageConfigFieldEdited') || {};
  const {
    requiredFieldCanNotEdit = false,
    createdFieldCanNotEdit = false,
    editedFieldCanNotEdit = false,
  } = pageConfigFieldEdited;
  // 是否禁止删除此字段 1.系统字段不可删除  2. 项目层下组织层字段不可删除
  const disabledDel = data?.get('pageConfigFieldEdited');
  const renderFieldName = ({ value, record, dataSet }: RenderProps) => (
    <div className={`${prefixCls}-text`}>

      <TableDropMenu
        menu={(
          <Menu onClick={() => onClickDel(record!, dataSet!)}>
            <Menu.Item>删除</Menu.Item>
          </Menu>
        )}
        text={(
          <>
            {!isDragDisabled && <Icon type="baseline-drag_indicator" className={`${prefixCls}-text-icon`} />}
            <span>{value}</span>
          </>
        )}
        isHasMenu={!disabledDel && showSplitLine}
      />
    </div>
  );

  function renderCheckBox({
    name, record, dataSet,
  }: RenderProps, editDisabled?: boolean) {
    return (
      <CheckBox
        disabled={isDragDisabled || editDisabled}
        checked={record?.get(name)}
        // record={record}
        // name={name!}
        // value={value}
        onChange={(val) => {
          record?.set(name as String, val);
        }}
      />
    );
    // return <input type="checkbox" checked={value} />;
  }
  const handleDelete = (record: Record, dataSet: DataSet) => {
    onDelete && onDelete(record?.toData());
    pageIssueTypeStore.addDeleteRecord(record!);
    dataSet?.remove(record!);
  };
  function onClickDel(record: Record, dataSet: DataSet) {
    Modal.confirm({
      title: `是否删除【${record?.get('fieldName')}】字段`,
      onOk: handleDelete.bind(this, record, dataSet),
    });
  }
  const renderAction = ({
    name, record, dataSet,
  }: RenderProps, editDisabled?: boolean) => (
    <div className={`${prefixCls}-action`}>
      {renderCheckBox({
        name, record, dataSet,
      }, editDisabled)}
      {
          (!disabledDel && !showSplitLine && record?.get('createdLevel') !== 'organization')
          && (
            <Button
              className={`${prefixCls}-action-button`}
              disabled={isDragDisabled}
              style={{ marginLeft: 10 }}
              onClick={() => onClickDel(record!, dataSet!)}
            >
              <Icon type="delete" style={{ fontSize: 18 }} />
            </Button>
          )
        }

    </div>
  );
  const getStyle = (draggableStyle: DraggingStyle | NotDraggingStyle | undefined) => ({
    ...draggableStyle,
    ...virtualizedStyle,
    cursor: 'all-scroll',
  });
  const transformDefaultValue = (fieldType: string, defaultValue: any) => {
    if (!defaultValue || defaultValue === '') {
      return defaultValue;
    }
    switch (fieldType) {
      case 'datetime':
        return moment(defaultValue).format('YYYY-MM-DD hh:mm:ss');
      case 'time':
        return moment(defaultValue).format('hh:mm:ss');
      case 'date':
        return moment(defaultValue).format('YYYY-MM-DD');
      default:
        return defaultValue;
    }
  };
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
        {renderFieldName({ value: data.get('fieldName'), record: data, dataSet: data.dataSet })}
      </div>
      <Tooltip title={transformDefaultValue(data.get('fieldType'), data.get('defaultValue'))} placement="top">
        <div className={`${prefixCls}-item ${prefixCls}-item-text`}>
          {transformDefaultValue(data.get('fieldType'), data.get('defaultValue'))}
        </div>
      </Tooltip>
      <div className={`${prefixCls}-item`}>
        {renderCheckBox({ record: data, name: 'required', dataSet: data.dataSet }, requiredFieldCanNotEdit)}
      </div>
      <div className={`${prefixCls}-item`}>
        {renderCheckBox({ record: data, name: 'edited', dataSet: data.dataSet }, createdFieldCanNotEdit)}
      </div>
      <div className={`${prefixCls}-item`}>
        {renderAction({ record: data, name: 'created', dataSet: data.dataSet }, editedFieldCanNotEdit)}
      </div>
    </div>

  );
};
export default observer(DraggableItem);
