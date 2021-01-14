/* eslint-disable no-param-reassign */
import React, { useCallback } from 'react';
import {
  DraggableProvided, DraggingStyle, NotDraggingStyle,
} from 'react-beautiful-dnd';
import classnames from 'classnames';
import { Menu, Modal } from 'choerodon-ui';
import {
  Button, Icon, Tooltip, DataSet, Select,
} from 'choerodon-ui/pro/lib';
import { RenderProps } from 'choerodon-ui/pro/lib/field/FormField';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import { Observer, observer } from 'mobx-react-lite';
import moment from 'moment';
import TableDropMenu from '@/common/TableDropMenu';
import CheckBox from '@/components/check-box';
import TextEditToggle from '@/components/TextEditTogglePro';
import { usePageIssueTypeStore } from '../../stores';
import { useSortTableContext } from './stores';
import useTextEditTogglePropsWithPage from './useTextEditToggle';
// import CheckBox from './components/Checkbox';

interface Props {
  data: Record,
  provided: DraggableProvided,
  virtualizedStyle?: React.CSSProperties,
  draggingClassName?: string,
  isDragDisabled?: boolean,
}

const DraggableItem: React.FC<Props> = ({
  data, isDragDisabled, virtualizedStyle, provided, draggingClassName,
}) => {
  const { pageIssueTypeStore } = usePageIssueTypeStore();
  const { onDelete, showSplitLine /** 显示则代表时组织层 */, prefixCls: originPrefixCls } = useSortTableContext();
  const prefixCls = `${originPrefixCls}-drag`;
  const pageConfigFieldEdited = data?.get('pageConfigFieldEdited') || {};
  const {
    requiredFieldCanNotEdit = false,
    createdFieldCanNotEdit = false,
    editedFieldCanNotEdit = false,
  } = pageConfigFieldEdited;
  // 是否禁止删除此字段 1.系统字段不可删除  2. 项目层下组织层字段不可删除
  const disabledDel = !!data?.get('pageConfigFieldEdited') || data.get('createdLevel') === 'system';
  const textEditToggleProps = useTextEditTogglePropsWithPage(data, !showSplitLine, { className: `${prefixCls}-item-defaultValue` });

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
        record={record}
        name={name!}
        // value={value}
        onChange={(val) => {
          record?.set(name as String, val);
        }}
      />
    );
    // return <input type="checkbox" checked={value} />;
  }
  const handleDelete = (record: Record, dataSet: DataSet) => {
    const recordData = record.toData();
    onDelete && onDelete(recordData);
    // 非本地提交数据删除 对其记录，方便后续添加已有字段时恢复数据
    if (!recordData.local) {
      pageIssueTypeStore.addDeleteRecord(record!);
    }

    dataSet?.remove(record!);
  };
  function onClickDel(record: Record, dataSet: DataSet) {
    Modal.confirm({
      title: `删除【${record?.get('fieldName')}】字段`,
      content: `确认删除【${record?.get('fieldName')}】字段吗？删除后会从当前问题类型移除此字段，并且字段数据会清空。`,
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

  const renderDefaultValue = useCallback(() => (
    <TextEditToggle
      {...textEditToggleProps}
    >
      <Observer>
        {() => data.get('showDefaultValueText') || ''}
      </Observer>
    </TextEditToggle>

  ), [data, textEditToggleProps]);

  return (

    <div
      role="none"
      ref={provided.innerRef}
      {...provided.draggableProps}
      style={getStyle(provided.draggableProps.style)}
      className={classnames(`${prefixCls}`, { [`${prefixCls}-split`]: showSplitLine }, draggingClassName)}
    >
      <div className={`${prefixCls}-item`} {...provided.dragHandleProps}>
        {renderFieldName({ value: data.get('fieldName'), record: data, dataSet: data.dataSet })}
      </div>
      <div
        role="none"
        className={`${prefixCls}-item ${prefixCls}-item-text`}
      >
        {renderDefaultValue()}
      </div>
      <div className={`${prefixCls}-item`} {...provided.dragHandleProps}>
        {renderCheckBox({ record: data, name: 'required', dataSet: data.dataSet }, requiredFieldCanNotEdit)}
      </div>
      <div className={`${prefixCls}-item`} {...provided.dragHandleProps}>
        {renderCheckBox({ record: data, name: 'edited', dataSet: data.dataSet }, createdFieldCanNotEdit)}
      </div>
      <div className={`${prefixCls}-item`} {...provided.dragHandleProps}>
        {renderAction({ record: data, name: 'created', dataSet: data.dataSet }, editedFieldCanNotEdit)}
      </div>
    </div>

  );
};
export default observer(DraggableItem);
