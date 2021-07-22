import React, { useCallback } from 'react';
import {
  DraggingStyle, NotDraggingStyle,
} from 'react-beautiful-dnd';
import classnames from 'classnames';
import {
  Icon, Tooltip, DataSet, Modal, Button,
} from 'choerodon-ui/pro';
import { RenderProps } from 'choerodon-ui/pro/lib/field/FormField';
import Record from 'choerodon-ui/pro/lib/data-set/Record';

import { Observer, observer } from 'mobx-react-lite';
import TableDropMenu from '@/components/table-drop-menu';
import CheckBox from '@/components/check-box';
import TextEditToggle from '@/components/TextEditTogglePro';
import { usePageIssueTypeStore } from '../../stores';
import { useSortTableContext } from './stores';
import useTextEditTogglePropsWithPage from './useTextEditToggle';
// import CheckBox from './components/Checkbox';

interface Props {
  data: Record,
  provided: any,
  virtualizedStyle?: React.CSSProperties,
  draggingClassName?: string,
  isDragDisabled?: boolean,
}
function SpanPlaceholder({ fieldType }: { fieldType: string }) {
  let placeholder = '请选择';
  if (['input', 'number', 'text'].includes(fieldType)) {
    placeholder = '请输入';
  }
  return <span style={{ color: 'rgba(0,0,0,0.6)', fontStyle: 'italic' }}>{placeholder}</span>;
}
const DraggableItem: React.FC<Props> = ({
  data, isDragDisabled, virtualizedStyle, provided, draggingClassName,
}) => {
  const { pageIssueTypeStore } = usePageIssueTypeStore();
  const {
    onDelete, showSplitLine /** 组织层与项目层都使用表格线 */, isProject, prefixCls: originPrefixCls,
  } = useSortTableContext();
  const prefixCls = `${originPrefixCls}-drag`;
  const pageConfigFieldEdited = data?.get('pageConfigFieldEdited') || {};
  const {
    requiredFieldCanNotEdit = false,
    createdFieldCanNotEdit = false,
    editedFieldCanNotEdit = false,
  } = pageConfigFieldEdited;
  // 是否禁止删除此字段 1.系统字段不可删除  2. 项目层下组织层字段不可删除 3.禁用问题类型字段不可操作
  const disabledDel = !pageIssueTypeStore.currentIssueType.enabled || !!data?.get('pageConfigFieldEdited') || data.get('createdLevel') === 'system';
  const textEditToggleProps = useTextEditTogglePropsWithPage(data, isProject, { className: `${prefixCls}-item-defaultValue`, disabled: !pageIssueTypeStore.currentIssueType.enabled });

  const renderFieldName = ({ value, record, dataSet }: RenderProps) => (
    <div className={classnames(`${prefixCls}-text`, { [`${prefixCls}-text-edit`]: !textEditToggleProps?.disabled })}>
      <TableDropMenu
        menuData={[{ action: () => onClickDel(record!, dataSet!), text: '删除' }]}
        text={(
          <>
            {!isDragDisabled && <Icon type="baseline-drag_indicator" className={`${prefixCls}-text-icon`} />}
            <span>{value}</span>
          </>
        )}
        showMenu={false}
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
    Modal.open({
      title: '确认删除？',
      children: `确认删除【${record?.get('fieldName')}】字段吗？这里仅删除字段和当前问题类型的关联关系，不会删除这个字段的数据或值。`,
      onOk: () => handleDelete(record, dataSet),
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
        (!disabledDel)
        && (
          <div className={`${prefixCls}-action-button-wrap`}>
            <span />
            <Button
              className={classnames(`${prefixCls}-action-button`, { [`${prefixCls}-action-button-disabled`]: isDragDisabled })}
              style={{ opacity: showSplitLine ? 1 : undefined }}
              icon="delete_sweep-o"
              size={'small' as any}
              onClick={() => onClickDel(record!, dataSet!)}
            />
          </div>
        )
      }

    </div>
  );
  // @ts-ignore
  const getStyle = (draggableStyle: DraggingStyle | NotDraggingStyle | undefined) => ({
    ...draggableStyle,
    ...virtualizedStyle,
    cursor: isDragDisabled ? 'auto' : 'all-scroll',
  });

  const renderDefaultValue = useCallback(() => (!textEditToggleProps ? <span>项目层</span> : (
    <TextEditToggle
      {...textEditToggleProps}
    >

      <Observer>
        {() => (
          <Tooltip title={data.get('showDefaultValueText') !== '' ? data.get('showDefaultValueText') : undefined}>
            <span className={`${prefixCls}-item-defaultValue-text`}>
              {(!textEditToggleProps?.disabled && (!data.get('showDefaultValueText') || data.get('showDefaultValueText') === '') ? <SpanPlaceholder fieldType={data.get('fieldType')} /> : data.get('showDefaultValueText') || '')}
            </span>
          </Tooltip>
        )}
      </Observer>
    </TextEditToggle>

  )), [data, prefixCls, textEditToggleProps]);
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
        {renderCheckBox({ record: data, name: 'required', dataSet: data.dataSet }, !pageIssueTypeStore.currentIssueType.enabled || requiredFieldCanNotEdit)}
      </div>
      <div className={`${prefixCls}-item`} {...provided.dragHandleProps}>
        {renderCheckBox({ record: data, name: 'edited', dataSet: data.dataSet }, !pageIssueTypeStore.currentIssueType.enabled || editedFieldCanNotEdit)}
      </div>
      <div className={`${prefixCls}-item`} {...provided.dragHandleProps}>
        {renderAction({ record: data, name: 'created', dataSet: data.dataSet }, !pageIssueTypeStore.currentIssueType.enabled || createdFieldCanNotEdit)}
      </div>
    </div>

  );
};
export default observer(DraggableItem);
