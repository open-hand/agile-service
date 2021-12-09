import React, { useCallback, useMemo } from 'react';
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
import { usePersistFn } from 'ahooks';
import { noop } from 'lodash';
import TableDropMenu from '@/components/table-drop-menu';
import CheckBox from '@/components/check-box';
import { usePageIssueTypeStore } from '../../stores';
import { useSortTableContext } from './stores';

import { disabledEditDefaultFields, orgDisabledEditDefaultFields } from '../../utils';
// import CheckBox from './components/Checkbox';

interface Props {
  data: Record,
  provided: any,
  virtualizedStyle?: React.CSSProperties,
  draggingClassName?: string,
  isDragDisabled?: boolean,
  renderColumns?: (columns: React.ReactElement[]) => React.ReactNode
}

const DraggableIBaseItem: React.FC<Props> = ({
  data, isDragDisabled, virtualizedStyle, provided, draggingClassName, renderColumns: propsRenderColumns,
}) => {
  const { pageIssueTypeStore } = usePageIssueTypeStore();
  const {
    onDelete: propsOnDelete, showSplitLine /** 组织层与项目层都使用表格线 */, isProject, prefixCls: originPrefixCls,
  } = useSortTableContext();
  const renderColumns = useMemo(() => propsRenderColumns || ((c: any, ...other: any[]) => c), [propsRenderColumns]);
  const onDelete = usePersistFn(propsOnDelete || noop);
  const prefixCls = `${originPrefixCls}-drag`;
  const pageConfigFieldEdited = data?.get('pageConfigFieldEdited') || {};
  const {
    requiredFieldCanNotEdit = false,
    createdFieldCanNotEdit = false,
    editedFieldCanNotEdit = false,
  } = pageConfigFieldEdited;
  const fieldName = data.get('fieldName');
  // 是否禁止删除此字段 1.系统字段不可删除  2. 项目层下组织层字段不可删除 3.禁用工作项类型字段不可操作
  const disabledDel = !pageIssueTypeStore.currentIssueType.enabled || (isProject && data.get('createdLevel') === 'organization') || data.get('createdLevel') === 'system';
  const disabledEdit = useMemo(() => {
    if (!pageIssueTypeStore.currentIssueType.enabled || (!isProject && data.get('createdLevel') === 'system' && orgDisabledEditDefaultFields.includes(data.get('fieldCode')))) {
      return true;
    }

    return false;
  }, [data, isProject, pageIssueTypeStore.currentIssueType.enabled]);
  const handleDelete = useCallback(() => {
    Modal.open({
      title: '确认删除？',
      children: `确认删除【${fieldName}】字段吗？这里仅删除字段和当前工作项类型的关联关系，不会删除这个字段的数据或值。`,
      onOk: () => {
        const recordData = data.toData();
        onDelete(data.toData());
        // 非本地提交数据删除 对其记录，方便后续添加已有字段时恢复数据
        if (!recordData.local) {
          pageIssueTypeStore.addDeleteRecord(data);
        }
        data.dataSet?.remove(data);
      },
    });
  }, [data, fieldName, onDelete, pageIssueTypeStore]);

  const renderFieldName = useCallback(() => (
    <div className={classnames(`${prefixCls}-text`, { [`${prefixCls}-text-edit`]: !disabledEdit })}>
      <TableDropMenu
        menuData={[{
          action: handleDelete,
          text: '删除',
        }]}
        text={(
          <>
            {!isDragDisabled && <Icon type="baseline-drag_indicator" className={`${prefixCls}-text-icon`} />}
            <span>{fieldName}</span>
          </>
        )}
        showMenu={false}
      />
    </div>
  ), [disabledEdit, fieldName, handleDelete, isDragDisabled, prefixCls]);
  const handleMouseEnterTooltip = usePersistFn((e:any) => {
    Tooltip.show(e.target, {
      title: '该组织字段选项值被使用，不可删除',
    });
  });
  const handleMouseLeaveTooltip = usePersistFn((e:any) => {
    Tooltip.hide();
  });
  const renderCheckBox = useCallback((name: string, editDisabled?: boolean) => (
    <CheckBox
      disabled={isDragDisabled || editDisabled}
      checked={data.get(name)}
      record={data}
      name={name}
      onChange={(val) => {
        data.set(name as String, val);
      }}
    />
  ), [data, isDragDisabled]);

  // @ts-ignore
  const getStyle = (draggableStyle: DraggingStyle | NotDraggingStyle | undefined) => ({
    ...draggableStyle,
    ...virtualizedStyle,
    cursor: isDragDisabled ? 'auto' : 'all-scroll',
  });

  return (
    <div
      role="none"
      ref={provided.innerRef}
      {...provided.draggableProps}
      style={getStyle(provided.draggableProps.style)}
      className={classnames(`${prefixCls}`, { [`${prefixCls}-split`]: showSplitLine }, draggingClassName)}
    >
      {renderColumns([
        <div className={`${prefixCls}-item`} {...provided.dragHandleProps}>
          {renderFieldName()}
        </div>,
        <div className={`${prefixCls}-item`} {...provided.dragHandleProps}>
          {renderCheckBox('required', !pageIssueTypeStore.currentIssueType.enabled || requiredFieldCanNotEdit)}
        </div>,
        <div className={`${prefixCls}-item`} {...provided.dragHandleProps}>
          {renderCheckBox('edited', !pageIssueTypeStore.currentIssueType.enabled || editedFieldCanNotEdit)}
        </div>,
        <div className={`${prefixCls}-item`} {...provided.dragHandleProps}>
          <div className={`${prefixCls}-action`}>
            {renderCheckBox('created', !pageIssueTypeStore.currentIssueType.enabled || createdFieldCanNotEdit)}
            {
              (!disabledDel)
              && (
                <div className={`${prefixCls}-action-button-wrap`}>
                  <span />
                  <Button
                    className={classnames(`${prefixCls}-action-button`, { [`${prefixCls}-action-button-disabled`]: isDragDisabled })}
                    style={{ opacity: showSplitLine ? 1 : undefined }}
                    icon="delete_sweep-o"
                    onMouseEnter={data.get('instanceCount') ? handleMouseEnterTooltip : undefined}
                    onMouseLeave={handleMouseLeaveTooltip}
                    disabled={data.get('instanceCount')}
                    size={'small' as any}
                    onClick={handleDelete}
                  />
                </div>
              )
            }
          </div>
        </div>])}

    </div>

  );
};
export default observer(DraggableIBaseItem);
